#lang racket

;;; TODO: Generalize into ld-proofs.rkt

(require linkeddata/json-ld
         linkeddata/n-quads
         linkeddata/date-utils
         crypto
         json
         net/base64)

(define (term-maker vocab-url)
  (lambda (term)
    (define str-val
      (string-append vocab-url term))
    (define sym-val
      (string->symbol str-val))
    (values str-val sym-val)))

(define security-context-url "https://w3id.org/security/v1")
(define sec-vocab-url "https://w3id.org/security#")
(define sec-term (term-maker sec-vocab-url))

(define-values (sec:proof sec:proof-sym)
  (sec-term "proof"))
(define-values (sec:signature sec:signature-sym)
  (sec-term "signature"))
(define-values (sec:owner sec:owner-sym)
  (sec-term "owner"))
(define-values (sec:publicKey sec:publicKey-sym)
  (sec-term "publicKey"))
(define-values (sec:signatureValue sec:signatureValue-sym)
  (sec-term "signatureValue"))
(define-values (sec:proofValue sec:proofValue-sym)
  (sec-term "proofValue"))
(define-values (sec:LinkedDataSignature2018 sec:LinkedDataSignature2018-sym)
  (sec-term "LinkedDataSignature2018"))

(define dc-vocab-url "http://purl.org/dc/terms/")
(define dc-term (term-maker dc-vocab-url))

(define-values (dc:creator dc:creator-sym)
  (dc-term "creator"))

(define security-context
  (call-with-input-file (build-path "contexts" "security.jsonld")
    read-json))

(struct suite
  (sig-uri
   canonize-jsonld-proc
   #;canonize-quads-proc
   hash-proc sign-proc
   make-signature-object-proc
   verify-proc))

(define (suite-canonize-jsonld suite json-ld)
  ((suite-canonize-jsonld-proc suite) json-ld))
#;(define (suite-canonize-quads suite quads)
  ((suite-canonize-quads-proc suite) quads))
(define (suite-hash suite obj)
  ((suite-hash-proc suite) obj))
(define (suite-sign suite obj private-key)
  ((suite-sign-proc suite) obj private-key))
(define (suite-verify suite obj)
  ((suite-verify-proc suite) obj))
(define (suite-make-signature-object suite sig-value sig-options)
  ((suite-make-signature-object-proc suite) sig-value sig-options))

(define (simple-signature-object-maker type
                                       #:expand-context
                                       [expand-context security-context-url])
  (lambda (signature-value sig-options)
    (let* ([result (hash-set sig-options sec:signatureValue-sym signature-value)]
           [result (hash-set result '@type type)])
      (expand-jsonld result #:expand-context expand-context))))

(define rsa-signature-2018-suite
  (suite
   "https://w3id.org/security#LinkedDataSignature2018"
   json-ld->urdna2015-nquads-string
   #;canonize-quads
   ;; hash-proc
   (lambda (obj)
     (digest 'sha256 obj))
   ;; sign-proc
   (lambda (obj privkey)
     (digest/sign privkey 'sha256 obj))
   ;; make-signature-object-proc
   (simple-signature-object-maker sec:LinkedDataSignature2018)
   ;; verify-proc
   (lambda (obj proof)
     (define pubkey
       'TODO)
     (digest/verify pubkey 'sha256 obj))))

;; The whole mechanism of signature options both being a dictionary of
;; options that are passed in and also something attached *to the
;; signature json* seems just very... weird to me.

(define (lds-sign-jsonld document sig-options private-key
                         #:suite [suite rsa-signature-2018-suite]
                         #:legacy-signature-field? [legacy-signature-field? #f])
  "Sign a json-ld document."
  (lds-sign-main document (suite-canonize-jsonld suite document)
                 sig-options private-key suite
                 #:legacy-signature-field? legacy-signature-field?))

(provide lds-sign-jsonld)

;; See https://github.com/w3c-dvcg/ld-signatures/issues/19
#;(define (lds-sign-quads document sig-options private-key
                        [suite rsa-signature-2018-suite])
  "Sign a list of n-quads."
  (lds-sign-main '(TODO: frame here?)
                 (suite-canonize-quads suite document)
                 sig-options private-key))

(define (lds-sign-main document canonicalized-document sig-options private-key suite
                       #:legacy-signature-field? [legacy-signature-field? #f])
  ;; 3: Create a value tbs that represents the data to be signed, and set it to
  ;; the result of running the Create Verify Hash Algorithm, passing the
  ;; information in options.
  (define tbs
    (create-verify-hash canonicalized-document suite sig-options))
  ;; 4: Digitally sign tbs using the privateKey and the the digital signature
  ;; algorithm (e.g. JSON Web Signature using RSASSA-PKCS1-v1_5 algorithm). The
  ;; resulting string is the signatureValue.
  (define signature-value
    (bytes->string/utf-8 (base64-encode (suite-sign suite tbs private-key))))

  ;; 5: Add a signature node to output containing a linked data signature using
  ;; the appropriate type and signatureValue values as well as all of the data
  ;; in the signature options (e.g. creator, created, and if given, any
  ;; additional signature options such as nonce and domain).
  ;; TODO: Note, I have no idea how to do this if it's not json right now.
  ;;   It seems we need to know the root of the graph.
  ;;   See https://github.com/w3c-dvcg/ld-signatures/issues/19
  (define expanded-document
    (car (expand-jsonld document)))

  (define pre-compacted-output
    (hash-set expanded-document (if legacy-signature-field?
                                    sec:signature-sym
                                    sec:proof-sym)
              (suite-make-signature-object suite signature-value sig-options)))

  ;; TODO: Compact it again with its original context... and the context of
  ;; the proof?  Or, look again at what pyld_sig is doing here
  ;; IMO if the toplevel context doesn't support the security vocabulary
  ;; that's its problem.

  ;; 6: Return output as the signed linked data document.
  (compact-jsonld pre-compacted-output (hash-ref document '@context #hasheq())))

(define (create-verify-hash canonicalized-document suite sig-options)
  ;; 1: Let options be a copy of input options. 
  ;; 2: If type, id, or signatureValue exists in options, remove the entry. 
  (let* ([options
          (for/fold ([options sig-options])
              ([to-remove '(type @type id @id signatureValue)])
            (if (hash-has-key? options to-remove)
                (hash-remove options to-remove)
                options))]
         ;; 3: If created does not exist in options, add an entry with a value
         ;; that is an ISO8601 combined date and time string containing the
         ;; current date and time accurate to at least one second, in Universal
         ;; Time Code format. For example: 2017-11-13T20:21:34Z.
         [options
          (if (not (hash-has-key? options 'created))
              ;; Note that while not using the best utility here, this should format
              ;; things properly as UTC.
              (hash-set options 'created
                        (http-date-str (seconds->date (current-seconds) #f)))
              options)]
         ;; Add the relevant context
         [options
          (if (not (hash-has-key? options '@context))
              (hash-set options '@context security-context-url)
              (hash-set options '@context
                        (match (hash-ref options '@context)
                          ;; if it's already a list, add to the list
                          [(list context ...)
                           (cons security-context-url context)]
                          ;; if not, make it a list
                          [context
                           (list security-context-url context)])))]
         ;; 4: Generate output by:
         ;; 4.1: Creating a canonicalized options document by canonicalizing
         ;; options according to the canonicalization algorithm (e.g. the
         ;; GCA2015 [RDF-DATASET-NORMALIZATION] algorithm).
         [canonicalized-options
          (parameterize ([context-loader
                          (simple-context-loader
                           #:url-map
                           (make-immutable-hash
                            `((,security-context-url . ,security-context)))
                           ;; Don't load unknown urls, fallback to the
                           ;; fallback loader instead
                           #:fallback-loader (context-loader))])
            (suite-canonize-jsonld suite options))]
         ;; 4.2: Hash canonicalized options document using the message digest
         ;; algorithm (e.g. SHA-256) and set output to the result.
         ;; 4.3: Hash canonicalized document using the message digest algorithm
         ;; (e.g. SHA-256) and append it to output.
         [output
          (bytes-append (suite-hash suite canonicalized-options)
                        (suite-hash suite canonicalized-document))])

    ;; 5: has a note: It is presumed that the 64-byte output will be used in a
    ;; signing algorithm that includes its own hashing algorithm, such as RS256
    ;; (RSA + SHA-256) or EdDsa (Ed25519 which uses SHA-512).
    ;; ^--- response to that spectext: Yeah, we use digest/sign
    output))

(define (lds-verify-jsonld signed-document suite sig-options
                           #:fetch-jsonld [fetch-jsonld http-get-jsonld])
  "Returns a boolean identifying whether the signature succeeded or failed.
If any object, such as the key or etc is unable to be retrieved, this will
raise an exception instead."
  (define expanded
    (car (expand-jsonld signed-document)))
  (define proof-field
    (cond [(hash-has-key? expanded sec:proof-sym)
           sec:proof-sym]
          [(hash-has-key? expanded sec:signature-sym)
           sec:signature-sym]
          [else (error "Missing proof/signature field")]))
  ;; FIXME: Handle multiple proofs
  (define proof-node
    (car (hash-ref expanded proof-field)))
  ;; 1. Get the public key by dereferencing its URL identifier in the
  ;; signature node of the default graph of signed document. Confirm
  ;; that the linked data document that describes the public key
  ;; specifies its owner and that its owner's URL identifier can be
  ;; dereferenced to reveal a bi-directional link back to the
  ;; key. Ensure that the key's owner is a trusted entity before
  ;; proceeding to the next step.
  ;;
  ;; FIXME: I really think owner is broken, though Manu and Dave
  ;; aren't convinced:
  ;;   https://github.com/w3c-dvcg/ld-signatures/issues/20
  (define public-key
    (match (hash-ref proof-node dc:creator-sym 'nothing)
      [(list (? hash? key))
       ;; TODO: we should add an always-fetch-key option
       key]
      [(list (? string? key-uri))
       (match (expand-jsonld (fetch-jsonld key-uri))
         [(list (? hash? key))
          key])]
      [_ (error "Missing or invalid creator field")]))
  ;; Throw an exception if the owner doesn't match
  (verify-owner public-key)
  ;; 2. Let document be a copy of signed document.
  (define document (hash-remove signed-document proof-field))
  ;; 3. Remove any signature nodes from the default graph in document and
  ;; save it as signature.
  (define signature proof-node)
  ;; 4. Generate a canonicalized document by canonicalizing document
  ;; according to the canonicalization algorithm (e.g. the GCA2015
  ;; [RDF-DATASET-NORMALIZATION] algorithm).
  (define canonicalized-document
   (suite-canonize-jsonld suite document))

  ;; 5. Create a value tbv that represents the data to be verified, and set
  ;; it to the result of running the Create Verify Hash Algorithm, passing
  ;; the information in signature.
  (define tbv
    (create-verify-hash canonicalized-document suite signature))

  ;; 6. Pass the signatureValue, tbv, and the public key to the signature
  ;; algorithm (e.g. JSON Web Signature using RSASSA-PKCS1-v1_5
  ;; algorithm). Return the resulting boolean value.
  (suite-verify suite tbv signature))

(define (verify-owner key)
  ;; Now let's make sure that the link is bidirectional.
  ;; Hm... It seems like there are two paths here:
  ;;  - The key is "embedded".
  ;;    In this case I think we want to assume that the key really is
  ;;    that key unless proven otherwise.  So the next steps are:
  ;;    - fetched-key? stays #f
  ;;    - Dereference the owner.  In fact we have to do this regardless
  ;;      of whether the owner is embedded.
  ;;    - Now we look for the key.  There are two valid cases:
  ;;      - The key is embedded.  Make sure the embedded key matches
  ;;        the key we have.  We don't need to fetch the key.
  ;;      - The key is linked to.  Okay great.  If we haven't fetched
  ;;        the key yet, then fetch it and make sure it matches this one.
  ;;        Mark fetched-key? #t
  ;;  - The key is included as an id that should be dereferenced.
  ;;    - Dereference the key.  mark fetched-key? as #t
  ;;    - I think the rest of the steps are the same...
  (define embedded-key #f)   ; should be expanded
  (define fetched-key #f)
  'TODO
  #;(match creator
    ;; It's embedded
    [(? hash?)
     (set! embedded-key creator)
     ;; Next, we need to fetch the owner and expand it.
     ;; In either case, we have to actually retrieve the owner
     ;; this time.
     (define owner-id
       (match (hash-ref key sec:owner-sym 'nothing)
         [(list (? string? owner))
          owner]
         [(list (? hash? owner))
          ;; FIXME: What do we do if the owner doesn't have an id?
          ;;   currently we error out.  It doesn't seem like any sensible
          ;;   behavior is possible under current lds rules
          (hash-ref owner '@id)]
         [_ (error "Missing or invalid creator field")]))
     (define expanded-owner
       (expand-jsonld (fetch-jsonld owner-id)))
     ;; Now we need to fetch the keys... there are two
     ;; ways to find a key.  One is that it's a uri reference
     ;; and the other is that we match the exact object.
     (match (hash-ref expanded-owner sec:publicKey)
       [(? string?)
        
        'TODO]
       [(? hash?)
        'TODO]
       
       )

     ]
    ;; It's a uri that we must dereference
    [(? string?)

     ]
    ))
