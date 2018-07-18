#lang racket

;;; TODO: Generalize into ld-proofs.rkt

(require linkeddata/json-ld
         linkeddata/n-quads
         linkeddata/date-utils
         crypto
         json
         net/base64
         linkeddata/pem)

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
(define-values (sec:publicKeyPem sec:publicKeyPem-sym)
  (sec-term "publicKeyPem"))
(define-values (sec:signatureValue sec:signatureValue-sym)
  (sec-term "signatureValue"))
(define-values (sec:proofValue sec:proofValue-sym)
  (sec-term "proofValue"))
(define-values (sec:nonce sec:nonce-sym)
  (sec-term "nonce"))
(define-values (sec:domain sec:domain-sym)
  (sec-term "domain"))
(define-values (sec:LinkedDataSignature2018 sec:LinkedDataSignature2018-sym)
  (sec-term "LinkedDataSignature2018"))

(define dc-vocab-url "http://purl.org/dc/terms/")
(define dc-term (term-maker dc-vocab-url))

(define-values (dc:creator dc:creator-sym)
  (dc-term "creator"))
(define-values (dc:created dc:created-sym)
  (dc-term "created"))

(define security-context
  (call-with-input-file (build-path "contexts" "security.jsonld")
    read-json))

(define (not-implemented)
  (error "Not implemented"))

(define proof-purpose%
  (class object%))

(define suite-interface
  (interface ()
    ;; -> string?
    suite-uri
    ;; args: doc
    ;;       hasheq? -> string?
    canonicalize
    ;; args: expanded-doc private-key  sig-options
    ;;       jsobj?           private-key? hasheq? -> jsobj?
    make-proof-object
    ;; args: canonicalized-doc creator proof
    ;;       jsobj?            jsobj?  jsobj? -> boolean?
    verify-proof))

(define cwebber-signature-2018-suite
  (new
   (class object%
     (super-new)
     (define/public (suite-uri)
       "https://dustycloud.org/#CwebberSignature2018")

     (define/public (canonicalize doc)
       (json-ld->urdna2015-nquads-string doc))

     ;; you know... we *have to* attach the proof object anyway, because we
     ;; have to do so to do the signature with the proof partly attached anyway
     ;; (before the proof's signature is added)
     ;; FIXME: We have to add proofPurpose specific behavior here
     ;; FIXME: We need to support multiple proofs
     (define/public (make-proof-object expanded-doc private-key sig-options
                                       #;proofPurpose)
       (define proof-obj
         `#hasheq((@type . ,(suite-uri))))
       ;; Add created field, defaulting to today
       (set! proof-obj
             (hash-set proof-obj dc:created-sym
                       (or (hash-ref sig-options dc:created-sym #f)
                           (http-date-str (seconds->date (current-seconds) #f)))))
       (define (maybe-add-to-proof! options-key set-key)
         (when (hash-has-key? sig-options options-key)
           (set! proof-obj (hash-set proof-obj set-key
                                     (hash-ref sig-options options-key)))))
       ;; Add creator/nonce/domain fields, if appropriate
       (maybe-add-to-proof! 'creator dc:creator-sym)
       (maybe-add-to-proof! 'nonce sec:nonce-sym)
       (maybe-add-to-proof! 'domain sec:domain-sym)

       ;; Prepare to get signature value.  We need to simulate adding the proof
       ;; to the document without the signature field
       ;; NOTE: This is the place where we differ from the rsa 2015 algorithm
       ;;   in that we dont' use create-verify-hash
       (define tbs
         (canonicalize (hash-set expanded-doc sec:proof-sym proof-obj)))
       (define signature-value
         (bytes->string/utf-8 (base64-encode (digest/sign private-key 'sha256 tbs))))

       ;; Attach the signature to the proof
       (set! proof-obj
             (hash-set proof-obj sec:signatureValue-sym
                       signature-value))

       ;; Return proof
       proof-obj)

     ;; Should this be verify proofs, or verify proof?
     ;; We need to verify each proof individually against a suite that's
     ;; available, right?
     ;; Note that we need to canonicalize the doc *as this proof is expected to check it*
     ;; at this stage.  That means modifying the proof section before normalization
     ;; appropriately.
     (define/public (verify-proof canonicalized-doc creator proof #;expectedProofPurpose)
       ;; TODO: Iterate through all keys until we find the right one?
       ;; (define pubkey-field
       ;;   (car (hash-ref creator sec:publicKey-sym)))
       (define pubkey-pem
         (hash-ref (car (hash-ref creator sec:publicKeyPem-sym)) '@value))
       (define pubkey
         (pem->public-key pubkey-pem))
       (define sig-value
         (match (hash-ref proof sec:signatureValue-sym)
           [(list (? hash? sv))
            (base64-decode (string->bytes/utf-8 (hash-ref sv '@value)))]))
       (digest/verify pubkey 'sha256 canonicalized-doc sig-value)))))

;; See https://github.com/w3c-dvcg/ld-signatures/issues/19
#;(define (lds-sign-quads document sig-options private-key
                        [suite rsa-signature-2018-suite])
  "Sign a list of n-quads."
  (lds-sign-main '(TODO: frame here?)
                 (suite-canonicalize-quads suite document)
                 sig-options private-key))

(define (lds-sign-jsonld document sig-options private-key
                         #:suite [suite cwebber-signature-2018-suite]
                         #:legacy-signature-field? [legacy-signature-field? #f])
  ;; Expand the document and attach the proof
  (define expanded-document
    (match (expand-jsonld document)
      [(list expanded) expanded]))

  ;; Generate the proof document off the canonicalized document
  (define proof-object
    (send suite make-proof-object
          expanded-document private-key sig-options))

  (define pre-compacted-output
    (hash-set expanded-document sec:proof-sym proof-object))

  ;; Compact the signed document using the context from the original document
  (compact-jsonld pre-compacted-output (hash-ref document '@context #hasheq())))

(provide lds-sign-jsonld)

;; TODO: This is very 2015, and isn't as general as it may appear
#;(define (create-verify-hash canonicalized-document suite sig-options)
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
            
            (send suite canonicalize-jsonld suite options))]
         ;; 4.2: Hash canonicalized options document using the message digest
         ;; algorithm (e.g. SHA-256) and set output to the result.
         ;; 4.3: Hash canonicalized document using the message digest algorithm
         ;; (e.g. SHA-256) and append it to output.
         [output
          (bytes-append (send suite hash suite canonicalized-options)
                        (send suite hash suite canonicalized-document))])

    ;; 5: has a note: It is presumed that the 64-byte output will be used in a
    ;; signing algorithm that includes its own hashing algorithm, such as RS256
    ;; (RSA + SHA-256) or EdDsa (Ed25519 which uses SHA-512).
    ;; ^--- response to that spectext: Yeah, we use digest/sign
    output))

;; FIXME: We shouldn't select the suite ourselves... we should be
;;   querying a registry of suites depending on the proof type
(define (lds-verify-jsonld signed-document suite sig-options
                           #:fetch-jsonld [fetch-jsonld http-get-jsonld])
  "Returns a boolean identifying whether the signature succeeded or failed.
If any object, such as the key or etc is unable to be retrieved, this will
raise an exception instead."
  (call/ec
   (lambda (return)
     (define expanded
       (car (expand-jsonld signed-document)))
     (define proof-key
       (cond [(hash-has-key? expanded sec:proof-sym)
              sec:proof-sym]
             [(hash-has-key? expanded sec:signature-sym)
              sec:signature-sym]
             [else (error "Missing proof/signature field")]))
     (for ([proof-node (hash-ref expanded proof-key)])
       ;; Fetch "creator" field, which is really the public key that signed this
       (define creator
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
       ;; FIXME: I really think owner is broken, though Manu and Dave
       ;; aren't convinced:
       ;;   https://github.com/w3c-dvcg/ld-signatures/issues/20
       #;(verify-owner creator)

       ;; The spec text doesn't say this.  What we're doing here is
       ;; removing the signature field from the proof node and re-attaching it
       ;; to the document.
       ;; FIXME: However, that may not be totally right.  The 2012/2015 algorithms
       ;; hashed the documents separately IIRC.
       (define document-to-canonicalize
         (let ([proof-node-without-sig
                (hash-remove proof-node sec:signatureValue-sym)])
           (hash-set expanded proof-key
                     proof-node-without-sig)))
       (define canonicalized-document
         (send suite canonicalize document-to-canonicalize))

       ;; 5. Create a value tbv that represents the data to be verified, and set
       ;; it to the result of running the Create Verify Hash Algorithm, passing
       ;; the information in signature.
       #;(define tbv
       (create-verify-hash canonicalized-document suite proof-node))

       ;; If this node isn't valid, then we return #f.
       (when (not (send suite verify-proof canonicalized-document creator proof-node))
         (return #f)))
     #t)))

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
