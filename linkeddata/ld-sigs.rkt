#lang racket

;;; TODO: Generalize into ld-proofs.rkt

(require linkeddata/json-ld
         linkeddata/n-quads
         linkeddata/date-utils
         crypto
         json
         net/base64)

(define security-context-url "https://w3id.org/security/v1")
(define sec-vocab-url "https://w3id.org/security#")
(define (sec-term term)
  (string-append sec-vocab-url term))

(define sec:proof
  (sec-term "proof"))
(define sec:signature
  (sec-term "signature"))

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

(define (simple-signature-object-maker context type)
  (lambda (signature-value sig-options)
    (let* ([result (hash-set sig-options 'signatureValue signature-value)]
           [result (hash-set result '@type type)]
           [result (hash-set result '@context context)])
      result)))

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
   (simple-signature-object-maker "https://w3id.org/security/v1"
                                  "LinkedDataSignature2018")
   ;; verify-proc
   'TODO))

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
                                    (string->symbol sec:signature)
                                    (string->symbol sec:proof))
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


