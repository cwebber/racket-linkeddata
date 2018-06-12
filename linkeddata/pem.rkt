#lang racket

(require net/base64 crypto)

(module+ test
  (require rackunit crypto/libcrypto)
  (define rsa-impl (get-pk 'rsa libcrypto-factory))
  (when (eq? (crypto-factories) '())
    (crypto-factories (list libcrypto-factory))))

(define (base64-encode-with-header bstr header)
  (string-append "-----BEGIN " header "-----\r\n"
                 (bytes->string/utf-8
                  (base64-encode bstr))
                 "-----END " header "-----\r\n"))

(define (private-key->pem pk-key)
  "Convert private key to PEM encoded string."
  (base64-encode-with-header (pk-key->datum pk-key 'PrivateKeyInfo)
                             "PRIVATE KEY"))

(define (public-key->pem pk-key)
  "Convert public key to PEM encoded string.
Will use only the public key portion if PK-KEY contains both."
  (base64-encode-with-header (pk-key->datum pk-key 'SubjectPublicKeyInfo)
                             "PUBLIC KEY"))

(define pem-rx
  #px"^-----BEGIN ([[:alnum:] ]+)-----([[:alnum:][:space:]+/=]+)-----END [[:alnum:] ]+-----")

(define (decode-delimited-base64 str)
  (define str-str
    (match str
      ([? bytes?] (bytes->string/utf-8 str))
      ([? string?] str)))
  (define-values (type b64-str)
    (match (regexp-match pem-rx str-str)
      [(list _ type b64-str) (values type b64-str)]))
  (values type
          (base64-decode (string->bytes/utf-8 b64-str))))

(define (pem->private-key pem-key)
  "Transform PEM encoded PEM-KEY to a private key"
  (let-values ([(_ pubkey-b64)
                (decode-delimited-base64 pem-key)])
    (datum->pk-key pubkey-b64 'PrivateKeyInfo)))

(define (pem->public-key pem-key)
  "Transform PEM encoded PEM-KEY to a public key"
  (let-values ([(_ pubkey-b64)
                (decode-delimited-base64 pem-key)])
    (datum->pk-key pubkey-b64 'SubjectPublicKeyInfo)))

(module+ test
  (define privkey (generate-private-key rsa-impl '((nbits 512))))
  (test-equal?
   "Assert that exporting private key there and back again from PEM
results in the same key material"
   (private-key->pem privkey)
   (private-key->pem (pem->private-key (private-key->pem privkey)))))
