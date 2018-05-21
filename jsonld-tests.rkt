#lang racket

(require json
         rackunit
         "json-ld.rkt")

(define (read-tests-json-file filename)
  (call-with-input-file (string-append "jsonld-test-suite/" filename)
    read-json))

(define compact-manifest
  (read-tests-json-file "compact-manifest.jsonld"))

(define expand-manifest
  (read-tests-json-file "expand-manifest.jsonld"))


(define (run-compact-test test #:catch-exceptions? [catch-exceptions? #t])
  (define name (hash-ref test 'name))
  (define input
    (read-tests-json-file (hash-ref test 'input)))
  (define context
    (hash-ref (read-tests-json-file (hash-ref test 'context)) '@context))
  (define expect
    (read-tests-json-file (hash-ref test 'expect)))
  (define options
    (hash-ref test 'option '#hasheq()))
  (define check-name
    (format "Compact: ~a" name))
  (define (run-test)
    (check-equal?
     (compact-jsonld input context
                     #:compact-arrays? (hash-ref options 'compactArrays #t)
                     #:base-iri (string-append
                                 (hash-ref compact-manifest 'baseIri)
                                 (hash-ref test 'input)))
     expect))
  (display (format "~a\n  purpose: ~a\n  input: jsonld-test-suite/~a\n  context: jsonld-test-suite/~a\n  expect: jsonld-test-suite/~a\n"
                   check-name
                   (hash-ref test 'purpose "(not supplied)")
                   (hash-ref test 'input)
                   (hash-ref test 'context)
                   (hash-ref test 'expect)))
  (if catch-exceptions?
      (check-not-exn run-test check-name)
      (run-test)))

(define (run-compact-tests #:catch-exceptions? [catch-exceptions? #t])
  (for-each (lambda (test)
              (run-compact-test test #:catch-exceptions? catch-exceptions?))
            (hash-ref compact-manifest 'sequence)))

(module+ main
  (run-compact-tests))


(define (run-expand-test test #:catch-exceptions? [catch-exceptions? #t])
  (define name (hash-ref test 'name))
  (define input
    (read-tests-json-file (hash-ref test 'input)))
  (define expect
    (read-tests-json-file (hash-ref test 'expect)))
  (define options
    (hash-ref test 'option '#hasheq()))
  (define check-name
    (format "Expand: ~a" name))
  (define (run-test)
    (check-equal?
     ;; FIXME: Add other options from options
     (expand-jsonld input
                    #:base-iri (string-append
                                (hash-ref expand-manifest 'baseIri)
                                (hash-ref test 'input)))
     expect))
  (display (format "~a\n  purpose: ~a\n  input: jsonld-test-suite/~a\n  expect: jsonld-test-suite/~a\n"
                   check-name
                   (hash-ref test 'purpose "(not supplied)")
                   (hash-ref test 'input)
                   (hash-ref test 'expect)))
  (if catch-exceptions?
      (check-not-exn run-test check-name)
      (run-test)))

(define (run-expand-tests #:catch-exceptions? [catch-exceptions? #t])
  (for-each (lambda (test)
              (run-expand-test test #:catch-exceptions? catch-exceptions?))
            (hash-ref expand-manifest 'sequence)))

(module+ main
  (run-expand-tests))
