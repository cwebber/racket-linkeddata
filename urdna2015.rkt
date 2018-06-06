#lang racket

(require "rdf.rkt"
         "n-quads.rkt"
         crypto)

(define (hash-sha256 obj)
  (digest 'sha256 obj))

(struct issuer
  (prefix
   [counter #:mutable]
   ;; Maybe this is... an alist of (identifier . bnode) ...?
   [issued #:mutable]))

(define (copy-issuer issuer-to-copy)
  (issuer (issuer-prefix issuer-to-copy)
          (issuer-counter issuer-to-copy)
          (issuer-issued issuer-to-copy)))

(define (issue-identifier issuer identifier)
  (match (assoc identifier (issuer-issued issuer))
    [(cons _ val)
     val]
    [#f
     (define issued-identifier
       (string-append (issuer-prefix issuer)
                      (number->string (issuer-counter ))))
     (set-issuer-issued!
      issuer (cons (cons identifier issued-identifier)
                   (issuer-issued issuer)))
     (set-issuer-counter! issuer (+ (issuer-counter issuer) 1))
     issued-identifier]))

(define (issuer-identifiers-in-order issuer)
  (foldl
   (match-lambda*
     [(list (cons key val) prev) (cons val prev)])
   '()
   (issuer-issued issuer)))

(define (issuer-has-id? issuer identifier)
  (assoc identifier (issuer-issued issuer)))

(define (issuer-ref issuer identifier)
  (match (assoc identifier (issuer-issued issuer))
    [(cons key val) val]
    [#f #f]))

(struct c14n-state
  (blank-to-quads hash-to-blanks canonical-issuer))

(define (make-issuer prefix)
  (issuer prefix 0 '()))

;; @@: We're using lists but we're storing in reverse order...
;;   maybe that's a problem.
(define (initial-c14n-state)
  (c14n-state (make-hash) (make-hash)
              (make-issuer "_:c14n")))

(define-syntax-rule (while condition body1 bodyn ...)
  (let lp ()
    (if condition
        (begin body1 bodyn ...
               (lp))
        (void))))

(module+ test
  (require rackunit)
  (let ([x 0])
    (test-equal?
     "while works"
     (with-output-to-string
       (lambda ()
         (while (< x 10)
           (display x)
           (set! x (+ x 1)))))
     "0123456789")))

(define (maybe-add-to-hash-list! hash key val)
  (define cur-list
    (hash-ref hash key '()))
  (unless (member val cur-list)
    (hash-set! hash key
               (cons val cur-list))))

(define (normalization-algorithm input-dataset)
  (define c14n-state
    (initial-c14n-state))
  (define blank-to-quads
    (c14n-state-blank-to-quads c14n-state))
  (define hash-to-blanks
    (c14n-state-hash-to-blanks c14n-state))
  (define (issued-identifier? identifier)
    (issuer-has-id? (c14n-state-canonical-issuer c14n-state) identifier))
  ;; 2
  (for ([quad input-dataset])
    ;; 2.1
    (for ([blank-node (filter blank-node? quad)])
      (maybe-add-to-hash-list! blank-to-quads blank-node quad)))
  ;; 3
  (define non-normalized-identifiers
    (apply mutable-set (hash-keys blank-to-quads)))
  ;; 4
  (define simple #t)
  ;; 5
  (while simple
    ;; 5.1
    (set! simple #f)
    ;; 5.2
    (hash-clear! hash-to-blanks)
    ;; 5.3
    (for ([identifier non-normalized-identifiers])
      ;; 5.3.1
      (define hash
        (hash-first-degree-quads c14n-state identifier))
      ;; 5.3.2
      (maybe-add-to-hash-list! blank-to-quads blank-node quad))
    ;; 5.4
    (for ([hash (sort (hash-keys hash-to-blanks) string<?)])
      (define identifier-list
        (hash-ref hash-to-blanks hash))
      (match identifier-list
        [(list identifier)
         ;; 5.4.2
         ;; This does return an identifier, but we don't use it.
         ;; I guess the real point here is the ordering but I'm not sure.
         (issue-identifier (c14n-state-canonical-issuer c14n-state) identifier)
         ;; 5.4.3
         (set-remove! non-normalized-identifiers identifier)
         ;; 5.4.4
         (hash-remove! hash-to-blanks hash)
         ;; 5.4.5
         (set! simple #t)]
        ;; 5.4.1
        [_ 'continue]))
    ;; 6
    (for ([hash (sort (hash-keys hash-to-blanks) string<?)])
      (define identifier-list
        (hash-ref hash-to-blanks hash))
      ;; 6.1
      (define hash-path-list
        (for/fold ([hash-path-list '()])
            ([identifier identifier-list])
          (if (issued-identifier? identifier)
              ;; 6.2.1
              hash-path-list
              (let (;; 6.2.2
                    [temporary-issuer
                     (make-issuer "_:b")])
                (issue-identifier temporary-issuer identifier)
                (cons (hash-n-degree-quads c14n-state temporary-issuer)
                      hash-path-list)))))
      ;; 6.3
      (for ([result (sort (lambda (item1 item2)
                            (string<? (ndq-result-hash item1)
                                      (ndq-result-hash item2)))
                          hash-path-list)])
        ;; 6.3.1
        (for ([existing-identifier
               ;; .... I think this is right...?
               (issuer-identifiers-in-order (ndq-result-issuer result))])
          (issue-identifier (c14n-state-canonical-issuer c14n-state)
                            existing-identifier))))
    ;; 7
    (for/fold ([normalized-dataset '()]
               #:result (reverse normalized-dataset))
        ([this-quad input-dataset])
      (define (maybe-replace field)
        (if (blank-node? field)
            (issue-identifier field)
            field))
      (cons (quad (maybe-replace (get-subject this-quad))
                  (maybe-replace (get-predicate this-quad))
                  (maybe-replace (get-object this-quad))
                  (maybe-replace (get-graph this-quad)))
            normalized-dataset))))

(define (hash-first-degree-quads c14n-state reference-bnode-identifier
                                 #:hash-func [hash-func hash-sha256])
  (define (maybe-replace-bnode obj)
    (cond
     [(not (blank-node? obj))
      obj]
     [(equal? obj reference-bnode-identifier)
      "_:a"]
     [else "_:b"]))
  (define nquads
    (for/fold ([nquads '()]
               #:result (sort nquads string<?))
        ([this-quad (hash-ref (c14n-state-blank-to-quads c14n-state)
                              reference-bnode-identifier)])
      (define adjusted-quad
        (quad (maybe-replace-bnode (get-subject this-quad))
              (maybe-replace-bnode (get-predicate this-quad))
              (maybe-replace-bnode (get-object this-quad))
              (maybe-replace-bnode (get-graph this-quad))))
      (cons (write-nquad adjusted-quad)
            nquads)))
  (hash-func (apply string-append nquads)))

(define (hash-related-blank-node c14n-state related quad issuer position
                                 #:hash-func [hash-func hash-sha256])
  (define canonical-issuer
    (c14n-state-canonical-issuer c14n-state))
  (define identifier
    (cond
     [(issuer-has-id? canonical-issuer related)
      (issuer-ref canonical-issuer related)]
     [(issuer-has-id? issuer related)
      (issuer-ref issuer related)]
     [else (hash-first-degree-quads c14n-state related)]))
  (define input
    (string-append position
                   (if (not (equal? position "g"))
                       (string-append "<" (get-predicate quad) ">")
                       "")
                   identifier))
  (hash-func input))

(define (rcompose . funcs)
  "Compose, but in reverse"
  (apply compose (reverse funcs)))

(struct ndq-result
  (issuer hash))

(define (hash-n-degree-quads c14n-state identifier issuer
                             #:hash-func [hash-func hash-sha256])
  (define c14n-issuer
    (c14n-state-canonical-issuer c14n-state))
  (define hash-to-related
    (for/fold ([hash-to-related #hash()])
        ([quad (hash-ref (c14n-state-blank-to-quads c14n-state)
                         identifier)])
      (define (handle-quad-component position component)
        (lambda (hash-to-related)
          (if (and (blank-node? component)
                   (not (equal? component identifier)))
              (let* ([hash (hash-related-blank-node c14n-state quad
                                                    issuer position)]
                     [cur-hash-lst (hash-ref hash-to-related hash '())])
                (hash-set hash-to-related hash (cons component cur-hash-lst)))
              hash-to-related)))
      (define handle-all
       (rcompose
        (handle-quad-component "s" (get-subject quad))
        (handle-quad-component "o" (get-object quad))
        (handle-quad-component "g" (get-graph quad))))
      (handle-all hash-to-related)))
  (define data-to-hash "")
  (define (append-to-dth! str)
    (set! data-to-hash
          (string-append data-to-hash str)))
  ;; 5
  (for ([related-hash (sort (hash-keys hash-to-related) string<?)])
    (define blank-node-list
      ;; in reverse because... cons
      (reverse (hash-ref hash-to-related related-hash)))
    ;; 5.1
    (append-to-dth! related-hash)
    (define chosen-path "")
    (define chosen-issuer 'undefined)
    ;; 5.4
    (for ([permutation blank-node-list])
      (call/ec
       (lambda (permutation-continue)
         (define issuer-copy
           (copy-issuer issuer))
         (define path "")
         (define recursion-list '())
         ;; 5.4.4
         (for ([related permutation])
           ;; 5.4.4.1
           (if (issuer-has-id? c14n-issuer related)
               (set! path (string-append path related))
               ;; 5.4.4.2
               (begin
                 ;; 5.4.4.2.1
                 (when (not (issuer-has-id? issuer-copy related))
                   (set! recursion-list (cons related recursion-list)))
                 ;; 5.4.4.2.2
                 (set! path
                       (string-append
                        path (issue-identifier issuer-copy related)))))
           ;; 5.4.4.3
           (when (and (not (equal? chosen-path ""))
                      (>= (string-length path)
                          (string-length chosen-path))
                      (string>? path chosen-path))
             (permutation-continue (void))))
         ;; 5.4.5
         (for ([related (reverse recursion-list)])
           ;; 5.4.5.1
           (define result
             (hash-n-degree-quads c14n-state related issuer-copy))
           ;; 5.4.5.2
           (set! path
                 (string-append
                  path (issue-identifier issuer-copy related)))
           ;; 5.4.5.3
           (set! path
                 (string-append
                  path "<" (ndq-result-hash result) ">"))
           ;; 5.4.5.4
           (set! issuer-copy (ndq-result-issuer result))
           ;; 5.4.5.5
           (when (and (not (equal? chosen-path ""))
                      (>= (string-length path)
                          (string-length chosen-path))
                      (string>? path chosen-path))
             (permutation-continue (void))))
         ;; 5.4.6
         (when (or (equal? chosen-path "")
                   (string<? path chosen-path))
           (set! chosen-path path)
           (set! chosen-issuer issuer-copy)))))
    ;; 5.5
    (set! data-to-hash
          (string-append data-to-hash chosen-path))
    ;; 5.6
    (set! issuer chosen-issuer))
  ;; 6
  (ndq-result issuer
              (hash-func data-to-hash)))

