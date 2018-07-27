#lang racket

;; "peeking" utilities

;; TODO: Move this into its own library

(provide pk pk-values)

(require racket/pretty)

;; kludge: emacs mis-renders this as a string, so...
(define hash-semicolon-semicolon
  (list->string '(#\# #\; #\;)))

;; for debugging
(define (pk . vals)
  "Peek at values for print debugging, but return 'em"
  (display hash-semicolon-semicolon)
  (display " pk\n")
  (pretty-print vals)
  ;; return the last value
  (last vals))

(define-syntax-rule (pk-values print-these ... body)
  ;; Like pk, but supporting multiple value return
  (call-with-values
      (lambda () body)
    (lambda vals
      (display hash-semicolon-semicolon)
      (display " pk-values\n")
      (pretty-print (list print-these ... '*values:* vals))
      (apply values vals))))
