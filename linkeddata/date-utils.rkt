#lang racket

;; TODO: We should probably replace this with Gregor.

(define (http-date-str date)
  "Format a date rfc3339 style.

Note that this doesn't handle timezones at present... it just treats
everything as UTC.  Perhaps that should be fixed."
  (define (2-pad d)
    (~a d #:min-width 2 #:align 'right #:pad-string "0"))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year date)
          (2-pad (date-month date))
          (2-pad (date-day date))
          (2-pad (date-hour date))
          (2-pad (date-minute date))
          (2-pad (date-second date))))

(provide http-date-str)
