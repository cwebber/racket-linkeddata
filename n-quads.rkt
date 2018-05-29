#lang racket

(require parser-tools/lex
         (prefix-in sre- parser-tools/lex-sre)
         "rdf.rkt")

;; Productions for terminals

;;   HEX            ::=  [0-9] | [A-F] | [a-f]
(define-lex-abbrev hex
  (sre-or (char-range #\0 #\9)
          (char-range #\A #\F)
          (char-range #\a #\f)))

;;   UCHAR          ::=  '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
(define-lex-abbrev uchar
  (sre-or (sre-: "\\u" hex hex hex hex)
          (sre-: "\\U" hex hex hex hex hex hex hex hex)))
;;   ECHAR          ::=  '\' [tbnrf"'\]
(define-lex-abbrev echar
  (sre-: #\\ (sre-or #\t #\b #\n #\r #\f #\" #\' #\\)))

;;   LANGTAG        ::=  '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*

(define-lex-abbrev langtag
  (sre-: #\@
         (sre-+ (sre-or (char-range #\a #\z)
                        (char-range #\A #\Z))
                (sre-* (sre-+ (sre-or (char-range #\a #\z)
                                      (char-range #\A #\Z)
                                      (char-range #\0 #\9)))))))
;;   EOL            ::=  [#xD#xA]+
(define-lex-abbrev eol
  (sre-+ (sre-or #\newline #\return)))

;;   IRIREF         ::=  '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
(define-lex-abbrev iriref
  (sre-: #\<
         (sre-* (sre-or
                 (char-complement (sre-or (char-range #\u00 #\u20)
                                          #\< #\>
                                          #\" #\{ #\} #\| #\` #\\))
                 uchar))
         #\>))

;;   STRING_LITERAL_QUOTE  ::=  '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'

(define-lex-abbrev string-literal-quote
  (sre-: #\"
         (sre-* (sre-or (char-complement (sre-or #\" #\\ #\newline #\return))
                        echar uchar))
         #\"))

;;   PN_CHARS_BASE  ::=  [A-Z] | [a-z] | [#x00C0-#x00D6] |
;;                       [#x00D8-#x00F6] |
;;                       [#x00F8-#x02FF] |
;;                       [#x0370-#x037D] |
;;                       [#x037F-#x1FFF] |
;;                       [#x200C-#x200D] |
;;                       [#x2070-#x218F] |
;;                       [#x2C00-#x2FEF] |
;;                       [#x3001-#xD7FF] |
;;                       [#xF900-#xFDCF] |
;;                       [#xFDF0-#xFFFD] |
;;                       [#x10000-#xEFFFF]
(define-lex-abbrev pn-chars-base
  (sre-or (char-range #\a #\z)
          (char-range #\A #\Z)
          (char-range #\u00C0 #\u00D6)
          (char-range #\u00D8 #\u00F6)
          (char-range #\u00F8 #\u02FF)
          (char-range #\u0370 #\u037D)
          (char-range #\u037F #\u1FFF)
          (char-range #\u200C #\u200D)
          (char-range #\u2070 #\u218F)
          (char-range #\u2C00 #\u2FEF)
          (char-range #\u3001 #\uD7FF)
          (char-range #\uF900 #\uFDCF)
          (char-range #\uFDF0 #\uFFFD)
          (char-range #\U00010000 #\U000EFFFF)))

;;   PN_CHARS_U     ::=  PN_CHARS_BASE | '_' | ':'
(define-lex-abbrev pn-chars-u
  (sre-or pn-chars-base
          #\_ #\:))

;;   PN_CHARS       ::=  PN_CHARS_U | '-' | [0-9] | #x00B7 |
;;                       [#x0300-#x036F] |
;;                       [#x203F-#x2040]
(define-lex-abbrev pn-chars
  (sre-or pn-chars-u #\-
          (char-range #\0 #\9)
          #\u00B7
          (char-range #\u0300 #\u036F)
          (char-range #\u203F #\u2040)))

;;   BLANK_NODE_LABEL  ::=  '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
(define-lex-abbrev blank-node-label
  (sre-: "_:"
         (sre-or pn-chars-u (char-range #\0 #\9))
         (sre-? (sre-* (sre-or pn-chars #\.)) pn-chars)))

;;   nquadsDoc  ::=  statement? (EOL statement)* EOL?
;;   statement  ::=  subject predicate object
;;                   graphLabel? '.'
;;   subject    ::=  IRIREF | BLANK_NODE_LABEL
;;   predicate  ::=  IRIREF
;;   object     ::=  IRIREF | BLANK_NODE_LABEL |
;;                   literal
;;   graphLabel ::=  IRIREF | BLANK_NODE_LABEL
;;   literal    ::=  STRING_LITERAL_QUOTE ('^^' IRIREF | LANGTAG)?

(define (lex-nquads in-port)
  (letrec ([strip-endchars
            (lambda (str)
              (substring str 1
                         (- (string-length str) 1)))]
           [string-lexer
            (lexer
             [(sre-* (sre-or (char-complement (sre-or #\" #\\ #\newline #\return))
                             echar uchar))
              (begin
                (unless (eq? (read-char input-port) #\")
                  (error "Missing end quote"))
                lexeme)])]
           ;; we'll already have eaten the @ by now
           [langtag-lexer
            (lexer
             [(sre-:
               (sre-+ (sre-or (char-range #\a #\z)
                              (char-range #\A #\Z)))
               (sre-* (sre-+ (sre-or (char-range #\a #\z)
                                     (char-range #\A #\Z)
                                     (char-range #\0 #\9)))))
              lexeme])]
           [add-literal-lang-or-type-tag
            (lambda (literal-str input-port)
              (match (read-char input-port)
                [#\@ (literal literal-str
                              rdf:langString
                              (langtag-lexer input-port))]
                [#\^
                 (begin
                   (unless (eq? (read-char input-port) #\^)
                     (error "Missing a ^"))
                   (literal literal-str
                            ((lexer
                              [iriref (strip-endchars lexeme)])
                             input-port)
                            #f))]
                [#\space
                 (literal literal-str xsd:string #f)]))]
           [this-lexer
            (lexer
             [eol (cons 'eol
                        (this-lexer input-port))]
             [iriref (cons (strip-endchars lexeme)
                           (this-lexer input-port))]
             [blank-node-label (cons (blank-node lexeme)
                                     (this-lexer input-port))]
             [#\"
              (cons (add-literal-lang-or-type-tag
                     (string-lexer input-port)
                     input-port)
                    (this-lexer input-port))]
             [#\. (cons 'dot
                        (this-lexer input-port))]
             [(sre-+ #\space) (this-lexer input-port)]
             [(eof) '(eof)])])
    (this-lexer in-port)))

(define (parse-nquads lexed-nquads)
  (define (subject/object? obj)
    (or (string? obj) (literal? obj) (blank-node? obj)))
  (define (graph? obj)
    (or (string? obj) (blank-node? obj)))
  (let lp ([lexed lexed-nquads])
    (match lexed
      ;; triple
      [(list (? subject/object? subject)
             (? string? predicate)
             (? subject/object? object)
             'dot rest ...)
       (cons (triple subject predicate object)
             (lp rest))]
      ;; quad
      [(list (? subject/object? subject)
             (? string? predicate)
             (? subject/object? object)
             (? graph? graphlabel)
             'dot rest ...)
       (cons (quad subject predicate object graphlabel)
             (lp rest))]
      ;; skip eol markers
      [(list 'eol rest ...)
       (lp rest)]
      ;; eof?  We're done!
      [(list 'eof) '()])))

(define (read-nquads input-port)
  (parse-nquads (lex-nquads input-port)))
