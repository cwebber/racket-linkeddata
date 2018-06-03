#lang racket

(require parser-tools/lex
         (prefix-in sre- parser-tools/lex-sre)
         "rdf.rkt")

;; TODO: redo this with megaparsack or etc?
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
                (sre-* (sre-: #\-
                              (sre-+ (sre-or (char-range #\a #\z)
                                             (char-range #\A #\Z)
                                             (char-range #\0 #\9))))))))

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
(define-lex-abbrev blank-node-label-abbrev
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

(define echars-map
  #hash(("\\t" . #\tab)
        ("\\b" . #\backspace)
        ("\\n" . #\newline)
        ("\\r" . #\return)
        ("\\f" . #\page)
        ("\\\"" . #\")
        ("\\\'" . #\')
        ("\\\\" . #\\)))

(define rev-echars-map
  (for/fold ([map #hasheq()])
      ([(key val) echars-map])
    (hash-set map val key)))

(define (lex-nquads in-port)
  (letrec ([strip-endchars
            (lambda (str)
              (substring str 1
                         (- (string-length str) 1)))]
           [string-lexer
            (lexer
             ;; most characters
             [(sre-+ (char-complement (sre-or #\" #\\ #\newline #\return)))
              (append (string->list lexeme)
                      (string-lexer input-port))]
             ;; escape characters
             [echar
              (cons (hash-ref echars-map lexeme)
                    (string-lexer input-port))]
             ;; unicode "uchars"
             [uchar
              (cons (integer->char (string->number (string-append "#x" (substring lexeme 2))))
                    (string-lexer input-port))]
             [#\" '()])]
           [read-string
            (lambda (port)
              (list->string (string-lexer port)))]
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
             [blank-node-label-abbrev (cons (blank-node (substring lexeme 2))
                                     (this-lexer input-port))]
             [#\"
              (cons (add-literal-lang-or-type-tag
                     (read-string input-port)
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

(provide read-nquads)

(module+ test
  (require rackunit)
  (define example-nquads
    "<http://example.com/Subj1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.com/Type> .
<http://example.com/Subj1> <http://example.com/prop1> <http://example.com/Obj1> .
<http://example.com/Subj1> <http://example.com/prop2> \"Plain\" .
<http://example.com/Subj1> <http://example.com/prop2> \"2012-05-12\"^^<http://www.w3.org/2001/XMLSchema#date> .
<http://example.com/Subj1> <http://example.com/prop2> \"English\"@en .
_:b0 <http://example.com/prop1> <http://example.com/Obj1> .
<http://example.com/Subj1> <http://example.com/prop1> _:b1 .
<http://example.com/Subj1> <http://example.com/prop1> <http://example.com/Obj1> <http://example.com/a-graph/> .
<http://example.com/Subj1> <http://example.com/prop1> <http://example.com/Obj1> _:b3 .")

  (test-equal?
   "Test output of read-nquads"
   (read-nquads (open-input-string example-nquads))
   (list
    (triple
     "http://example.com/Subj1"
     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
     "http://example.com/Type")
    (triple
     "http://example.com/Subj1"
     "http://example.com/prop1"
     "http://example.com/Obj1")
    (triple
     "http://example.com/Subj1"
     "http://example.com/prop2"
     (literal "Plain" "http://www.w3.org/2001/XMLSchema#string" #f))
    (triple
     "http://example.com/Subj1"
     "http://example.com/prop2"
     (literal "2012-05-12" "http://www.w3.org/2001/XMLSchema#date" #f))
    (triple
     "http://example.com/Subj1"
     "http://example.com/prop2"
     (literal
      "English"
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
      "en"))
    (triple
     (blank-node "b0")
     "http://example.com/prop1"
     "http://example.com/Obj1")
    (triple
     "http://example.com/Subj1"
     "http://example.com/prop1"
     (blank-node "b1"))
    (quad
     "http://example.com/Subj1"
     "http://example.com/prop1"
     "http://example.com/Obj1"
     "http://example.com/a-graph/")
    (quad
     "http://example.com/Subj1"
     "http://example.com/prop1"
     "http://example.com/Obj1"
     (blank-node "b3")))))

(define (nquads-list->dataset nquads)
  (for/fold ([dataset `#hash((#f . ,(set)))])
      ([triple-or-quad nquads])
    (define this-triple
      (if (quad? triple-or-quad)
          (triple (get-subject triple-or-quad)
                  (get-predicate triple-or-quad)
                  (get-object triple-or-quad))
          triple-or-quad))
    (define graph-label
      (if (quad? triple-or-quad)
          (get-graph triple-or-quad)
          #f))
    (define dataset-graph
      (hash-ref dataset graph-label (set)))
    (hash-set dataset graph-label (set-add dataset-graph this-triple))))

(define (read-nquads-dataset in-port)
  (nquads-list->dataset (read-nquads in-port)))

(provide nquads-list->dataset read-nquads-dataset)

(module+ test
  (test-equal?
   "read-nquads-dataset works"
   (read-nquads-dataset (open-input-string example-nquads))
   (make-immutable-hash
    (list
     (cons (blank-node "b3")
           (set (triple
                 "http://example.com/Subj1"
                 "http://example.com/prop1"
                 "http://example.com/Obj1")))
     (cons "http://example.com/a-graph/"
           (set
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop1"
             "http://example.com/Obj1")))
     (cons #f
           (set
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop2"
             (literal
              "English"
              "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
              "en"))
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop2"
             (literal "Plain" "http://www.w3.org/2001/XMLSchema#string" #f))
            (triple
             "http://example.com/Subj1"
             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
             "http://example.com/Type")
            (triple
             (blank-node "b0")
             "http://example.com/prop1"
             "http://example.com/Obj1")
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop2"
             (literal "2012-05-12" "http://www.w3.org/2001/XMLSchema#date" #f))
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop1"
             "http://example.com/Obj1")
            (triple
             "http://example.com/Subj1"
             "http://example.com/prop1"
             (blank-node "b1"))))))))

(define (echar-char? char)
  (hash-has-key? rev-echars-map char))

(define (char->uchar-str char)
  (define char-as-hex
    (format "~x" (char->integer char)))
  (define (str-of-n-char n char)
    (list->string (for/list ([i (in-range n)]) char)))
  (if (> (string-length char-as-hex) 4)
      (string-append "\\U"
                     (str-of-n-char (- 8 (string-length char-as-hex))
                                    #\0)
                     char-as-hex)
      (string-append "\\u"
                     (str-of-n-char (- 4 (string-length char-as-hex))
                                    #\0)
                     char-as-hex)))

(define (char-in-range? char char-min char-max)
  (and (char-ci>=? char char-min)
       (char-ci<=? char char-max)))

(define (write-nquad quad port)
  (define (write-string str)
    (write-char #\" port)
    (for ([char str])
      (match char
        ;; I'm just copying what jsonld.py does here.  This hardly seems
        ;; formally defined enough, though.
        [#\\ (display "\\\\" port)]
        [#\tab (display "\\t" port)]
        [#\newline (display "\\n" port)]
        [#\return (display "\\r" port)]
        [#\" (display "\\\"" port)]
        [_ (write-char char port)]))
    (write-char #\" port))
  (define (write-literal literal)
    (write-string (literal-lexical-form literal))
    (cond
     ;; xsd:string
     [(and (equal? (literal-datatype-iri literal)
                   xsd:string)
           (not (literal-language-tag literal)))
      (void)] ; no-op  if it's a string
     ;; rdf:langString
     [(and (equal? (literal-datatype-iri literal)
                   rdf:langString)
           (literal-language-tag literal))
      (write-char #\@ port)
      (for ([char (literal-language-tag literal)])
        (unless (or (eq? char #\-)
                    (char-in-range? char #\a #\z)
                    (char-in-range? char #\A #\Z)
                    (char-in-range? char #\0 #\9))
          (error "Invalid character for literal language tag" char))
        (write-char char port))]
     ;; any other literal tag
     [(not (literal-language-tag literal))
      (display "^^" port)
      (write-iri (literal-datatype-iri literal))]
     ;; guess it had a literal language tag though it wasn't a literal datatype
     [else (error "malformed literal")]))
  (define (write-blank-node bnode)
    (display "_:" port)
    (for ([char (blank-node-label bnode)])
      (if (char-whitespace? char)
          ;; TODO: I think this should be enough to protect against
          ;;   escape attacks, but really we should be using the same
          ;;   structure from the lexer/parser as a predicate to check
          ;;   if this is legitimate.  Sadly the default racket lexer/parser
          ;;   is not very composable / reusable as predicates
          (error "Blank node must not have whitespace characters")
          (write-char char port))))
  (define (write-iri iri)
    (write-char #\< port)
    (for ([char iri])
      (match char
        [(? echar-char?)
         (display (hash-ref rev-echars-map char) port)]
        [(? (lambda (c)
              (or (char<=? c #\u0020)
                  (member c '(#\< #\> #\" #\{ #\} #\| #\` #\\)))))
         (display (char->uchar-str char) port)]
        [_ (write-char char port)]))
    (write-char #\> port))
  (define (write-component obj)
    (match obj
      [(? string?)
       (write-iri obj)]
      [(? blank-node?)
       (write-blank-node obj)]
      [(? literal?)
       (write-literal obj)]))
  (define (write-triple obj)
    (write-component (get-subject obj))
    (display " " port)
    (write-component (get-predicate obj))
    (display " " port)
    (write-component (get-object obj))
    (display " ." port))
  (define (write-quad obj)
    (write-component (get-subject obj))
    (display " " port)
    (write-component (get-predicate obj))
    (display " " port)
    (write-component (get-object obj))
    (when (get-graph obj)
      (write-component (get-graph obj)))
    (display " ." port))
  (match quad
    [(? quad?)
     (write-quad quad)]
    [(? triple? triple)
     (write-triple triple)]))

(define (nquad->string quad)
  'TODO)

(define (write-nquads quads port)
  'TODO)

(define (nquads->string quads port)
  'TODO)

(provide write-nquad nquad->string write-nquads nquads->string)
