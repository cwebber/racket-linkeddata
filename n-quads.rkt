#lang racket

(require data/monad data/applicative data/either
         megaparsack megaparsack/text
         (prefix-in sre- parser-tools/lex-sre)
         "rdf.rkt")

(module+ test
  (require rackunit))

;; Productions for terminals

;;   HEX            ::=  [0-9] | [A-F] | [a-f]
(define hex/p
  (or/p (char-between/p #\0 #\9)
        (char-between/p #\A #\F)
        (char-between/p #\a #\f)))

;;   UCHAR          ::=  '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
(define uchar/p
  (do (char/p #\\)
      [hexchars
       <- (or/p (do (char/p #\u)
                    (many/p hex/p #:min 4 #:max 4))
                (do (char/p #\U)
                    (many/p hex/p #:min 8 #:max 8)))]
      (pure (integer->char
             (string->number
              (string-append "#x" (list->string hexchars)))))))


;;   ECHAR          ::=  '\' [tbnrf"'\]
(define echar/p
  (do (char/p #\\)
      [echar <- (one-of/p '(#\t #\b #\n #\r #\f #\" #\' #\\))]
      (pure
       (match echar
         (#\t #\tab)
         (#\b #\backspace)
         (#\n #\newline)
         (#\r #\return)
         (#\f #\page)
         (#\" #\")
         (#\' #\')
         (#\\ #\\)))))

;;   LANGTAG        ::=  '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
(define langtag/p
  (do (char/p #\@)
      [first-chars <- (many+/p (or/p (char-between/p #\a #\z)
                                     (char-between/p #\A #\Z)))]
      [dash-chars
       <- (many/p (do (char/p #\-)
                      [after-dash
                       <- (many+/p
                           (or/p (char-between/p #\a #\z)
                                 (char-between/p #\A #\Z)
                                 (char-between/p #\0 #\9)))]
                      (pure (cons #\- after-dash))))]
      (pure (list->string (append first-chars (apply append dash-chars))))))

;;   EOL            ::=  [#xD#xA]+
(define eol/p
  (many+/p (one-of/p '(#\newline #\return))))

;;   IRIREF         ::=  '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
(define iriref/p
  (do (char/p #\<)
      [chars
       <- (many/p (or/p
                   (satisfy/p
                    (lambda (c)
                      (not (or (char-in-range? c #\u00 #\u20)
                               (member c '(#\< #\>
                                           #\" #\{ #\} #\| #\` #\\))))))
                   uchar/p))]
      (char/p #\>)
      (pure (list->string chars))))

;;   STRING_LITERAL_QUOTE  ::=  '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
(define string-literal/p
  (do (char/p #\")
      [chars
       <- (many/p (or/p
                   (label/p
                    "plain character that isn't \", \\, \\n, \\r"
                    (satisfy/p
                     (lambda (c)
                       (not (member c '(#\" #\\ #\newline #\return))))))
                   (try/p echar/p) uchar/p))]
      (char/p #\")
      (pure (list->string chars))))

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
(define pn-chars-base/p
  (or/p (char-between/p #\a #\z)
        (char-between/p #\A #\Z)
        (char-between/p #\u00C0 #\u00D6)
        (char-between/p #\u00D8 #\u00F6)
        (char-between/p #\u00F8 #\u02FF)
        (char-between/p #\u0370 #\u037D)
        (char-between/p #\u037F #\u1FFF)
        (char-between/p #\u200C #\u200D)
        (char-between/p #\u2070 #\u218F)
        (char-between/p #\u2C00 #\u2FEF)
        (char-between/p #\u3001 #\uD7FF)
        (char-between/p #\uF900 #\uFDCF)
        (char-between/p #\uFDF0 #\uFFFD)
        (char-between/p #\U00010000 #\U000EFFFF)))

;;   PN_CHARS_U     ::=  PN_CHARS_BASE | '_' | ':'
(define pn-chars-u/p
  (or/p pn-chars-base/p
        (char/p #\_) (char/p #\:)))

;;   PN_CHARS       ::=  PN_CHARS_U | '-' | [0-9] | #x00B7 |
;;                       [#x0300-#x036F] |
;;                       [#x203F-#x2040]
(define pn-chars/p
  (or/p pn-chars-u/p (char/p #\-)
        (char-between/p #\0 #\9)
        (char/p #\u00B7)
        (char-between/p #\u0300 #\u036F)
        (char-between/p #\u203F #\u2040)))

;;   BLANK_NODE_LABEL  ::=  '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
(define blank-node-label/p
  (do (string/p "_:")
      [first-char
       <- (or/p pn-chars-u/p (char-between/p #\0 #\9))]
      [rest-chars
       ;; FIXME: Technically we're allowing this to end with '.' which isn't permited!
       <- (many/p (or/p pn-chars/p (char/p #\.)))]
      (pure (blank-node (list->string (cons first-char rest-chars))))))

;;   literal    ::=  STRING_LITERAL_QUOTE ('^^' IRIREF | LANGTAG)?
(define literal/p
  (do [string-literal
       <- string-literal/p]
      [iri-or-langtag
       <- (many/p
           #:min 0 #:max 1
           (or/p (do (string/p "^^")
                     [iriref <- iriref/p]
                     (pure (cons 'iriref iriref)))
                 (do [langtag <- langtag/p]
                     (pure (cons 'langtag langtag)))))]
      (pure
       (match iri-or-langtag
         [(list (cons 'langtag langtag))
          (literal string-literal
                   rdf:langString
                   langtag)]
         [(list (cons 'iriref iriref))
          (literal string-literal
                   iriref
                   #f)]
         ['()
          (literal string-literal
                   xsd:string
                   #f)]))))

(module+ test
  (test-equal?
   "literal/p parsing string literal"
   (parse-result! (parse-string literal/p "\"foop\""))
   (literal "foop" "http://www.w3.org/2001/XMLSchema#string" #f))

  (test-equal?
   "literal/p parsing typed literal"
   (parse-result! (parse-string literal/p "\"foop\"^^<urn:foo>"))
   (literal "foop" "urn:foo" #f))

  (test-equal?
   "literal/p parsing literal with language tag"
   (parse-result! (parse-string literal/p "\"foop\"@beep-boop"))
   (literal
    "foop"
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
    "beep-boop")))

;;   subject    ::=  IRIREF | BLANK_NODE_LABEL

(define subject/p
  (or/p iriref/p
        blank-node-label/p))

;;   predicate  ::=  IRIREF
;;   @@: json-ld allows for blank nodes as preciates
(define predicate/p
  (or/p iriref/p
        blank-node-label/p))

;;   object     ::=  IRIREF | BLANK_NODE_LABEL |
;;                   literal
(define object/p
  (or/p iriref/p
        blank-node-label/p
        literal/p))

;;   graphLabel ::=  IRIREF | BLANK_NODE_LABEL
(define graph-label/p
  (or/p iriref/p
        blank-node-label/p))

;;   statement  ::=  subject predicate object
;;                   graphLabel? '.'

(define statement/p
  (do [subject <- subject/p]
      (many/p (char/p #\space))
      [predicate <- predicate/p]
      (many/p (char/p #\space))
      [object <- object/p]
      (many/p (char/p #\space))
      ;; maybe get a graph
      [maybe-graph
       <-
       (many/p #:min 0 #:max 1
               (do [g <- graph-label/p]
                   (many/p (char/p #\space))
                   (pure g)))]
      (char/p #\.)
      (many/p (char/p #\space))
      (pure
       (match maybe-graph
         ['()
          (triple subject predicate object)]
         [(list graph)
          (quad subject predicate object graph)]))))

;;   nquadsDoc  ::=  statement? (EOL statement)* EOL?

(define nquads-doc/p
  (do [statements
       <- (many/p statement/p #:sep eol/p)]
      (many/p (one-of/p '(#\newline #\return)))
      eof/p
      (pure statements)))

(define (string->nquads str)
  (parse-result! (parse-string nquads-doc/p str)))

(provide string->nquads)

(module+ test
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

  (define example-quads
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
      (blank-node "b3"))))

  (test-equal?
   "Test output of string->-nquads"
   (string->nquads example-nquads)
   example-quads))

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

(define (string->nquads-dataset str)
  (nquads-list->dataset (string->nquads str)))

(provide nquads-list->dataset string->nquads-dataset)

(module+ test
  (test-equal?
   "read-nquads-dataset works"
   (string->nquads-dataset example-nquads)
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

(define (echar-char? char)
  (hash-has-key? rev-echars-map char))

(define (char-in-range? char char-min char-max)
  (and (char-ci>=? char char-min)
       (char-ci<=? char char-max)))

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
    (define bnode-str
      (string-append "_:" (blank-node-label bnode)))
    ;; Ensure this is a valid blank node
    (unless (success? (parse-string (do blank-node-label/p eof/p)
                                    bnode-str))
      (error "Invalid blank node label"))
    (display bnode-str port))
  (define (write-iri iri)
    (write-char #\< port)
    (for ([char iri])
      (match char
        [(? (lambda (c)
              (or (char<=? c #\u0020)
                  (member c '(#\< #\> #\" #\{ #\} #\| #\^ #\` #\\)))))
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
      (display " " port)
      (write-component (get-graph obj)))
    (display " ." port))
  (match quad
    [(? quad?)
     (write-quad quad)]
    [(? triple? triple)
     (write-triple triple)]))

(define (nquad->string quad)
  (call-with-output-string
    (lambda (p)
      (write-nquad quad p))))

(define (write-nquads quads port)
  (let lp ([quads quads]
           [first #t])
    (match quads
      [(list quad rest ...)
       (unless first
         (newline port))
       (write-nquad quad port)
       (lp rest #f)]
      ['() (void)])))

(define (nquads->string quads)
  (call-with-output-string
    (lambda (p)
      (write-nquads quads p))))

(provide write-nquad nquad->string write-nquads nquads->string)

(module+ test
  (test-equal?
   "nquads->string basic examples"
   (nquads->string example-quads)
   example-nquads)

  (define problematic-triples
    (list
     (triple
      "http://foo.example/> <http://bar.example/> \"baz\" .\n<data:little> <data:bobby> <data:tables> .\n<data:in-ur-base"
      "http://quux.example/"
      (blank-node "b0"))))

  ;; Check for an attack.  This tries to break out into multiple entries.
  ;; Lazily, we're seeing if it succeeds by counting out how many newlines are emitted.
  (test-equal?
   "nquads->string not susceptable to tuple insertion attack via iri"
   (count
    (lambda (x) (equal? x #\newline))
    (string->list
     (nquads->string problematic-triples)))
   0)

  ;; There and back again
  (test-equal?
   "nquads->string and restoring with tuple insertion attempt should restore original"
   (string->nquads
    (nquads->string problematic-triples))
   problematic-triples)

  (test-exn
   "catch literal language tag attack attempt"
   exn:fail?
   (lambda ()
     (nquads->string
      (list
       (triple (blank-node "b0") "http://quux.example/"
               (literal "beep" rdf:langString
                        "foo .\n <urn:in> <urn:ur> <urn:base>"))))))

  (test-exn
   "catch blank node injection attack attempt"
   exn:fail?
   (lambda ()
     (nquads->string
      (list
       (triple (blank-node "b0<urn:bar>") "http://quux.example/"
               (blank-node "b1")))))))
