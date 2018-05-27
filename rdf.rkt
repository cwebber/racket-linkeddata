#lang racket

(struct triple
  (subject predicate object)
  #:transparent)

(struct quad triple (graph)
        #:transparent)

(provide triple triple? quad quad?
         (rename-out [triple-subject get-subject]
                     [triple-predicate get-predicate]
                     [triple-object get-object]
                     [quad-graph get-graph]))

(struct bnode ())

(provide bnode bnode?)

;; https://www.w3.org/TR/rdf11-concepts/#dfn-literal
(struct literal
  (lexical-form datatype-iri language-tag))

(define (make-literal lexical-form datatype-iri [language-tag #f])
  (literal lexical-form datatype-iri language-tag))

(provide (rename-out [make-literal literal])
         literal? literal-lexical-form
         literal-datatype-iri literal-language-tag)

