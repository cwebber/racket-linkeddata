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

(struct blank-node (label)
        #:transparent)

(provide blank-node blank-node? blank-node-label)

;; https://www.w3.org/TR/rdf11-concepts/#dfn-literal
(struct literal
  (lexical-form datatype-iri language-tag)
  #:transparent)

(define (make-literal lexical-form datatype-iri [language-tag #f])
  (literal lexical-form datatype-iri language-tag))

(provide (rename-out [make-literal literal])
         literal? literal-lexical-form
         literal-datatype-iri literal-language-tag)

(define rdf-uri
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define rdf:type
  (string-append rdf-uri "type"))
(define rdf:langString
  (string-append rdf-uri "langString"))
(define rdf:nil
  (string-append rdf-uri "nil"))
(define rdf:first
  (string-append rdf-uri "first"))
(define rdf:rest
  (string-append rdf-uri "rest"))
(define rdfs-uri
  "http://www.w3.org/2000/01/rdf-schema#")
(define xsd-uri
  "http://www.w3.org/2001/XMLSchema#")
(define xsd:boolean
  (string-append xsd-uri "boolean"))
(define xsd:double
  (string-append xsd-uri "double"))
(define xsd:integer
  (string-append xsd-uri "integer"))
(define xsd:string
  (string-append xsd-uri "string"))

(provide rdf-uri rdf:type rdf:langString rdf:nil rdf:first rdf:rest
         rdfs-uri
         xsd-uri xsd:boolean xsd:double xsd:integer xsd:string)
