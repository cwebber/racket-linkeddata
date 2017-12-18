#lang racket

;;; racket-linkeddata --- Linked data tooling for Racket
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require json
         net/url)

;; Special meaning in <active-context>
(define undefined 'undefined)

(define (undefined? val)
  "Used to see if some record type fields have been defined yet"
  (eq? val undefined))

(define (defined? val)
  "Used to see if some record type fields have been defined yet"
  (not (undefined? val)))

(struct active-context
  (;; Base URI, if any
   ;;   equiv to "@base" in jsonld.py
   base
   ;; Term mappings, aka a hashmap mapped to a term definition
   ;;   equiv to "mappings" in json-ld.py
   terms
   ;; Inverse context I guess?
   ;;   equiv to "inverse" in json-ld.py
   inverse
   ;; Default language for this thing
   ;;   equiv to "@language" in json-ld.py
   language
   ;; Vocabulary mapping
   ;;   equiv to "@vocab" in json-ld.py
   vocab)
  ;; @@: Maybe remove this later
  #:transparent)

(define-syntax-rule (copy-active-context acontext flds ...)
  (struct-copy active-context acontext flds ...))

(define initial-active-context
  (active-context 'null #hasheq() 'null undefined undefined))

(define *context-cache*
  (box #hash()))
(define (context-cache-ref url)
  (hash-ref (unbox *context-cache*) url #f))
(define (context-cache-set! url val)
  (set-box! *context-cache*
            (hash-set (unbox *context-cache*) url val)))

(define-syntax-rule (get-or-cache-context url body ...)
  (or (context-cache-ref url)
      (let ([result (begin body ...)])
        (context-cache-set! url result)
        result)))

(define (basic-deref-remote-context iri)
  (get-or-cache-context
   iri
   (string->jsexpr
    (call/input-url iri (curry get-pure-port #:redirections 5)
                    port->string
                    '("Accept: application/ld+json")))))

(define *context-loader*
  (make-parameter basic-deref-remote-context))

(define (load-context iri)
  ;; TODO: Eventually we'll need to support more than just http URIs..
  (let ([iri
         (match iri
           ((? symbol?)
            (string->url (symbol->string iri)))
           ((? string?)
            (string->url iri))
           ((? url?) iri))])
    ((*context-loader*) iri)))

(define (absolute-uri? obj)
  "Check if OBJ is an absolute uri or not."
  (match obj
    ((? string?)
     (string-contains? obj ":"))
    ((? symbol?)
     (string-contains? (symbol->string obj) ":"))
    (_ #f)))

;; @@: For ease of converting
(define string-startswith? string-prefix?)

(define jsobj? (lambda (x) (and (hash? x) (hash-eq? x))))
(define %nothing '(nothing))
(define (nothing? obj)
  (eq? obj %nothing))

;;; Helpers for legacy code
(define (hash-assoc key htbl)
  (let ([result (hash-ref htbl key %nothing)])
    (if (eq? result %nothing)
        #f
        (cons key result))))
(define (hash-cons key val htbl)
  (hash-set htbl key val))

(define (jsobj-assoc obj key)
  (hash-assoc key obj))
(define (jsobj-ref obj key)
  (hash-ref obj key #f))

(define (blank-node? obj)
  "See if OBJ is a blank node (a string that starts with \"_:\")"
  (and (string? obj)
       (string-prefix? obj "_:")))

(define (list-object? obj)
  "A list object is a JSON object that has a @list member"
  (and (jsobj? obj)
       (jsobj-assoc obj '@list)))

(define (value-object? obj)
  "A value object is a JSON object that has a @value member"
  (and (jsobj? obj)
       (jsobj-assoc obj '@value)))

(define (set-object? obj)
  "A set object is a JSON object that has a @set member"
  (and (jsobj? obj)
       (jsobj-assoc obj '@set)))

(define (maybe-append-uri-to-base uri base)
  "A sorta-correct way to join a URI to BASE, assuming there is a BASE,
and assuming URI isn't a URI on its own.

If not, it just returns URI.

Does the dumbest possible thing: string-appends together the base and URI.
This might not be the best way to do it, further reading into
  http://tools.ietf.org/html/rfc3986#section-5.1
should be done.

TODO: It loooks like the correct version of this is done in jsonld.py
"
  (if (and (string? base)
           (not (absolute-uri? uri)))
      (string-append base uri)
      uri))


;; @@: Should we also include ":"?
(define json-ld-keywords
  '(@context
    @id @value @language @type
    @container @list @set @reverse
    @index @base @vocab @graph))

(define (json-ld-keyword? obj)
  "See if OBJ is a json-ld special keyword

As a mild speed optimization, returns the remainder of json-ld-keywords
rathr than #t if true (#f of course if false)"
  (member (maybe-symbolify obj) json-ld-keywords))

(define (scalar? obj)
  (or (eq? obj #t)
      (eq? obj #f)
      (number? obj)
      (string? obj)))

;; ... helper funcs
(define (active-context-terms-assoc key active-context)
  "Pull key out of a active-context's mapping"
  (let* ([key (maybe-symbolify key)]
         [result (hash-ref (active-context-terms active-context)
                           key %nothing)])
    (if (eq? result %nothing)
        #f
        (cons key result))))

(define (active-context-terms-ref key active-context)
  "Pull key out of a active-context's mapping"
  (let* ([key (maybe-symbolify key)]
         [result (hash-ref (active-context-terms active-context)
                           key %nothing)])
    (if (eq? result %nothing)
        #f
        result)))

(define (active-context-terms-cons key val active-context)
  "Assign key to value in a active-context's mapping and return new active-context"
  (let ([key (maybe-symbolify key)])
    (copy-active-context
     active-context
     [terms (hash-set (active-context-terms active-context)
                      key val)])))

(define (active-context-terms-delete key active-context)
  (let ([key (maybe-symbolify key)])
    (copy-active-context
     active-context
     [terms (hash-remove (active-context-terms active-context) key)])))

(define (%active-context-term-prop term-prop)
  (lambda (key active-context)
    (let* ([term-def (active-context-terms-ref key active-context)])
      (and term-def
           (hash-ref term-def term-prop #f)))))

(define active-context-container-mapping
  (%active-context-term-prop '@container))
(define active-context-reverse-property?
  (%active-context-term-prop '@reverse))
(define active-context-type-mapping
  (%active-context-term-prop '@type))
(define active-context-language-mapping
  (%active-context-term-prop '@language))
(define active-context-iri-mapping
  (%active-context-term-prop '@id))

;; @@: We may not need these two next macros...
;;  remove soon if not used

(define-syntax-rule (chain-value-calls proc1 proc2 ...)
  ;; Chain procedures which accept and multi-value-return the
  ;; same number of values/args together.
  ;; 
  ;; The chained function expects a producer function.
  ;; 
  ;; Example:
  ;; 
  ;;   (let ((proc1 (lambda (animals foods)
  ;;                  (values (cons 'hippos animals) (cons 'pizza foods))))
  ;;         (proc2 (lambda (animals foods)
  ;;                  (values (cons 'tigers animals) (cons 'seitan foods))))
  ;;         (proc3 (lambda (animals foods)
  ;;                  (values (cons 'rats animals) (cons 'mints foods)))))
  ;;     ((chain-value-calls proc1 proc2 proc3)
  ;;      (lambda () (values '(cats dogs) '(tofu cookies)))))
  (lambda (producer)
    (call-with-values 
        (lambda () 
          (call-with-values producer proc1))
      proc2) ...))

(define-syntax-rule (chain-values-with-input (proc1 proc2 ...) input ...)
  ;; Chain procedures together, but then call with input
  ;; 
  ;; Example:
  ;; 
  ;;   (let ((proc1 (lambda (animals foods)
  ;;                  (values (cons 'hippos animals) (cons 'pizza foods))))
  ;;         (proc2 (lambda (animals foods)
  ;;                  (values (cons 'tigers animals) (cons 'seitan foods))))
  ;;         (proc3 (lambda (animals foods)
  ;;                  (values (cons 'rats animals) (cons 'mints foods)))))
  ;;     (chain-values-with-input (proc1 proc2 proc3)
  ;;      '(cats dogs) '(tofu cookies)))
  ((chain-value-calls proc1 proc2 ...)
   (lambda () (values input ...))))


(define-syntax-rule (compose-forward func1 func2 ...)
  ;; "Like compose, but run arguments first to last"
  (apply compose
         (reverse (list func1 func2 ...))))


;;; ==== Some crufty utilities ====

(define (symbol>? sym1 sym2)
  (not (or (eq? sym1 sym2)
           (symbol<? sym1 sym2))))

(define (jsobj->sorted-unique-alist jsobj [compare symbol<?])
  "Return a unique and sorted alist

Protip: change compare to symbol>? if you want to
fold instead of fold-right >:)"
  (map (lambda (k)
         (cons k (hash-ref jsobj k)))
       (sort (hash-keys jsobj) compare)))

(define (maybe-stringify obj)
  "Symbols or strings as strings"
  (match obj
    ((? symbol?) (symbol->string obj))
    (_ obj)))

(define (maybe-symbolify obj)
  "Symbols or strings as symbols"
  (match obj
    ((? string?) (string->symbol obj))
    (_ obj)))

;; for debugging
(define (pk . vals)
  "Peek at values for print debugging, but return 'em"
  (display ";;; ")
  (write vals)
  (newline)
  ;; return the last value
  (last vals))

(define-syntax-rule (pk-values print-these ... body)
  ;; Like pk, but supporting multiple value return
  (call-with-values
      (lambda () body)
    (lambda vals
      (pk print-these ... '*pk-values:* vals)
      (apply values vals))))

(define (listy? obj)
  "Fast close-enough check to see if something's list-like"
  (or (pair? obj)
      (null? obj)))

(define flatten-append
  (lambda (obj1 obj2)
    (if (listy? obj2)
        (if (listy? obj1)
            (append obj1 obj2)
            (cons obj1 obj2))
        (if (listy? obj1)
            (append obj1 (list obj2))
            (cons obj1 (list obj2))))))

(define (absolute->relative-url url url-base)
  "Converts URL (a string) into a relative url against URL-BASE"
  (let ([url (maybe-stringify url)]
        [url-base (maybe-stringify url-base)])
    (if (string-prefix? url url-base)
        (substring url (string-length url-base))
        url)))

;;; =============



;; Algorithm 6.1

(define (process-context active-context local-context [remote-contexts '()]
                         #:base-iri [base-iri 'null])
  "This function builds up a new active-context based on the
remaining context information to process from local-context"
  (let loop (;; 1. Initialize result to the result of cloning active context.
             [result active-context]
             ;; 2. If local context is not an array, set it to an array
             ;;    containing only local context.
             [local-context (match local-context
                              ((? listy?) local-context)
                              (_ (list local-context)))]
             [remote-contexts remote-contexts])
    ;; Some helper functions...
    ;; (the variables these reference get overriden
    ;; with let later, but we only use this early on)
    (define (append-to-base uri)
      "Append to the current base (if appropriate)"
      ;; Not useful if this is the first invocation of result,
      ;; but we don't use it there, so no biggie
      (maybe-append-uri-to-base uri (active-context-base result)))

    (define (equal-including-checking-base? uri1 uri2)
      "A check method to see if A matches B, or B with base apended"
      (or (equal? uri1 uri2)
          (equal? uri1 (append-to-base uri2))))

    ;; 3. For each item context in local context: 
    (match local-context
      ;; Are we done processing contexts?
      ('()
       ;; 4. Return result
       result)
      ;; Process this item
      ((list context next-contexts ...)
       (match context
         ;; 3.1
         ;; If null, result is a newly-initialized active context 
         ('null
          (loop
           ;; new active context based on initial-active-context
           ;; but setting base-iri
           ;; @@: Are we doing this right?  Here's the text:
           ;; 3.1: If context is null, set result to a newly-initialized
           ;;      active context and continue with the next context.
           ;;      The base IRI of the active context is set to the IRI
           ;;      of the currently being processed document (which might
           ;;      be different from the currently being processed context),
           ;;      if available; otherwise to null. If set, the base option
           ;;      of a JSON-LD API Implementation overrides the base IRI.
           (copy-active-context initial-active-context
                                [base base-iri])
           next-contexts remote-contexts))

         ;; Okay it's a string, great, that means it's an iri
         ((? string?)
          (let ([context (append-to-base context)])
            (when (member context remote-contexts equal-including-checking-base?)
              (error 'json-ld-error
                     "recursive context inclusion"
                     context))
            (let ((derefed-context (load-context context))
                  (remote-contexts (cons context remote-contexts)))
              (when (not (and (jsobj? derefed-context)
                              (jsobj-ref derefed-context '@context)))
                (error 'json-ld-error
                       "invalid remote context"))
              ;; We made it this far, so recurse on the derefed context
              ;; then continue with that updated result
              (let* ((context (jsobj-ref derefed-context '@context))
                     (result (process-context result context
                                              remote-contexts)))
                (loop result next-contexts remote-contexts)))))

         ((? jsobj? context)
          ;; Time to process over a json object of data.  Yay!
          ;; We're really just folding over this object here,
          ;; but three keys are special:
          ;; '@base, '@vocab, and '@language.
          ;; Otherwise, we process using the "Create term definition"
          ;; algorithm.
          ;;
          ;; Because that's a lot of steps, for readability
          ;; we break these out into functions then do the fold.
          (define (modify-result-from-base result base)
            (if (and base
                     (eq? remote-contexts 'null))
                ;; In this case we'll adjusting the result's '@base
                ;; depending on what this context's @base is
                (match base
                  ;; If the @base in this context is null, remove
                  ;; whatever current @base is in the result
                  ('null
                   ;; Remove base iri from result
                   (copy-active-context result [base undefined]))

                  ;; If it's an absolute URI, let's set that as the result's
                  ;; @base
                  ((? absolute-uri? base-uri)
                   (copy-active-context result [base base-uri]))

                  ;; Otherwise... if it's a string, we assume it's
                  ;; still a relative URI
                  ;; @@: Are more precise ways to define a relative URI at
                  ;;   this point?
                  ((? string? relative-base-uri)
                   ;; If the current *result's* base-uri is not null....
                   ;; resolve it against current base URI of result
                   (if (string? (jsobj-ref result '@base))
                       (copy-active-context result
                                            [base
                                             (maybe-append-uri-to-base
                                              relative-base-uri (active-context-base result))])
                       ;; Otherwise, this is an error...
                       ;; "Value of @base in a @context must be an
                       ;;  absolute IRI or empty string."
                       (error 'json-ld-error
                              ;; @@: context vs result seems kinda vague
                              ;;   to a user through this whole function, maybe
                              "invalid base IRI" context
                              result relative-base-uri)))
                  (invalid-base-value
                   (error 'json-ld-error
                          "invalid base IRI" invalid-base-value)))
                ;; Otherwise, return unmodified result
                result))

          (define (modify-result-from-vocab result vocab)
            (cond ((eq? vocab 'null)
                   ;; remove vocabulary mapping from result
                   (copy-active-context result [vocab undefined]))
                  ;; If either an absolute IRI or blank node,
                  ;; @vocab of result is set to vocab
                  ((or (absolute-uri? vocab)
                       (blank-node? vocab))
                   (copy-active-context result [vocab vocab]))
                  (else
                   (error 'json-ld-error "invalid vocab mapping"))))

          (define (modify-result-from-language result language)
            (cond ((eq? language 'null)
                   ;; remove vocabulary mapping from result
                   (copy-active-context result [language undefined]))
                  ((string? language)
                   (copy-active-context result [language
                                                (string-downcase language)]))
                  (else
                   (error 'json-ld-error "invalid default language"))))

          (define (build-result)
            (car
             (sequence-fold
              (lambda (prev ctx-key ctx-val)
                (match prev
                  ((cons result defined)
                   (match ctx-key
                     ('@base
                      (cons (modify-result-from-base result ctx-val)
                            defined))
                     ('@vocab
                      (cons (modify-result-from-vocab result ctx-val)
                            defined))
                     ('@language
                      (cons (modify-result-from-language result ctx-val)
                            defined))
                     (_
                      ;; Notably we aren't passing ctx-ctx-key here because
                      ;; (I suppose) create-term-definition has the whole context
                      ;; and so can look it up anyway...
                      (let-values ([(result defined)
                                    (create-term-definition
                                     result context ctx-key defined)])
                        (cons result defined)))))))
              (cons result #hasheq()) ;; second value here is "defined"
              context)))

          (loop
           (build-result)
           next-contexts remote-contexts))

         ;; 3.3: Anything else at this point is an error...
         (_ (error 'json-ld-error
                   "invalid local context"
                   context)))))))


;; Algorithm 6.2
;; -------------

(define (create-term-definition active-context local-context term defined)
  ;; Let's see, has this term been defined, or started to be
  ;; defined yet?...
  (match (hash-assoc term defined)
    ;; If term definition already was created, we do nothing
    ;; so return what we got!
    ((cons _ #t)
     (values active-context defined))
    ;; If term definition is false, that means term definition
    ;; started but never completed... a cycle!  Abort, abort!
    ((cons _ #f)
     (error 'json-ld-error "cyclic IRI mapping"))
    ;; Not referenced yet in defined, continue
    (#f
     (when (json-ld-keyword? term)
       (error 'json-ld-error
              "keyword redefinition"))

     (let (;; Set defined's value for this key to false, indicating
           ;; that we started processing 
           (defined
             (hash-cons term #f defined))
           ;; @@: Do we really need to remove any existing term
           ;; definition for term in active context?  The spec says so,
           ;; but might it just be overridden?
           (active-context
            (active-context-terms-delete term active-context))
           (value (jsobj-ref local-context (maybe-symbolify term))))
       (cond
        ;; If value is null or a json object with '@id mapping to null,
        ;; then mark term as defined and set term in
        ;; resulting context to null
        ((or (eq? value 'null)
             (and (jsobj? value)
                  (eq? (jsobj-ref value '@id) 'null)))
         (values
          (active-context-terms-cons term 'null active-context)
          (hash-cons term #t defined)))
        ;; otherwise, possibly convert value and continue...
        (else
         (let* ((value (cond ((string? value)
                              `#hasheq((@id . ,value)))
                             ((jsobj? value)
                              value)
                             (else
                              (error 'json-ld-error
                                     "invalid term definition")))))
           (call/ec
            (lambda (return)
              (define (definition-handle-type definition active-context defined)
                (match (jsobj-assoc value '@type)
                  ;; no match, return definition as-is
                  (#f (values definition active-context defined))
                  ;; type value must be a string
                  ((cons _ (? string? type-prop))
                   (let-values ([(expanded-iri active-context defined)
                                 (iri-expansion active-context type-prop
                                                #:vocab #t
                                                #:document-relative #f
                                                #:local-context local-context
                                                #:defined defined)])
                     (values
                      (hash-set definition '@type
                                expanded-iri)
                      active-context defined)))
                  ;; Otherwise, it's an error!
                  (_
                   (error 'json-ld-error
                          "invalid type mapping"))))

              ;; sec 11
              (define (definition-handle-reverse definition active-context defined)
                (match (jsobj-assoc value '@reverse)
                  ;; no match, carry on!
                  (#f (values definition active-context defined))
                  ;; value must be a string
                  ((cons _ (? string? reverse-prop))
                   (when (jsobj-assoc value '@id)
                     (error 'json-ld-error
                            "invalid reverse property"))

                   (let-values ([(expanded-iri active-context)
                                 (iri-expansion active-context reverse-prop
                                                #:vocab #t #:document-relative #f
                                                #:local-context local-context
                                                #:defined defined)])
                     (when (not (absolute-uri? expanded-iri))
                       ;; Uhoh
                       (error 'json-ld-error
                              "invalid IRI mapping"))

                     (let ((definition
                             (hash-set
                              ;; 11.4
                              (%definition-handle-container-reverse
                               definition)
                              'reverse #t)))
                       ;; return early with new active context
                       ;; w/ term definition and defined
                       (return
                        (active-context-terms-cons
                         term definition active-context)
                        (hash-cons term #t defined)))))
                  (_
                   (error 'json-ld-error
                          "invalid IRI mapping"))))

              ;; Helper method for 11
              (define (%definition-handle-container-reverse definition)
                ;; 11.4
                (match (jsobj-assoc value '@container)
                  ;; just return original efinition if no @container
                  (#f definition)
                  ;; Otherwise make sure it's @set or @index or @nil
                  ;; and set @container to this
                  ((cons _ (? (lambda (x) (member x '("@set" "@index" 'null))) container))
                   (hash-set definition '@container container))
                  ;; Uhoh, looks like that wasn't valid...
                  (_
                   (error 'json-ld-error
                          "invalid reverse property"))))

              ;; 12
              (define (definition-set-reverse-to-false definition active-context defined)
                (values (hash-set definition 'reverse #f) active-context defined))

              ;; This one is an adjustment deluxe, it does a significant
              ;; amount of adjustments to the definition and builds
              ;; up an active context to be used as well.
              ;; @@: I wish I had a better name for this.
              (define (more-definition-adjustments
                       definition active-context defined)
                (cond
                 ;; sec 13
                 ((and (jsobj-assoc value '@id)
                       (not (equal? (jsobj-ref value '@id)
                                    term)))
                  (let ((id-val (jsobj-ref value '@id)))
                    (when (not (string? id-val))
                      (error 'json-ld-error
                             "invalid IRI mapping"))

                    (let-values ([(expanded-iri active-context defined)
                                  (iri-expansion active-context id-val
                                                 #:vocab #t #:document-relative #f
                                                 #:local-context local-context
                                                 #:defined defined)])
                      (when (not (or (json-ld-keyword? expanded-iri)
                                     (absolute-uri? expanded-iri)
                                     (blank-node? expanded-iri)))
                        (error 'json-ld-error
                               "invalid IRI mapping"))
                      (when (equal? (maybe-stringify expanded-iri) "@context")
                        (error 'json-ld-error
                               " invalid keyword alias"))

                      ;; otherwise, onwards and upwards
                      (values (hash-set definition '@id expanded-iri)
                              active-context defined))))

                 ;; sec 14
                 ((absolute-uri? term)
                  ;; Check for compact iri
                  (match (string-split (symbol->string term) ":")
                    ((list prefix suffix-list ...)
                     (let-values ([(active-context defined)
                                   ;; see if we should update the context...
                                   (if (jsobj-assoc local-context term)
                                       ;; It's in the local context...
                                       ;; so we should update the active context so we can
                                       ;; match against it!
                                       (create-term-definition
                                        active-context local-context
                                        prefix defined)
                                       ;; oh okay don't update in that case
                                       (values active-context defined))])
                       (let ((prefix-in-context
                              (active-context-terms-assoc prefix active-context)))
                         (if prefix-in-context
                             (values (hash-set definition '@id
                                               (string-append
                                                (jsobj-ref (cdr prefix-in-context) '@id)
                                                (string-join suffix-list ":")))
                                     active-context defined)
                             ;; okay, yeah, it's set-iri-mapping-of-def-to-term
                             ;; but we want to return the new active-context
                             (values (hash-set definition '@id term)
                                     active-context defined)))))))

                 ;; sec 15
                 ((jsobj-assoc active-context '@vocab)
                  (values (hash-set definition '@id
                                    (string-append
                                     (jsobj-ref active-context '@vocab)
                                     term))
                          active-context defined))

                 (else
                  (error 'json-ld-error
                         "invalid IRI mapping"))))

              ;; 16
              (define (definition-handle-container definition active-context defined)
                (let ((value-container (jsobj-assoc value '@container)))
                  (if value-container
                      ;; Make sure container has an appropriate value,
                      ;; set it in the definition
                      (let ((container (cdr value-container)))
                        (when (not (member (maybe-stringify container)
                                           '("@list" "@set"
                                             "@index" "@language")))
                          (error 'json-ld-error
                                 "invalid container mapping"))
                        (values (hash-set definition '@container container)
                                active-context defined))
                      ;; otherwise, no adjustment needed apparently
                      (values definition active-context defined))))

              ;; 17
              (define (definition-handle-language definition active-context defined)
                (let ((value-language (jsobj-assoc value '@language)))
                  (if value-language
                      ;; Make sure language has an appropriate value,
                      ;; set it in the definition
                      (let ((language (cdr value-language)))
                        (when (not (or (eq? language 'null) (string? language)))
                          (error 'json-ld-error
                                 "invalid language mapping"))
                        (values (hash-set definition '@language language)
                                active-context defined))
                      ;; otherwise, no adjustment needed apparently
                      (values definition active-context defined))))

              (let-values ([(definition active-context defined)
                            ((compose-forward definition-handle-type
                                              ;; might return early on this one
                                              definition-handle-reverse
                                              ;; If we got this far, we didn't return early
                                              definition-set-reverse-to-false
                                              more-definition-adjustments
                                              definition-handle-container
                                              definition-handle-language)
                             #hasheq() active-context defined)])
                (values (active-context-terms-cons term definition active-context)
                        (hash-cons term #t defined))))))))))))

;; TODO: We have to redefine *ALL* entries that call iri-expansion
;;   to accept multiple value binding where the second value is defined
;;   because this function itself may call (create-term-definition)!
;;   ... maybe a good time to refactor the end of (create-term-definition)
;;   to be a bit more compose-y

;; Algorithm 6.3
(define (iri-expansion active-context value
                       #:document-relative (document-relative #f)
                       #:vocab (vocab #f)
                       #:local-context (local-context 'null)
                       ;; @@: spec says defined should be null, but
                       ;;   #hasheq() seems to make more sense in our case
                       #:defined (defined #hasheq()))
  "IRI expansion on VALUE within ACTIVE-CONTEXT

Does a multi-value-return of (expanded-iri active-context defined)"
  (define (maybe-update-active-context)
    (if (and (jsobj? local-context)
             (jsobj-assoc local-context value)
             (not (eq? (jsobj-ref local-context value)
                       #t)))
        ;; Okay, we're updating the context even further...
        (create-term-definition active-context local-context value defined)
        ;; nope, return as-is
        (values active-context defined)))

  (if (or (eq? value 'null)
          (json-ld-keyword? value))
      ;; keywords / null are just returned as-is
      (values value active-context defined)
      ;; Otherwise, see if we need to update the active context
      ;; and continue with expansion...
      (let-values ([(active-context defined)
                    (maybe-update-active-context)])
        (cond
         ;; 3
         ((and (eq? vocab #t)
               (active-context-terms-assoc value active-context))
          (values
           (jsobj-ref (cdr (active-context-terms-assoc value active-context)) '@id)
           active-context
           defined))
         ;; 4
         ((absolute-uri? value)
          (let* ((split-string (string-split (maybe-stringify value) ":"))
                 (prefix (car split-string))
                 (suffix (string-join (cdr split-string) ":")))
            (if (or (equal? prefix "_")
                    (string-startswith? suffix "//"))
                ;; It's a blank node or absolute IRI, return!
                (values value active-context defined)
                ;; otherwise, carry on to 4.3...
                (let-values ([(active-context defined)
                              (if (and (not (eq? local-context 'null))
                                       (jsobj-assoc local-context prefix)
                                       (not (eq? (jsobj-ref local-context prefix))))
                                  ;; ok, update active-context and defined
                                  (create-term-definition active-context local-context
                                                          prefix defined)
                                  ;; nah leave them as-is
                                  (values active-context defined))])
                  (match (active-context-terms-assoc prefix active-context)
                    ;; We've got a match, which means we're returning
                    ;; the term definition uri for prefix concatenated
                    ;; with the suffix
                    ((cons _ prefix-term-def)
                     (values
                      (string-append
                       (jsobj-ref prefix-term-def '@id) suffix)
                      active-context
                      defined))
                    ;; otherwise, return the value; it's already an absolute IRI!
                    (_ (values value active-context defined)))))))
         ;; 5
         ((and (eq? vocab #t)
               (defined? (active-context-vocab active-context)))
          (values
           (string-append
            (active-context-vocab active-context) value)
           active-context
           defined))
         ;; 6
         ((eq? document-relative #t)
          (values
           (maybe-append-uri-to-base
            value (active-context-base active-context))
           active-context
           defined))

         ;; 7
         (else (values value active-context defined))))))


;; 7.1, expansion algorithm

;; Oh boy....

(define (expand-json-array active-context active-property json-array)
  (define (expand-items)
    (foldr
     (lambda (item prev)
       (match prev
         ((cons result active-context)
          (let-values ([(expanded-item active-context)
                        (expand-element active-context active-property item)])
            (let ((active-property-term-result
                   (active-context-terms-assoc active-property active-context)))
              ;; Boo, it's sad that json-ld prevents lists of lists.
              ;; ... but we're doing as the spec says :\
              (when (and (or (equal? (maybe-symbolify active-property) '@list)
                             (and active-property-term-result
                                  (equal?
                                   (jsobj-ref (cdr active-property-term-result)
                                              '@container)
                                   "@list")))
                         (or (listy? expanded-item)
                             (list-object? expanded-item)))
                (error 'json-error
                       "list of lists"))

              ;; TODO: this seems super wrong
              (match expanded-item
                ((? listy? _)
                 (cons (append expanded-item result)
                       active-context))
                ;; TODO: Is this right?  Shouldn't we just skip if null?
                ('null
                 (cons 'null active-context))
                (_
                 (cons (cons expanded-item result) active-context))))))))
     (cons '() active-context)
     json-array))

  (match (expand-items)
    ((cons expanded-array active-context)
     (values expanded-array active-context))))


;; helper method for expand-json-object
(define (expand-json-object-pair key value result active-context active-property)
  "Process a KEY VALUE pair, building up from RESULT within ACTIVE-CONTEXT"
  (let* ((term-mapping
          (delay (active-context-terms-assoc key active-context)))
         (container-mapping
          (delay
            (when (force term-mapping)
              (jsobj-ref (cdr (force term-mapping))
                         '@container)))))
    (define (get-expanded-value return)
      "Get expanded value; return is a prompt to bail out early"
      ;; 7.4.1, if key is @context, continue to next key
      (when (equal? key '@context)
        (return result active-context))
      ;; otherwise, on to 7.4.3
      (let-values ([(expanded-property active-context defined)
                    (iri-expansion active-context key #:vocab #t)])
        (cond
         ;; 7.3
         ((or (eq? 'null expanded-property)
              (not (or (absolute-uri? expanded-property)
                       (json-ld-keyword? expanded-property))))
          ;; carry on to the next key
          (return result active-context))
         ;; 7.4... get ready for a doosy
         ((json-ld-keyword? expanded-property)
          (when (eq? (maybe-symbolify active-property) '@reverse)
            (error 'json-ld-error
                   "invalid reverse property map"))
          ;; already defined, uhoh
          (when (jsobj-assoc result expanded-property)
            (error 'json-ld-error
                   "colliding keywords"))

          (call-with-values
              (lambda ()
                (match (maybe-symbolify expanded-property)
                  ;; 7.4.3
                  ('@id
                   (when (not (string? value))
                     (error 'json-ld-error
                            "invalid @id value"))
                   ;; calls to iri-expansion also multi-value-return "defined"
                   ;; as well, but as the third argument, we ignore it
                   (iri-expansion active-context value
                                  #:document-relative #t))
                  ;; 7.4.4
                  ('@type
                   (match value
                     ((? string?)
                      (iri-expansion active-context value
                                     #:vocab #t #:document-relative #t))
                     ((list (? string?) ...)
                      (let lp ((items value)
                               (result '())
                               (active-context active-context))
                        (match items
                          ('() (values result active-context))
                          ((list item remaining-items ...)
                           (call-with-values
                               (lambda ()
                                 (iri-expansion active-context item
                                                #:document-relative #t))
                             (lambda (expanded active-context [%ignored #f])
                               (lp remaining-items expanded active-context)))))))
                     (_ (error 'json-ld-error "invalid type value"))))

                  ;; 7.4.5
                  ('@graph
                   (expand-element active-context '@graph value))

                  ;; 7.4.6
                  ('@value
                   (match value
                     ('null
                      ;; jump out of processing this pair
                      (return (hash-set result '@value 'null)
                              active-context))
                     ;; otherwise, expanded value *is* value!
                     ((? scalar?)
                      (values value active-context))
                     (_ (error 'json-ld-error #:code "invalid value object"))))

                  ;; 7.4.7
                  ('@language
                   (match value
                     ((? string?)
                      (values (string-downcase value) active-context))
                     (_ (error 'json-ld-error #:code "invalid language-tagged string"))))

                  ;; 7.4.8
                  ('@index
                   (match value
                     ((? string?)
                      (values value active-context))
                     (_ (error 'json-ld-error #:code "invalid @index value"))))

                  ;; 7.4.9
                  ('@list
                   ;; Bail out early if null or @graph to remove free-floating list
                   (when (member active-property '(null "@graph"))
                     (return result active-context))
                   (let-values ([(expanded-value active-context)
                                 (expand-element active-context active-property value)])
                     ;; oops!  no lists of lists
                     (when (list-object? expanded-value)
                       (error 'json-ld-error "list of lists"))
                     ;; otherwise, continue with this as expanded value
                     (values expanded-value active-context)))

                  ;; 7.4.10
                  ('@set
                   (expand-element active-context active-property value))

                  ;; 7.4.11
                  ;; I'm so sorry this is so complicated
                  ('@reverse
                   (when (not (jsobj? value))
                     (error 'json-ld-error "invalid @reverse value"))

                   (let-values ([(expanded-value active-context)
                                 (expand-element active-context '@reverse value)])
                     (return
                      ;; here might be a great place to break out
                      ;; another function
                      (cond
                       ((jsobj-assoc expanded-value '@reverse)
                        (sequence-fold
                         (lambda (result property item)
                           (let ((property-in-result
                                  (jsobj-assoc result property)))
                             ;; @@: hash-set maybe?
                             (hash-set result property
                                       (if property-in-result
                                           (cons item (cdr property-in-result))
                                           (list item)))))
                         result
                         (jsobj-ref expanded-value '@reverse)))
                       ((listy? expanded-value)
                        (sequence-fold
                         (lambda (result property items)
                           (if (equal? property '@reverse)
                               ;; skip this one
                               result
                               ;; otherwise, continue
                               (foldl
                                (lambda (item result)
                                  (let ((reverse-map (jsobj-ref result '@reverse)))
                                    (when (or (value-object? item)
                                              (list-object? item))
                                      (error 'json-ld-error
                                             "invalid reverse property value"))
                                    (hash-set result '@reverse
                                              (hash-set reverse-map key
                                                        (cons item
                                                              ;; @@: this can be simplified
                                                              (if (jsobj-assoc reverse-map property)
                                                                  (jsobj-ref reverse-map property)
                                                                  '()))))))
                                result
                                items)))
                         (if (jsobj-assoc result '@reverse)
                             result
                             ;; TODO: fix this
                             ;; TODO: What were we fixing
                             (hash-set result '@reverse #hasheq()))
                         expanded-value))
                       (else result))
                      active-context)))))
            (lambda (expanded-value active-context [ignored #f]) ; ignore defined here
              (return
               (if (eq? expanded-value 'null)
                   ;; return as-is
                   result
                   ;; otherwise, set expanded-property member of result
                   ;; to expanded-value
                   (hash-set result expanded-property expanded-value))
               active-context))))

         ;; 7.5
         ;; If key's container mapping in active-context is @language and
         ;; value is a jsobj then value is expanded from a language map
         ((and (equal? (force container-mapping) '@language)
               (jsobj? value))
          (values
           (foldl
            (lambda (x expanded-value)
              (match x
                ((cons language language-value)
                 (foldr
                  (lambda (item expanded-value)
                    (when (not (string? item))
                      (error 'json-ld-error "invalid language map value"))
                    (cons
                     `#hasheq((@value . ,item)
                              (@language . ,(string-downcase language)))
                     expanded-value))
                  expanded-value
                  (if (listy? language-value)
                      language-value
                      (list language-value))))))
            '()
            ;; As a hack, this is sorted in REVERSE!
            ;; This way we can use normal foldl instead of foldr.
            ;; Mwahahaha!
            (jsobj->sorted-unique-alist value symbol>?))
           expanded-property
           active-context))
         
         ;; 7.6
         ((and (equal? (force container-mapping) '@index)
               (jsobj? value))
          ;; @@: In reality the code here is very similar to 
          ;;   in 7.5, but I think this is much more readable...
          (let loop ((l (jsobj->sorted-unique-alist value symbol>?))
                     (active-context active-context)
                     (expanded-value '()))
            (match l
              ('()
               (values result active-property active-context))
              ((list (cons index index-value) rest ...)
               (let-values ([(index-value active-context)
                             (expand-element active-context key
                                             (if (listy? index-value)
                                                 index-value
                                                 (list index-value)))])
                 (loop rest active-context
                       (foldr
                        (lambda (item expanded-value)
                          (cons
                           (if (jsobj-assoc item '@index)
                               item
                               (hash-set item '@index index))
                           expanded-value))
                        expanded-value
                        index-value)))))))

         ;; 7.7
         (else
          (let-values ([(expanded-value active-context)
                        (expand-element active-context key value)])
            (values expanded-value expanded-property active-context))))))

    (call/ec
     (lambda (return)
       (let-values ([(expanded-value expanded-property active-context)
                     (get-expanded-value return)])
         (define (append-prop-val-to-result expanded-property expanded-value
                                            result)
           (hash-set result (maybe-symbolify expanded-property)
                     (flatten-append
                      expanded-value
                      (if (jsobj-assoc result expanded-property)
                          (jsobj-ref result expanded-property)
                          '()))))

         ;; 7.8
         ;; if expanded value is null, ignore key by continuing
         ;; @@: could just be in the cond, right?
         (when (eq? expanded-value 'null)
           (return result active-context))
         
         ;; Augh, these 7.9-7.11 sections are frustrating
         ;; continue with line 1927 in jsonld.py
         ;; # convert expanded value to @list if container specifies it

         (cond
          ;; 7.9
          ((and (equal? (force container-mapping) '@list)
                (not (list-object? expanded-value)))
           (values
            (append-prop-val-to-result
             expanded-property
             `#hasheq((@list . ,(if (listy? expanded-value)
                                    expanded-value
                                    (list expanded-value))))
             result)
            active-context))

          ;; 7.10
          ;; Looks like a reverse property?
          ((and (force term-mapping)
                (jsobj-ref (cdr (force term-mapping)) 'reverse))
           (let* ((result (if (jsobj-assoc result '@reverse)
                              result
                              (hash-set result '@reverse '())))
                  (reverse-map (jsobj-ref result '@reverse))
                  (expanded-value (if (listy? expanded-value)
                                      expanded-value
                                      (list expanded-value))))
             
             (values
              (foldl
               (lambda (item result)
                 (when (or (value-object? item)
                           (list-object? item))
                   (error 'json-ld-error "invalid reverse property value"))
                 (hash-set result '@reverse
                           (hash-set reverse-map expanded-property
                                     (cons item
                                           (if (jsobj-assoc reverse-map expanded-property)
                                               (jsobj-ref reverse-map expanded-property)
                                               '())))))
               result
               expanded-value)
              active-context)))

          ;; 7.11
          (else
           (values (append-prop-val-to-result
                    expanded-property expanded-value result)
                   active-context))))))))

(define (expand-json-object active-context active-property jsobj)
  (define (build-result active-context)
    ;; Thaere's a (admittedly unlikely?) chance that builing up the
    ;; active-context this way could result in things being wrong?
    ;; we're consing in the other direction, so...
    (let loop ((l (jsobj->sorted-unique-alist jsobj symbol>?))
               (active-context active-context)
               (result #hasheq()))
      (match l
        ('()
         (values result active-context))
        ((list (cons key val) rest ...)
         (let-values ([(result active-context)
                       (expand-json-object-pair key val result active-context active-property)])
           (loop rest active-context result))))))

  (let* ((jsobj-context (jsobj-assoc jsobj '@context))
         (active-context
          (if jsobj-context
              (process-context active-context (cdr jsobj-context))
              active-context))
         (permitted-value-results '(@value @language @type @index)))
    (let-values ([(result active-context)
                  (build-result active-context)])
      (call/ec
       (lambda (return)
         (define (adjust-result-1 result)
           (cond
            ;; sec 8
            ((jsobj-assoc result '@value)
             ;; 8.1, make sure result does not contain keys outside
             ;;   of permitted set
             (when (or (not (match (hash->list result)
                              ((list (cons (? (lambda (x)
                                                (member x permitted-value-results))
                                              key)
                                           val)
                                     ...)
                               #t)
                              (_ #f)))
                       (and (jsobj-assoc result '@language)
                            (jsobj-assoc result '@type)))
               (error 'json-ld-error "invalid value object"))

             (let ((result-value (jsobj-ref result '@value)))
               (cond ((eq? result-value 'null)
                      (return 'null active-context))
                     ((and (not (string? result-value))
                           (jsobj-assoc result '@language))
                      (error 'json-ld-error "invalid typed value"))
                     ((and (jsobj-assoc result '@type)
                           (not (absolute-uri? (jsobj-ref result '@type))))
                      (error 'json-ld-error "invalid typed value"))
                     (else result))))
            ;; sec 9
            ;; @@: unnecessarily pulling type out of result several times,
            ;;   we could do it just once... maybe with a (delay) at top
            ;;   of cond?
            ((and (jsobj-assoc result '@type)
                  (listy? (jsobj-ref result '@type)))
             (hash-set result '@type (jsobj-ref result '@type)))

            ;; sec 10
            ((or (jsobj-assoc result '@set)
                 (jsobj-assoc result '@list))
             ;; @@: Hacky
             (let* ((num-members (hash-count result)))
               ;; 10.1
               (when (not (or (eqv? num-members 1)
                              (and (jsobj-assoc result '@index)
                                   (eqv? num-members 2))))
                 (error 'json-ld-error "invalid set or list object"))

               ;; 10.2
               (let ((set-mapping (jsobj-assoc result '@set)))
                 (if set-mapping
                     (cdr set-mapping)
                     result))))
            (else result)))

         ;; sec 11
         (define (adjust-result-2 result)
           (if (and (jsobj-assoc result '@language)
                    (eqv? (hash-count result) 1))
               (return 'null active-context)
               result))

         ;; sec 12
         (define (adjust-result-3 result)
           ;; Graph adjustments...
           (if (member active-property '(null "@graph"))
               ;; drop free-floating values
               (cond ((or (eqv? (hash-count result) 0)
                          (jsobj-assoc result '@value)
                          (jsobj-assoc result '@list))
                      (return 'null active-context))
                     ;; @@: Do we need to check jsobj? at this point?
                     ;;   I think the only other thing result becomes is 'null
                     ;;   and we return it explicitly in such a case
                     ((and (jsobj-assoc result '@id)
                           (eqv? 1 (hash-count result)))
                      (return 'null active-context))

                     (else result))
               ;; otherwise, do nothing
               result))

         ;; sec 13
         (values
          ((compose-forward adjust-result-1 adjust-result-2 adjust-result-3)
           result)
          active-context))))))

(define (expand-element active-context active-property element)
  (match element
    ('null
     (values 'null active-context))
    ((? scalar? _)
     (if (member active-property '(null "@graph"))
         (values 'null active-context)
         ;; Note that value-expansion should also do a multi-value return
         ;; with active-context... in theory...
         (value-expansion active-context active-property element)))
    ((? listy? _)
     ;; Does a multi-value return with active context
     (expand-json-array active-context active-property element))
    ((? jsobj? _)
     (expand-json-object active-context active-property element))))

(define (expand jsobj #:return-active-context [return-active-context #f])
  "Expand (v?)json using json-ld processing algorithms"
  (let-values ([(expanded-result active-context)
                (expand-element initial-active-context 'null jsobj)])
    ;; final other than arrayify that is!
    (define (final-adjustments expanded-result)
      (cond ((and (jsobj? expanded-result)
                  (eqv? 1 (hash-count expanded-result))
                  (jsobj-assoc expanded-result '@graph))
             (jsobj-ref expanded-result '@graph))
            ((eq? expanded-result 'null)
             '())
            (else expanded-result)))
    (define (arrayify expanded-result)
      (if (listy? expanded-result)
          expanded-result
          (list expanded-result)))
    (let ([final-result ((compose-forward final-adjustments arrayify)
                         expanded-result)])
      (if return-active-context
          (values final-result active-context)
          final-result))))


;; Algorithm 7.2

(define (value-expansion active-context active-property value)
  (call/ec
   (lambda (return)
     (let* ((term-mapping (active-context-terms-assoc active-property active-context))
            (type-mapping
             (if term-mapping
                 (jsobj-assoc (cdr term-mapping) '@type)
                 #f)))
       (define (id-or-vocab-return expansion-thunk)
         (let-values ([(result active-context)
                       (expansion-thunk)])
           (return
            `#hasheq((@id . ,result))
            active-context)))

       ;; sec 1
       (when (and type-mapping (eq? (cdr type-mapping) '@id))
         (id-or-vocab-return
          (lambda ()
            (iri-expansion active-context value
                           #:document-relative #t))))

       ;; sec 2
       (when (and type-mapping (eq? (cdr type-mapping) '@vocab))
         (id-or-vocab-return
          (lambda ()
            (iri-expansion active-context value
                           #:vocab #t
                           #:document-relative #t))))

       ;; sec 3
       (let ((result `#hasheq((@value . ,value))))
         (cond
          ;; sec 4
          (type-mapping
           (values
            (hash-set result '@type (cdr type-mapping))
            active-context))
          ;; sec 5
          ((string? value)
           (let ((language-mapping
                  (if term-mapping
                      (jsobj-assoc (cdr term-mapping) '@language)
                      #f)))
             (match language-mapping
               ;; if no mapping, or the mapping value is nil,
               ;; return as-is
               ((cons _ 'null)
                (values result active-context))
               ;; Otherwise if there's a match add @language to result
               ((cons _ language)
                (values
                 (hash-set result '@language language)
                 active-context))
               ;; otherwise...
               (_
                (let ((default-language (active-context-language active-context)))
                  (if (defined? default-language)
                      (values (hash-set result '@language default-language)
                              active-context)
                      (values result active-context)))))))
          (else (values result active-context))))))))


;;; Compaction

;;; Algorithm 8.1
(define (compact-element active-context inverse-context active-property
                         element [compact-arrays #t])
  (match element
    ;; sec 1
    ;; If the element is a scalar, it is already in its most compact form
    ((? scalar?) element)
    ;; sec 2
    ;; If the element is an array, compact each item recursively and return
    ;; in a new array
    ((list items ...)
     ;; 2.1 - 2.2
     (define result
       (foldr
        (lambda (item prev)
          (define compacted-item
            (compact-element active-context inverse-context active-property
                             item compact-arrays))
          (if (eq? compacted-item 'null)
              result
              (cons compacted-item prev)))
        '()
        items))
     ;; 2.3 / 2.4
     (if (and (= (length result) 1)
              (not (active-context-container-mapping active-property
                                                     active-context))
              compact-arrays)
         (first result)
         result))
    ;; sec 3
    ((? jsobj?)
     (cond
      ;; sec 4
      ((and (or (hash-has-key? element '@value)
                (hash-has-key? element '@id))
            (scalar? (compact-value active-context inverse-context
                                    active-property element)))
       element)
      (else
       (let ([inside-reverse (eq? active-property '@reverse)])  ; sec 5
         ;; sec 7
         (sequence-fold
          (lambda (result expanded-property expanded-value)
            (cond
             ;; 7.1
             ((member expanded-property '(@id @type))
              (define compacted-value
                (if (string? expanded-value)
                    ;; 7.1.1
                    (iri-compaction active-context inverse-context
                                    expanded-value (eq? expanded-property
                                                        '@type))
                    ;; 7.1.2
                    (let ([intermediate-cv
                           (map
                            (lambda (expanded-type)
                              (iri-compaction active-context
                                              inverse-context
                                              expanded-type
                                              #t))
                            expanded-value)])
                      (if (= (length intermediate-cv) 1)
                          (first intermediate-cv)
                          intermediate-cv))))
              ;; 7.1.3
              (define alias
                (iri-compaction active-context inverse-context
                                expanded-property #t))
              (hash-set result alias compacted-value))
             ;; 7.2
             ((eq? expanded-property '@reverse)
              ;; 7.2.1
              (define initial-compacted-value
                (compact-element active-context inverse-context
                                 '@reverse expanded-value compact-arrays))
              (define boxed-result (box result))
              (define (result-box-set! key val)
                (set-box! boxed-result
                          (hash-set (unbox boxed-result)
                                    key val)))
              ;; 7.2.2
              (define compacted-value
                (sequence-fold
                 (lambda (compacted-value property value)
                   ;; 7.2.2.1
                   ;; This section is very... confusing, with the ifs
                   (if (active-context-reverse-property? property active-context)
                       (let* ([value
                               (if (and (or (eq? (active-context-container-mapping
                                                  property
                                                  active-context)
                                                 '@set)
                                            (not compact-arrays))
                                        (not (listy? value)))
                                   (list value)
                                   value)])
                         (if (not (hash-has-key? (unbox boxed-result) property))
                             ;; 7.2.2.1.2
                             (result-box-set! property value)
                             ;; 7.2.2.1.3
                             (result-box-set! property
                                              ;; @@: Or should it be the reverse order?
                                              (flatten-append (hash-ref (unbox boxed-result)
                                                                        property)
                                                              value)))
                         ;; 7.2.2.1.4
                         (hash-remove compacted-value property))
                       ;; implied that this is the default for the non-if
                       ;; in 7.2.2.1
                       compacted-value))
                 initial-compacted-value
                 initial-compacted-value))
              ;; 7.2.3
              (when (not (hash-empty? compacted-value))
                (let (;; 7.2.3.1
                      [alias (iri-compaction active-context inverse-context
                                             '@reverse #t)])
                  (result-box-set! alias compacted-value)))
              (unbox result))
             
             ;; 7.3
             ((and (eq? expanded-property '@index)
                   (eq? (active-context-container-mapping
                         active-property
                         active-context)
                        '@index))
              result)

             ;; 7.4
             ((member expanded-property '(@index @value @language))
              (let (;; 7.4.1
                    [alias (iri-compaction active-context inverse-context
                                           expanded-property #t)])
                (hash-set result alias expanded-value)))

             ;; 7.5
             ;; @@: But is this actually supposed to be part of the next
             ;;   section where we do 7.6?  It doesn't say
             ;;   "continue with next expanded property"!
             ;;   But you'd think that would be true because otherwise 7.6.1
             ;;   would clobber it
             ((null? expanded-value)
              (let ([item-active-property
                     (iri-compaction active-context inverse-context
                                     expanded-property expanded-value
                                     #t inside-reverse)])
                (if (not (hash-has-key? result item-active-property))
                    ;; 7.5.2
                    (hash-set result item-active-property '())
                    (match (hash-ref result item-active-property)
                      ((list items ...)
                       ;; I guess we leave it as is?  This section
                       ;; is very confusing
                       result)
                      (iap-cur-val
                       (hash-set result item-active-property
                                 (list iap-cur-val)))))))

             ;; 7.6
             (else
              (foldr
               (lambda (expanded-item result)
                 (let* (;; 7.6.1
                        [item-active-property
                         (iri-compaction active-context inverse-context
                                         expanded-property expanded-item
                                         #t inside-reverse)]
                        ;; 7.6.2
                        [container
                         (maybe-symbolify
                          (or (active-context-container-mapping
                               item-active-property
                               active-context)
                              'null))]
                        ;; 7.6.3
                        [compacted-item
                         (compact-element active-context inverse-context
                                          item-active-property
                                          (if (hash-has-key? expanded-item '@list)
                                              (hash-ref expanded-item '@list)
                                              expanded-item))]
                        ;; 7.6.4
                        [compacted-item
                         (if (listy? expanded-item)
                             (let* (;; 7.6.4.1
                                    [compacted-item
                                     (if (listy? compacted-item)
                                         compacted-item
                                         (list compacted-item))])
                               ;; 7.6.4.2
                               (cond
                                ((not (eq? container '@list))
                                 ;; 7.4.6.2.1
                                 (let ([c-i
                                        `#hasheq((,(iri-compaction active-context
                                                                   inverse-context
                                                                   '@list)
                                                  . ,compacted-item))])
                                   ;; 7.4.6.2.2
                                   (if (hash-has-key? expanded-item '@index)
                                       (hash-set c-i
                                                 (iri-compaction active-context
                                                                 inverse-context
                                                                 '@index)
                                                 (hash-ref expanded-item '@index))
                                       c-i)))
                                ;; 7.6.4.3
                                ((not (hash-has-key? result item-active-property))
                                 (error 'json-ld-error
                                        "compaction to lists of lists"))
                                ;; the default, though unsaid
                                (else compacted-item)))
                             compacted-item)])
                   (if (member container '(@language @index))
                       ;; 7.6.5
                       (let* (;; 7.6.5.1
                              [result
                               (if (not (hash-has-key? result item-active-property))
                                   (hash-set result item-active-property
                                             #hasheq())
                                   result)]
                              [map-object
                               (hash-ref result item-active-property)]
                              ;; 7.6.5.2
                              [compacted-item
                               (if (and (eq? container 'language)
                                        (hash-has-key? compacted-item '@value))
                                   (hash-ref compacted-item '@value)
                                   compacted-item)]
                              ;; 7.6.5.3
                              [map-key
                               (hash-ref expanded-item container)]
                              ;; 7.6.5.4
                              [map-object
                               (if (not (hash-has-key? map-object map-key))
                                   (hash-set map-object map-key compacted-item)
                                   (hash-set map-object
                                             (append
                                              (match (hash-ref map-object map-key)
                                                ((list items ...)
                                                 items)
                                                (item (list item)))
                                              (list compacted-item))))])
                         (hash-set result item-active-property map-object))
                       ;; 7.6.6
                       (let ([compacted-item
                              (if (and (or (not compact-arrays)
                                           (member container '(@set @list))
                                           (member expanded-property '(@list @graph)))
                                       (not (listy? compacted-item)))
                                  (list compacted-item)
                                  compacted-item)])
                         (if (not (hash-has-key? result item-active-property))
                             (hash-set result item-active-property compacted-item)
                             (let ([cur-val
                                    (match (hash-ref result item-active-property)
                                      ((? listy? cur-val)
                                       cur-val)
                                      (cur-val (list cur-val)))])
                               (hash-set result
                                         item-active-property
                                         ;; @@: I get this sense that doing this
                                         ;;   append stuff is unnecessary... we could
                                         ;;   do things way more efficiently if we cons
                                         ;;   in the right direction...
                                         (flatten-append cur-val compacted-item))))))))
               result
               expanded-value))))
          '#hasheq()  ; sec 6
          element)))))))

(define (compact jsobj context #:compact-arrays [compact-arrays #t])
  (let* ([active-context
          (process-context initial-active-context context)]
         [inverse-context
          (create-inverse-context active-context)]
         [active-property 'null]
         [element (expand jsobj)]
         [result
          (match (compact-element active-context inverse-context
                                  active-property element
                                  compact-arrays)
            ;; compaction algorithm epilogue
            ((? listy? result)
             `#hasheq((,(iri-compaction active-context inverse-context '@graph)
                       . ,result)))
            (result result))])
    (if (not (hash-empty? context))
        (hash-set result '@context context)
        result)))

(define (create-inverse-context active-context)
  (define terms
    (active-context-terms active-context))
  ;; for 3
  (define sorted-term-keys
    ;; We convert all the symbols to strings and back again up front
    ;; so we don't have to do it per comparison
    (map
     string->symbol
     (sort (map symbol->string (hash-keys terms))
           (lambda (k1 k2)
             ;; Sort keys K1 and K2 by string length, or fall back to by
             ;; the lexicographically least term
             (define k1-len (string-length k1))
             (define k2-len (string-length k2))
             (if (= k1-len k2-len)
                 (string<? k1 k2)
                 (< k1-len k2-len))))))
  ;; note that unlike most of the other algorithms, we mutate this one...
  ;; maybe we'll have reason to convert that later
  ;; sec 1
  (define result
    (make-hash))
  (define default-language
    (if (defined? (active-context-language active-context))
        (active-context-language active-context)
        '@none))
  ;; sec 3
  (for ([term sorted-term-keys])
    (define term-definition
      (hash-ref terms term))
    ;; 3.1
    ;; if the term definition is null, nothing to do here
    (unless (eq? term-definition 'null)
      ;; otherwise, continue...
      (let* ([container
              (or (active-context-container-mapping
                   term-definition
                   active-context)
                  '@none)]
             ;; 3.3
             ;; @@: I think this is what the iri mapping is?
             [iri (jsobj-ref term-definition '@id)]
             ;; 3.4 / 3.5
             [container-map
              (or (hash-ref result iri #f)
                  (let ([hv (make-hash)])
                    (hash-set! result iri hv)
                    hv))]
             ;; 3.6 / 3.7
             [type/language-map
              (or (hash-ref container-map container #f)
                  (let ([tlm (make-hash `([@language . ,(make-hash)]
                                          [@type . ,(make-hash)]))])
                    (hash-set! container-map container tlm)
                    tlm))])
        (cond
         ;; 3.8
         ((active-context-reverse-property? term active-context)
          (let ([type-map
                 (hash-ref type/language-map '@type)])
            (when (not (hash-has-key? type-map '@reverse))
              (hash-set! type-map '@reverse term))))
         ;; 3.9
         ((hash-has-key? term-definition '@type)
          (let ([tm-for-term-def (hash-ref term-definition '@type)]
                [type-map
                 (hash-ref type/language-map '@type)])
            (when (not (hash-has-key? type-map tm-for-term-def))
              (hash-set! type-map tm-for-term-def term))))
         ;; 3.10
         ((hash-has-key? term-definition '@language)
          (let* ([language-mapping
                  (hash-ref term-definition '@language)]
                 ;; 3.10.1
                 [language-map
                  (hash-ref type/language-map '@language)]
                 ;; 3.10.2
                 [language
                  (if (eq? language-mapping 'null)
                      '@null
                      ;; ?? I think this is right?
                      language-mapping)])
            (when (not (hash-has-key? language-map language))
              (hash-set! language-map language term))))
         ;; 3.11
         (else
          (let ([language-map (hash-ref type/language-map '@language)])
            (when (not (hash-has-key? language-map default-language))
              (hash-set! language-map default-language term))
            (when (not (hash-has-key? language-map '@none))
              (hash-set! language-map '@none term))
            (let ([type-map (hash-ref type/language-map '@type)])
              (when (not (hash-has-key? type-map '@none))
                (hash-set! language-map '@none term)))))))))
  result)

(define (compact-value active-context inverse-context active-property value)
  (define number-members
    ;; sec 1
    (- (hash-count value)
       ;; sec 2
       (if (and (hash-has-key? value '@index)
                (eq? (active-context-container-mapping
                      active-property
                      active-context) '@index))
           1 0)))
  (cond
   ;; sec 3
   ;; If number-members is greater than 2, we return value as-is because
   ;; it can't be further compacted
   ((> number-members 2)
    value)
   ;; sec 4
   ((hash-has-key? value '@id)
    (let ([type-mapping (active-context-type-mapping active-property active-context)])
      (cond
       ;; 4.1
       ((and (= number-members 1)
             (eq? type-mapping '@id))
        (iri-compaction active-context inverse-context (hash-ref value '@id)))
       ;; 4.2
       ((and (= number-members 1)
             (eq? type-mapping '@vocab))
        (iri-compaction active-context inverse-context (hash-ref value '@id)))
       ;; 4.3
       (else value))))
   ;; @@: these next three could be wrapped in an or but I guess
   ;;   it's fine as-is
   ;; sec 5
   ((and (hash-has-key? value '@type)
         (equal? (hash-ref value '@type)
                 (active-context-type-mapping active-property active-context)))
    (hash-ref value '@value))
   ;; sec 6
   ((and (hash-has-key? value '@language)
         (equal? (hash-ref value '@language)
                 (active-context-language-mapping active-property active-context)))
    (hash-ref value '@value))
   ;; sec 7
   ((and (= number-members 1)
         (or (not (string? (hash-ref value '@value)))
             (not (active-context-language active-context))
             (eq? (active-context-language-mapping active-property active-context)
                  'null)))
    (hash-ref value '@value))
   (else value)))

(define (iri-compaction active-context inverse-context iri
                        [value #f] [vocab? #f] [reverse? #f])
  (call/ec
   (lambda (return)
     (cond
      ;; sec 1
      ((eq? iri 'null)
       'null)
      ;; sec 2
      ((and vocab? (hash-has-key? inverse-context iri))
       (let* (;; 2.1
              [default-language (active-context-language active-context)]
              ;; 2.2
              [containers '()]
              ;; 2.3
              [type/language '@language]
              [type/language-value '@null])
         ;; 2.4
         (when (and (hash? value)
                    (hash-has-key? value '@index))
           (set! containers (cons '@index containers)))
         (cond
          ;; 2.5
          (reverse?
           (set! type/language '@type)
           (set! type/language-value '@reverse)
           (set! containers (cons '@set containers)))
          ;; 2.6
          ((hash-has-key? value '@list)
           ;; 2.6.1
           (when (not (hash-has-key? value '@index))
             (set! containers (cons '@set containers)))
           (let* (;; 2.6.2
                  [lst (hash-ref value '@list)]
                  ;; 2.6.3
                  [common-type 'null]
                  [common-language
                   (if (null? lst)
                       default-language
                       'null)])
             (call/ec
              (lambda (escape-loop)
                ;; 2.6.4
                (for ([item lst])
                  (let (;; 2.6.4.1
                        [item-language '@none]
                        [item-type '@none])
                    (if (hash-has-key? item '@value)
                        (cond
                         ((hash-has-key? item '@language)
                          (set! item-language (hash-ref item '@language)))
                         ((hash-has-key? item '@type)
                          (set! item-type (hash-ref item '@type)))
                         (else
                          (set! item-language '@null)))
                        (set! item-type '@id))
                    (cond
                     ;; 7.6.4.4
                     ((eq? common-language 'null)
                      (set! common-language item-language))
                     ;; 7.6.4.5
                     ((and (not (hash-has-key? item-language common-language))
                           (hash-has-key? item '@value))
                      (set! common-language '@none)))
                    (cond
                     ;; 7.6.4.6
                     ((eq? common-type 'null)
                      (set! common-type item-type))
                     ;; 7.6.4.7
                     ((not (equal? item-type common-type))
                      (set! common-type '@none)))
                    ;; 7.6.4.8
                    (when (and (eq? common-language '@none)
                               (eq? common-type '@none))
                      (escape-loop))))))
             ;; 2.6.5
             (when (eq? common-language 'null)
               (set! common-language '@none))
             ;; 2.6.6
             (when (eq? common-type 'null)
               (set! common-type '@none))
             (if (not (eq? common-type '@none))
                 ;; 2.6.7
                 (begin
                   (set! type/language '@type)
                   (set! type/language-value common-type))
                 ;; 2.6.8
                 (set! type/language common-language))))
          ;; 2.7
          (else
           (if (hash-has-key? value '@value)
               ;; 2.7.1
               (cond
                ;; 2.7.1.1
                ((and (hash-has-key? value '@language)
                      (not (hash-has-key? value '@index)))
                 (set! type/language-value
                       (hash-ref value '@language)))
                ;; 2.7.1.2
                ((hash-has-key? value '@type)
                 (set! type/language-value '@type)
                 (set! type/language '@type)))
               ;; 2.7.2
               (begin
                 (set! type/language '@type)
                 (set! type/language-value '@id)))))
         (set! containers (cons '@set containers))
         ;; 2.8
         (set! containers (cons '@none containers))
         ;; 2.9
         (when (eq? type/language-value 'null)
           (set! type/language-value '@null))
         ;; 2.10
         (let ([preferred-values '()])
           ;; 2.11
           (when (eq? type/language-value '@reverse)
             (set! preferred-values (cons '@reverse preferred-values)))
           ;; 2.12
           (if (and (member type/language-value '(@id @reverse))
                    (hash-has-key? value '@id))
               ;; 2.12
               (if (equal?
                    (active-context-iri-mapping
                     (iri-compaction active-context
                                     inverse-context
                                     (hash-ref value '@id)
                                     #t #t #t)
                     active-context)
                    (hash-ref value '@id))
                   ;; @@: Note that these look backwards because lisp lists :P
                   ;; 2.12.1
                   (set! preferred-values
                         (cons '@none
                               (cons '@id
                                     (cons '@vocab preferred-values))))
                   ;; 2.12.2
                   (set! preferred-values
                         (cons '@none
                               (cons '@vocab
                                     (cons '@id preferred-values)))))
               ;; 2.13
               (set! preferred-values
                     (cons '@none
                           (cons type/language-value preferred-values))))
           ;; 2.14
           (let ([term (term-selection inverse-context iri containers
                                       type/language preferred-values)])
             ;; 2.15
             (when (not (eq? term 'null))
               (return term))))))
      (else
       ;; 3
       (when (and vocab? (defined? (active-context-vocab active-context)))
         (let ([iri-str (maybe-stringify iri)]
               [vocab-mapping (maybe-stringify
                               (active-context-vocab active-context))])
           ;; 3.1
           (when (and iri-str
                      (string-prefix? iri-str vocab-mapping)
                      (> (string-length iri-str) (string-length vocab-mapping)))
             (let ([suffix (substring iri-str (string-length vocab-mapping))])
               (return suffix)))))
       ;; 4
       (let ([iri-str (maybe-stringify iri)]
             [compact-iri 'null])
         ;; 5
         (for ([(term term-definition) (active-context-terms active-context)])
           (let ([term-str (symbol->string term)])
             ;; 5.1, 5.2
             (cond
              ;; 5.1
              ((string-contains? term-str ":")
               'skip-me)
              ;; 5.2
              ((or (eq? (pk 'term-definition term-definition) 'null)
                   (let ([iri-mapping (hash-ref term-definition '@id)])
                     (or (equal? iri-mapping iri-str)
                         (and iri-str iri-mapping (not (string-prefix? iri-str iri-mapping))))))
               'skip-me)
              (else
               (let* ([iri-mapping
                       (hash-ref term-definition '@id)]
                      ;; 5.3
                      [candidate (string-append term-str ":"
                                                (substring iri-str (string-length iri-mapping)))])
                 ;; 5.4
                 ;; the grouping of ands and ors is really unclear here to me
                 (when (and (or (eq? compact-iri 'null)
                                (< (string-length candidate) (string-length compact-iri))
                                (and (= (string-length candidate) (string-length compact-iri))
                                     (string<? candidate compact-iri)))
                            (or (not (active-context-terms-assoc candidate active-context))
                                (and (equal? iri-mapping iri)
                                     (eq? value 'null))))
                   (set! compact-iri candidate)))))))
         ;; 6
         (when (not (eq? compact-iri 'null))
           (return (maybe-symbolify compact-iri)))
         ;; 7
         (when (not vocab?)
           ;; @@: It says document's base IRI, but I assumes it means this?
           (return (maybe-symbolify
                    (absolute->relative-url
                     iri (active-context-base active-context)))))
         ;; 8
         iri))))))

(define (term-selection inverse-context iri containers
                        type/language preferred-values)
  (call/ec
   (lambda (return)
     (define container-map (hash-ref inverse-context iri))
     (for ([container containers])
       ;; 2.1
       (when (hash-has-key? container-map container)
         (let* ([type/language-map (hash-ref container-map container)]
                [value-map (hash-ref type/language-map type/language)])
           ;; 2.4
           (for ([item preferred-values])
             ;; 2.4.1
             (when (hash-has-key? value-map item)
               (return (hash-ref value-map item)))))))
     ;; 3
     'null)))
