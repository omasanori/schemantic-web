;;; -*- Mode: Scheme; scheme48-package: rdf-turtle-parser -*-

;;;; Schemantic Web
;;;; RDF Turtle Parser

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; RDF Turtle is described at <http://www.dajobe.org/2004/01/turtle/>.

(define-parser (turtle-parser:document)
  (let loop ((triples '())
             (namespace-map (make-namespace-map)))
    (parser:sequence
     (turtle-parser:ws*)                ;Simplify production rule [2]
     (parser:choice
      (*parser (((parser:end)))
        (parser:return (reverse triples)))
      (*parser ((statement (turtle-parser:statement namespace-map)))
        (case (car statement)
          ((PREFIX)
           (loop triples
                 (let ((abbreviation (cadr statement))
                       (expansion (caddr statement)))
                   (add-namespace namespace-map abbreviation expansion))))
          ((TRIPLES)
           (loop (append-reverse (cdr statement) triples)
                 namespace-map))
          (else
           (error "Bogus statement:" statement))))))))

(define-parser (turtle-parser:statement namespace-map)
  (*parser ((statement
             (parser:choice (turtle-parser:directive)
                            (turtle-parser:triples namespace-map)))
            ((turtle-parser:ws*))
            ((parser:char= #\.)))
    (parser:return statement)))

(define-parser (turtle-parser:directive)
  (*parser (((parser:string= "@prefix"))
            ((turtle-parser:ws+))
            (abbreviation (parser:optional #f (turtle-parser:prefix-name)))
            ((parser:char= #\:))
            ((turtle-parser:ws+))
            (expansion (turtle-parser:uri-ref)))
    (parser:return `(PREFIX ,abbreviation ,expansion))))

(define-parser (turtle-parser:triples namespace-map)
  (*parser ((subject.triples (turtle-parser:subject namespace-map))
            ((turtle-parser:ws+))
            (predicate/object-list.triples
             (turtle-parser:predicate/object-list namespace-map)))
    (parser:return
     `(TRIPLES ,@(append (map (let ((subject (car subject.triples)))
                                (lambda (predicate.object)
                                  (make-rdf-triple subject
                                                   (car predicate.object)
                                                   (cdr predicate.object))))
                              (car predicate/object-list.triples))
                         (cdr subject.triples)
                         (cdr predicate/object-list.triples))))))

;;;; Predicate/Object Lists

(define-parser (turtle-parser:predicate/object-list namespace-map)
  (*parser ((predicate (turtle-parser:verb namespace-map))
            ((turtle-parser:ws+))
            (objects.triples (turtle-parser:object-list namespace-map))
            (predicate/object-list.triples
             (turtle-parser:predicate/object-list-continuation namespace-map))
            ((turtle-parser:ws*))
            ((parser:optional-noise (parser:char= #\;))))
    (parser:return
     (cons (append (map (lambda (object)
                          (cons predicate object))
                        (car objects.triples))
                   (append-map car predicate/object-list.triples))
           (append (cdr objects.triples)
                   (append-map cdr predicate/object-list.triples))))))

(define-parser (turtle-parser:verb namespace-map)
  (parser:choice (parser:backtrackable
                  (*parser (((parser:char= #\a))
                            ((parser:peek (turtle-parser:ws))))
                    (parser:return rdf:type)))
                 (turtle-parser:predicate namespace-map)))

(define-parser (turtle-parser:object-list namespace-map)
  (*parser ((object.triples (turtle-parser:object namespace-map))
            ((turtle-parser:ws*))
            (objects.triples
             (parser:repeated
              (*parser (((parser:char= #\,))
                        ((turtle-parser:ws*))
                        (object.triples (turtle-parser:object namespace-map))
                        ((turtle-parser:ws*)))
                (parser:return object.triples)))))
    (parser:return
     (cons (cons (car object.triples)
                 (map car objects.triples))
           (append (cdr object.triples)
                   (append-map cdr objects.triples))))))

(define-parser (turtle-parser:predicate/object-list-continuation namespace-map)
  (parser:repeated
   (*parser (((turtle-parser:ws*))
             ((parser:char= #\;))
             ((turtle-parser:ws*))
             (predicate (turtle-parser:verb namespace-map))
             ((turtle-parser:ws+))
             (objects.triples (turtle-parser:object-list namespace-map)))
     (parser:return
      (cons (map (lambda (object)
                   (cons predicate object))
                 (car objects.triples))
            (cdr objects.triples))))))

;;; `Sole' means that it won't generate any auxiliary triples.

(define-parser (turtle-parser:sole parser)
  (*parser ((item parser))
    (parser:return (cons item '()))))

;;;; Resources

(define-parser (turtle-parser:subject namespace-map)
  (parser:choice (turtle-parser:sole (turtle-parser:resource namespace-map))
                 (turtle-parser:blank namespace-map)))

(define-parser (turtle-parser:predicate namespace-map)
  (turtle-parser:resource namespace-map))

(define-parser (turtle-parser:object namespace-map)
  (parser:choice (turtle-parser:sole (turtle-parser:literal namespace-map))
                 (turtle-parser:blank namespace-map)
                 (turtle-parser:sole (turtle-parser:resource namespace-map))))

(define-parser (turtle-parser:resource namespace-map)
  (parser:choice
   (*parser ((uri-text (turtle-parser:uri-ref)))
     (cond ((maybe-string->uri uri-text) => parser:return)
           (else
            (parser:error
             (string-append "Malformed URI text `" uri-text "'")))))
   (turtle-parser:qname namespace-map)))

(define-parser (turtle-parser:uri-ref)
  (parser:list->string
   (parser:bracketed* (parser:char= #\<) (parser:char= #\>)
     (turtle-parser:string-char turtle-char-set:ucharacter
                                turtle-string-escapes:uri-ref))))

(define-parser (turtle-parser:literal namespace-map)
  (parser:choice (turtle-parser:string namespace-map)
                 (turtle-parser:number)
                 (turtle-parser:boolean)))

(define-parser (turtle-parser:string namespace-map)
  (*parser ((text (turtle-parser:quoted-string))
            (type
             (parser:choice (turtle-parser:string-language)
                            (turtle-parser:string-datatype namespace-map)
                            (parser:return #f))))
    (parser:return (make-rdf-literal text type))))

(define-parser (turtle-parser:string-language)
  (parser:sequence
   (parser:char= #\@)
   (parser:match->string
    (matcher:sequence
     (matcher:at-least 1
       (matcher:char-in-set turtle-char-set:language-initial))
     (matcher:repeated
      (matcher:sequence
       (matcher:char= #\-)
       (matcher:at-least 1
         (matcher:char-in-set turtle-char-set:language-trailing))))))))

(define-parser (turtle-parser:string-datatype namespace-map)
  (parser:sequence (parser:string= "^^")
                   (turtle-parser:resource namespace-map)))

(define-parser (turtle-parser:boolean)
  (*parser ((boolean
             (parser:choice (parser:string= "true")
                            (parser:string= "false"))))
    (parser:return (make-rdf-literal boolean xsd:boolean))))

;;;;; Numbers

(define-parser (turtle-parser:number)
  (parser:sequence (parser:choice (parser:char= #\+)
                                  (parser:char= #\-)
                                  (parser:char-in-set char-set:digit))
                   (parser:error "Number parsing is not yet supported.")))

;;;;; Strings

(define-parser (turtle-parser:quoted-string)
  (parser:sequence
   (parser:char= #\")
   (parser:choice
    (parser:sequence (parser:backtrackable (parser:string= "\"\""))
                     (turtle-parser:long-string-contents))
    (turtle-parser:short-string-contents))))

(define-parser (turtle-parser:long-string-contents)
  (parser:list->string
   (parser:repeated-until (parser:backtrackable (parser:string= "\"\"\""))
     (turtle-parser:string-char turtle-char-set:lcharacter
                                turtle-string-escapes:long-string))))

(define-parser (turtle-parser:short-string-contents)
  (parser:list->string
   (parser:repeated-until (parser:char= #\")
     (turtle-parser:string-char turtle-char-set:scharacter
                                turtle-string-escapes:short-string))))

(define-parser (turtle-parser:string-char char-set escapes)
  (parser:choice (*parser ((char (parser:char= #\\)))
                   (turtle-parser:string-escape escapes))
                 (parser:char-in-set char-set)))

(define-parser (turtle-parser:string-escape escapes)
  (*parser ((escape-char (parser:char)))
    (cond ((char=? escape-char #\u) (turtle-parser:unicode-escape 4))
          ((char=? escape-char #\U) (turtle-parser:unicode-escape 8))
          ((assv escape-char escapes)
           => (lambda (entry)
                (parser:return (cadr entry))))
          (else
           (parser:error
            (string-append "Invalid string escape `\\"
                           (string escape-char)
                           "'"))))))

(define-parser (turtle-parser:unicode-escape length)
  (*parser ((hex-string (parser:hex-string length)))
    (parser:return
     (let ((number (string->number hex-string #x10)))
       (if (< number ascii-limit)
           (ascii->char number)
           #\?)))))                     ;++ fix

(define turtle-string-escapes
  `((#\\ #\\)
    (#\t ,(ascii->char #x09))           ;Horizontal tab
    (#\n ,(ascii->char #x0A))           ;Line feed
    (#\r ,(ascii->char #x0D))))         ;Carriage return

(define turtle-string-escapes:long-string
  `((#\" #\")
    ,@turtle-string-escapes))

(define turtle-string-escapes:short-string
  turtle-string-escapes:long-string)

(define turtle-string-escapes:uri-ref
  `((#\> #\>)
    ,@turtle-string-escapes))

;;;;; Blank Nodes

(define-parser (turtle-parser:blank namespace-map)
  (parser:choice (turtle-parser:sole (turtle-parser:bnode:named))
                 (turtle-parser:sole (turtle-parser:bnode:empty))
                 (turtle-parser:bnode:compound namespace-map)
                 (turtle-parser:bnode:collection namespace-map)))

(define-parser (turtle-parser:bnode:named)
  (*parser ((node-id (turtle-parser:node-id)))
    (parser:return (make-rdf-bnode node-id))))

(define-parser (turtle-parser:node-id)
  (parser:sequence (parser:string= "_:")
                   (turtle-parser:name)))

(define-parser (turtle-parser:bnode:empty)
  (parser:backtrackable
   (*parser (((parser:string= "[]")))
     (parser:return (make-rdf-bnode)))))

(define-parser (turtle-parser:bnode:compound namespace-map)
  (*parser (((parser:char= #\[))
            ((turtle-parser:ws*))
            (predicate/object-list.triples
             (turtle-parser:predicate/object-list namespace-map))
            ((turtle-parser:ws*))       ;++ I think this is unnecessary.
            ((parser:char= #\])))
    (parser:return
     (let ((bnode (make-rdf-bnode)))
       (cons bnode
             (append (map (lambda (predicate.object)
                            (make-rdf-triple bnode
                                             (car predicate.object)
                                             (cdr predicate.object)))
                          (car predicate/object-list.triples))
                     (cdr predicate/object-list.triples)))))))

(define-parser (turtle-parser:bnode:collection namespace-map)
  (parser:map (parser:bracketed*
                  (parser:sequence (parser:char= #\( )
                                   (turtle-parser:ws*))
                  (parser:char= #\) )
                (*parser ((item.triples (turtle-parser:object namespace-map))
                          ((turtle-parser:ws*)))
                  (parser:return item.triples)))
    (lambda (item.triples-list)
      (let ((items (map car item.triples-list))
            (triples (append-map cdr item.triples-list)))
        (if (pair? items)
            (let ((bnode (make-rdf-bnode)))
              (cons bnode
                    (turtle-collection->triples items bnode triples)))
            (cons rdf:nil triples))))))

(define (turtle-collection->triples items bnode triples)
  (let ((item (car items)) (items (cdr items)))
    (cons (make-rdf-triple bnode rdf:first item)
          (if (pair? items)
              (let ((bnode* (make-rdf-bnode)))
                (cons (make-rdf-triple bnode rdf:rest bnode*)
                      (turtle-collection->triples items bnode* triples)))
              (cons (make-rdf-triple bnode rdf:rest rdf:nil) triples)))))

;;;; Names

(define-parser (turtle-parser:qname namespace-map)
  (*parser ((prefix (parser:optional #f (turtle-parser:prefix-name)))
            ((parser:char= #\:))
            (suffix (parser:optional #f (turtle-parser:name))))
    (cond ((expand-namespace namespace-map prefix)
           => (lambda (expansion)
                (parser:return
                 (if suffix
                     (string-append expansion suffix)
                     expansion))))
          (else
           (parser:error
            (string-append "Unknown prefix `" prefix "'"))))))

(define-parser (turtle-parser:prefix-name)
  (parser:match->string
   (matcher:sequence
    (matcher:char-in-set (char-set-delete turtle-char-set:name-initial #\_))
    (matcher:repeated (matcher:char-in-set turtle-char-set:name-trailing)))))

(define-parser (turtle-parser:name)
  (parser:match->string
   (matcher:sequence
    (matcher:char-in-set turtle-char-set:name-initial)
    (matcher:repeated (matcher:char-in-set turtle-char-set:name-trailing)))))

;;;; Miscellaneous

(define-parser (turtle-parser:ws)
  (parser:choice (parser:char-in-set turtle-char-set:ws)
                 (turtle-parser:comment)))

(define-parser (turtle-parser:ws*)
  (parser:repeated (turtle-parser:ws)))

(define-parser (turtle-parser:ws+)
  (parser:at-least 1 (turtle-parser:ws)))

(define-parser (turtle-parser:comment)
  (parser:sequence
   (parser:char= #\#)
   (parser:repeated (parser:char-not-in-set turtle-char-set:line-break))))

(define-parser (parser:hex-string length)
  (parser:list->string
   (parser:exactly length (parser:char-in-set char-set:hex-digit))))

;;;; Turtle Character Sets

;;; We use SRFI 14's UCS-RANGE->CHAR-SET several times here.  The upper
;;; bounds are all one off from what you'll find in the Turtle
;;; specification, because, for whatever stupid reason, SRFI 14's
;;; UCS-RANGE->CHAR-SET works with exclusive upper bounds.  I am sorry.

(define turtle-char-set:line-break
  (char-set (ascii->char #x0A)          ;Line feed
            (ascii->char #x0D)))        ;Carriage return

(define turtle-char-set:ws
  (char-set (ascii->char #x09)          ;Horizontal tab
            (ascii->char #x0A)          ;Line feed
            (ascii->char #x0D)          ;Carriage return
            (ascii->char #x20)))        ;Horizontal space

(define turtle-char-set:character
  (char-set-union (ucs-range->char-set #x20 #x5C)
                  (ucs-range->char-set #x5D #x110000)))

(define turtle-char-set:ucharacter
  (char-set-delete turtle-char-set:character #\>))

(define turtle-char-set:scharacter
  (char-set-delete turtle-char-set:character #\"))

(define turtle-char-set:lcharacter
  (char-set-adjoin turtle-char-set:character
                   (ascii->char #x09)   ;Horizontal tab
                   (ascii->char #x0A)   ;Carriage return
                   (ascii->char #x0D))) ;Line feed

(define turtle-char-set:language-initial
  char-set:lower-case)

(define turtle-char-set:language-trailing
  (char-set-union char-set:lower-case
                  char-set:digit))

(define turtle-char-set:name-initial
  (char-set-union (char-set #\_)
                  char-set:letter
                  (ucs-range->char-set #x00C0 #x00D7)
                  (ucs-range->char-set #x00D8 #x00F7)
                  (ucs-range->char-set #x00F8 #x0300)
                  (ucs-range->char-set #x0370 #x037E)
                  (ucs-range->char-set #x037F #x2000)
                  (ucs-range->char-set #x200C #x200E)
                  (ucs-range->char-set #x2070 #x2190)
                  (ucs-range->char-set #x2C00 #x2FF0)
                  (ucs-range->char-set #x3001 #xD800)
                  (ucs-range->char-set #xF900 #xFDD0)
                  (ucs-range->char-set #xFDF0 #xFFFE)
                  (ucs-range->char-set #x10000 #xF0000)))

(define turtle-char-set:name-trailing
  (char-set-union turtle-char-set:name-initial
                  (char-set #\- (ascii->char #x00B7))
                  char-set:digit
                  (ucs-range->char-set #x0300 #x0370)
                  (ucs-range->char-set #x203F #x2041)))

;;;; Namespace Maps

(define (make-namespace-map)
  '())

(define (add-namespace namespace-map abbreviation expansion)
  (cons (cons abbreviation expansion) namespace-map))

(define (expand-namespace namespace-map abbreviation)
  (any (if abbreviation
           (lambda (namespace)
             (let ((abbreviation* (car namespace)))
               (and (string? abbreviation*)
                    (string=? abbreviation abbreviation*)
                    (cdr namespace))))
           (lambda (namespace)
             (and (not (car namespace))
                  (cdr namespace))))
       namespace-map))

;;;; URI Utilities

(define rdf-namespace
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))

(define (rdf-uri content)
  (merge-uris (string->uri content) rdf-namespace))

(define rdf:type (rdf-uri "type"))
(define rdf:first (rdf-uri "first"))
(define rdf:rest (rdf-uri "rest"))
(define rdf:nil (rdf-uri "nil"))

(define xsd-namespace
  (string->uri "http://www.w3.org/2001/XMLSchema#"))

(define (xsd-uri content)
  (merge-uris (string->uri content) xsd-namespace))

(define xsd:boolean (xsd-uri "boolean"))
(define xsd:decimal (xsd-uri "decimal"))
(define xsd:double (xsd-uri "double"))
(define xsd:integer (xsd-uri "integer"))

;;;; RDF Data Structures

(define (make-rdf-triple subject predicate object)
  (list 'RDF-TRIPLE subject predicate object))

(define *rdf-bnode-id* 0)

(define (make-rdf-bnode . name-option)
  (list 'RDF-BNODE
        (if (pair? name-option)
            (car name-option)
            (let ((id *rdf-bnode-id*))
              (set! *rdf-bnode-id* (+ id 1))
              id))))

(define (make-rdf-literal text type)
  (list 'RDF-LITERAL type text))
