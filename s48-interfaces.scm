;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web
;;;; Interface Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface uris-interface
  (export
    make-uri
    uri?
    absolute-uri?
    relative-uri?
    uri-absolute?
    uri-relative?
    uri-scheme
    uri-authority
    uri-path
    uri-query
    uri-fragment
    uri=?

    ;; URI Authorities
    make-uri-authority
    uri-authority?
    uri-authority-userinfo
    uri-authority-host
    uri-authority-port
    uri-authority=?

    ;; URI Component Predicates
    uri-scheme?
    uri-userinfo?
    uri-host?
    uri-port?
    uri-path?
    uri-path-absolute?
    uri-path-relative?
    uri-query?
    uri-fragment?

    ;; URI Operations
    merge-uris

    ;; URI->String Conversion
    uri->string
    uri-authority->string
    write-uri
    write-uri-authority

    ;; String->URI Conversion
    object->uri
    object->absolute-uri
    object->relative-uri
    maybe-string->uri
    maybe-string->absolute-uri
    maybe-string->relative-uri
    string->uri
    string->absolute-uri
    string->relative-uri
    ))

;;;; RDF Utilities

(define-interface rdf-interface
  (export
    make-rdf-triple
    rdf-triple?
    rdf-triple/subject
    rdf-triple/predicate
    rdf-triple/object
    rdf-triple=?
    rdf-triple-hash
    rdf-triple-hash-mod

    rdf-subject=?       rdf-subject-hash        rdf-subject-hash-mod
    rdf-predicate=?     rdf-predicate-hash      rdf-predicate-hash-mod
    rdf-object=?        rdf-object-hash         rdf-object-hash-mod

    make-rdf-bnode
    rdf-bnode?
    rdf-bnode/name
    rdf-bnode=?
    rdf-bnode-hash

    make-rdf-plain-literal
    make-rdf-typed-literal
    rdf-literal?
    rdf-plain-literal?
    rdf-typed-literal?
    rdf-literal/lexical-form
    rdf-plain-literal/language-tag
    rdf-typed-literal/datatype-uri
    rdf-literal=?
    rdf-literal-hash

    rdf-uri-ref?
    rdf-uri-ref->string
    string->rdf-uri-ref
    rdf-uri-ref=?
    rdf-uri-ref-hash
    ))

;;;; Parsers

(define-interface uri-parser-interface
  (export
    uri-parser:uri-reference
    uri-parser:uri
    uri-parser:relative-ref
    uri-parser:absolute-uri             ;No fragment
    uri-matcher:uri-reference
    uri-matcher:uri
    uri-matcher:relative-ref
    uri-matcher:absolute-uri
    ))

(define-interface rdf-nt-parser-interface
  (export
    nt-parser:document
    make-nt-parser-context
    ))

(define-interface rdf-turtle-parser-interface
  (export
    turtle-parser:document
    make-turtle-parser-context
    ))

(define-interface sparql-format-interface
  (export
    sparql-format:!
    sparql-format:+
    sparql-format:+:list
    sparql-format:-
    sparql-format:*
    sparql-format:*:list
    sparql-format:/
    sparql-format:=
    sparql-format:=:list
    sparql-format:!=
    sparql-format:!=:list
    sparql-format:<
    sparql-format:<:list
    sparql-format:>
    sparql-format:>:list
    sparql-format:<=
    sparql-format:<=:list
    sparql-format:>=
    sparql-format:>=:list
    sparql-format:~
    sparql-format:and
    sparql-format:and:list
    sparql-format:ask
    sparql-format:base
    sparql-format:bnode
    sparql-format:boolean
    sparql-format:call
    sparql-format:call-list
    sparql-format:collection
    sparql-format:collection-list
    sparql-format:construct
    sparql-format:describe
    sparql-format:false
    sparql-format:filter
    sparql-format:from-dataset
    sparql-format:from-named-dataset
    sparql-format:graph-pattern
    sparql-format:group
    sparql-format:iri-ref
    sparql-format:keyword
    sparql-format:limit
    sparql-format:named-bnode
    sparql-format:number
    sparql-format:object-list
    sparql-format:objects
    sparql-format:offset
    sparql-format:optional
    sparql-format:or
    sparql-format:or:list
    sparql-format:order-by
    sparql-format:order-by:list
    sparql-format:parenthesis
    sparql-format:pattern-list
    sparql-format:plain-literal
    sparql-format:prefix
    sparql-format:prefixed-name
    sparql-format:properties
    sparql-format:property
    sparql-format:property-list
    sparql-format:select
    sparql-format:triple-pattern
    sparql-format:true
    sparql-format:typed-literal
    sparql-format:union
    sparql-format:union-list
    sparql-format:$variable
    sparql-format:?variable
    sparql-format:variable
    sparql-format:variable-prefix
    ))

(define-interface sparql-sexp-interface
  (export
    compile-sparql
    compile-sparql-prologue
    compile-sparql-query
    compile-sparql-pattern
    compile-sparql-patterns
    compile-sparql-expression
    compile-sparql-expressions
    sparql->condensed-string
    sparql->string
    write-sparql
    write-sparql-condensed
    ))

(define-interface sparql-results-interface
  (export
    parse-sparql-results
    ))

(define-interface sparql-http-client-interface
  (export
    http-get-sparql
    http-post-sparql
    ))

(define-interface simple-http-client-interface
  (export
    call-with-http-response
    default-http-uri-authority
    earliest-http-version
    http-get
    http-head
    http-header-field/name
    http-header-field/value
    http-post
    http-response?
    http-response/header-fields
    http-response/reason
    http-response/status-code
    http-response/status-type
    http-response/version
    http-version?
    http-version=?
    http-version<=?
    http-version/major
    http-version/minor
    latest-http-version
    make-http-header-field
    make-http-response
    make-http-version
    read-http-entity-body
    with-default-http-uri-authority
    ))
