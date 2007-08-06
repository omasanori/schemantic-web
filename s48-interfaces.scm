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

    make-rdf-bnode
    rdf-bnode?
    rdf-bnode/name
    rdf-bnode=?

    make-rdf-plain-literal
    make-rdf-typed-literal
    rdf-literal?
    rdf-plain-literal?
    rdf-typed-literal?
    rdf-literal/lexical-form
    rdf-plain-literal/language-tag
    rdf-typed-literal/datatype-uri
    rdf-literal=?

    rdf-uri-ref?
    rdf-uri-ref->string
    string->rdf-uri-ref
    rdf-uri-ref=?
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
    ))

(define-interface rdf-turtle-parser-interface
  (export
    turtle-parser:document
    ))
