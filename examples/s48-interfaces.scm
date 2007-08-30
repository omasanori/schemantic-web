;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web Examples
;;;; Interface Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface rdf-examples-interface
  (export
    read-turtle-file
    display-test-manifest
    ))

(define-interface sparql-examples-interface
  (export
    dbpedia:persons-born-in-berlin-before-1700
    ))
