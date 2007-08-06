;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web
;;;; Package Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;;; URIs

(define-structures ((uris uris-interface)
                    (uri-parser uri-parser-interface))
  (open scheme
        srfi-1                          ;List Library
        srfi-6                          ;Basic String Ports
        srfi-9                          ;define-record-type
        srfi-13                         ;String Library
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        (subset i/o (write-string))
        (subset extended-ports (call-with-string-output-port))
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        parse-errors
        )
  (optimize auto-integrate)

  (open tables weak)
  (begin
    ;++ This is bogus.  Also, `internment camp' might, just might, not
    ;++ be quite the right name, but it works for now.
    (define (make-string-internment-camp) (make-string-table))
    (define (intern internment-camp string generator)
      (define (generate)
        (let ((datum (generator)))
          (table-set! internment-camp string (make-weak-pointer datum))
          datum))
      (cond ((table-ref internment-camp string)
             => (lambda (weak-pointer)
                  (or (weak-pointer-ref weak-pointer)
                      (generate))))
            (else (generate))))
    (define (soft-intern internment-camp string)
      (cond ((table-ref internment-camp string)
             => weak-pointer-ref)
            (else #f)))
    )

  (files uri)

  ;; Make URIs print nicely.
  (open (subset define-record-types (define-record-discloser)))
  (begin
    ;; Using symbols for the tag avoids quotes when pretty-printed.
    ;; Using STRING->SYMBOL lets us maintain the case.
    (define-record-discloser <uri>
      (let ((symbol (string->symbol "URI")))
        (lambda (uri)
          `(,symbol ,(%uri-string uri)))))
    (define-record-discloser <uri-authority>
      (let ((symbol (string->symbol "URI-authority")))
        (lambda (authority)
          `(,symbol ,(uri-authority-string authority)))))
    ))

(define-structure uri-tests (export uri-tests)
  (open scheme simple-testing uris uri-parser)
  (optimize auto-integrate)
  (files test-uri))

;;;; RDF Structures

(define-structure rdf rdf-interface
  (open scheme
        srfi-9                          ;define-record-type
        srfi-23                         ;error
        uris
        )
  (optimize auto-integrate)
  (files rdf)

  ;; Make RDF data structures print nicely.
  (open (subset define-record-types (define-record-discloser)))
  (begin

    (define-record-discloser <rdf-triple>
      (lambda (triple)
        (list 'RDF-TRIPLE
              (let ((subject (rdf-triple/subject triple)))
                (cond ((rdf-uri-ref? subject) (rdf-uri-ref->string subject))
                      ((rdf-bnode? subject) `(BNODE ,(rdf-bnode/name subject)))
                      (else `(ILLEGAL-SUBJECT ,subject))))
              (let ((predicate (rdf-triple/predicate triple)))
                (if (rdf-uri-ref? predicate)
                    (rdf-uri-ref->string predicate)
                    `(ILLEGAL-PREDICATE ,predicate)))
              (let ((object (rdf-triple/object triple)))
                (cond ((rdf-uri-ref? object) (rdf-uri-ref->string object))
                      ((rdf-bnode? object) `(BNODE ,(rdf-bnode/name object)))
                      ((rdf-literal? object)
                       `(LITERAL ,@(disclose-rdf-literal object)))
                      (else `(ILLEGAL-OBJECT ,object)))))))

    (define-record-discloser <rdf-bnode>
      (lambda (bnode)
        (list 'RDF-BNODE (rdf-bnode/name bnode))))

    (define-record-discloser <rdf-literal>
      (lambda (literal)
        (cons 'RDF-LITERAL (disclose-rdf-literal literal))))))

;;;; RDF Parsers

(define-structure rdf-nt-parser rdf-nt-parser-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        uris
        uri-parser
        rdf
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        )
  (optimize auto-integrate)
  (files rdf-nt-parser))

(define-structure rdf-turtle-parser rdf-turtle-parser-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        uris
        uri-parser
        rdf
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        )
  (optimize auto-integrate)
  (files rdf-turtle-parser))

;; (define-structure rdf-xml-parser ...)
