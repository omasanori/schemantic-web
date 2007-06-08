;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web
;;;; Package Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

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
    (define-record-discloser <uri>
      (lambda (uri)
        `("URI" ,(%uri-string uri))))
    (define-record-discloser <uri-authority>
      (lambda (authority)
        `("URI-authority" ,(uri-authority-string authority))))
    ))

;;;; RDF Parsers

(define-structure rdf-nt-parser rdf-nt-parser-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        uris
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
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        )
  (optimize auto-integrate)
  (files rdf-turtle-parser))

;; (define-structure rdf-xml-parser ...)
