;;; -*- Mode: Scheme; scheme48-package: rdf-nt-parser -*-

;;;; Schemantic Web
;;;; RDF N-Triples Parser

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This parser was derived from the grammar at
;;; <http://www.w3.org/TR/rdf-testcases/#ntriples>.

(define (nt-parser:document)
  (let loop ((triples '()))
    (parser:choice (*parser (((parser:end)))
                     (parser:return (reverse triples)))
                   (*parser ((item (nt-parser:line)))
                     (loop (if item (cons item triples) triples))))))

(define (nt-parser:line)
  (*parser (((nt-parser:ws*))
            (item
             (parser:choice (nt-parser:comment)
                            (nt-parser:triple)
                            (parser:return #f)))
            ((nt-parser:eoln)))
    (parser:return item)))

(define (nt-parser:comment)
  (parser:sequence
   (parser:char= #\#)
   (parser:repeated (parser:char-not-in-set nt-char-set:line-break))
   (parser:return #f)))

(define (nt-parser:triple)
  (*parser ((subject (nt-parser:subject))
            ((nt-parser:ws+))
            (predicate (nt-parser:predicate))
            ((nt-parser:ws+))
            (object (nt-parser:object))
            ((nt-parser:ws*))
            ((parser:char= #\.))
            ((nt-parser:ws*)))
    (parser:return (make-rdf-triple subject predicate object))))

(define (nt-parser:subject)
  (parser:choice (nt-parser:resource)
                 (nt-parser:blank)))

(define (nt-parser:predicate)
  (nt-parser:resource))

(define (nt-parser:object)
  (parser:choice (nt-parser:resource)
                 (nt-parser:blank)
                 (nt-parser:literal)))

(define (nt-parser:resource)
  (*parser ((uri-ref (nt-parser:uri-ref)))
    (if (match-string? (uri-matcher:uri-reference) uri-ref)
        (parser:return (string->rdf-uri-ref uri-ref))
        (parser:error
         (string-append "Malformed URI text `" uri-ref "'")))))

(define (nt-parser:blank)
  (*parser ((node-id (nt-parser:node-id)))
    (parser:return (make-rdf-bnode node-id))))

(define (nt-parser:uri-ref)
  (parser:list->string
   (parser:bracketed* (parser:char= #\<)
                      (parser:char= #\>)
     (parser:char-in-set nt-char-set:uri-ref))))

(define (nt-parser:node-id)
  (*parser (((parser:string= "_:")))
    (parser:match->string
     (matcher:sequence
      (matcher:char-in-set nt-char-set:name-initial)
      (matcher:repeated (matcher:char-in-set nt-char-set:name-trailing))))))

(define (nt-parser:literal)
  (*parser ((lexical-form (nt-parser:string)))
    (parser:choice
     (*parser ((datatype-uri (nt-parser:literal-datatype-uri)))
       (parser:return (make-rdf-typed-literal lexical-form datatype-uri)))
     (*parser ((language-tag
                (parser:choice (nt-parser:literal-language-tag)
                               (parser:return #f))))
       (parser:return (make-rdf-plain-literal lexical-form language-tag))))))

(define (nt-parser:literal-datatype-uri)
  (parser:sequence (parser:string= "^^")
                   (nt-parser:resource)))

(define (nt-parser:literal-language-tag)
  (parser:sequence
   (parser:char= #\@)
   (parser:match->string
    (matcher:sequence
     (matcher:at-least 1
       (matcher:char-in-set nt-char-set:language-initial))
     (matcher:repeated
      (matcher:sequence
       (matcher:char= #\-)
       (matcher:at-least 1
         (matcher:char-in-set nt-char-set:language-trailing))))))))

(define (nt-parser:string)
  (parser:list->string
   (parser:bracketed* (parser:char= #\")
                      (parser:char= #\")
     (nt-parser:string-char))))

(define (nt-parser:string-char)
  (*parser ((char (parser:char)))
    (if (char=? char #\\)
        (nt-parser:string-escape)
        (parser:return char))))

(define (nt-parser:string-escape)
  (*parser ((escape-char (parser:char)))
    (case escape-char
      ((#\t) (parser:return (ascii->char #x09)))
      ((#\n) (parser:return (ascii->char #x0A)))
      ((#\r) (parser:return (ascii->char #x0D)))
      ((#\u) (nt-parser:unicode-escape 4))
      ((#\U) (nt-parser:unicode-escape 8))
      (else (parser:return escape-char)))))

(define (nt-parser:unicode-escape length)
  (*parser ((hex-string (parser:hex-string length)))
    (parser:return
     (let ((number (string->number hex-string #x10)))
       (if (< number ascii-limit)
           (ascii->char number)
           #\?)))))                     ;++ fix

(define (parser:hex-string length)
  (parser:list->string
   (parser:exactly length (parser:char-in-set char-set:hex-digit))))

(define (nt-parser:eoln)
  (let ((CR (parser:char= (ascii->char #x0D)))
        (LF (parser:char= (ascii->char #x0A))))
    (parser:choice (parser:sequence CR (parser:choice LF (parser:return '())))
                   LF)))

(define (nt-parser:ws)
  (parser:char-in-set nt-char-set:ws))

(define (nt-parser:ws*)
  (parser:repeated (nt-parser:ws)))

(define (nt-parser:ws+)
  (parser:at-least 1 (nt-parser:ws)))

;;;; N-Triples Character Sets

(define nt-char-set:ws
  (char-set (ascii->char #x09)          ;Horizontal tab
            (ascii->char #x20)))        ;Space

(define nt-char-set:line-break
  (char-set (ascii->char #x0A)          ;Line feed
            (ascii->char #x0D)))        ;Carriage return

(define nt-char-set:language-initial
  char-set:lower-case)

(define nt-char-set:language-trailing
  (char-set-union char-set:lower-case
                  char-set:digit))

(define nt-char-set:name-initial
  char-set:letter)

(define nt-char-set:name-trailing
  (char-set-union char-set:letter
                  char-set:digit))

(define nt-char-set:uri-ref
  (char-set-union char-set:letter
                  char-set:digit
                  (string->char-set "!#$&'()*+,-./:;=?@_~")))
