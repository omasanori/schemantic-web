;;; -*- Mode: Scheme; scheme48-package: sparql-examples -*-

;;;; Schemantic Web Examples
;;;; SPARQL

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (dbpedia:persons-born-in-berlin-before-1700)
  (http-get-sparql
   "http://dbpedia.org/sparql/"
   '((prologue
      (prefix foaf "http://xmlns.com/foaf/0.1/")
      (prefix dbpedia "http://dbpedia.org/")
      (prefix xsd "http://www.w3.org/2001/XMLSchema#"))
     (select (name birthdate) ()
       (triples person
         ((: dbpedia "birthplace")      "http://dbpedia.org/resource/Berlin")
         ((: dbpedia "birth")           birthdate)
         ((: foaf "name")               name))
       (filter (< birthdate
                  (literal (typed (: xsd "date"))
                           "1700-01-01")))))))
