;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Auxiliary Packages for the Schemantic Web
;;;; Interface Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface foof-loop-interface
  (export
    ((loop
      lazy-loop
      syntactic-error
      loop-clause-error

      ;; Accumulators
      listing
      listing-reverse
      appending
      appending-reverse
      listing!
      listing-into!
      summing
      multiplying
      minimizing
      maximizing

      in-list
      in-lists

      in-vector
      in-vector-reverse
      in-string
      in-string-reverse

      in-port
      in-file

      up-from
      down-from
      ) :syntax)
    ))

(define-interface nested-foof-loop-interface
  (export
    ((
      collect-average
      collect-count
      collect-display
      collect-list
      collect-list!
      collect-list-into!
      collect-list-reverse
      collect-max
      collect-min
      collect-product
      collect-stream
      collect-string
      collect-string-of-length
      collect-sum
      collect-vector
      collect-vector-of-length
      iterate
      iterate*
      iterate!
      iterate-values
      lazy-recur
      lazy-recur*
      recur
      recur*
      recur-values
      ) :syntax)
    ))

(define-interface extended-parameter-operators-interface
  (export
    (with-extended-parameter-operators :syntax)
    (with-extended-parameter-operators* :syntax)
    ))
