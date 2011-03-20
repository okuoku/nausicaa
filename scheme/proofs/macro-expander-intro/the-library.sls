;;; -*- coding: utf-8-unix -*-

#!r6rs
(library (the-library)
  (export doit)
  (import (for (language-for-run) run)
    (for (language-for-expand) expand)
    (for (only (rnrs) define-syntax) run))

  (define (doit)
    (write (the-macro))
    (newline))

  (define-syntax the-macro
    (lambda (stx)
      #'(quote (1 2 3))))

  )

;;; end of file
