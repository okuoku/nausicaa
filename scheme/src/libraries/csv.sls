;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: comma-separated value (CSV) tokeniser
;;;Date: Mon Jul 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (csv)
  (export
    csv->list/comma		csv->list)
  (import (nausicaa)
    (silex lexer)
    (csv unquoted-data-lexer)
    (csv unquoted-data-comma-lexer)
    (csv strings-lexer))


(define csv->list/comma
  (case-lambda
   ((port)
    (csv->list/comma port (lambda (field) field)))

   ((port swirl-field)
    (let* ((IS			(lexer-make-IS (:port port) (:counters 'all)))
	   (string-lexer	(lexer-make-lexer csv-strings-table IS))
	   (data-lexer		(lexer-make-lexer csv-unquoted-data-table/comma IS))
	   (result		'())
	   (record		'()))
      (let-values (((port field) (open-string-output-port)))

	(define (%add-token-to-field token)
	  (write-char token port))

	(define (%add-string-to-field)
	  (%add-token-to-field #\")
	  (do ((token (string-lexer) (string-lexer)))
	      ((not token)
	       (%add-token-to-field #\"))
	    (%add-token-to-field token)))

	(define (%add-field-to-record)
	  (set! record (cons (swirl-field (field)) record)))

	(define (%add-record-to-result)
	  (set! result (cons (reverse record) result))
	  (set! record '()))

	(do ((token (data-lexer) (data-lexer)))
	    ((not token)
	     (%add-field-to-record)
	     (reverse (cons (reverse record) result)))
	  (case token

	    ((eol)
	     (%add-field-to-record)
	     (%add-record-to-result))

	    ((field)
	     (%add-field-to-record))

	    ((string)
	     (%add-string-to-field))

	    (else
	     (%add-token-to-field token)))))))))


(define csv->list
  (case-lambda
   ((port separators)
    (csv->list port separators (lambda (field) field)))

   ((port separators swirl-field)
    (let* ((IS			(lexer-make-IS (:port port) (:counters 'all)))
	   (string-lexer	(lexer-make-lexer csv-strings-table IS))
	   (data-lexer		(lexer-make-lexer csv-unquoted-data-table/comma IS))
	   (result		'())
	   (record		'()))
      (let-values (((port field) (open-string-output-port)))

	(define (%add-token-to-field token)
	  (write-char token port))

	(define (%add-string-to-field)
	  (%add-token-to-field #\")
	  (do ((token (string-lexer) (string-lexer)))
	      ((not token)
	       (%add-token-to-field #\"))
	    (%add-token-to-field token)))

	(define (%add-field-to-record)
	  (set! record (cons (swirl-field (field)) record)))

	(define (%add-record-to-result)
	  (set! result (cons (reverse record) result))
	  (set! record '()))

	(do ((token (data-lexer) (data-lexer)))
	    ((not token)
	     (%add-field-to-record)
	     (reverse (cons (reverse record) result)))
	  (cond

	   ((eq? 'eol token)
	    (%add-field-to-record)
	    (%add-record-to-result))

	   ((memv token separators)
	    (%add-field-to-record))

	   ((eq? 'string token)
	    (%add-string-to-field))

	   (else
	    (%add-token-to-field token)))))))))


;;;; done

)

;;; end of file
