;;;
;;;Part of: Nausicaa/Uriparser
;;;Contents: interface to Uriparser
;;;Date: Tue Dec 23, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (uriparser)
  (export

    ;; condition
    &uriparser
    (rename (make-uriparser-condition* make-uriparser-condition))
    uriparser-condition?
    uriparser-numeric-value uriparser-symbolic-value
    raise-uriparser-error

    ;; basic parsing
    uri-parser

    ;; recomposition
    uri->string

    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (uriparser stub)
    (uriparser sizeof))



;;;; condition

(define-condition-type &uriparser &error
  make-uriparser-condition
  uriparser-condition?
  (numeric-value uriparser-numeric-value)
  (symbolic-value uriparser-symbolic-value))

(define (make-uriparser-condition* numeric-value)
  (make-uriparser-condition numeric-value
			    (uriparser-error->symbol/or-error numeric-value)))

(define raise-uriparser-error
  (case-lambda
   ((who numeric-value)
    (raise-uriparser-error who numeric-value #f))
   ((who numeric-value irritants)
    (raise (condition (make-who-condition who)
		      (make-message-condition
		       (uriparser-strerror (uriparser-error->symbol/or-error numeric-value)))
		      (make-uriparser-condition* numeric-value)
		      (make-irritants-condition irritants))))))


;;;; basic parsing

(define-syntax call-uriparser
  (syntax-rules ()
    ((_ ?func ?arg0 ?arg ...)
     (let ((result (?func ?arg0 ?arg ...)))
       (unless (= URI_SUCCESS result)
	 (raise-uriparser-error (quote ?func) result))))))

(define (uri-parser uri uri-string)
  (with-compensations
    (let ((cstr		(string->cstring/c uri-string))
	  (parser	(malloc-block/c sizeof-UriParserStateA)))
      (UriParserStateA-uri-set! parser uri)
      (call-uriparser uriParseUriA parser cstr)
      uri)))

;;       (let* ((*scheme	(UriUriA-scheme-ref uri))
;; 	     (*beg	(UriTextRangeA-first-ref *scheme))
;; 	     (*end	(UriTextRangeA-afterLast-ref *scheme)))
;; (format #t "uri ~s beg ~s end ~s~%"
;; 	(pointer->integer uri)
;; 	(pointer->integer *beg)
;; 	(pointer->integer *end))
;; 	(cstring->string/len uri 3
;; 			     ;(pointer-diff *end *beg)
;; 			     )))))

(define (uri->string uri)
  (with-compensations
    (let* ((*req	(malloc-small/c))
	   (req		(let ((result	(uriToStringCharsRequiredA uri *req)))
			  (unless (= URI_SUCCESS result)
			    (raise-uriparser-error 'uri->string result))
			  (+ 1 (peek-signed-int *req 0))))
	   (cstr	(malloc-block/c 10000)))
      (call-uriparser uriToStringA cstr uri req pointer-null)
      (cstring->string cstr))))



;;;; recomposition

;; (define (uriToStringCharsRequiredA uri)
;;   (with-compensations
;;     (let ((*chars-required (malloc-small/c))
;; 	  (result (platform-uriToStringCharsRequiredA uri *chars-required)))
;;       (if (= URI_SUCCESS result)
;; 	  (peek-signed-int *chars-required 0)
;; 	(raise-uriparser-error 'uriToStringCharsRequiredA result)))))

;; (define (uriToStringA uri))



;;;; done

)

;;; end of file
