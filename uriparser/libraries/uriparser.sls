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
    uriParseUriA platform-uriParseUriA
    (rename (platform-uriFreeUriMembersA uriFreeUriMembersA)) platform-uriFreeUriMembersA

    ;; recomposition
    uriToStringCharsRequiredA platform-uriToStringCharsRequiredA

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

(define (make-uriparser-condition* uriparser-numeric-value)
  (make-uriparser-condition uriparser-numeric-value
			    (uriparser-error->symbol/or-error uriparser-numeric-value)))

(define raise-uriparser-error
  (case-lambda
   ((who uriparser)
    (raise-uriparser-error who uriparser #f))
   ((who uriparser irritants)
    (raise (condition (make-who-condition who)
		      (make-message-condition (strerror uriparser))
		      (make-uriparser-condition* uriparser)
		      (make-irritants-condition irritants))))))



;;;; basic parsing


(define (uriParseUriA parser uri)
  (with-compensations
    (let* ((cstr	(string->cstring uri))
	   (result	(platform-uriParseUriA parser cstr)))
      (unless (= URI_SUCCESS result)
	(raise-uriparser-error 'uriParseUriA result uri)))))


;;;; recomposition

(define-c-function platform-uriToStringCharsRequiredA
  (int uriToStringCharsRequiredA (pointer pointer)))

(define-c-function platform-uriToStringA
  (int uriToStringA (char* pointer int pointer)))

(define (uriToStringCharsRequiredA uri)
  (with-compensations
    (let ((*chars-required (malloc-small/c))
	  (result (platform-uriToStringCharsRequiredA uri *chars-required)))
      (if (= URI_SUCCESS result)
	  (peek-signed-int *chars-required 0)
	(raise-uriparser-error 'uriToStringCharsRequiredA result)))))

(define (uriToStringA uri))



;;;; done

)

;;; end of file
