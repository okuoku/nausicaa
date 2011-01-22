;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: routines to handle alists with identifiers as keys
;;;Date: Mon Dec 20, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language identifier-alists)
  (export
    identifier-alist-cons
    identifier-alist-replace
    identifier-alist-new
    identifier-alist-ref
    identifier-alist-remove
    identifier-alist-exists)
  (import (rnrs))


(define (identifier-alist-cons key value table)
  ;;Given the alist  TABLE: store KEY and VALUE in  the alist and return
  ;;the new alist.  Does no check for duplicate KEY in TABLE.
  ;;
  `((,key . ,value) . ,table))

(define (identifier-alist-replace key value table)
  ;;Given the alist  TABLE: store KEY and VALUE in  the alist and return
  ;;the new alist.  Any occurrences of KEY in TABLE are removed.
  ;;
  `((,key . ,value) . ,(identifier-alist-remove table key)))

(define (identifier-alist-new key value table)
  ;;Given the alist  TABLE: store KEY and VALUE in  the alist and return
  ;;the new alist.  Check if KEY is  already in TABLE: if it is raise an
  ;;assertion violation.
  ;;
  (if (identifier-alist-exists table key)
      (assertion-violation 'identifier-alist-new
	"key already present in identifier alist" key)
    `((,key . ,value) . ,table)))

(define (identifier-alist-exists table key)
  ;;Given the alist TABLE: return the  first pair in the list having KEY
  ;;as car.  If KEY is not present: return false.
  ;;
  (assp (lambda (pair-key)
	  (free-identifier=? pair-key key))
    table))

(define (identifier-alist-ref table key default)
  ;;Given the  alist TABLE: look for  the identifier KEY  and return its
  ;;value.  If KEY is not present: return DEFAULT.
  ;;
  (let ((pair (identifier-alist-exists table key)))
    (if pair
	(cdr pair)
      default)))

(define (identifier-alist-remove table key)
  ;;Given the alist TABLE: remove  the occurrences of KEY and return the
  ;;new alist.
  ;;
  (remp (lambda (pair)
	  (free-identifier=? (car pair) key))
    table))


;;;; done

)

;;; end of file
