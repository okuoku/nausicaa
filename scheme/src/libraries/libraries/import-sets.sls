;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: handling import sets
;;;Date: Fri Apr 16, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (libraries import-sets)
  (export
    )
  (import (nausicaa)
    (matches)
    (libraries references))


(define-class <import-spec>
  (fields (immutable import-set)
		;The  import set  of this  specification; it  can  be an
		;<import-set> object or <library-reference> object.
	  (immutable import-levels)
		;A list of exact integers representing the import levels
		;for which  this import set was  requested.  It contains
		;at least one element.
	  (immutable original)
		;The original import set.
	  )

  (protocol (lambda (make-<top>)
	      (lambda (sexp)
		(receive (import-set import-levels)
		    (import-spec-parse sexp)
		  ((make-<top>) import-set import-levels sexp)))))

  (nongenerative nausicaa:libraries:<import-spec>))


(define (import-spec-parse sexp)
  (match sexp

    (('for ?import-set)
     (values (import-set-parse ?import-set '(0))))

    (('for ?import-set (:predicate import-level? ?import-levels) ...)
     (values (import-set-parse ?import-set)
	     (import-levels-parse ?import-levels)))

    (('for ?import-set . _)
     (assertion-violation 'import-spec-parse "invalid import specification" sexp))

    (?import-set
     (import-set-parse ?import-set '(0)))))

(define (import-levels-parse levels)
  (map (lambda (level)
	 (define (%error)
	   (assertion-violation 'import-levels-parse "invalid import level" level))
	 (case level
	   ((run)
	    0)
	   ((expand)
	    1)
	   (else
	    (if (and (list? level)
		     (= 2 (length level))
		     (eq? 'meta (car level)))
		(let ((n (cadr level)))
		  (if (and (integer? n)
			   (exact?   n))
		      n
		    (%error)))
	      (%error)))))
    levels))

(define (import-set-parse spec)
  (match spec
    ;;The  RENAME  clause can  appear  with and  without
    ;;renamings.
    (('rename ?import-set)
     (import-set-parse ?import-set))
    (('rename ?import-set . _)
     (import-set-parse ?import-set))

    ;;The  ONLY  clause  can  appear  with  and  without
    ;;symbols.
    (('only ?import-set)
     (import-set-parse ?import-set))
    (('only ?import-set . _)
     (import-set-parse ?import-set))

    ;;The  EXCEPT  clause can  appear  with and  without
    ;;symbols.
    (('except ?import-set)
     (import-set-parse ?import-set))
    (('except ?import-set . _)
     (import-set-parse ?import-set))

    ;;The  PREFIX  clause must  appear  with a  symbolic
    ;;prefix.
    (('prefix ?import-set (:predicate symbol? ?prefix))
     (import-set-parse ?import-set))

    ;;The LIBRARY  clause allows library  names starting
    ;;with FOR, ONLY, etc.
    (('library (:predicate library-reference? ?library-reference))
     ?library-reference)

    ;;A plain library name.
    ((:predicate library-reference? ?library-reference)
     ?library-reference)

    ;;Everything else is an error.
    (?import-set
     (assertion-violation 'import-set-parse "invalid import set" spec))))


(define (import-level? obj)
  ;;Return true if OBJ is a valid <import level> as specified by R6RS.
  ;;
  (case obj
    ((run expand)
     #t)
    (else
     (and (pair? obj)
	  (eq? 'meta (car obj))
	  (integer?  (cdr obj))
	  (exact?    (cdr obj))))))



;;;; done

)

;;; end of file
