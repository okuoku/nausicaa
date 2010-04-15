;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level library inspection routines
;;;Date: Thu Apr  8, 2010
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


(library (libraries low)
  (export

    %library-version-ref

    %library-name?			%library-version?
    %import-level?
    %list-of-symbols?
    %list-of-renamings?			%renaming?
    %library-reference?			%version-reference?

    %sub-version-conforms-to-sub-version-reference?

    %apply-import-spec/only
    %apply-import-spec/except
    %apply-import-spec/prefix
    %apply-import-spec/rename)
  (import (nausicaa)
    (matches)
    (only (lists) take-right drop-right))


;;;; helpers

(define-syntax %normalise-to-boolean
  (syntax-rules ()
    ((_ ?expr)
     (if ?expr #t #f))))




(define (%import-level? obj)
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


(define (%library-reference? obj)
  (and (list? obj)
       (not (null? obj))
       (if (< 1 (length obj))
	   (let ((version (car (take-right obj 1)))
		 (name    (drop-right obj 1)))
	     (cond ((null? version)
		    (for-all symbol? name))
		   ((pair? version)
		    (and (for-all symbol? name)
			 (%version-reference? version)))
		   (else
		    (for-all symbol? obj))))
	 (for-all symbol? obj))))

(define (%version-reference? obj)
  (define (main)
    (match obj
      (('and ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('or  ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('not ?sub-version)
       (%match-sub-version ?sub-version))
      (?sub-version
       (for-all %match-sub-version ?sub-version))))
  (define (%match-sub-version sub-version)
    (match sub-version
      (('and ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('or  ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('not ?sub-version)
       (%match-sub-version ?sub-version))
      (('<= ?sub-version)
       (%match-sub-version ?sub-version))
      (('>= ?sub-version)
       (%match-sub-version ?sub-version))
      ((:and (:predicate integer?) (:predicate exact?))
       #t)
      (*
       #f)))
  (main))


(define (%library-name-conforms-to-library-reference? name reference)
  (assert (%library-name?      name))
  (assert (%library-reference? reference))
  (define (%error-invalid-library-reference)
    (assertion-violation '%library-name-conforms-to-library-reference?
      "invalid library reference" reference))
  (receive (name reference)
      (let loop ((name      name)
		 (reference reference))
	(if (and (symbol? (car name))
		 (symbol? (car reference))
		 (eq? (car name) (car reference)))
	    (loop (cdr name) (cdr reference))
	  (values name reference)))
    (if (and (%library-version?   (car name))
	     (%version-reference? (car reference)))
	(%normalise-to-boolean
	 (case reference
	   ((and)
	    (for-all (lambda (reference)
		       (%library-name-conforms-to-library-reference? name reference))
		     (cdr reference)))
	   ((or)
	    (find (lambda (reference)
		    (%sub-name-conforms-to-library-reference? name reference))
		  (cdr reference)))

	   ((not)
	    (if (= 1 (length (cdr reference)))
		(not (%library-name-conforms-to-library-reference? name (cadr reference)))
	      (%error-invalid-library-reference)))
	   (else
	    )))
      #f)))


(define (%sub-version-conforms-to-sub-version-reference? sub-version sub-version-reference)
  (define (%error-invalid-sub-version-reference)
    (assertion-violation '%sub-version-conforms-to-sub-version-reference?
      "invalid library sub-version reference" sub-version-reference))
  (unless (and (integer?  sub-version)
	       (exact?    sub-version)
	       (or (zero? sub-version)
		   (positive? sub-version)))
    (assertion-violation '%sub-version-conforms-to-sub-version-reference?
      "invalid library sub-version number" sub-version))
  (%normalise-to-boolean
   (cond ((list? sub-version-reference)
	  (when (zero? (length (cdr sub-version-reference)))
	    (%error-invalid-sub-version-reference))
	  (case (car sub-version-reference)
	    ((>=)
	     (>= sub-version (cadr sub-version-reference)))
	    ((<=)
	     (<= sub-version (cadr sub-version-reference)))
	    ((and)
	     (for-all (lambda (sub-version-reference)
			(%sub-version-conforms-to-sub-version-reference? sub-version
									 sub-version-reference))
		      (cdr sub-version-reference)))
	    ((or)
	     (find (lambda (sub-version-reference)
		     (%sub-version-conforms-to-sub-version-reference? sub-version
								      sub-version-reference))
		   (cdr sub-version-reference)))

	    ((not)
	     (if (= 1 (length (cdr sub-version-reference)))
		 (not (%sub-version-conforms-to-sub-version-reference? sub-version
								       (cadr sub-version-reference)))
	       (%error-invalid-sub-version-reference)))

	    (else
	     (%error-invalid-sub-version-reference))))
	 ((and (integer?  sub-version-reference)
	       (exact?    sub-version-reference)
	       (or (zero? sub-version-reference)
		   (positive? sub-version-reference)))
	  (= sub-version sub-version-reference))
	 (else
	  (%error-invalid-sub-version-reference)))))


(define (%list-of-symbols? obj)
  ;;Return true if OBJ is a list of symbols.
  ;;
  (and (list? obj)
       (for-all symbol? obj)))

(define (%list-of-renamings? obj)
  ;;Return true if OBJ is a list of lists, each holding two symbols.  It
  ;;is meant to be the last part of a RENAME import specification.
  ;;
  (and (list? obj)
       (for-all %renaming? obj)))

(define (%renaming? obj)
  ;;Return true of OBJ is a list holding two symbols.  It is meant to be
  ;;an elements in the last part of a RENAME import specification.
  ;;
  (and (list? obj)
       (= 2 (length obj))
       (symbol? (car obj))
       (symbol? (cadr obj))))

(define (%apply-prefix prefix-symbol id-symbol)
  ;;Apply  a  prefix to  a  symbol, as  required  in  the PREFIX  import
  ;;specification.
  ;;
  (assert (symbol? prefix-symbol))
  (assert (symbol? id-symbol))
  (string->symbol (string-append (symbol->string prefix-symbol)
				 (symbol->string id-symbol))))

(define (%apply-rename single-renaming replacement)
  ;;Interpret  RENAMING as  a single  renaming specification  as  in the
  ;;RENAME import spec; replace the output symbol with REPLACEMENT.
  ;;
  (assert (%renaming? single-renaming))
  (assert (symbol? replacement))
  (cons (car single-renaming) replacement))


(define (%apply-import-spec/only renamings list-of-ids)
  (assert (%list-of-renamings? renamings))
  (assert (%list-of-symbols?   list-of-ids))
  (filter (lambda (renaming)
	    (memq (cadr renaming) list-of-ids))
    renamings))

(define (%apply-import-spec/except renamings list-of-ids)
  (assert (%list-of-renamings? renamings))
  (assert (%list-of-symbols?   list-of-ids))
  (filter (lambda (renaming)
	    (not (memq (cadr renaming) list-of-ids)))
    renamings))

(define (%apply-import-spec/prefix renamings prefix)
  (assert (%list-of-renamings? renamings))
  (assert (symbol? prefix))
  (map (lambda (renaming)
	 (let ((in (car  renaming))
	       (ou (cadr renaming)))
	   (list in (%apply-prefix prefix ou))))
    renamings))

(define (%apply-import-spec/rename renamings rename-spec)
  (reverse (fold-left (lambda (knil single-renaming)
			(let ((res (exists (lambda (spec)
					     (if (eq? (cadr single-renaming) (car spec))
						 (list (car single-renaming) (cadr spec))
					       #f))
					   rename-spec)))
			  (cons (if res res single-renaming) knil)))
		      '()
		      renamings)))


;;;; done

)

;;; end of file
