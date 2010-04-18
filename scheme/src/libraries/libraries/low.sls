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

    %list-of-symbols?
    %list-of-renamings?			%renaming?

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
