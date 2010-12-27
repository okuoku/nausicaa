;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: search tree using symbols as keys
;;;Date: Mon Dec 27, 2010
;;;
;;;Abstract
;;;
;;;	This  library handles  search trees  in which  keys are  list of
;;;	Scheme  symbols and  values can  be anything;  the tree  has the
;;;	structure of  nested alists.   Inserting the following  keys and
;;;	values (in this order):
;;;
;;;         (a b1 c1 d1)	1
;;;         (a b1 c1 d2)	2
;;;         (a b1 c2 d1)	1
;;;         (a b1 c1 d3)	3
;;;         (a b2 c1)		4
;;;         (a b1 c2 d2)	2
;;;         (a b2 c2)		5
;;;         (a b1 c2 d3)	3
;;;         (a b2 c3)		6
;;;         (a b2)		7
;;;         (a b1 c2)		8
;;;
;;;	yields the following tree:
;;;
;;;	    ((a . ((b2 . ((#f . 7)
;;;                       (c3 . 6)
;;;                       (c2 . 5)
;;;                       (c1 . 4)))
;;;                (b1 . ((c2 . ((#f . 8)
;;;                              (d3 . 3)
;;;                              (d2 . 2)
;;;                              (d1 . 1)))
;;;                       (c1 . ((d3 . 3)
;;;                              (d2 . 2)
;;;                              (d1 . 1))))))))
;;;
;;;     notice that the "short keys" are stored as sequences ending with
;;;     a  pair  having #f  as  key,  and such  pairs  are  kept at  the
;;;     beginning of the alist.
;;;
;;;	Storing a key/value pair whose key already exists causes the old
;;;	value to be overwritten.
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


#!r6rs
(library (nausicaa symbols-tree)
  (export
    tree-cons treeq)
  (import (rnrs)
    (rnrs mutable-pairs))


;;;; building

(define (tree-cons key value tree)
  ;;Add KEY/VALUE  pair to TREE.  KEY  must be a list  of symbols, VALUE
  ;;can be anything.
  ;;
  (define (main key value tree)
    (assert (or (null? tree) (pair? tree)))
    (%cons key value tree))

  (define (%cons key value tree)
    (cond ((null? key)
	   (cond ((null? tree)
		  `((#f . ,value)))
		 ((not (pair? tree)) ;overwriting value for non-short key
		  value)
		 ((caar tree) ;the first entry has non-#f key
		  `((#f . ,value) . ,tree))
		 (else ;the first entry has #f key, overwriting value for short key
		  (set-cdr! (car tree) value)
		  tree)))
	  ((null? tree)
	   `(,(%key->entry key value)))
	  (else
	   (let ((entry (assq (car key) tree)))
	     (cond (entry
		    (set-cdr! entry (%cons (cdr key) value (cdr entry)))
		    tree)
		   ((caar tree) ;the first entry has non-#f key
		    `(,(%key->entry key value) . ,tree))
		   (else ;the first entry has #f key
		    `(,(car tree)
		      ,(%key->entry key value)
		      . ,(cdr tree))))))))

  (define (%key->entry key value)
    ;;Given a  key and  a value,  build and return  a search  tree entry
    ;;holding values from KEY.  Example, for the key and value:
    ;;
    ;;	(A B C D)  VALUE
    ;;
    ;;we build the entry:
    ;;
    ;;	(A . ((B . ((C . ((D . VALUE)))))))
    ;;
    ;;while for the key and value:
    ;;
    ;;	(A)	VALUE
    ;;
    ;;we build the entry:
    ;;
    ;;	(A . VALUE)
    ;;
    (let ((A (car key))
	  (D (cdr key)))
      (cons A (if (null? D)
		  value
		(let recur ((a (car D))
			    (d (cdr D)))
		  (if (null? d)
		      `((,a . ,value))
		    `((,a . ,(recur (car d) (cdr d))))))))))

  (main key value tree))


;;;; searching

(define (treeq key tree default)
  (define (%search key tree)
    (if (null? key)
	(cond ((null? tree)
	       default)
	      ((not (pair? tree)) ;found a value for a non-short key
	       tree)
	      ((caar tree) ;the first entry has non-#f key
	       default)
	      (else ;the first entry has #f key, found a value for a short key
	       (cdar tree)))
      (let ((entry (assq (car key) tree)))
	(if entry
	    (%search (cdr key) (cdr entry))
	  default))))
  (assert (or (null? tree) (pair? tree)))
  (%search key tree))


;;;; done

)

;;; end of file
