;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for symbols tree
;;;Date: Mon Dec 27, 2010
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
(import (nausicaa)
  (nausicaa symbols-tree)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing symbols tree\n")


(parametrise ((check-test-name	'cons))

  (check	;single symbol keys
      (let ((tree '()))
        (set! tree (tree-cons '(a) 1 tree))
        (set! tree (tree-cons '(b) 2 tree))
        (set! tree (tree-cons '(c) 3 tree))
	tree)
    => '((c . ((#f . 3)))
	 (b . ((#f . 2)))
	 (a . ((#f . 1)))))

  (check	;multi symbol keys
      (let ((tree '()))
        (set! tree (tree-cons '(a b c d1) 1 tree))
        (set! tree (tree-cons '(a b c d2) 2 tree))
        (set! tree (tree-cons '(a b c d3) 3 tree))
	tree)
    => '((a . ((b . ((c . ((d3 . ((#f . 3)))
			   (d2 . ((#f . 2)))
			   (d1 . ((#f . 1)))))))))))

  (check	;multi symbol keys
      (let ((tree '()))
        (set! tree (tree-cons '(a b c1 d1) 1 tree))
        (set! tree (tree-cons '(a b c1 d2) 2 tree))
        (set! tree (tree-cons '(a b c1 d3) 3 tree))
        (set! tree (tree-cons '(a b c2 d1) 1 tree))
        (set! tree (tree-cons '(a b c2 d2) 2 tree))
        (set! tree (tree-cons '(a b c2 d3) 3 tree))
	tree)
    => '((a . ((b . ((c2 . ((d3 . ((#f . 3)))
			    (d2 . ((#f . 2)))
			    (d1 . ((#f . 1)))))
		     (c1 . ((d3 . ((#f . 3)))
			    (d2 . ((#f . 2)))
			    (d1 . ((#f . 1)))))
		     ))))))

  (check	;multi symbol keys
      (let ((tree '()))
        (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
        (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
        (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
        (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
        (set! tree (tree-cons '(a b2 c1) 4 tree))
        (set! tree (tree-cons '(a b2 c2) 5 tree))
        (set! tree (tree-cons '(a b2 c3) 6 tree))
	tree)
    => '((a . ((b2 . ((c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))))
	       ))))

  (check	;multi symbol keys
      (let ((tree '()))
        (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
        (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
        (set! tree (tree-cons '(a b2 c1) 4 tree))
        (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
        (set! tree (tree-cons '(a b2 c2) 5 tree))
        (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
        (set! tree (tree-cons '(a b2 c3) 6 tree))
	tree)
    => '((a . ((b2 . ((c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))))
	       ))))

  (check	;multi symbol keys, some shorter keys
      (let ((tree '()))
        (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
        (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
        (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
        (set! tree (tree-cons '(a b2 c1) 4 tree))
        (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
        (set! tree (tree-cons '(a b2 c2) 5 tree))
        (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
        (set! tree (tree-cons '(a b2 c3) 6 tree))
        (set! tree (tree-cons '(a b2) 7 tree))
        (set! tree (tree-cons '(a b1 c2) 8 tree))
	tree)
    => '((a . ((b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))))
	       ))))

  #t)


(parametrise ((check-test-name	'replace))

  (define (build-tree1)
    (let ((tree '()))
      (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
      (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c1) 4 tree))
      (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
      (set! tree (tree-cons '(a b2 c2) 5 tree))
      (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c3) 6 tree))
      (set! tree (tree-cons '(a b2) 7 tree))
      (set! tree (tree-cons '(a b1 c2) 8 tree))
      tree))

  (check
      (build-tree1)
    => '((a . ((b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1))))))
		   ))
	    )))

;;; --------------------------------------------------------------------

  (check	;replace value for short key (a b1 c2)
      (tree-cons '(a b1 c2) 99 (build-tree1))
    => '((a . ((b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 99)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))))
	       ))))

  (check	;replace value for key (a b1 c2 d2)
      (tree-cons '(a b1 c2 d2) 99 (build-tree1))
    => '((a . ((b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 99)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))))
	       ))))

  #t)


(parametrise ((check-test-name	'search))

  (define (build-tree1)
    (let ((tree '()))
      (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
      (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c1) 4 tree))
      (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
      (set! tree (tree-cons '(a b2 c2) 5 tree))
      (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c3) 6 tree))
      (set! tree (tree-cons '(a b2) 7 tree))
      (set! tree (tree-cons '(a b1 c2) 8 tree))
      tree))

  (define (build-tree2)
    (let ((tree '()))
      (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
      (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c1) 4 tree))
      (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
      (set! tree (tree-cons '(a b2 c2) 5 tree))
      (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c3) 6 tree))
      (set! tree (tree-cons '(a b2) 7 tree))
      (set! tree (tree-cons '(a b1 c2) 8 tree))
      (set! tree (tree-cons '() 10 tree))
      (set! tree (tree-cons '(a) 11 tree))
      tree))

  (check
      (build-tree1)
    => '((a . ((b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1))))))
		   ))
	    )))

  (check
      (build-tree2)
    => '((#f . 10)
	 (a . ((#f . 11)
	       (b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1))))))
		   ))
	    )))

;;; --------------------------------------------------------------------

  (let ((tree (build-tree1)))

    (check (treeq '(a b1 c1 d1) tree #t)	=> 1)
    (check (treeq '(a b1 c1 d2) tree #t)	=> 2)
    (check (treeq '(a b1 c1 d3) tree #t)	=> 3)

    (check (treeq '(a b1 c2 d1) tree #t)	=> 1)
    (check (treeq '(a b1 c2 d2) tree #t)	=> 2)
    (check (treeq '(a b1 c2 d3) tree #t)	=> 3)
    (check (treeq '(a b1 c2)    tree #t)	=> 8)

    (check (treeq '(a b2 c1) tree #t)	=> 4)
    (check (treeq '(a b2 c2) tree #t)	=> 5)
    (check (treeq '(a b2 c3) tree #t)	=> 6)
    (check (treeq '(a b2)    tree #t)	=> 7)

    #t)

  (let ((tree (build-tree2)))

    (check (treeq '(a b1 c1 d1) tree #t)	=> 1)
    (check (treeq '(a b1 c1 d2) tree #t)	=> 2)
    (check (treeq '(a b1 c1 d3) tree #t)	=> 3)

    (check (treeq '(a b1 c2 d1) tree #t)	=> 1)
    (check (treeq '(a b1 c2 d2) tree #t)	=> 2)
    (check (treeq '(a b1 c2 d3) tree #t)	=> 3)
    (check (treeq '(a b1 c2)    tree #t)	=> 8)

    (check (treeq '(a b2 c1) tree #t)	=> 4)
    (check (treeq '(a b2 c2) tree #t)	=> 5)
    (check (treeq '(a b2 c3) tree #t)	=> 6)
    (check (treeq '(a b2)    tree #t)	=> 7)

    (check (treeq '() tree #t)	=> 10)
    (check (treeq '(a) tree #t)	=> 11)

    #t)

  (let ((tree '((a . ((#f . 1))))))

    (check (treeq '(a) tree #t)	=> 1)
    (check (treeq '()  tree #t)	=> #t)
    (check (treeq '(Z) tree #t)	=> #t)

    #t)

  (let ((tree '((#f . 0)
		(a  . ((#f . 1))))))

    (check (treeq '(a) tree #t)	=> 1)
    (check (treeq '()  tree #t)	=> 0)
    (check (treeq '(Z) tree #t)	=> #t)

    #t)

  #t)


(parametrise ((check-test-name	'iter))

  (define (build-tree1)
    (begin0-let ((tree '()))
      (set! tree (tree-cons '(a b2) 2 tree))
      (set! tree (tree-cons '(a b1) 1 tree))
      (set! tree (tree-cons '(a)    3 tree))
      ))

  (define (build-tree2)
    (let ((tree '()))
      (set! tree (tree-cons '(a b1 c1 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d2) 2 tree))
      (set! tree (tree-cons '(a b1 c2 d1) 1 tree))
      (set! tree (tree-cons '(a b1 c1 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c1) 4 tree))
      (set! tree (tree-cons '(a b1 c2 d2) 2 tree))
      (set! tree (tree-cons '(a b2 c2) 5 tree))
      (set! tree (tree-cons '(a b1 c2 d3) 3 tree))
      (set! tree (tree-cons '(a b2 c3) 6 tree))
      (set! tree (tree-cons '(a b2) 7 tree))
      (set! tree (tree-cons '(a b1 c2) 8 tree))
      (set! tree (tree-cons '() 10 tree))
      (set! tree (tree-cons '(a) 11 tree))
      tree))

  (define-syntax check-iter
    (syntax-rules ()
      ((_ ?iter ?result)
       (check
	   (receive (bool key val)
	       (?iter)
	     (vector bool key val))
	 => ?result))))

  (check
      (build-tree1)
    => '((a . ((#f . 3)
	       (b1 . ((#f . 1)))
	       (b2 . ((#f . 2)))))))

  (check
      (build-tree2)
    => '((#f . 10)
	 (a . ((#f . 11)
	       (b2 . ((#f . 7)
		      (c3 . ((#f . 6)))
		      (c2 . ((#f . 5)))
		      (c1 . ((#f . 4)))))
	       (b1 . ((c2 . ((#f . 8)
			     (d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1)))))
		      (c1 . ((d3 . ((#f . 3)))
			     (d2 . ((#f . 2)))
			     (d1 . ((#f . 1))))))
		   ))
	    )))

;;; --------------------------------------------------------------------

  (let ((I (make-tree-iterator (build-tree1))))

    (check-iter I '#(#t (a)		3))
    (check-iter I '#(#t (b1 a)		1))
    (check-iter I '#(#t (b2 a)		2))
    (check-iter I '#(#f #f #f))

    #f)

  (let ((I (make-tree-iterator (build-tree2))))

    (check-iter I '#(#t ()		10))
    (check-iter I '#(#t (a)		11))
    (check-iter I '#(#t (b2 a)		7))
    (check-iter I '#(#t (c3 b2 a)	6))
    (check-iter I '#(#t (c2 b2 a)	5))
    (check-iter I '#(#t (c1 b2 a)	4))
    (check-iter I '#(#t (c2 b1 a)	8))
    (check-iter I '#(#t (d3 c2 b1 a)	3))
    (check-iter I '#(#t (d2 c2 b1 a)	2))
    (check-iter I '#(#t (d1 c2 b1 a)	1))
    (check-iter I '#(#t (d3 c1 b1 a)	3))
    (check-iter I '#(#t (d2 c1 b1 a)	2))
    (check-iter I '#(#t (d1 c1 b1 a)	1))
    (check-iter I '#(#f #f #f))

    #f)

  (let ((I (make-tree-iterator '())))

    (check-iter I '#(#f #f #f))

    #f)

  (let ((I (make-tree-iterator '((#f . 1)))))

    (check-iter I '#(#t ()		1))
    (check-iter I '#(#f #f #f))

    #f)

  (let ((I (make-tree-iterator '((#f . 1)
				 (a  . ((#f . 2)))))))

    (check-iter I '#(#t ()		1))
    (check-iter I '#(#t (a)		2))
    (check-iter I '#(#f #f #f))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
