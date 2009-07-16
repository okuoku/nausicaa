; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; Fonction "merge" qui elimine les repetitions
(define noeps-merge-1
  (lambda (l1 l2)
    (cond ((null? l1)
	   l2)
	  ((null? l2)
	   l1)
	  (else
	   (let ((t1 (car l1))
		 (t2 (car l2)))
	     (cond ((< t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) l2)))
		   ((= t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) (cdr l2))))
		   (else
		    (cons t2 (noeps-merge-1 l1 (cdr l2))))))))))

; Fabrication des voisinages externes
(define noeps-mkvois
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (begin
	      (let loop2 ((trans (vector-ref trans-v n)) (ends '()))
		(if (null? trans)
		    (vector-set! arcs n ends)
		    (let* ((tran (car trans))
			   (class (car tran))
			   (end (cdr tran)))
		      (loop2 (cdr trans) (if (eq? class 'eps)
					     (noeps-merge-1 ends (list end))
					     ends)))))
	      (loop1 (+ n 1)))))
      arcs)))

; Fabrication des valeurs initiales
(define noeps-mkinit
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (init (make-vector nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (begin
	      (vector-set! init n (list n))
	      (loop (+ n 1)))))
      init)))

; Traduction d'une liste d'arcs
(define noeps-trad-arcs
  (lambda (trans dict)
    (let loop ((trans trans))
      (if (null? trans)
	  '()
	  (let* ((tran (car trans))
		 (class (car tran))
		 (end (cdr tran)))
	    (if (eq? class 'eps)
		(loop (cdr trans))
		(let* ((new-end (vector-ref dict end))
		       (new-tran (cons class new-end)))
		  (cons new-tran (loop (cdr trans))))))))))

; Elimination des transitions eps
(define noeps
  (lambda (nl-start no-nl-start arcs acc)
    (let* ((digraph-arcs (noeps-mkvois arcs))
	   (digraph-init (noeps-mkinit arcs))
	   (dict (digraph digraph-arcs digraph-init noeps-merge-1))
	   (new-nl-start (vector-ref dict nl-start))
	   (new-no-nl-start (vector-ref dict no-nl-start)))
      (let loop ((i (- (vector-length arcs) 1)))
	(if (>= i 0)
	    (begin
	      (vector-set! arcs i (noeps-trad-arcs (vector-ref arcs i) dict))
	      (loop (- i 1)))))
      (list new-nl-start new-no-nl-start arcs acc))))
