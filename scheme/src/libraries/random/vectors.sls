;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: randomness related vector functions
;;;Date: Thu Jul  2, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (random vectors)
  (export
    %random-vector-shuffle		random-vector-shuffle
    %random-vector-shuffle!		random-vector-shuffle!

    %random-vector-sample		random-vector-sample
    %random-vector-sample-population	random-vector-sample-population
    )
  (import (rnrs)
    (random)
    (vectors low)
    (rename (only (vectors) %vector-unpack)
	    (%vector-unpack unpack)))


;;;; low level

(define (%random-vector-shuffle! source vec start past)
  (let* ((len		(- past start))
	 (integer-maker	(random-source-integers-maker source)))
    (do ((k len (- k 1)))
	((= k 1)
	 vec)
      (let* ((i (+ start (- k 1)))
	     (j (+ start (integer-maker k)))
	     (xi (vector-ref vec i))
	     (xj (vector-ref vec j)))
	(vector-set! vec i xj)
	(vector-set! vec j xi)))))

(define (%random-vector-shuffle source vec start past)
  (let ((dst (%vector-copy vec start past)))
    (%random-vector-shuffle! source vec start past)))

(define (%random-vector-sample source vec start past)
  (let ((index-maker	(random-source-integers-maker source))
	(max		(- past start)))
    (lambda ()
      (vector-ref vec (+ start (index-maker max))))))

(define (%random-vector-sample-population source len vec start past)
  (let ((sampler	(%random-vector-sample source vec start past)))
    (lambda ()
      (do ((i 0 (+ 1 i))
	   (individual (make-vector len)))
	  ((= i len)
	   individual)
	(vector-set! individual i (sampler))))))


;;;; high level

(define-syntax random-vector-shuffle
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-shuffle ?source vec start past)))))

(define-syntax random-vector-shuffle!
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-shuffle! ?source vec start past)))))

(define-syntax random-vector-sample
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-sample ?source vec start past)))))

(define-syntax random-vector-sample-population
  (syntax-rules ()
    ((_ ?V ?len ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-sample-population ?source ?len vec start past)))))


;; (define (vector-permute! data permutation)
;;   (let ((len (vector-length permutation)))
;;     (unless (= len (vector-length data))
;;       (assertion-violation 'vector-permute!
;; 	(string-append "expected vectors with equal length, got data vector of length"
;; 		       (number->string (vector-length data))
;; 		       " and permutation vector of length "
;; 		       (number->string len))
;; 	data permutation))
;;     (let ((x (vector-ref data (vector-ref permutation 0))))
;;       (do ((i 1 (+ 1 i)))
;; 	  ((= i len))
;; 	(let ((j (vector-ref permutation i)))
;; 	  (vector-set! data (vector-ref data i)))
;; 	  (set! x (vector-ref data j))
;; 	))))


;;;; done

)

;;; end of file
