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

    random-integers-with-sum
    random-reals-with-sum		random-reals-with-sum-refine)
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

(define (random-integers-with-sum requested-sum number-of-numbers
				  range-min ; inclusive
				  range-max ; exclusive
				  source)
  ;;This algorithms comes from the followign answer at StackOverflow:
  ;;
  ;;http://stackoverflow.com/questions/472013/generate-a-series-of-random-numbers-that-add-up-to-n-in-c
  ;;
  ;;URL last verified Thu Jul 2, 2009.
  ;;
;;   (when (<= (- range-max range-min) 0)
;;     (assertion-violation 'random-integers-with-sum
;;       "invalid range limits" range-min range-max))
;;   (when (< requested-sum (* range-min number-of-numbers))
;;     (assertion-violation 'random-integers-with-sum
;;       (string-append "impossible to generate requested sum "
;; 		     (number->string requested-sum)
;; 		     " using at most "
;; 		     (number->string number-of-numbers)
;; 		     " numbers greater or equal to "
;; 		     (number->string range-min))))
;;   (when (> requested-sum (* (- range-max 1) number-of-numbers))
;;     (assertion-violation 'random-integers-with-sum
;;       (string-append "impossible to generate requested sum "
;; 		     (number->string requested-sum)
;; 		     " using at least "
;; 		     (number->string number-of-numbers)
;; 		     " numbers less than "
;; 		     (number->string range-max))))
  (let* ((integers-maker	(random-source-integers-maker source))
	 (result		(make-vector number-of-numbers))
	 (sum-so-far		0)
	 (range-max		(- range-max 1))) ; now it is inclusive
    (do ((i 0 (+ 1 i)))
	((= i number-of-numbers)
	 ;;This shuffling is required  because the generated numbers are
	 ;;biased once "min" and "max" start to get close each other.
	 (%random-vector-shuffle! source result 0 number-of-numbers)
	 result)
      (let* ((available-sum	(- requested-sum sum-so-far))
	     (still-to-do-after-this-one (- number-of-numbers i 1))
	     ;;If all  the numbers after this one  are "range-max", this
	     ;;one can be at least "min".
	     (min		(let ((a (- available-sum (* range-max still-to-do-after-this-one))))
				  (if (< a range-min) range-min a)))
	     ;;If all  the numbers after this one  are "range-min", this
	     ;;one can be at most "max".
  	     (max		(let ((b (- available-sum (* range-min still-to-do-after-this-one))))
				  (if (> b range-max) range-max b))))
	(let ((N (if (= min max)
		     min
		   (+ min (integers-maker (- max min))))))
	  (vector-set! result i N)
	  (set! sum-so-far (+ N sum-so-far)))))))

(define (random-reals-with-sum requested-sum number-of-numbers
			       range-min    ; exclusive
			       range-max    ; exclusive
			       source)
  ;;This algorithms comes from the followign answer at StackOverflow:
  ;;
  ;;http://stackoverflow.com/questions/472013/generate-a-series-of-random-numbers-that-add-up-to-n-in-c
  ;;
  ;;URL last verified Thu Jul 2, 2009.
  ;;
;;;These  are commented  out because  real  numbers can  be positive  or
;;;negative.
;;;
;;;   (when (<= (- range-max range-min) 0)
;;;     (assertion-violation 'random-reals-with-sum
;;;       "invalid range limits" range-min range-max))
;;;   (when (< requested-sum (* range-min number-of-numbers))
;;;     (assertion-violation 'random-reals-with-sum
;;;       (string-append "impossible to generate requested sum "
;;; 		     (number->string requested-sum)
;;; 		     " using at most "
;;; 		     (number->string number-of-numbers)
;;; 		     " numbers greater or equal to "
;;; 		     (number->string range-min))))
;;;   (when (> requested-sum (* (- range-max 1) number-of-numbers))
;;;     (assertion-violation 'random-reals-with-sum
;;;       (string-append "impossible to generate requested sum "
;;; 		     (number->string requested-sum)
;;; 		     " using at least "
;;; 		     (number->string number-of-numbers)
;;; 		     " numbers less than "
;;; 		     (number->string range-max))))
  (let* ((reals-maker	(random-source-reals-maker source))
	 (result	(make-vector number-of-numbers))
	 (sum-so-far	0))
    (do ((i 0 (+ 1 i)))
	((= i number-of-numbers)
	 ;;This shuffling is required  because the generated numbers are
	 ;;biased once,  at the end of  the vector, "min"  and "max" get
	 ;;close each other.
	 (%random-vector-shuffle! source result 0 number-of-numbers)
	 (values result sum-so-far))
      (let* ((available-sum	(- requested-sum sum-so-far))
	     (still-to-do-after-this-one
	      (- number-of-numbers i 1))
	     (min		(let ((a (- available-sum (* range-max still-to-do-after-this-one))))
				  (if (< a range-min) range-min a)))
  	     (max		(let ((b (- available-sum (* range-min still-to-do-after-this-one))))
				  (if (> b range-max) range-max b))))
	(let ((N (if (= min max)
		     min
		   (+ min (* (reals-maker) (- max min))))))
	  (vector-set! result i N)
	  (set! sum-so-far (+ N sum-so-far)))))))


(define (random-reals-with-sum-refine vec actual-sum requested-sum tolerance max-ntries)
  (define (difference actual-sum requested-sum)
    (abs (- actual-sum requested-sum)))
  (let ((len   (vector-length vec)))
    (do ((i 0 (+ 1 i))
	 (diff (difference actual-sum requested-sum)
	       (difference actual-sum requested-sum)))
	((or (= i max-ntries)
	     (< diff tolerance))
	 (values vec actual-sum))
      (let ((e (/ diff len)))
	(write (list 'fixing-diff e))(newline)
	(vector-map! (lambda (idx num) (- num e))
		     vec)
	(set! actual-sum (vector-fold (lambda (idx prev num) (+ prev num))
				      0 vec))))))


;;;; done

)

;;; end of file
