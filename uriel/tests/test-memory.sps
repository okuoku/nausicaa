;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: tests for low level memory functions
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-16 21:09:19 marco>
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

(import (r6rs)
  (uriel printing)
  (uriel test)
  (uriel memory)
  (uriel lang)
  (srfi parameters))

(check-set-mode! 'report-failed)


;;;; pointers

(parameterize ((testname 'pointer))

  (check
      (pointer->integer (integer->pointer 123))
    => 123)

  (check
      (pointer->integer (integer->pointer 0))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((i 123)
	     (p (integer->pointer i)))
	(pointer? p))
    => #t)

  (check
      (pointer? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer-null? pointer-null)
    => #t)

  (check
      (pointer-null? (integer->pointer 0))
    => #t)

  (check
      (pointer-null? (integer->pointer 123))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer-diff (integer->pointer 123) (integer->pointer 120))
    => 3)

  (check
      (pointer-diff (integer->pointer 118) (integer->pointer 120))
    => -2)

;;; --------------------------------------------------------------------

  (check
      (pointer-add (integer->pointer 100) 23)
    (=> pointer=?) (integer->pointer 123))

  (check
      (pointer-add (integer->pointer 100) -23)
    (=> pointer=?) (integer->pointer 77))

;;; --------------------------------------------------------------------

  (check
      (pointer=? (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 123)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 456))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<? (integer->pointer 123))
    => #t)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 123))
    => #f)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 789))
    => #t)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer>? (integer->pointer 123))
    => #t)

  (check
      (pointer>? (integer->pointer 123)
		 (integer->pointer 123))
    => #f)

  (check
      (pointer>? (integer->pointer 789)
		 (integer->pointer 456)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer>? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<=? (integer->pointer 123))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 789))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer>=? (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 123)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 789)
		  (integer->pointer 456)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<>? (integer->pointer 123))
    => #f)

  (check
      (pointer<>? (integer->pointer 123)
		  (integer->pointer 123))
    => #f)

  (check
      (pointer<>? (integer->pointer 789)
		  (integer->pointer 456)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer<>? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #t)

  )

;;; --------------------------------------------------------------------


;;;; memory block accessors

(parameterize ((testname 'memory))

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 65)
		  (pointer-ref-c-signed-char p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 128)
		  (pointer-ref-c-signed-char p 100))))
	(primitive-free p)
	d)
    => -128)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 65)
		  (pointer-ref-c-unsigned-char p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 128)
		  (pointer-ref-c-unsigned-char p 100))))
	(primitive-free p)
	d)
    => 128)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-short! p 100 65)
		  (pointer-ref-c-signed-short p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-short! p 100 32768)
		  (pointer-ref-c-signed-short p 100))))
	(primitive-free p)
	d)
    => -32768)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-short! p 100 65)
		  (pointer-ref-c-unsigned-short p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-short! p 100 32768)
		  (pointer-ref-c-unsigned-short p 100))))
	(primitive-free p)
	d)
    => 32768)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-int! p 100 65)
		  (pointer-ref-c-signed-int p 100))))
	(primitive-free p)
	d)
    => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 (expt 2 31))
;; 		  (pointer-ref-c-signed-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => (- (expt 2 31)))

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-int! p 100 65)
		  (pointer-ref-c-unsigned-int p 100))))
	(primitive-free p)
	d)
    => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 (expt 2 31))
;; 		  (pointer-ref-c-unsigned-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => (expt 2 31))

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-long! p 64 65)
		  (pointer-ref-c-signed-long p 64))))
	(primitive-free p)
	d)
    => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 (expt 2 (if on-64-bits-system 63 31)))
;; 		  (pointer-ref-c-signed-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => (- (expt 2 (if on-64-bits-system 63 31))))

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-long! p 64 65)
		  (pointer-ref-c-unsigned-long p 64))))
	(primitive-free p)
	d)
    => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 (expt 2 (if on-64-bits-system 63 31)))
;; 		  (pointer-ref-c-unsigned-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => (expt 2 (if on-64-bits-system 63 31)))

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-float! p 64 4.5)
		  (pointer-ref-c-float p 64))))
	(primitive-free p)
	d)
    => 4.5)

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-double! p 64 4.5)
		  (pointer-ref-c-double p 64))))
	(primitive-free p)
	d)
    => 4.5)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5))))
	(begin0
	    (begin
	      (pointer-set-c-pointer! p 100 (integer->pointer 90))
	      (pointer->integer (pointer-ref-c-pointer p 100)))
	  (primitive-free p)))
    => 90)

  )



;;;; bytevector functions

(parameterize ((testname	'bytevector))
  (with-compensations
    (let ((bv #vu8(0 1 2 3 4 5 6 7 8 9)))
      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   10)
	=> bv)

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   5)
	=> #vu8(0 1 2 3 4))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   5
	   3)
	=> #vu8(3 4 5 6 7))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated 8)
	   8)
	=> #vu8(0 1 2 3 4 5 6 7))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated 5 2)
	   5)
	=> #vu8(2 3 4 5 6))

;;; --------------------------------------------------------------------

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   10)
	=> bv)

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   5)
	=> #vu8(0 1 2 3 4))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   5
	   3)
	=> #vu8(3 4 5 6 7))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated 8)
	   8)
	=> #vu8(0 1 2 3 4 5 6 7))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated 5 2)
	   5)
	=> #vu8(2 3 4 5 6)))))




;;;; done

(check-report)

;;; end of file
