;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: tests for low level memory functions
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-17 13:48:50 marco>
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
  (srfi parameters)
  (srfi format))

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


;;;; allocation

(parameterize ((testname 'alloc))

  (define (failing-alloc . args)
    pointer-null)

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else exc))
	(let ((p (malloc 4096)))
	  (primitive-free p)
	  #t))
    => #t)

  (check
      (guard (exc (else exc))
	(let* ((p (malloc 4096))
	       (q (realloc p 8192)))
	  (primitive-free q)
	  #t))
    => #t)

  (check
      (guard (exc (else exc))
	(let ((p (calloc 4096 4)))
	  (primitive-free p)
	  #t))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else
		   (list (out-of-memory-condition? exc)
			 (out-of-memory-number-of-bytes exc))))
	(parameterize ((primitive-malloc-function failing-alloc))
	  (let ((p (malloc 4096)))
	    (primitive-free p)
	    #t)))
    => '(#t 4096))

  (check
      (guard (exc (else
		   (list (out-of-memory-condition? exc)
			 (out-of-memory-number-of-bytes exc))))
	(parameterize ((primitive-realloc-function failing-alloc))
	  (let* ((p (malloc 4096))
		 (q (realloc p 8192)))
	    (primitive-free q)
	    #t)))
    => '(#t 8192))

  (check
      (guard (exc (else
		   (list (out-of-memory-condition? exc)
			 (out-of-memory-number-of-bytes exc))))
	(parameterize ((primitive-calloc-function failing-alloc))
	  (let ((p (calloc 4096 4)))
	    (primitive-free p)
	    #t)))
    => (list #t (* 4 4096)))

;;; --------------------------------------------------------------------

  (check
      (let-values (((port get-string) (open-string-output-port)))
	(define (logging-malloc number-of-bytes)
	  (let ((p (platform-malloc number-of-bytes)))
	    (display (format "malloc:\t~s ~s\n" p number-of-bytes) port)
	    p))

	(define (logging-free pointer)
	  (display (format "free:\t~s\n" pointer) port)
	  (platform-free pointer))

	(parameterize ((primitive-malloc-function logging-malloc)
		       (primitive-free-function logging-free))
	  (let ((p (malloc 4096)))
	    (primitive-free p)
	    ;;(display (get-string))
	    #t)))
    => #t)

  (check
      (let-values (((port get-string) (open-string-output-port)))
	(define (logging-malloc number-of-bytes)
	  (let ((p (platform-malloc number-of-bytes)))
	    (display (format "malloc:\t~s ~s\n" p number-of-bytes) port)
	    p))

	(define (logging-realloc pointer new-number-of-bytes)
	  (let ((p (platform-realloc pointer new-number-of-bytes)))
	    (display (format "realloc:\t~s ~s ~s\n" p pointer new-number-of-bytes) port)
	    p))

	(define (logging-free pointer)
	  (display (format "free:\t~s\n" pointer) port)
	  (platform-free pointer))

	(parameterize ((primitive-malloc-function logging-malloc)
		       (primitive-realloc-function logging-realloc)
		       (primitive-free-function logging-free))
	  (let* ((p (malloc 4096))
		 (q (realloc p 8192)))
	    (primitive-free q)
	    ;;(display (get-string))
	    #t)))
    => #t)

  (check
      (let-values (((port get-string) (open-string-output-port)))
	(define (logging-calloc count element-size)
	  (let ((p (platform-calloc count element-size)))
	    (display (format "calloc:\t~s ~s ~s\n" p count element-size) port)
	    p))

	(define (logging-free pointer)
	  (display (format "free:\t~s\n" pointer) port)
	  (platform-free pointer))

	(parameterize ((primitive-calloc-function logging-calloc)
		       (primitive-free-function logging-free))
	  (let ((p (calloc 4 4096)))
	    (primitive-free p)
	    ;;(display (get-string))
	    #t)))
    => #t)

  )



;;;; bytevector functions

(parameterize ((testname 'bytevector))
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



;;;; memblocks and buffers

(parameterize ((testname 'buffers))

  (define len 4096)

  (check
      (let* ((p		(malloc len))
	     (buf	(make-buffer p len 0)))
	(begin0
	    (list (buffer-size buf)
		  (buffer-used-size buf)
		  (buffer-free-size buf)
		  (buffer-used? buf)
		  (buffer-full? buf)
		  (buffer-empty? buf))
	  (primitive-free (buffer-pointer buf))))
    => (list len 0 len #f #f #t))

  (check
      (let* ((p		(malloc len))
	     (buf	(make-buffer p len 100)))
	(begin0
	    (list (buffer-size buf)
		  (buffer-used-size buf)
		  (buffer-free-size buf)
		  (buffer-used? buf)
		  (buffer-full? buf)
		  (buffer-empty? buf))
	  (primitive-free (buffer-pointer buf))))
    => (list len 100 (- len 100) #t #f #f))

  (check
      (let* ((p		(malloc len))
	     (buf	(make-buffer p len len)))
	(begin0
	    (list (buffer-size buf)
		  (buffer-used-size buf)
		  (buffer-free-size buf)
		  (buffer-used? buf)
		  (buffer-full? buf)
		  (buffer-empty? buf))
	  (primitive-free (buffer-pointer buf))))
    => (list len len 0 #t #t #f))

  (check
      (let* ((p		(malloc len))
	     (buf	(make-buffer p len 100))
	     (mb	(buffer-used-memblock buf)))
	(list (= 100 (memblock-size mb))
	      (pointer=? (buffer-pointer buf)
			 (memblock-pointer mb))))
    => '(#t #t))

  (check
      (let* ((p		(malloc len))
	     (buf	(make-buffer p len 100))
	     (mb	(buffer-free-memblock buf)))
	(list (= (- len 100) (memblock-size mb))
	      (pointer=? (pointer-add (buffer-pointer buf) 100)
			 (memblock-pointer mb))))
    => '(#t #t))

  (check
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	       (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
	       (mb3	(make-memblock (malloc 10) 10))
	       (len	4096)
	       (buf	(make-buffer (malloc len) len 0)))
	  (buffer-push-memblock buf mb1)
	  (buffer-push-memblock buf mb2)
	  (buffer-pop-memblock mb3 buf)
	  (memblock->bytevector mb3)))
    => #vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	       (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
	       (mb3	(make-memblock (malloc 5) 5))
	       (mb4	(make-memblock (malloc 5) 5))
	       (len	4096)
	       (buf	(make-buffer (malloc len) len 0)))
	  (buffer-push-memblock buf mb1)
	  (buffer-push-memblock buf mb2)
	  (buffer-pop-memblock mb3 buf)
	  (buffer-pop-memblock mb4 buf)
	  (list (memblock->bytevector mb3)
		(memblock->bytevector mb4))))
    => '(#vu8(0 1 2 3 4) #vu8(5 6 7 8 9)))

  )


;;;; memory block accessors

(parameterize ((testname 'accessors))

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



;;;; done

(check-report)

;;; end of file