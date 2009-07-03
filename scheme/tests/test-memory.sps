;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (checks)
  (foreign memory)
  (foreign ffi sizeof)
  (format))

(check-set-mode! 'report-failed)
(display "*** testing memory\n")


(parameterize ((check-test-name 'pointers))

  (check
      (pointer-null? pointer-null)
    => #t)

  (check
      (pointer-null? (integer->pointer 123))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer? pointer-null)
    => #t)

  (check
      (pointer? (integer->pointer 123))
    => #t)

  (check
      (pointer? 123)
    => #f)

  (check
      (pointer-null? (integer->pointer 123))
    => #f)

  (check
      (pointer->integer (integer->pointer 123))
    => 123)

  (check
      (pointer->integer (integer->pointer 0))
    => 0)

  (check
      (pointer-null? (integer->pointer 0))
    => #t)

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


(parameterize ((check-test-name 'alloc))

  (define (failing-alloc . args)
    pointer-null)

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else
		   (write (condition-who exc))(newline)
		   (write (condition-message exc))(newline)
		   exc))
	(let ((p (malloc 4096)))
	  (primitive-free p)
	  #t))
    => #t)

  (check
      (guard (exc (else
		   exc))
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


(parameterize ((check-test-name 'bytevectors))
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

      (check 'this
	(memblock->bytevector
	 (bytevector->memblock bv malloc-block/compensated 5 2)
	 5)
	=> #vu8(2 3 4 5 6)))))


(parameterize ((check-test-name 'buffers))

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
      (with-compensations
	(let* ((p		(malloc/c len))
	       (buf	(make-buffer p len 100))
	       (mb	(buffer-used-memblock buf)))
	  (list (= 100 (memblock-size mb))
		(pointer=? (buffer-pointer buf)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-buffer p len 100))
	       (mb	(buffer-free-memblock buf)))
	  (list (= (- len 100) (memblock-size mb))
		(pointer=? (pointer-add (buffer-pointer buf) 100)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	       (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
	       (mb3	(make-memblock (malloc/c 10) 10))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-memblock! buf mb1)
	  (buffer-push-memblock! buf mb2)
	  (buffer-pop-memblock! mb3 buf)
	  (memblock->bytevector mb3)))
    => #vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	       (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
	       (mb3	(make-memblock (malloc/c 5) 5))
	       (mb4	(make-memblock (malloc/c 5) 5))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-memblock! buf mb1)
	  (buffer-push-memblock! buf mb2)
	  (buffer-pop-memblock! mb3 buf)
	  (buffer-pop-memblock! mb4 buf)
	  (list (memblock->bytevector mb3)
		(memblock->bytevector mb4))))
    => '(#vu8(0 1 2 3 4) #vu8(5 6 7 8 9)))

  (check
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4 5 6 7 8 9) malloc/c))
	       (mb2	(make-memblock (malloc/c 5) 5))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-memblock! buf mb1)
	  (buffer-consume-bytes! buf 5)
	  (buffer-pop-memblock! mb2 buf)
	  (memblock->bytevector mb2)))
    => #vu8(5 6 7 8 9))

  (check
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-bytevector! buf bv)
	  (memblock->bytevector (buffer-used-memblock buf))))
    => #vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (with-compensations
	(let* ((bv1	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (bv2	(make-bytevector 5))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-bytevector! buf bv1)
	  (buffer-pop-bytevector! bv2 buf)
	  bv2))
    => #vu8(0 1 2 3 4))

  (check
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (src	(make-buffer (malloc/c len) len 0))
	       (dst	(make-buffer (malloc/c 5) 5 0)))
	  (buffer-push-bytevector! src bv)
	  (buffer-push-buffer! dst src)
	  (memblock->bytevector (buffer-used-memblock dst))))
    => #vu8(0 1 2 3 4))

  )


(parameterize ((check-test-name 'buffer-alloc))

  (check
      (with-compensations
	(parameterize ((memory-buffer-pool
			(make-buffer (malloc/c 4096) 4096 0)))
	  (let ((p (buffer-malloc 1000)))
	    (pointer-null? p))))
    => #f)

  (check
      (with-compensations
	(parameterize ((memory-buffer-pool
			(make-buffer (malloc/c 4096) 4096 0)))
	  (list (pointer-null? (buffer-malloc 1000))
		(pointer-null? (buffer-malloc 1000))
		(pointer-null? (buffer-malloc 1000))
		(pointer-null? (buffer-malloc 1000))
		(primitive-buffer-malloc 1000))))
    => '(#f #f #f #f #f))

  (check
      (with-compensations
	(parameterize ((memory-buffer-pool
			(make-buffer (malloc/c 4096) 4096 0)))
	  (guard (exc (else
		       (list (out-of-memory-condition? exc)
			     (out-of-memory-number-of-bytes exc))))
	    (buffer-malloc 5000))))
    => '(#t 5000))

  )


(parameterize ((check-test-name 'refcount))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-release p)
	   #t)))
    => '(#t (freed)))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-release p)
	   (pointer-release p)
	   #t)))
    => '(#t (freed)))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-dismiss p)
	   #t)))
    => '(#t (freed)))

  )


(parameterize ((check-test-name 'pokers))

  (define-syntax doit
    (syntax-rules ()
      ((_ value setter getter)
       (let* ((p #f))
	 (dynamic-wind
	     (lambda () (set! p (malloc (expt 10 5))))
	     (lambda ()
;;;	       (write (list 'poking value))(newline)
	       (setter p 100 value)
	       (let ((v (getter p 100)))
;;;		 (write (list 'peeked v))(newline)
		 v))
	     (lambda () (primitive-free p)))))))
  (define-syntax generic-test-it
    (syntax-rules ()
      ((_ value setter getter)
       (check (doit value setter getter) => value))))
  (define-syntax generic-test-it/error
    (syntax-rules ()
      ((_ value setter getter)
       (check (guard (exc ((condition? exc) #t)
			  (else #f))
		(doit value setter getter))
	 => #t))))

;;; --------------------------------------------------------------------

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-char! pointer-ref-c-signed-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-char! pointer-ref-c-signed-char))))
    (test-it 65)
    (test-it 127)
    (test-it -128)
    (test-it/error 200)
    (test-it/error -200))

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-char! pointer-ref-c-unsigned-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-char! pointer-ref-c-unsigned-char))))
    (test-it 65)
    (test-it 0)
    (test-it 255)
    (test-it/error 300)
    (test-it/error -200))

;;; --------------------------------------------------------------------

  (let* ((bits	16)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-short! pointer-ref-c-signed-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-short! pointer-ref-c-signed-short))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	16)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-short! pointer-ref-c-unsigned-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-short! pointer-ref-c-unsigned-short))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------

  (let* ((bits	32)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1))))
	 (poke pointer-set-c-signed-int!)
	 (peek pointer-ref-c-signed-int))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value poke peek))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-int! pointer-ref-c-signed-int))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 101))
    (test-it/error (- min 100)))

  (let* ((bits	32)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-int! pointer-ref-c-unsigned-int))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-int! pointer-ref-c-unsigned-int))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------

  (let* ((bits	(if on-32-bits-system 32 64))
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-long! pointer-ref-c-signed-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-long! pointer-ref-c-signed-long))))
    (test-it 65)
    (test-it max)
    (test-it min)
;;    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	(if on-32-bits-system 32 64))
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-long! pointer-ref-c-unsigned-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-long! pointer-ref-c-unsigned-long))))
    (test-it 65)
;;    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
;;    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------

  (let* ((bits	64)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-long-long! pointer-ref-c-signed-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-long-long!
				pointer-ref-c-signed-long-long))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

  (let* ((bits	64)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-long-long! pointer-ref-c-unsigned-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-long-long!
				pointer-ref-c-unsigned-long-long))))
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-float! p 64 4.5)
		  (pointer-ref-c-float p 64))))
	(primitive-free p)
	d)
    => 4.5)

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-double! p 64 4.5)
		  (pointer-ref-c-double p 64))))
	(primitive-free p)
	d)
    => 4.5)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (malloc (expt 10 5))))
	(begin0
	    (begin
	      (pointer-set-c-pointer! p 100 (integer->pointer 90))
	      (pointer->integer (pointer-ref-c-pointer p 100)))
	  (primitive-free p)))
    => 90)

  )


(parameterize ((check-test-name 'array))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-char-array 16))))
	  (poke-array-signed-char! a 5 65)
	  (poke-array-unsigned-char! a 6 66)
	  (list (peek-array-signed-char a 5)
		(peek-array-unsigned-char a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-int-array 16))))
	  (poke-array-signed-int! a 5 65)
	  (poke-array-unsigned-int! a 6 66)
	  (list (peek-array-signed-int a 5)
		(peek-array-unsigned-int a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-short-array 16))))
	  (poke-array-signed-short! a 5 65)
	  (poke-array-unsigned-short! a 6 66)
	  (list (peek-array-signed-short a 5)
		(peek-array-unsigned-short a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-long-array 16))))
	  (poke-array-signed-long! a 5 65)
	  (poke-array-unsigned-long! a 6 66)
	  (list (peek-array-signed-long a 5)
		(peek-array-unsigned-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-long-long-array 16))))
	  (poke-array-signed-long-long! a 5 65)
	  (poke-array-unsigned-long-long! a 6 66)
	  (list (peek-array-signed-long-long a 5)
		(peek-array-unsigned-long-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-float-array 16))))
	  (poke-array-float! a 5 65.1)
	  (poke-array-float! a 6 66.2)
	  (cons (peek-array-float a 5)
		(peek-array-float a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-double-array 16))))
	  (poke-array-double! a 5 65.1)
	  (poke-array-double! a 6 66.2)
	  (cons (peek-array-double a 5)
		(peek-array-double a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-pointer-array 16))))
	  (poke-array-pointer! a 5 (integer->pointer 65))
	  (poke-array-pointer! a 6 (integer->pointer 66))
	  (map pointer->integer
	    (list (peek-array-pointer a 5)
		  (peek-array-pointer a 6)))))
    => '(65 66))

  )


;;;; done

(check-report)

;;; end of file
