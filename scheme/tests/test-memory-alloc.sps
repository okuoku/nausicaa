;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (foreign memory) allocation functions
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
  (formations)
  (debugging))

(check-set-mode! 'report-failed)
(display "*** testing memory allocation\n")


(parametrise ((check-test-name 'system))

  (check
      (let ((i (system-malloc 4096)))
	(begin0
	    (integer? i)
	  (system-free i)))
    => #t)

  (check
      (let* ((i (system-malloc 4096))
	     (j (system-realloc i (* 2 4096))))
	(begin0
	    (integer? j)
	  (system-free i)))
    => #t)

  (check
      (let ((i (system-calloc 4096 10)))
	(begin0
	    (integer? i)
	  (system-free i)))
    => #t)

  #t)


(parametrise ((check-test-name 'platform))

  (check
      (let ((i (platform-malloc 4096)))
	(begin0
	    (pointer? i)
	  (platform-free i)))
    => #t)

  (check
      (let* ((i (platform-malloc 4096))
	     (j (platform-realloc i (* 2 4096))))
	(begin0
	    (pointer? j)
	  (platform-free i)))
    => #t)

  (check
      (let ((i (platform-calloc 4096 10)))
	(begin0
	    (pointer? i)
	  (platform-free i)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((i (platform-malloc* 4096)))
	(begin0
	    (pointer? i)
	  (platform-free i)))
    => #t)

  (check
      (let* ((i (platform-malloc* 4096))
	     (j (platform-realloc* i (* 2 4096))))
	(begin0
	    (pointer? j)
	  (platform-free i)))
    => #t)

  (check
      (let ((i (platform-calloc* 4096 10)))
	(begin0
	    (pointer? i)
	  (platform-free i)))
    => #t)

  #t)


(parametrise ((check-test-name 'primitve))

  (define (failing-alloc . args)
    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((p (primitive-malloc 4096)))
	(primitive-free p)
	#t)
    => #t)

  (check
      (let* ((p (primitive-malloc 4096))
	     (q (primitive-realloc p 8192)))
	(primitive-free q)
	#t)
    => #t)

  (check
      (let ((p (primitive-calloc 4096 4)))
	(primitive-free p)
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (parametrise ((primitive-malloc-function failing-alloc))
	(primitive-malloc 4096))
    => #f)

  (check
      (parametrise ((primitive-realloc-function failing-alloc))
	(let ((p (primitive-malloc 4096)))
	  (begin0
	      (primitive-realloc p 8192)
	    (primitive-free p))))
    => #f)

  (check
      (parametrise ((primitive-calloc-function failing-alloc))
	(primitive-calloc 4096 4))
    => #f)

  #t)


(parametrise ((check-test-name 'high))

  (define (failing-alloc . args)
    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((p (malloc 4096)))
	(primitive-free p)
	#t)
    => #t)

  (check
      (let* ((p (malloc 4096))
	     (q (realloc p 8192)))
	(primitive-free q)
	#t)
    => #t)

  (check
      (let ((p (calloc 4096 4)))
	(primitive-free p)
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-message E)))
	(parametrise ((primitive-malloc-function failing-alloc))
	  (malloc 4096)))
    => "out of memory")

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-message E)))
	(parametrise ((primitive-realloc-function failing-alloc))
	  (let ((p (malloc 4096)))	;let's leak it
	    (realloc p 8192))))
    => "out of memory")

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-message E)))
	(parametrise ((primitive-calloc-function failing-alloc))
	  (calloc 4096 4)))
    => "out of memory")

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

	(parametrise ((primitive-malloc-function logging-malloc)
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

	(parametrise ((primitive-malloc-function logging-malloc)
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

	(parametrise ((primitive-calloc-function logging-calloc)
		      (primitive-free-function logging-free))
	  (let ((p (calloc 4 4096)))
	    (primitive-free p)
	    ;;(display (get-string))
	    #t)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
