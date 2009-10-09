;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions for buffers
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
  (nos)
  (records)
  (foreign memory)
  (for (foreign memory record-typedefs) expand)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory buffers\n")


(parametrise ((check-test-name 'record-inspection))

  (define len 4096)

  (check
      (let* ((p		(malloc len))
	     (buf	(membuffer p len)))
	(with-record-fields (((pointer size used-size) <membuffer> buf))
	  (with-virtual-fields (((free-size full? empty?) <membuffer> buf))
	    (begin0
		(list (is-a? buf <membuffer>)
		      (pointer? pointer) size used-size
		      free-size full? empty?)
	      (primitive-free pointer)))))
    => (list #t #t len 0 len #f #t))

  (check
      (let* ((p		(malloc len))
	     (buf	(membuffer* (pointer p)
				    (size    len))))
	(with-record-fields (((pointer size used-size) <membuffer> buf))
	  (with-virtual-fields (((free-size full? empty?) <membuffer> buf))
	    (begin0
		(list (is-a? buf <membuffer>)
		      (pointer? pointer) size used-size
		      free-size full? empty?)
	      (primitive-free pointer)))))
    => (list #t #t len 0 len #f #t))

  (check
      (let* ((p		(malloc len))
  	     (buf	(membuffer p len)))
	(with-record-fields (((pointer first-used size used-size) <membuffer> buf))
	  (with-virtual-fields (((free-size full? empty?) <membuffer> buf))
	    (set! first-used (pointer-add first-used 100))
	    (begin0
		(list size used-size free-size full? empty?)
	      (primitive-free pointer)))))
	=> (list len 100 (- len 100) #f #f))

  ;; (check
  ;;     (let* ((p		(malloc len))
  ;; 	     (buf	(make <membuffer> p len len)))
  ;; 	(begin0
  ;; 	    (list (membuffer-size buf)
  ;; 		  (membuffer-used-size buf)
  ;; 		  (membuffer-free-size buf)
  ;; 		  (membuffer-used? buf)
  ;; 		  (membuffer-full? buf)
  ;; 		  (membuffer-empty? buf))
  ;; 	  (primitive-free (membuffer-pointer buf))))
  ;;   => (list len len 0 #t #t #f))

  #t)


#;(parametrise ((check-test-name 'used-memblock))

  (define len 4096)

  (check
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-membuffer p len 100))
	       (mb	(membuffer-used-memblock buf)))
	  (list (= 100 (memblock-size mb))
		(pointer=? (membuffer-pointer buf)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check	;full used
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-membuffer p len len))
	       (mb	(membuffer-used-memblock buf)))
	  (list (= len (memblock-size mb))
		(pointer=? (membuffer-pointer buf)
			   (memblock-pointer mb)))))
    => '(#t #t))

  #t)


#;(parametrise ((check-test-name 'free-memblock))

  (define len 4096)

  (check
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-membuffer p len 100))
	       (mb	(membuffer-free-memblock buf)))
	  (list (= (- len 100) (memblock-size mb))
		(pointer=? (pointer-add (membuffer-pointer buf) 100)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check	;full empty
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-membuffer p len len))
	       (mb	(membuffer-free-memblock buf)))
	  (list (zero? (memblock-size mb))
		(pointer=? (pointer-add (membuffer-pointer buf) len)
			   (memblock-pointer mb)))))
    => '(#t #t))

  #t)


#;(parametrise ((check-test-name 'push-pop-memblock))

  (check
      (with-compensations
	(let (	;input memblocks
	      (mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	      (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
		;output memblock
	      (mb3	(make-memblock (malloc/c 10) 10)))
	  (let* ((len	4096)
		 (buf	(make-membuffer (malloc/c len) len 0)))
	    (membuffer-push-memblock! buf mb1)
	    (membuffer-push-memblock! buf mb2)
	    (membuffer-pop-memblock! mb3 buf)
	    (list (memblock->bytevector mb3)
		  (membuffer-empty? buf)))))
    => '(#vu8(0 1 2 3 4 5 6 7 8 9) #t))

  (check
      (with-compensations
	(let (	;input memblocks
	      (mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	      (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
		;output memblocks
	      (mb3	(make-memblock (malloc/c 5) 5))
	      (mb4	(make-memblock (malloc/c 5) 5)))
	  (let* ((len	4096)
		 (buf	(make-membuffer (malloc/c len) len 0)))
	    (membuffer-push-memblock! buf mb1)
	    (membuffer-push-memblock! buf mb2)
	    (membuffer-pop-memblock! mb3 buf)
	    (membuffer-pop-memblock! mb4 buf)
	    (list (memblock->bytevector mb3)
		  (memblock->bytevector mb4)
		  (membuffer-empty? buf)))))
    => '(#vu8(0 1 2 3 4) #vu8(5 6 7 8 9) #t))

  (check
      (with-compensations
	(let (	;input memblock
	      (mb1	(bytevector->memblock #vu8(0 1 2 3 4 5 6 7 8 9) malloc/c))
		;output memblock
	      (mb2	(make-memblock (malloc/c 5) 5)))
	  (let* ((len	4096)
		 (buf	(make-membuffer (malloc/c len) len 0)))
	    (membuffer-push-memblock! buf mb1)
	    (membuffer-consume-bytes! buf 5)
	    (membuffer-pop-memblock! mb2 buf)
	    (memblock->bytevector mb2))))
    => #vu8(5 6 7 8 9))

  #t)


#;(parametrise ((check-test-name 'push-pop-bytevector))

  (check	;push, get used
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (buf	(make-membuffer (malloc/c len) len 0)))
	  (membuffer-push-bytevector! buf bv)
	  (memblock->bytevector (membuffer-used-memblock buf))))
    => #vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;push, pop
      (with-compensations
	(let* ((bv1	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (bv2	(make-bytevector 5))
	       (len	4096)
	       (buf	(make-membuffer (malloc/c len) len 0)))
	  (membuffer-push-bytevector! buf bv1)
	  (membuffer-pop-bytevector!  bv2 buf)
	  bv2))
    => #vu8(0 1 2 3 4))

  #t)


#;(parametrise ((check-test-name 'push-pop-buffer))

  (check
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (src	(make-membuffer (malloc/c len) len 0))
	       (dst	(make-membuffer (malloc/c 5) 5 0)))
	  (membuffer-push-bytevector! src bv)
	  (membuffer-push-membuffer! dst src)
	  (memblock->bytevector (membuffer-used-memblock dst))))
    => #vu8(0 1 2 3 4))

  (check
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (src	(make-membuffer (malloc/c len) len 0))
	       (mid	(make-membuffer (malloc/c len) len 0))
	       (dst	(make-membuffer (malloc/c 5) 5 0)))
	  (membuffer-push-bytevector! src bv)
	  (membuffer-push-membuffer!  mid src)
	  (membuffer-pop-membuffer!   dst mid)
	  (memblock->bytevector (membuffer-used-memblock dst))))
    => #vu8(0 1 2 3 4))

  #f)


;;;; done

(check-report)

;;; end of file
