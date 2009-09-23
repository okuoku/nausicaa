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
  (foreign memory)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory buffers\n")


(parametrise ((check-test-name 'record-inspection))

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

  #t)


(parametrise ((check-test-name 'used-memblock))

  (define len 4096)

  (check
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-buffer p len 100))
	       (mb	(buffer-used-memblock buf)))
	  (list (= 100 (memblock-size mb))
		(pointer=? (buffer-pointer buf)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check	;full used
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-buffer p len len))
	       (mb	(buffer-used-memblock buf)))
	  (list (= len (memblock-size mb))
		(pointer=? (buffer-pointer buf)
			   (memblock-pointer mb)))))
    => '(#t #t))

  #t)


(parametrise ((check-test-name 'free-memblock))

  (define len 4096)

  (check
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-buffer p len 100))
	       (mb	(buffer-free-memblock buf)))
	  (list (= (- len 100) (memblock-size mb))
		(pointer=? (pointer-add (buffer-pointer buf) 100)
			   (memblock-pointer mb)))))
    => '(#t #t))

  (check	;full empty
      (with-compensations
	(let* ((p	(malloc/c len))
	       (buf	(make-buffer p len len))
	       (mb	(buffer-free-memblock buf)))
	  (list (zero? (memblock-size mb))
		(pointer=? (pointer-add (buffer-pointer buf) len)
			   (memblock-pointer mb)))))
    => '(#t #t))

  #t)


(parametrise ((check-test-name 'push-pop-memblock))

  (check	;push twice, pop everything
      (with-compensations
	(let* ((mb1	(bytevector->memblock #vu8(0 1 2 3 4) malloc/c))
	       (mb2	(bytevector->memblock #vu8(5 6 7 8 9) malloc/c))
	       (mb3	(make-memblock (malloc/c 10) 10))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-memblock! buf mb1)
	  (buffer-push-memblock! buf mb2)
	  (buffer-pop-memblock! mb3 buf)
	  (list (memblock->bytevector mb3)
		(buffer-empty? buf))))
    => '(#vu8(0 1 2 3 4 5 6 7 8 9) #t))

  (check	;push twice, pop twice
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
		(memblock->bytevector mb4)
		(buffer-empty? buf))))
    => '(#vu8(0 1 2 3 4) #vu8(5 6 7 8 9) #t))

  (check	;push, consume, pop
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

  #t)


(parametrise ((check-test-name 'push-pop-bytevector))

  (check	;push, get used
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-bytevector! buf bv)
	  (memblock->bytevector (buffer-used-memblock buf))))
    => #vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;push, pop
      (with-compensations
	(let* ((bv1	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (bv2	(make-bytevector 5))
	       (len	4096)
	       (buf	(make-buffer (malloc/c len) len 0)))
	  (buffer-push-bytevector! buf bv1)
	  (buffer-pop-bytevector!  bv2 buf)
	  bv2))
    => #vu8(0 1 2 3 4))

  #t)


(parametrise ((check-test-name 'push-pop-buffer))

  (check	;push, pop buffer
      (with-compensations
	(let* ((bv	#vu8(0 1 2 3 4 5 6 7 8 9))
	       (len	4096)
	       (src	(make-buffer (malloc/c len) len 0))
	       (dst	(make-buffer (malloc/c 5) 5 0)))
	  (buffer-push-bytevector! src bv)
	  (buffer-push-buffer! dst src)
	  (memblock->bytevector (buffer-used-memblock dst))))
    => #vu8(0 1 2 3 4))

  #f)


;;;; done

(check-report)

;;; end of file
