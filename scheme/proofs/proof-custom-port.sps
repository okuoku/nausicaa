;;;Part of: Nausicaa/Scheme
;;;Contents: proofs about custom ports
;;;Date: Fri Jul 31, 2009
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


(import (rnrs))


(define (make-custom-string-input-port string)
  ;;This is a clone of "open-string-input-port".
  ;;
  (let ((offset 0)
	(len   (string-length address-string)))
    (make-custom-textual-input-port
     "email address string input port"
     (lambda (output-string start count)
       (do ((past (+ start count))
	    (i offset (+ 1 i))
	    (j start  (+ 1 j)))
	   ((or (= i len) (= j past))
	    (set! offset i)
	    (- j start))
	 (string-set! output-string j (string-ref address-string i))))
     (lambda ()
       offset)
     #f		;set-position!
     #f)))	;close



;;; end of file
