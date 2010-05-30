;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for JSON
;;;Date: Sun May 30, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (json helpers)
  (export unquote-string)
  (import (rnrs))


(define (unquote-string string)
  ;;Remove  the quoting  backslash characters  from STRING.   Return the
  ;;clean string.  See the tests in the test suite.
  ;;
  (let-values (((len)		(string-length string))
	       ((port getter)	(open-string-output-port)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (getter))
      (let ((ch (string-ref string i))
	    (i1 (+ 1 i)))
	(when (and (char=? #\\ ch)
		   (not (= len i1)))
	  (set! i i1)
	  (set! ch (string-ref string i)))
	(put-char port ch)))))


;;;; done

)

;;; end of file
