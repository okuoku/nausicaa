;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: debugging helpers
;;;Date: Wed Aug 12, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (debugging)
  (export
    debug debugging debug-print-condition)
  (import (nausicaa)
    (formations))


(define debugging
  (make-parameter #f))

(define (debug thing . args)
  (when (debugging)
    (let ((port (current-error-port)))
      (if (string? thing)
	  (apply format port thing args)
	(write thing port))
      (newline port))))

(define (debug-print-condition message exc)
  (debug "~a\nwho: ~s\nmessage: ~s\nirritants: ~s"
	 message
	 (if (who-condition? exc)
	     (condition-who exc)
	   'no-who)
	 (if (message-condition? exc)
	     (condition-message exc)
	   #f)
	 (if (irritants-condition? exc)
	     (condition-irritants exc)
	   #f)))


;;;; done

)

;;; end of file
