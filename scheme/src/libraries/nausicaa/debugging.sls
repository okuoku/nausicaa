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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (debugging)
  (export
    debug debugging debug-print-condition)
  (import (nausicaa)
    (nausicaa formations))


(define debugging
  (make-parameter #f))

(define (debug thing . args)
  (when (debugging)
    (let ((port (current-error-port)))
      (if (string? thing)
	  (apply format port thing args)
	(write thing port))
      (newline port))))

(define (debug-print-condition message E)
  (debug "~a\n\twho: ~s\n\tmessage: ~s\n\tirritants: ~s"
	 message
	 (if (who-condition? E)
	     (condition-who E)
	   'no-who)
	 (if (message-condition? E)
	     (condition-message E)
	   #f)
	 (if (irritants-condition? E)
	     (condition-irritants E)
	   #f))
  (when (syntax-violation? E)
    (debug "\tsyntax violation form: ~s\n\tsyntax violation subform: ~s"
	   (syntax-violation-form E) (syntax-violation-subform E))))


;;;; done

)

;;; end of file
