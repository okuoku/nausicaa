;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: infix->prefix syntax
;;;Date: Sat Aug 15, 2009
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

(library (infix syntax)
  (export infix->prefix* infix->prefix*/context)
  (import (rnrs)
    (for (infix) expand))

  (define-syntax infix->prefix*
    (lambda (stx)
      (syntax-case stx ()
	((k . ?infix)
	 (syntax (infix->prefix*/context k ?infix))))))

  (define-syntax infix->prefix*/context
    (lambda (stx)
      (syntax-case stx ()
	((k ?context-identifier . ?infix)
	 (datum->syntax (syntax ?context-identifier)
			(infix->prefix (syntax->datum (syntax ?infix))))))))

;;   (define-syntax infix->prefix*
;;     (lambda (stx)
;;       (syntax-case stx ()
;; 	((k . ?infix)
;; 	 (let ((prefix (infix->prefix (syntax->datum (syntax ?infix)))))
;; 	   (with-syntax ((?prefix (datum->syntax (syntax k) prefix)))
;; 	     (syntax ?prefix)))))))
  )

;;; end of file
