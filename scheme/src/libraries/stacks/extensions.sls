;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record extension for stacks
;;;Date: Wed Oct 14, 2009
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


(library (stacks extensions)
  (export <stack*> stack-top stack-empty? stack-length)
  (import (rnrs)
    (records)
    (stacks types))

  (define (stack-top que)
    (assert (<stack>? que))
    (let ((p (<stack>-first-pair que)))
      (if (null? p)
	  (error 'stack-top "stack is empty" que)
	(car p))))

  (define (stack-empty? que)
    (assert (<stack>? que))
    (null? (<stack>-first-pair que)))

  (define (stack-length que)
    (assert (<stack>? que))
    (length (<stack>-first-pair que)))

  (define-record-extension <stack*>
    (parent <stack>)
    (fields (top	stack-top	#f)
	    (empty?	stack-empty?	#f)
	    (length	stack-length	#f))))

;;; end of file
