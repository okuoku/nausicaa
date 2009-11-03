;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to POSIX functions for R6RS Scheme
;;;Date: Mon Nov 24, 2008
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


(library (foreign posix environment)
  (export
    getenv setenv environ
    environ-table environ->table table->environ)
  (import (rnrs)
    (begin0)
    (only (strings)
	  string-index)
    (only (foreign posix helpers)
	  define-primitive-parameter)
    (prefix (foreign posix environment primitives)
	    primitive:))


(define-primitive-parameter setenv-function primitive:setenv)
(define-primitive-parameter getenv-function primitive:getenv)
(define-primitive-parameter environ-function primitive:environ)

(define setenv
  (case-lambda
   ((varname newvalue)
    (setenv varname newvalue #t))
   ((varname newvalue replace)
    ((setenv-function) varname newvalue replace))))

(define (getenv varname)
  ((getenv-function) varname))

(define (environ)
  ((environ-function)))

(define (environ-table)
  (environ->table (environ)))

(define (environ->table environ)
  (begin0-let ((table (make-eq-hashtable)))
    (for-each (lambda (str)
		(let ((idx (string-index str #\=)))
		  (hashtable-set! table
				  (string->symbol (substring str 0 idx))
				  (substring str (+ 1 idx) (string-length str)))))
      environ)))

(define (table->environ table)
  (let-values (((names values) (hashtable-entries table)))
    (let ((len (vector-length names))
	  (environ '()))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   environ)
	(set! environ (cons (string-append (let ((n (vector-ref names i)))
					     (if (string? n)
						 n
					       (symbol->string n)))
					   "="
					   (vector-ref values i))
			    environ))))))


;;;; done

)

;;; end of file
