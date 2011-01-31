;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lightweight formatting of strings
;;;Date: Mon Jan 31, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language printf)
  (export printf)
  (import (rnrs)
    (only (nausicaa language extensions) define-inline)
    (nausicaa language matches))


(define (printf . args)
  (define-inline (true? x)
    (and (boolean? x) (boolean=? x #t)))
  (define-inline (false? x)
    (and (boolean? x) (boolean=? x #f)))
  (match args
    ;;The  clauses are  sorted  in probability  order,  the most  likely
    ;;first.
    (((:predicate string? template) . datums)
     (the-printf #f template datums))
    (((:predicate true?) (:predicate string? template) . datums)
     (the-printf (current-output-port) template datums))
    (((:predicate port? port) (:predicate string? template) . datums)
     (the-printf port template datums))
    (((:predicate false?) (:predicate string? template) . datums)
     (the-printf #f template datums))
    (_
     (assertion-violation 'printf
       "invalid arguments, expected string template or port and string template" args))))


(define (the-printf output-port template datums)
  (let-values (((port getter)	(open-string-output-port))
	       ((len)		(string-length template)))
    (let next-char ((i 0) (ell datums))

      (define (%print-datum printer i ell)
	(if (null? ell)
	    (error 'printf "found more escape sequences than datums" template)
	  (begin
	    (printer (car ell) port)
	    (next-char (+ 1 i) (cdr ell)))))

      (define (%print-hex-datum upper-case? i ell)
	(if (null? ell)
	    (error 'printf "found more escape sequences than datums" template)
	  (let ((obj (car ell)))
	    (unless (integer? obj)
	      (error 'printf "expected integer number as datum for ~x escape" template obj))
	    (let ((str (number->string obj 16)))
	      (display (if upper-case?
			   (string-upcase str)
			 (string-downcase str)) port)
	      (next-char (+ 1 i) (cdr ell))))))

      (define (%print-num-datum escape i ell)
	(if (null? ell)
	    (error 'printf "found more escape sequences than datums" template)
	  (let ((obj (car ell)))
	    (unless (integer? obj)
	      (error 'printf (string-append "expected integer number as datum for ~"
					    (string escape) " escape")
		     template obj))
	    (display (number->string obj (case escape
					   ((#\o #\O)	8)
					   ((#\b #\B)	2)))
		     port)
	    (next-char (+ 1 i) (cdr ell)))))

      (if (= i len)
	  (if (null? ell)
	      (let ((str (getter)))
		(when output-port
		  (display str output-port))
		str)
	    (error 'printf "found more datums than escape sequences" template datums))
	(let ((ch (string-ref template i)))
	  (case ch
	    ((#\~)
	     (let ((i (+ 1 i)))
	       (if (= i len)
		   (error 'printf "end of template string found while reading escape sequence"
			  template datums)
		 (let ((ch (string-ref template i)))
		   (case ch
		     ((#\~)
		      (put-char port #\~)
		      (next-char (+ 1 i) ell))
		     ((#\%) ;just because some people is used to it from Common Lisp
		      (put-char port #\newline)
		      (next-char (+ 1 i) ell))
		     ((#\a #\A)
		      (%print-datum display i ell))
		     ((#\s #\S)
		      (%print-datum write i ell))
		     ((#\X)
		      (%print-hex-datum #t i ell))
		     ((#\x)
		      (%print-hex-datum #f i ell))
		     ((#\b #\B)
		      (%print-num-datum ch i ell))
		     ((#\o #\O)
		      (%print-num-datum ch i ell))
		     (else
		      (error 'printf "invalid escape sequence" template ch)))))))
	    (else
	     (put-char port ch)
	     (next-char (+ 1 i) ell))))))))


;;;; done

)

;;; end of file
