;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for macro expansions in FFI libraries
;;;Date: Fri May  7, 2010
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


(library (ffi syntax-helpers)
  (export %prepend %enclose map-identifier-syntax-object
	  syntax->list)
  (import (rnrs))


(define (%prepend context/stx prefix identifier/stx)
  (datum->syntax context/stx
		 (string->symbol (string-append prefix
						(symbol->string
						 (if (symbol? identifier/stx)
						     identifier/stx
						   (syntax->datum identifier/stx)))))))

(define (%enclose context/stx prefix identifier/stx suffix)
  (datum->syntax context/stx
		 (string->symbol (string-append prefix
						(symbol->string
						 (if (symbol? identifier/stx)
						     identifier/stx
						   (syntax->datum identifier/stx)))
						suffix))))

(define (map-identifier-syntax-object proc stx)
  (datum->syntax stx (proc (syntax->datum stx))))

(define (syntax->list stx)
  ;;Given a syntax object STX holding  a list, decompose it and return a
  ;;list of syntax  objects.  Take care of returning  a proper list when
  ;;the input is a syntax object holding a proper list.
  ;;
  ;;This functions  provides a workaround  for bugs in Ikarus  and Mosh,
  ;;which expand syntax objects holding a list into IMproper lists.
  ;;
  (syntax-case stx ()
    (()			'())
    ((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
    (?atom		#'?atom)))


;;;; done

)

;;; end of file
