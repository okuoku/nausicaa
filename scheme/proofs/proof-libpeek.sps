;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proof of peeking inside library through syntax object
;;;Date: Fri Apr  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (nausicaa)
  (for (proof-libpeek-gateway) run expand))

(define-syntax maker
  (lambda (stx)
    (define (%maker name)
      (string->symbol (string-append "make-" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?lens ?arg ...)
       (begin
	 (write (syntax->datum #'?lens))(newline)
	 (with-syntax ((MAKER (datum->syntax (syntax->datum #'?lens) 'make-<type>
					     ;;(%maker (syntax->datum #'?lens))
					     )))
	   #'(MAKER ?arg ...))
	 )))))

(write <type>)(newline)
(write (maker <type> 1 2))(newline)
;;(write (datum->syntax <type> 'make-<type>))(newline)


;;;; code


;;; end of file
