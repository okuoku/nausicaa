;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Iconv
;;;Contents: platform bindings to iconv
;;;Date: Fri Nov 27, 2009
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


(library (foreign i18n iconv platform)
  (export iconv_open iconv iconv_close)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign i18n iconv sizeof)
    (foreign i18n iconv shared-object))


(define-syntax define-c-callouts
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define dummy
	 (shared-object ?shared-object))
       (define-c-function ?name
	 (?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define char*		'pointer)
(define char**		'pointer)
(define size_t*		'pointer)

(define-c-callouts iconv-shared-object
  (iconv_open	(iconv_t iconv_open (char* char*)))
  (iconv	(size_t iconv (iconv_t char** size_t* char** size_t*)))
  (iconv_close	(int iconv_close (iconv_t))))


;;;; done

)

;;; end of file
