;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: library of cstring functions
;;;Date: Thu Dec 18, 2008
;;;Time-stamp: <2008-12-18 07:45:01 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (glibc cstring)
  (export

    memchr		memrchr
    strchr		strrchr
    strstr		memmem

    )
  (import (r6rs)
    (uriel ffi)
    (uriel memory)
    (uriel cstring))



;;;; code

(define-c-function memchr
  (char* memchr (char* int size_t)))

(define-c-function memrchr
  (char* memrchr (char* int size_t)))

(define-c-function strchr
  (char* strchr (char* int)))

(define-c-function strrchr
  (char* strchr (char* int)))

(define-c-function strstr
  (char* strstr (char* int)))

(define-c-function memmem
  (void* memmem (void* size_t void* size_t)))



;;;; done

)

;;; end of file
