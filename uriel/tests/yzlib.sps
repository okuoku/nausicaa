;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: test of Ypsilon binding to Zlib
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-11-30 10:02:52 marco>
;;;
;;;Abstract
;;;
;;;	This is  a test to verify  how Ypsilon's FFI  works.  It assumes
;;;	that the  shared library "libz.so"  is loadable with in  the way
;;;	"dlopen()" does.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (ffi)
  (srfi lightweight-testing))

(check-set-mode! 'report-failed)


;;;; code

(define zlib (load-shared-object "libz.so"))

(define deflateInit_f	(lookup-shared-object zlib 'deflateInit_))
(define deflate_f	(lookup-shared-object zlib "deflate"))
(define deflateEnd_f	(lookup-shared-object zlib 'deflateEnd))

(check
    (integer? deflateInit_f)
  => #t)

(check
    (integer? deflate_f)
  => #t)

(check
    (integer? deflateEnd_f)
  => #t)

(define (deflateInit zstream level)
  (stdcall-shared-object->int deflateInit_f zstream level))

(define (deflate zstream flush)
  (stdcall-shared-object->int deflate_f zstream flush))

(define (deflateEnd zstream)
  (stdcall-shared-object->int deflateEnd_f zstream))



;;;; done

(check-report)

;;; end of file
