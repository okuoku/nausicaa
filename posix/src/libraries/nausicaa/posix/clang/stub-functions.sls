;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: type definitions for time structures
;;;Date: Tue Jan 25, 2011
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
(library (nausicaa posix clang stub-functions)
  (export
    <pointer-to-tms>-tms_utime
    <pointer-to-tms>-tms_stime
    <pointer-to-tms>-tms_cutime
    <pointer-to-tms>-tms_cstime
    <pointer-to-tms>-tms_utime-set!
    <pointer-to-tms>-tms_stime-set!
    <pointer-to-tms>-tms_cutime-set!
    <pointer-to-tms>-tms_cstime-set!)
  (import (rnrs)
    (only (nausicaa language extensions) define-inline)
    (nausicaa ffi)
    (nausicaa posix clang type-translation)
    (nausicaa posix shared-object))


;;;; interface to "struct tms"

(define-c-functions libnausicaa-posix
  (%pointer-to-tms-tms_utime	(double nausicaa_posix_tms_utime_ref (void*)))
  (%pointer-to-tms-tms_stime	(double nausicaa_posix_tms_stime_ref (void*)))
  (%pointer-to-tms-tms_cutime	(double nausicaa_posix_tms_cutime_ref (void*)))
  (%pointer-to-tms-tms_cstime	(double nausicaa_posix_tms_cstime_ref (void*)))
  (%pointer-to-tms-tms_utime-set!	(void nausicaa_posix_tms_utime_set (void* double)))
  (%pointer-to-tms-tms_stime-set!	(void nausicaa_posix_tms_stime_set (void* double)))
  (%pointer-to-tms-tms_cutime-set!	(void nausicaa_posix_tms_cutime_set (void* double)))
  (%pointer-to-tms-tms_cstime-set!	(void nausicaa_posix_tms_cstime_set (void* double))))

(define-inline (<pointer-to-tms>-tms_utime pointer)
  (exact (%pointer-to-tms-tms_utime pointer)))

(define-inline (<pointer-to-tms>-tms_stime pointer)
  (exact (%pointer-to-tms-tms_stime pointer)))

(define-inline (<pointer-to-tms>-tms_cutime pointer)
  (exact (%pointer-to-tms-tms_cutime pointer)))

(define-inline (<pointer-to-tms>-tms_cstime pointer)
  (exact (%pointer-to-tms-tms_cstime pointer)))

(define-inline (<pointer-to-tms>-tms_utime-set! pointer value)
  (%pointer-to-tms-tms_utime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-tms_stime-set! pointer value)
  (%pointer-to-tms-tms_stime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-tms_cutime-set! pointer value)
  (%pointer-to-tms-tms_cutime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-tms_cstime-set! pointer value)
  (%pointer-to-tms-tms_cstime-set! pointer (inexact value)))


;;;; done

)

;;; end of file
