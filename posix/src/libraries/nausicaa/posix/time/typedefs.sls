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
(library (nausicaa posix time typedefs)
  (export
    pointer: wrapper: mirror: malloc:
    <pointer-to-tms> <struct-tms> <tms>)
  (import (nausicaa)
    (nausicaa language sentinel)
    (nausicaa language makers)
    (nausicaa ffi)
    (nausicaa posix clang type-translation)
    (nausicaa posix shared-object)
    (nausicaa posix sizeof))


;;;; label interface to "struct tms" pointers

(define-c-functions libnausicaa-posix
  (%pointer-to-tms-utime	(double nausicaa_posix_tms_utime_ref (void*)))
  (%pointer-to-tms-stime	(double nausicaa_posix_tms_stime_ref (void*)))
  (%pointer-to-tms-cutime	(double nausicaa_posix_tms_cutime_ref (void*)))
  (%pointer-to-tms-cstime	(double nausicaa_posix_tms_cstime_ref (void*)))
  (%pointer-to-tms-utime-set!	(void nausicaa_posix_tms_utime_set (void* double)))
  (%pointer-to-tms-stime-set!	(void nausicaa_posix_tms_stime_set (void* double)))
  (%pointer-to-tms-cutime-set!	(void nausicaa_posix_tms_cutime_set (void* double)))
  (%pointer-to-tms-cstime-set!	(void nausicaa_posix_tms_cstime_set (void* double))))

(define-inline (<pointer-to-tms>-utime pointer)
  (exact (%pointer-to-tms-utime pointer)))

(define-inline (<pointer-to-tms>-stime pointer)
  (exact (%pointer-to-tms-stime pointer)))

(define-inline (<pointer-to-tms>-cutime pointer)
  (exact (%pointer-to-tms-cutime pointer)))

(define-inline (<pointer-to-tms>-cstime pointer)
  (exact (%pointer-to-tms-cstime pointer)))

(define-inline (<pointer-to-tms>-utime-set! pointer value)
  (%pointer-to-tms-utime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-stime-set! pointer value)
  (%pointer-to-tms-stime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-cutime-set! pointer value)
  (%pointer-to-tms-cutime-set! pointer (inexact value)))

(define-inline (<pointer-to-tms>-cstime-set! pointer value)
  (%pointer-to-tms-cstime-set! pointer (inexact value)))

(define-label <pointer-to-tms>
  (custom-maker <pointer-to-tms>-maker)
  (virtual-fields (mutable stime)
		  (mutable utime)
		  (mutable cutime)
		  (mutable cstime)))

(define-maker <pointer-to-tms>-maker
  %<pointer-to-tms>-maker
  ((pointer:	sentinel	(without wrapper: mirror:))
   (wrapper:	sentinel	(without pointer: mirror:))
   (mirror:	sentinel	(without pointer: wrapper:))
   (malloc:	sentinel	(mandatory))))

(define-syntax %<pointer-to-tms>-maker
  (syntax-rules (sentinel)
    ((_ sentinel sentinel sentinel ?malloc)
     (?malloc (c-sizeof struct-tms)))
    ((_ ?pointer sentinel sentinel ?malloc)
     (tms-pointer->tms-pointer ?pointer ?malloc))
    ((_ sentinel ?wrapper sentinel ?malloc)
     (tms-wrapper->tms-pointer ?wrapper ?malloc))
    ((_ sentinel sentinel ?mirror ?malloc)
     (tms-mirror->tms-pointer ?mirror ?malloc))
    ))

(define (tms-pointer->tms-pointer (src <pointer-to-tms>) malloc)
  (let (((dst <pointer-to-tms>) (malloc (c-sizeof struct-tms))))
    (set! dst.stime  src.stime)
    (set! dst.utime  src.utime)
    (set! dst.cstime src.cstime)
    (set! dst.cutime src.cutime)
    dst))

(define (tms-wrapper->tms-pointer (src <struct-tms>) malloc)
  (let (((dst <pointer-to-tms>) (malloc (c-sizeof struct-tms))))
    (set! dst.stime  src.stime)
    (set! dst.utime  src.utime)
    (set! dst.cstime src.cstime)
    (set! dst.cutime src.cutime)
    dst))

(define (tms-mirror->tms-pointer (src <tms>) malloc)
  (let (((dst <pointer-to-tms>) (malloc (c-sizeof struct-tms))))
    (set! dst.stime  src.stime)
    (set! dst.utime  src.utime)
    (set! dst.cstime src.cstime)
    (set! dst.cutime src.cutime)
    dst))


;;;; class wrapper for "struct tms" pointers

(define-class <struct-tms>
  (nongenerative nausicaa:posix:<struct-tms>)
  (maker ()
	 (pointer:	sentinel	(without wrapper: mirror:))
	 (wrapper:	sentinel	(without pointer: mirror:))
	 (mirror:	sentinel	(without pointer: wrapper:))
	 (malloc:	sentinel	(mandatory)))
  (maker-transformer <struct-tms>-maker)
  (fields (immutable pointer))
  (virtual-fields (mutable utime)
		  (mutable stime)
		  (mutable cutime)
		  (mutable cstime)))

(define-syntax <struct-tms>-maker
  (syntax-rules (sentinel)
    ((_ ?constructor sentinel sentinel sentinel ?malloc)
     (?constructor (?malloc (c-sizeof struct-tms))))
    ((_ ?constructor ?pointer sentinel sentinel ?malloc)
     (tms-pointer->tms-wrapper ?constructor ?pointer ?malloc))
    ((_ ?constructor sentinel ?wrapper sentinel ?malloc)
     (tms-wrapper->tms-wrapper ?constructor ?wrapper ?malloc))
    ((_ ?constructor sentinel sentinel ?mirror ?malloc)
     (tms-mirror->tms-wrapper  ?constructor ?mirror ?malloc))))

(define (tms-pointer->tms-wrapper constructor pointer malloc)
  (constructor (tms-pointer->tms-pointer pointer malloc)))

(define (tms-wrapper->tms-wrapper constructor wrapper malloc)
  (constructor (tms-wrapper->tms-pointer wrapper malloc)))

(define (tms-mirror->tms-wrapper constructor mirror malloc)
  (constructor (tms-mirror->tms-pointer mirror malloc)))

(define-syntax <struct-tms>-utime
  (syntax-rules ()
    ((_ ?pointer)
     (<pointer-to-tms>-utime (<struct-tms>-pointer ?pointer)))))

(define-syntax <struct-tms>-utime-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (<pointer-to-tms>-utime-set! (<struct-tms>-pointer ?pointer) ?value))))

(define-syntax <struct-tms>-stime
  (syntax-rules ()
    ((_ ?pointer)
     (<pointer-to-tms>-stime (<struct-tms>-pointer ?pointer)))))

(define-syntax <struct-tms>-stime-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (<pointer-to-tms>-stime-set! (<struct-tms>-pointer ?pointer) ?value))))

(define-syntax <struct-tms>-cutime
  (syntax-rules ()
    ((_ ?pointer)
     (<pointer-to-tms>-cutime (<struct-tms>-pointer ?pointer)))))

(define-syntax <struct-tms>-cutime-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (<pointer-to-tms>-cutime-set! (<struct-tms>-pointer ?pointer) ?value))))

(define-syntax <struct-tms>-cstime
  (syntax-rules ()
    ((_ ?pointer)
     (<pointer-to-tms>-cstime (<struct-tms>-pointer ?pointer)))))

(define-syntax <struct-tms>-cstime-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (<pointer-to-tms>-cstime-set! (<struct-tms>-pointer ?pointer) ?value))))


;;;; class mirror for "struct tms"

(define-class <tms>
  (nongenerative nausicaa:posix:<tms>)
  (maker ()
	 (pointer:	sentinel	(without wrapper: mirror:))
	 (wrapper:	sentinel	(without pointer: mirror:))
	 (mirror:	sentinel	(without pointer: wrapper:)))
  (maker-transformer <tms>-maker)
  (fields (mutable utime)
	  (mutable stime)
	  (mutable cutime)
	  (mutable cstime)))

(define-syntax <tms>-maker
  (syntax-rules (sentinel)
    ((_ ?constructor ?pointer sentinel sentinel)
     (tms-pointer->tms-mirror ?constructor ?pointer))
    ((_ ?constructor sentinel ?wrapper sentinel)
     (tms-wrapper->tms-mirror ?constructor ?wrapper))
    ((_ ?constructor sentinel sentinel ?mirror)
     (tms-mirror->tms-mirror  ?constructor ?mirror))))

(define (tms-pointer->tms-mirror constructor (pointer <pointer-to-tms>))
  (constructor pointer.utime
	       pointer.stime
	       pointer.cutime
	       pointer.cstime))

(define (tms-wrapper->tms-mirror constructor (wrapper <struct-tms>))
  (constructor wrapper.utime
	       wrapper.stime
	       wrapper.cutime
	       wrapper.cstime))

(define (tms-mirror->tms-mirror constructor (mirror <tms>))
  (constructor mirror.utime
	       mirror.stime
	       mirror.cutime
	       mirror.cstime))


;;;; done

)

;;; end of file
