;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: entity record wrappers
;;;Date: Sun Dec  6, 2009
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


(library (foreign posix typedefs)
  (export

    <posix-wrapper>
    make-<posix-wrapper>	<posix-wrapper>?
    <posix-wrapper>-object

    file-descriptor		file-descriptor?
    integer->file-descriptor	(rename (<posix-wrapper>-object file-descriptor->integer))

    FILE*			FILE*?
    pointer->FILE*		(rename (<posix-wrapper>-object FILE*->pointer))

    fdset			fdset?
    make-fdset
    pointer->fdset		(rename (<posix-wrapper>-object fdset->pointer))

    struct-flock
    make-struct-flock		struct-flock?
    struct-flock->pointer	pointer->struct-flock

    struct-timeval
    make-struct-timeval		struct-timeval?
    struct-timeval->pointer	pointer->struct-timeval
    )
  (import (rnrs)
    (foreign posix sizeof))


(define-record-type <posix-wrapper>
  (fields (immutable object)))

(define-record-type (file-descriptor integer->file-descriptor file-descriptor?)
  (parent <posix-wrapper>))

(define-record-type (FILE* pointer->FILE* FILE*?)
  (parent <posix-wrapper>))

;;; --------------------------------------------------------------------

(define-record-type (fdset pointer->fdset fdset?)
  (parent <posix-wrapper>))

(define (make-fdset malloc)
  (pointer->fdset (malloc sizeof-fdset)))

;;; --------------------------------------------------------------------

(define-record-type (struct-flock pointer->struct-flock struct-flock?)
  (parent <posix-wrapper>))

(define (make-struct-flock malloc)
  (pointer->struct-flock (malloc sizeof-flock)))

(define struct-flock->pointer <posix-wrapper>-object)


(define-record-type (struct-timeval pointer->struct-timeval struct-timeval?)
  (parent <posix-wrapper>))

(define struct-timeval->pointer <posix-wrapper>-object)

(define (make-struct-timeval malloc)
  (pointer->struct-timeval (malloc sizeof-timeval)))


;;;; done

)

;;; end of file
