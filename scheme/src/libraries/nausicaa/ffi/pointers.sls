;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: raw memory pointer data type
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi pointers)
  (export
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add
    pointer-incr!		pointer-decr!
    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?)
  (import (rnrs)
    (nausicaa ffi pointers compat))

  (define-syntax pointer-incr!
    (syntax-rules ()
      ((_ ?pointer ?expr)
       (set! ?pointer (pointer-add ?pointer ?expr)))))

  (define-syntax pointer-decr!
    (syntax-rules ()
      ((_ ?pointer ?expr)
       (set! ?pointer (pointer-add ?pointer (- ?expr))))))

  )

;;; end of file
