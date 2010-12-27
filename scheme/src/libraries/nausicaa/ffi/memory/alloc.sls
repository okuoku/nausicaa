;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: raw memory allocation functions
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


(library (ffi memory alloc)
  (export
    system-free		platform-free		primitive-free
    system-malloc	system-calloc		system-realloc
    platform-malloc	platform-calloc		platform-realloc
    platform-malloc*	platform-calloc*	platform-realloc*
    primitive-malloc	primitive-calloc	primitive-realloc
    malloc		realloc			calloc

    primitive-malloc-function	primitive-calloc-function
    primitive-realloc-function	primitive-free-function)
  (import (rnrs)
    (parameters)
    (ffi pointers)
    (ffi memory conditions)
    (ffi memory alloc compat))


(define (platform-malloc* number-of-bytes)
  (let ((p (platform-malloc number-of-bytes)))
    (if (pointer-null? p) #f p)))

(define (platform-calloc* count element-size)
  (let ((p (platform-calloc count element-size)))
    (if (pointer-null? p) #f p)))

(define (platform-realloc* pointer new-size)
  (let ((p (platform-realloc pointer new-size)))
    (if (pointer-null? p) #f p)))

;;; --------------------------------------------------------------------

(define primitive-free-function
  (make-parameter platform-free
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-malloc-function
  (make-parameter platform-malloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-realloc-function
  (make-parameter platform-realloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-calloc-function
  (make-parameter platform-calloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

;;; --------------------------------------------------------------------

(define (primitive-free pointer)
  ((primitive-free-function) pointer))

(define (primitive-malloc number-of-bytes)
  ((primitive-malloc-function) number-of-bytes))

(define (primitive-calloc count element-size)
  ((primitive-calloc-function) count element-size))

(define (primitive-realloc pointer new-size)
  ((primitive-realloc-function) pointer new-size))

;;; --------------------------------------------------------------------

(define (malloc number-of-bytes)
  (or (primitive-malloc number-of-bytes)
      (raise-memory-request 'malloc number-of-bytes #f)))

(define (realloc pointer new-size)
  (or (primitive-realloc pointer new-size)
      (raise-memory-request 'realloc new-size #f)))

(define (calloc count element-size)
  (or (primitive-calloc count element-size)
      (raise-memory-request 'calloc (* count element-size) #t)))


;;;; done

)

;;; end of file
