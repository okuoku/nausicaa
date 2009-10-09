;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: conventional record types for builtin Scheme values
;;;Date: Wed Sep 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (records builtins)
  (export
    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>)
  (import (rnrs)
    (only (nausicaa) cond-expand))


(define-record-type <top>
  (nongenerative nausicaa:<top>))

(define-record-type <builtin>
  (parent <top>)
  (nongenerative nausicaa:<builtin>))


(define-record-type <pair>
  (parent <builtin>)
  (nongenerative nausicaa:<pair>))

(define-record-type <list>
  (parent <pair>)
  (nongenerative nausicaa:<list>))

(define-record-type <char>
  (parent <builtin>)
  (nongenerative nausicaa:<char>))

(define-record-type <string>
  (parent <builtin>)
  (nongenerative nausicaa:<string>))

(define-record-type <vector>
  (parent <builtin>)
  (nongenerative nausicaa:<vector>))

(define-record-type <bytevector>
  (parent <builtin>)
  (nongenerative nausicaa:<bytevector>))

(cond-expand (larceny
	      (define <hashtable> (record-rtd (make-eq-hashtable))))
	     (else
	      (define-record-type <hashtable>
		(parent <builtin>)
		(nongenerative nausicaa:<hashtable>))))


(define-record-type <record>
  (parent <builtin>)
  (nongenerative nausicaa:<record>))

(define-record-type <condition>
  (parent <record>)
  (nongenerative nausicaa:<condition>))


(define-record-type <port>
  (parent <builtin>)
  (nongenerative nausicaa:<port>))

(define-record-type <input-port>
  (parent <port>)
  (nongenerative nausicaa:<input-port>))

(define-record-type <output-port>
  (parent <port>)
  (nongenerative nausicaa:<output-port>))

(define-record-type <binary-port>
  (parent <port>)
  (nongenerative nausicaa:<binary-port>))

(define-record-type <textual-port>
  (parent <port>)
  (nongenerative nausicaa:<textual-port>))


(define-record-type <number>
  (parent <builtin>)
  (nongenerative nausicaa:<number>))

(define-record-type <complex>
  (parent <number>)
  (nongenerative nausicaa:<complex>))

(define-record-type <real-valued>
  (parent <complex>)
  (nongenerative nausicaa:<real-valued>))

(define-record-type <real>
  (parent <real-valued>)
  (nongenerative nausicaa:<real>))

(define-record-type <rational-valued>
  (parent <real>)
  (nongenerative nausicaa:<rational-valued>))

(define-record-type <flonum>
  (parent <real>)
  (nongenerative nausicaa:<flonum>))

(define-record-type <rational>
  (parent <rational-valued>)
  (nongenerative nausicaa:<rational>))

(define-record-type <integer-valued>
  (parent <rational-valued>)
  (nongenerative nausicaa:<integer-valued>))

(define-record-type <integer>
  (parent <integer-valued>)
  (nongenerative nausicaa:<integer>))

(define-record-type <fixnum>
  (parent <integer>)
  (nongenerative nausicaa:<fixnum>))


;;;; done

)

;;; end of file
