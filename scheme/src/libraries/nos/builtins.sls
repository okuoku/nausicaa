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


(library (nos builtins)
  (export
    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>)
  (import (rnrs))


(define-record-type <top>
  (nongenerative nausicaa:nos:<top>))

(define-record-type <builtin>
  (parent <top>)
  (nongenerative nausicaa:nos:<builtin>))

(define-record-type <pair>		(parent <builtin>))
(define-record-type <list>		(parent <pair>))
(define-record-type <char>		(parent <builtin>))
(define-record-type <string>		(parent <builtin>))
(define-record-type <vector>		(parent <builtin>))
(define-record-type <bytevector>	(parent <builtin>))
(define-record-type <hashtable>		(parent <builtin>))

(define-record-type <record>		(parent <builtin>))
(define-record-type <condition>		(parent <record>))

(define-record-type <port>		(parent <builtin>))
(define-record-type <input-port>	(parent <port>))
(define-record-type <output-port>	(parent <port>))
(define-record-type <binary-port>	(parent <port>))
(define-record-type <textual-port>	(parent <port>))

(define-record-type <number>		(parent <builtin>))
(define-record-type <complex>		(parent <number>))
(define-record-type <real-valued>	(parent <complex>))
(define-record-type <real>		(parent <real-valued>))
(define-record-type <rational-valued>	(parent <real>))
(define-record-type <flonum>		(parent <real>))
(define-record-type <rational>		(parent <rational-valued>))
(define-record-type <integer-valued>	(parent <rational-valued>))
(define-record-type <integer>		(parent <integer-valued>))
(define-record-type <fixnum>		(parent <integer>))


;;;; done

)

;;; end of file
