;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: foreign library inspection generator
;;;Date: Mon Feb  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (foreign ffi inspector-maker))


;;; these come from "f2c.h"

(define-c-type integer		signed-int)
(define-c-type uinteger		unsigned-int)
(define-c-type shortint		signed-int)
(define-c-type real		float)
(define-c-type doublereal	float)
(define-c-type logical		signed-int)
(define-c-type shortlogical	signed-int)
(define-c-type logical1		signed-int)
(define-c-type integer1		signed-int)
(define-c-type ftnlen		signed-int)
(define-c-type flag		signed-int)
(define-c-type ftnint		signed-int)

(define-c-type-alias address		void*)
(define-c-type-alias integer*		void*)
(define-c-type-alias logical*		void*)
(define-c-type-alias real*		void*)
(define-c-type-alias doublereal*	void*)
(define-c-type-alias complex*		void*)
(define-c-type-alias doublecomplex*	void*)

(define-c-type-alias C_fp		callback)	;; typedef void (*C_fp)()
(define-c-type-alias D_fp		callback)	;; typedef doublereal (*D_fp)()
(define-c-type-alias E_fp		callback)	;; typedef doublereal (*E_fp)()
(define-c-type-alias H_fp		callback)	;; typedef void (*H_fp)()
(define-c-type-alias I_fp		callback)	;; typedef integer (*I_fp)()
(define-c-type-alias J_fp		callback)	;; typedef shortint (*J_fp)()
(define-c-type-alias K_fp		callback)	;; typedef shortlogical (*K_fp)()
(define-c-type-alias L_fp		callback)	;; typedef logical (*L_fp)()
(define-c-type-alias R_fp		callback)	;; typedef real (*R_fp)()
(define-c-type-alias S_fp		callback)	;; typedef int (*S_fp)()
(define-c-type-alias U_fp		callback)	;; typedef int (*U_fp)()
(define-c-type-alias Z_fp		callback)	;; typedef void (*Z_fp)()

(define-c-struct complex
  "complex"
  (float	r)
  (float	i))

(define-c-struct doublecomplex
  "doublecomplex"
  (float	r)
  (float	i))


;;;; done

(define lapack-library-spec
  '(foreign math lapack sizeof))

(define-shared-object lapack libclapack.so)

(autoconf-lib-write "configuration/lapack-inspector.m4" lapack-library-spec)
(sizeof-lib-write   "src/libraries/foreign/math/lapack/sizeof.sls.in" lapack-library-spec)

;;; end of file
