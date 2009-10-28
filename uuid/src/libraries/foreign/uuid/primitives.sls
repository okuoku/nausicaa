;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/UUID
;;;Contents: primitives for OSSP UUID bindings
;;;Date: Tue Oct 27, 2009
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


(library (foreign uuid primitives)
  (export
    ;; UUID object handling
    uuid-create
    (rename (uuid_destroy uuid-destroy))
    uuid-clone

    ;; UUID generation
    uuid-load
    uuid-make

    ;; UUID comparison
    uuid-isnil?
    uuid-compare

    ;; UUID import/export
    uuid-import
    uuid-export

    ;; library utilities
    uuid-error
    (rename (uuid_version uuid-version)))
  (import (rnrs)
    (compensations)
    (foreign memory)
    (only (foreign cstrings)
	  string->cstring/c	cstring->string)
    (foreign ffi sizeof)
    (foreign uuid platform)
    (foreign uuid sizeof))


;;;; helpers

(define-syntax %check
  (syntax-rules ()
    ((_ ?name ?form ?body0 ?body ...)
     (let ((rc ?form))
       (if (= rc UUID_RC_OK)
	   (begin ?body0 ?body ...)
	 (assertion-violation (quote ?name) (uuid-error rc)))))))


;;;; UUID object handling

(define (uuid-create)
  (with-compensations
    (let ((*uuid (malloc-block/c sizeof-pointer)))
      (%check uuid-create (uuid_create *uuid)
	      (pointer-ref-c-pointer *uuid 0)))))

(define (uuid-clone uuid)
  (with-compensations
    (let ((*clone (malloc-block/c sizeof-pointer)))
      (%check uuid-clone (uuid_clone uuid *clone)
	      (pointer-ref-c-pointer *clone 0)))))


;;;; UUID generation

(define (uuid-load spec)
  (with-compensations
    (let ((*uuid (malloc-block/c sizeof-pointer)))
      (%check uuid-create (uuid_create *uuid)
	      (let ((uuid (pointer-ref-c-pointer *uuid 0)))
		(%check uuid-load (uuid_load uuid (string->cstring/c spec))
			uuid))))))

(define uuid-make
  (case-lambda
   ((mode)
    (with-compensations
      (let ((*uuid (malloc-block/c sizeof-pointer)))
	(%check uuid-make (uuid_create *uuid)
		(let ((uuid (pointer-ref-c-pointer *uuid 0)))
		  (%check uuid-make (uuid_make uuid mode)
			  uuid))))))
   ((mode src-uuid name)
    (with-compensations
      (let ((*uuid (malloc-block/c sizeof-pointer)))
	(%check uuid-make (uuid_create *uuid)
		(let ((uuid (pointer-ref-c-pointer *uuid 0)))
		  (%check uuid-make (uuid_make uuid mode src-uuid (string->cstring/c name))
			  uuid))))))))


;;;; UUID comparison

(define (uuid-isnil? uuid)
  (with-compensations
    (let ((*result (malloc-block/c sizeof-int)))
      (%check uuid-isnil
		   (uuid_isnil uuid *result)
		   (not (zero? (pointer-ref-c-signed-int *result 0)))))))

(define (uuid-compare uuid-1 uuid-2)
  (with-compensations
    (let ((*result (malloc-block/c sizeof-int)))
      (%check uuid-compare
		   (uuid_compare uuid-1 uuid-2 *result)
		   (pointer-ref-c-signed-int *result 0)))))


;;;; UUID import/export

(define (uuid-import format input.ptr input.len)
  (with-compensations
    (let ((*uuid (malloc-block/c sizeof-int)))
      (%check uuid-import
	      (uuid_create *uuid)
	      (let ((uuid (pointer-ref-c-pointer *uuid 0)))
		(%check uuid-import
			(uuid_import uuid format input.ptr input.len)
			uuid))))))

(define uuid-export
  (let ((fmt-1 (list UUID_FMT_STR UUID_FMT_TXT UUID_FMT_SIV)))
    (lambda (uuid format)
      (with-compensations
	(let ((*ptr (malloc-block/c sizeof-pointer))
	      (*len (malloc-block/c sizeof-int)))
	  (pointer-set-c-pointer! *ptr 0 pointer-null)
	  (letrec ((ptr (compensate
			    (%check uuid-export
				    (uuid_export uuid format *ptr *len)
				    (pointer-ref-c-pointer *ptr 0))
			  (with
			   (primitive-free ptr))))
		   (len (pointer-ref-c-signed-int *len 0)))
	    (cond
	     ((memv format fmt-1)
	      (cstring->string ptr (- len 1)))
	     ((= format UUID_FMT_BIN)
	      (pointer->bytevector ptr len)))))))))


;;;; library utilities

(define (uuid-error rc)
  (cstring->string (uuid_error rc)))


;;;; done

)

;;; end of file
