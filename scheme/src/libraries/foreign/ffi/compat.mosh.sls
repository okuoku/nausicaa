;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility layer for Mosh FFI
;;;Date: Thu Jun 25, 2009
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


;;;; setup

(library (foreign ffi compat)
  (export

    ;;shared object loading
    shared-object primitive-open-shared-object self-shared-object

    ;;interface functions
    primitive-make-c-function primitive-make-c-function/with-errno

    errno)
  (import (rnrs)
    (mosh ffi)
    (foreign ffi sizeof)
    (foreign memory))


;;;; dynamic loading

(define self-shared-object (open-shared-object ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object open-shared-object)



;;;; values normalisation: Foreign -> Mosh

;;;This mapping function normalises  the C type identifiers supported by
;;;Nausicaa  to  the  identifiers  supported by  Ypsilon.   Notice  that
;;;currently (Ypsilon checkout 281)  there is neither support for "char"
;;;nor for "long" in Ypsilon.
;;;
;;;Care  must  be  taken  in  selecting  types,  because:
;;;
;;;* selecting "void*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  bytevector and use as value;
;;;
;;;* selecting "char*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  string and use it as value.
;;;
(define (external->internal type)
  (case type
    ((void)
     'void)
    ((int
      signed-int ssize_t uint unsigned unsigned-int size_t
      long signed-long ulong unsigned-long)
     'int)
    ((double)
     'double)
    ((float)
     'float)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    (else
     (assertion-violation 'make-c-function
       "unknown C language type identifier" type))))



;;;; interface functions, no errno

(define (select-cast-and-stub ret-type)
  (let ((identity (lambda (x) x)))
    (case ret-type
      ((void)
       (values identity stdcall-shared-object->void))
      ((int)
       (values identity stdcall-shared-object->int))
      ((double) ; float is not supported as return type
       (values identity stdcall-shared-object->double))
      ((void*)
       (values integer->pointer stdcall-shared-object->intptr))
      (else
       (assertion-violation 'make-c-callout
	 "unknown C language type identifier used for return value" ret-type)))))

(define (select-argument-mapper arg-type)
  (case (external->internal arg-type)
    ((void)
     (lambda (x) x))
    ((int)
     assert-int)
    ((float)
     assert-float)
    ((double)
     assert-double)
    ((callback)
     assert-closure)
    ((void*)
     assert-pointer)
    (else
     (assertion-violation 'make-c-callout
       "unknown C language type identifier used for function argument" arg-type))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (receive (cast-func stub-func)
      (select-cast-and-stub (external->internal ret-type))
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (when (equal? '(void) arg-types)
	(set! arg-types '()))
      (let* ((mappers (map select-argument-mapper arg-types)))
	(case (length mappers)
	  ((0)
	   (lambda ()
	     (cast-func (stub-func f))))
	  ((1)
	   (let ((mapper (car mappers)))
	     (lambda (arg)
	       (cast-func (stub-func f (mapper arg))))))
	  ((2)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers)))
	     (lambda (arg1 arg2)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2))))))
	  ((3)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers)))
	     (lambda (arg1 arg2 arg3)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2)
				     (mapper3 arg3))))))
	  ((4)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers))
		 (mapper4 (cadddr mappers)))
	     (lambda (arg1 arg2 arg3 arg4)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2)
				     (mapper3 arg3)
				     (mapper4 arg4))))))
	  (else
	   (lambda args
	     (unless (= (length args) (length mappers))
	       (assertion-violation funcname
		 (format "wrong number of arguments, expected ~a" (length mappers))
		 args))
	     (cast-func (apply stub-func f
			       (map (lambda (m a)
				      (m a)) mappers args))))))))))



;;;; interface functions, with errno

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (receive (cast-func stub-func)
      (select-cast-and-stub (external->internal ret-type))
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (when (equal? '(void) arg-types)
	(set! arg-types '()))
      (let* ((mappers (map select-argument-mapper arg-types)))
	(case (length mappers)
	  ((0)
	   (lambda ()
	     ;;We  have  to  use  LET*  here to  enforce  the  order  of
	     ;;evaluation: we want to gather the "errno" value AFTER the
	     ;;foreign function call.
	     (let* ((retval	(begin
				  (errno 0)
				  (cast-func (stub-func f 0))))
		    (errval	(errno)))
	       (values retval errval))))
	  ((1)
	   (let ((mapper (car mappers)))
	     (lambda (arg)
	       ;;We  have to  use  LET*  here to  enforce  the order  of
	       ;;evaluation: we  want to gather the  "errno" value AFTER
	       ;;the foreign function call.
	       (let* ((retval	(begin
				  (errno 0)
				  (cast-func (stub-func f (mapper arg)))))
		      (errval	(errno)))
		 (values retval errval)))))
	  ((2)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers)))
	     (lambda (arg1 arg2)
	       ;;We  have to  use  LET*  here to  enforce  the order  of
	       ;;evaluation: we  want to gather the  "errno" value AFTER
	       ;;the foreign function call.
	       (let* ((retval	(begin
				  (errno 0)
				  (cast-func (stub-func f
						      (mapper1 arg1)
						      (mapper2 arg2)))))
		      (errval	(errno)))
		 (values retval errval)))))
	  ((3)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers)))
	     (lambda (arg1 arg2 arg3)
	       ;;We  have to  use  LET*  here to  enforce  the order  of
	       ;;evaluation: we  want to gather the  "errno" value AFTER
	       ;;the foreign function call.
	       (let* ((retval	(begin
				  (errno 0)
				  (cast-func (stub-func f
							(mapper1 arg1)
							(mapper2 arg2)
							(mapper3 arg3)))))
		      (errval	(errno)))
		 (values retval errval)))))
	  ((4)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers))
		 (mapper4 (cadddr mappers)))
	     (lambda (arg1 arg2 arg3 arg4)
	       ;;We  have to  use  LET*  here to  enforce  the order  of
	       ;;evaluation: we  want to gather the  "errno" value AFTER
	       ;;the foreign function call.
	       (let* ((retval	(begin
				  (errno 0)
				  (cast-func (stub-func f
							(mapper1 arg1)
							(mapper2 arg2)
							(mapper3 arg3)
							(mapper4 arg4)))))
		      (errval	(errno)))
		 (values retval errval)))))
	  (else
	   (lambda args
	     (unless (= (length args) (length mappers))
	       (assertion-violation funcname
		 (format "wrong number of arguments, expected ~a" (length mappers))
		 args))
	     ;;We  have  to  use  LET*  here to  enforce  the  order  of
	     ;;evaluation: we want to gather the "errno" value AFTER the
	     ;;foreign function call.
	     (let* ((retval	(begin
				  (errno 0)
				  (cast-func
				   (apply stub-func f
					  (map (lambda (m a)
						 (m a)) mappers args)))))
		    (errval	(errno)))
	       (values retval errval)))))))))



;;;; done

)

;;; end of file
