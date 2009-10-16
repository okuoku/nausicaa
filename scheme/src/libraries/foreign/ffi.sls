;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;	This is the core of the foreign functions interface.
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign ffi)
  (export

    ;;shared object access
    open-shared-object
    shared-object			self-shared-object

    ;;interface functions
    primitive-make-c-function		primitive-make-c-function/with-errno
    make-c-function			make-c-function/with-errno
    define-c-function			define-c-function/with-errno
    primitive-make-c-callback		primitive-free-c-callback
    make-c-callback
    errno

    ;;foreign struct accessors
    define-c-struct-accessor-and-mutator
    define-c-struct-accessor		define-c-struct-mutator
    define-c-struct-field-pointer-accessor)

  (import (rnrs)
    (only (foreign ffi pointers)
	  pointer-add)
    (only (foreign ffi compat)
	  shared-object primitive-open-shared-object self-shared-object
	  primitive-make-c-function primitive-make-c-function/with-errno
	  primitive-make-c-callback primitive-free-c-callback
	  errno))


(define (open-shared-object library-name)
  (primitive-open-shared-object (if (symbol? library-name)
				    (symbol->string library-name)
				  library-name)))

(define-syntax make-c-function
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-function ret '?funcname (list arg ...)))))))

(define-syntax make-c-function/with-errno
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-function/with-errno ret '?funcname (list arg ...)))))))

(define-syntax define-c-function
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function ?ret-type ?funcname (?arg-type0 ?arg-type ...))))))

(define-syntax define-c-function/with-errno
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function/with-errno ?ret-type ?funcname (?arg-type0 ?arg-type ...))))))


(define-syntax make-c-callback
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?scheme-function (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-callback ret ?scheme-function (list arg ...)))))))


;;;; foreign structures accessors

(define-syntax define-c-struct-accessor-and-mutator
  (syntax-rules ()
    ((_ ?mutator-name ?accessor-name ?field-offset ?foreign-type-mutator ?foreign-type-accessor)
     (begin
       (define-c-struct-accessor ?accessor-name ?field-offset ?foreign-type-accessor)
       (define-c-struct-mutator  ?mutator-name ?field-offset ?foreign-type-mutator)))))

(define-syntax define-c-struct-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset ?foreign-type-accessor)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (?foreign-type-accessor struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))

(define-syntax define-c-struct-mutator
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?mutator-name ?field-offset ?foreign-type-mutator)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?mutator-name
	       (syntax-rules ()
		 ((_ struct-pointer value)
		  (?foreign-type-mutator struct-pointer
					?field-offset
					value))))
	 #'(define-syntax ?mutator-name
	     (syntax-rules ()
	       ((_ struct-pointer value)
		(raise-unimplemented-error (quote ?mutator-name))))))))))

(define-syntax define-c-struct-field-pointer-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (pointer-add struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))


;;;; done

)

;;; end of file
