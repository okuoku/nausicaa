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


(library (foreign ffi compat)
  (export
    shared-object primitive-open-shared-object self-shared-object
    primitive-make-c-function primitive-make-c-function/with-errno
    primitive-make-c-callback primitive-free-c-callback
    errno)
  (import (rnrs)
    (only (system)
	  make-parameter)
    (rename (only (mosh ffi)
		  open-shared-library make-c-function
		  make-c-callback free-c-callback
		  shared-errno)
	    (shared-errno errno)
	    (free-c-callback primitive-free-c-callback)))


;;;; dynamic loading

(define self-shared-object
  (open-shared-library ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object open-shared-library)


;;;; values normalisation: Foreign -> Mosh
;;
;;According to "lib/mosh/ffi.ss" (revision 2140):
;;
;;* The accepted return values for callouts are:
;;
;;  void
;;  void*	char*
;;  int
;;  double
;;
;;* The accepted arguments for callouts are:
;;
;;  int
;;  double
;;  void*	char*
;;
;;  an empty list represents no arguments.
;;
;;* The accepted return values for callbacks are:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;  float		double
;;  void*
;;
;;* The accepted arguments for callbacks are:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;  float		double
;;  void*
;;
;;  an empty list represents no arguments.
;;

(define (nausicaa-type->mosh-type type)
  (case type
    ((void)
     'void)
    ((char schar signed-char uchar unsigned-char)
     'int)
    ((int signed-int ssize_t)
     'int)
    ((long signed-long)
     'int)
    ((uint unsigned unsigned-int)
     'int)
    ((size_t)
     'int)
    ((long signed-long)
     'int)
    ((ulong unsigned-long)
     'int)
    ((float)
     'double)
    ((double)
     'double)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    (else
     (assertion-violation 'make-c-callback
       "unknown C language type identifier" type))))

(define (nausicaa-type->mosh-type/callback type)
  (case type
    ((void)
     'void)
    ((char schar signed-char uchar unsigned-char)
     'char)
    ((int signed-int ssize_t)
     'int)
    ((uint unsigned unsigned-int)
     'unsigned-int)
    ((size_t)
     'size_t)
    ((long signed-long)
     'long)
    ((ulong unsigned-long)
     'unsigned-long)
    ((float)
     'float)
    ((double)
     'double)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    (else
     (assertion-violation 'make-c-callback
       "unknown C language type identifier" type))))


;;;; interface functions

(define (primitive-make-c-function ret-type funcname arg-types)
  (make-c-function (shared-object)
		   (nausicaa-type->mosh-type ret-type)
		   funcname
		   (if (equal? '(void) arg-types)
		       '()
		     (map nausicaa-type->mosh-type arg-types))))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (let ((f (primitive-make-c-function ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply f args)))
	     (errval	(errno)))
	(values retval errval)))))

(define (primitive-make-c-callback ret-type scheme-function arg-types)
  (make-c-callback (nausicaa-type->mosh-type/callback ret-type)
		   (if (equal? '(void) arg-types)
		       '()
		     (map nausicaa-type->mosh-type/callback arg-types))
		   scheme-function))


;;;; done

)

;;; end of file
