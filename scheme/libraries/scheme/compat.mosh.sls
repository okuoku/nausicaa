;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Mosh compatibility library for (scheme) language
;;;Date: Wed Jan 21, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

#!r6rs
(library (scheme compat)
  (export

    equal-hash pretty-print implementation-features

    ;; parameters
    make-parameter parameterize

    ;; environment variables
    get-environment-variable get-environment-variables)
  (import (rnrs)
    (only (system)
	  get-environment-variable get-environment-variables)
    (only (mosh pp)
	  pretty-print))

  (define implementation-features
    '(mosh))

  (define make-parameter
    (case-lambda
     ((val)
      (make-parameter val values))
     ((val guard)
      (unless (procedure? guard)
	(assertion-violation 'make-parameter "not a procedure" guard))
      (let ((p (case-lambda
		(() val)
		((x) (set! val (guard x))))))
	(p val)
	p))))

  ;;Derived from Ikarus's implementation of parameterize.
  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
        ((_ () ?expr0 ?expr ...)
         (syntax (letrec* () ?expr0 ?expr ...)))
        ((_ ((?parm ?value) ...) ?expr0 ?expr ...)
         (with-syntax
	     (((tp ...) (generate-temporaries (syntax (?parm ...))))
	      ((te ...) (generate-temporaries (syntax (?value ...)))))
           (syntax (let ((tp ?parm) ...
			 (te ?value) ...)
		     (let ((swap (lambda ()
				   (let ((t (tp)))
				     (tp te)
				     (set! te t))
				   ...)))
		       (dynamic-wind
			   swap
			   (lambda () ?expr0 ?expr ...)
			   swap))))))))))

;;; end of file
