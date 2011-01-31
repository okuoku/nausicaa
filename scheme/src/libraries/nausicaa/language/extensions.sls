;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: simple language extensions
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;;; copyright notice for some SRFI implementations and the XOR macro
;;;
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

;;;; copyright notice for the REC definition, renamed to RECURSIION
;;;
;;;Copyright (c) 2002 Dr. Mirko Luedde <Mirko.Luedde@SAP.com>
;;;All Rights Reserved.
;;;
;;;Modified by Derick Eddington as port to R6RS.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

;;;; copyright notice for the CUT and CUTE definitions
;;;
;;;Reference implementation for SRFI-26 "cut"
;;;
;;;Copyright (c) 2002 Sebastian.Egner@philips.com, 5-Jun-2002.
;;;Copyright (c) 2002 Al Petrofsky <al@petrofsky.org>
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

;;;; copyright notice for the shared structures SRFI
;;;
;;;Copyright (C) Ray Dillinger 2003.  All Rights Reserved.
;;;
;;;This document and  translations of it may be  copied and furnished to
;;;others, and derivative works that  comment on or otherwise explain it
;;;or assist  in its implementation  may be prepared,  copied, published
;;;and  distributed, in  whole or  in part,  without restriction  of any
;;;kind, provided that the above copyright notice and this paragraph are
;;;included  on all  such  copies and  derivative  works. However,  this
;;;document itself may  not be modified in any way,  such as by removing
;;;the  copyright  notice  or  references  to  the  Scheme  Request  For
;;;Implementation process  or editors, except as needed  for the purpose
;;;of  developing SRFIs  in  which case  the  procedures for  copyrights
;;;defined  in the  SRFI process  must be  followed, or  as  required to
;;;translate it into languages other than English.
;;;
;;;The limited permissions  granted above are perpetual and  will not be
;;;revoked by the authors or their successors or assigns.
;;;
;;;This document and the information  contained herein is provided on an
;;;"AS  IS" basis  and  THE AUTHOR  AND  THE SRFI  EDITORS DISCLAIM  ALL
;;;WARRANTIES,  EXPRESS OR  IMPLIED, INCLUDING  BUT NOT  LIMITED  TO ANY
;;;WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;;RIGHTS OR ANY IMPLIED WARRANTIES  OF MERCHANTABILITY OR FITNESS FOR A
;;;PARTICULAR PURPOSE.
;;;
;;;A printer that  shows all sharing of substructures.   Uses the Common
;;;Lisp  print-circle notation:  #n# refers  to a  previous substructure
;;;labeled with #n=.  Takes O(n^2) time.
;;;
;;;Code attributed to Al* Petrofsky, modified by Dillinger.
;;;
;;;Minor tweaks for ERR5RS/R6RS by Ken Dickey
;;;
;;;NOTE: This pre-R6RS  code does not support full  R6RS lexical syntax.
;;;This   library  is  a   last  resort   fall-back  because   the  R6RS
;;;implementation should supply this functionality.

;;;;copyright notice for RECEIVE
;;;
;;;Copyright (C) John David Stone (1999). All Rights Reserved.
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
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(library (nausicaa language extensions)
  (export
    and-let* begin0 begin0-let begin0-let* begin0-letrec
    receive recursion cut cute <> <...> xor
    do* while while* do-while do-while*
    dotimes dolist loop-upon-list ensure unwind-protect
    set-cons! incr! decr!
    define-identifier-accessor-mutator identifier-syntax-accessor-mutator
    with-accessor-and-mutator
    define-inline define-values define-constant define-constant-values
    define-syntax* define-auxiliary-syntax define-auxiliary-syntaxes
    define-for-expansion-evaluation)
  (import (rnrs)
    (only (nausicaa language auxiliary-syntaxes) <> <...>))


(define-syntax and-let*
  (syntax-rules ()
    ((_ . r)
     (and-let*-core #t . r))))

(define-syntax and-let*-core
  (lambda (stx)
    (syntax-case stx ()
      ((kw _ ((var expr) . c) . b)
       #'(let ((var expr))
	   (and var
		(kw var c . b))))
      ((kw last ((expr) . c) . b)
       #'(kw last ((t expr) . c) . b))
      ((kw _ (id . c) . b)
       (identifier? #'id)
       #'(and id
	      (kw id c . b)))
      ((_ last ())
       #'last)
      ((_ _ () . b)
       #'(let () . b)))))

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda ?formals ?form0 ?form ...)))))

(define-syntax define-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?var ... ?var0) ?form0 ?form ...)
       (with-syntax (((VAR ... VAR0) (generate-temporaries #'(?var ... ?var0))))
	 #'(begin
	     ;;We  must make  sure that  the ?FORMs  do not  capture the
	     ;;?VARs.
	     (define (dummy)
	       ?form0 ?form ...)
	     (define ?var  #f)
	     ...
	     (define ?var0
	       (let-values (((VAR ... VAR0) (dummy)))
		 (set! ?var  VAR)
		 ...
		 VAR0))))))))

(define-syntax define-constant-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?var ... ?var0) ?form0 ?form ...)
       (let ((stx #'(?var ... ?var0)))
	 (with-syntax (((VAR ... VAR0) (generate-temporaries stx))
		       ((ID  ... ID0)  (generate-temporaries stx)))
	   #'(begin
	       (define (dummy)
		 ?form0 ?form ...)
	       (define ID  #f)
	       ...
	       (define ID0
		 (let-values (((VAR ... VAR0) (dummy)))
		   (set! ID  VAR)
		   ...
		   VAR0))
	       (define-syntax ?var
		 (identifier-syntax ID))
	       ...
	       (define-syntax ?var0
		 (identifier-syntax ID0))
	       )))))))

(define-syntax xor
  (syntax-rules ()
    ((_ expr ...)
     (xor-aux #F expr ...))))

(define-syntax xor-aux
  (syntax-rules ()
    ((_ r)
     r)
    ((_ r expr)
     (let ((x expr))
       (if r
           (and (not x) r)
	 x)))
    ((_ r expr0 expr ...)
     (let ((x expr0))
       (and (or (not r) (not x))
	    (let ((n (or r x)))
	      (xor-aux n expr ...)))))))


(define-syntax recursion
  (syntax-rules ()
    ((_ (?name . ?variables) . ?body)
     (letrec ((?name (lambda ?variables . ?body))) ?name))
    ((_ ?name ?expr)
     (letrec ((?name ?expr)) ?name))))


(define-syntax cut
  (syntax-rules ()
    ((cut . slots-or-exprs)
     (%internal-cut () () . slots-or-exprs))))

(define-syntax cute
  (syntax-rules ()
    ((cute . slots-or-exprs)
     (%internal-cute () () () . slots-or-exprs))))

(define-syntax %internal-cut
  (syntax-rules (<> <...>)
    ((internal-cut (?slot-name ...) (?proc ?arg ...))
     (lambda (?slot-name ...) ((begin ?proc) ?arg ...)))
    ((internal-cut (?slot-name ...) (?proc ?arg ...) <...>)
     (lambda (?slot-name ... . rest-slot) (apply ?proc ?arg ... rest-slot)))
    ((internal-cut (?slot-name ...)	(?position ...)		<>   . ?se)
     (internal-cut (?slot-name ... x)	(?position ... x)	     . ?se))
    ((internal-cut (?slot-name ...)	(?position ...)		?nse . ?se)
     (internal-cut (?slot-name ...)	(?position ... ?nse)	     . ?se))))

(define-syntax %internal-cute
  (syntax-rules (<> <...>)
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...))
     (let ?nse-bindings (lambda (?slot-name ...) (?proc ?arg ...))))
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...) <...>)
     (let ?nse-bindings (lambda (?slot-name ... . x) (apply ?proc ?arg ... x))))
    ((internal-cute (?slot-name ...)   ?nse-bindings  (?position ...)   <>  . se)
     (internal-cute (?slot-name ... x) ?nse-bindings  (?position ... x)     . se))
    ((internal-cute ?slot-names        ?nse-bindings  (?position ...)   nse . se)
     (internal-cute ?slot-names ((x nse) . ?nse-bindings) (?position ... x) . se))))


(define-syntax do*
  ;;Like DO,  but binds  the iteration variables  like LET*  rather than
  ;;like LET.   Notice the "quoting"  of the ellipsis in  the LET-SYNTAX
  ;;expressions.
  (syntax-rules ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (let-syntax ((the-expr (syntax-rules ()
			      ((_)
			       (values))
			      ((_ ?-expr0 ?-expr (... ...))
			       (begin ?-expr0 ?-expr (... ...)))))
		  (the-step (syntax-rules ()
			      ((_ ?-var)
			       ?-var)
			      ((_ ?-var ?-step)
			       ?-step)
			      ((_ ?-var ?-step0 ?-step (... ...))
			       (syntax-violation 'do*
						 "invalid step specification"
						 '(?-step0 ?-step (... ...)))))))
       (let* ((?var ?init) ...)
	 (let loop ((?var ?var) ...)
	   (if ?test
	       (the-expr ?expr ...)
	     (begin
	       ?form ...
	       (loop (the-step ?var ?step ...) ...)))))))))


(define-syntax while
  (syntax-rules ()
    ((_ ?test ?body0 ?body ...)
     (let loop ()
       (when ?test ?body0 ?body ... (loop))))))

(define-syntax do-while
  (syntax-rules ()
    ((_ ?test ?body0 ?body ...)
     (let loop ()
       ?body0 ?body ...
       (when ?test (loop))))))

(define-syntax while*
  (lambda (stx)
    (syntax-case stx ()
      ((?use ?test ?body0 ?body ...)
       (with-syntax ((BREAK (datum->syntax #'?use 'break)))
	 #'(call-with-current-continuation
	       (lambda (BREAK)
		 (let loop ()
		   (when ?test ?body0 ?body ... (loop))))))))))

(define-syntax do-while*
  (lambda (stx)
    (syntax-case stx ()
      ((?use ?test ?body0 ?body ...)
       (with-syntax ((BREAK (datum->syntax #'?use 'break)))
	 #'(call-with-current-continuation
	       (lambda (BREAK)
		 (let loop ()
		   ?body0 ?body ...
		   (when ?test (loop))))))))))


;;;; loop syntaxes

(define-syntax dotimes
  (syntax-rules ()
    ((_ (?varname ?exclusive-count) ?form0 ?form ...)
     (dotimes (?varname ?exclusive-count #f) ?form0 ?form ...))
    ((_ (?varname ?exclusive-count ?result) ?form0 ?form ...)
     (let ((exclusive-count ?exclusive-count))
       (do ((?varname 0 (+ 1 ?varname)))
	   ((>= ?varname ?exclusive-count)
	    ?result)
	 ?form0 ?form ...)))))

(define-syntax dolist
  (syntax-rules ()
    ((_ (?varname ?list) ?form0 ?form ...)
     (dolist (?varname ?list #f) ?form0 ?form ...))
    ((_ (?varname ?list ?result) ?form0 ?form ...)
     (let ((ell ?list))
       (let loop ((?varname (car ell))
		  (the-list (cdr ell)))
	 ?form0 ?form ...
	 (if (null? the-list)
	     ?result
	   (loop (car the-list) (cdr the-list))))))))

(define-syntax loop-upon-list
  (syntax-rules (break-when)
    ((_ (?varname ?list) (break-when ?condition) ?form0 ?form ...)
     (loop-upon-list (?varname ?list #f) (break-when ?condition) ?form0 ?form ...))
    ((_ (?varname ?list ?result) (break-when ?condition) ?form0 ?form ...)
     (let ((exit	(lambda () #f ?result))
	   (ell		?list))
       (let loop ((ell		(cdr ell))
		  (?varname	(car ell)))
	 (if ?condition
	     (exit)
	   (begin
	     ?form0 ?form ...
	     (if (null? ell)
		 (exit)
	       (loop (cdr ell) (car ell))))))))))


(define-syntax incr!
  (syntax-rules ()
    ((_ ?id)
     (set! ?id (+ ?id 1)))
    ((_ ?id ?delta)
     (set! ?id (+ ?id ?delta)))))

(define-syntax decr!
  (syntax-rules ()
    ((_ ?id)
     (set! ?id (- ?id 1)))
    ((_ ?id ?delta)
     (set! ?id (- ?id ?delta)))))

(define-syntax set-cons!
  (syntax-rules ()
    ((_ ?name ?form)
     (set! ?name (cons ?form ?name)))))


(define-syntax ensure
  (syntax-rules (by else else-by)
    ((_ ?condition
	(by ?by-form0 ?by-form ...)
	(else-by ?else-by-form0 ?else-by-form ...) ...
	(else ?else-form0 ?else-form ...))
     (let ((retval #f))
       (loop-upon-list
	   (expr (list (lambda () ?by-form0 ?by-form ...)
		       (lambda () ?else-by-form0 ?else-by-form ...)
		       ...
		       (lambda () ?else-form0 ?else-form ...))
		 retval)
	   (break-when ?condition)
	 (set! retval (expr)))))))

(define-syntax unwind-protect
  (syntax-rules ()
    ((_ ?body ?cleanup0 ?cleanup ...)
     (dynamic-wind
         values
	 (lambda () ?body)
	 (lambda () ?cleanup0 ?cleanup ...)))))


(define-syntax begin0
  ;;This  syntax  comes from  the  R6RS  original  document, Appendix  A
  ;;``Formal semantics''.
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda args
	 ?expr ...
	 (apply values args))))))

(define-syntax begin0-let
  (syntax-rules ()
    ((_ (((?var0 ...) ?expr0) ((?var ...) ?expr) ...) ?form0 ?form ...)
     (let-values (((?var0 ...) ?expr0)
		  ((?var  ...) ?expr)
		  ...)
       ?form0 ?form ...
       (values ?var0 ...)))
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
     (let ((?var0 ?expr0)
	   (?var  ?expr)
	   ...)
       ?form0 ?form ...
       ?var0))))

(define-syntax begin0-let*
  (syntax-rules ()
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
     (let* ((?var0 ?expr0)
	    (?var  ?expr)
	    ...)
       ?form0 ?form ...
       ?var0))))

(define-syntax begin0-letrec
  (syntax-rules ()
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
     (letrec ((?var0 ?expr0)
	      (?var  ?expr)
	      ...)
       ?form0 ?form ...
       ?var0))))


;;;; macro definition helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ...) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ...)
	  (begin ?form0 ?form ...)))))))


(define-syntax define-identifier-accessor-mutator
  (syntax-rules ()
    ((_ ?name ?thing ?accessor ?mutator)
     (define-syntax ?name
       (identifier-syntax
	(_		(?accessor ?thing))
	((set! _ expr)	(?mutator  ?thing expr)))))
    ((_ ?name ?thing ?accessor)
     (define-syntax ?name
       (identifier-syntax (?accessor ?thing))))))

(define-syntax identifier-syntax-accessor-mutator
  (syntax-rules ()
    ((_ ?thing ?accessor ?mutator)
     (identifier-syntax
      (_		(?accessor ?thing))
      ((set! _ expr)	(?mutator  ?thing expr))))
    ((_ ?thing ?accessor)
     (identifier-syntax (?accessor ?thing)))))

(define-syntax with-accessor-and-mutator
  (syntax-rules ()
    ((_ ((?name ?thing ?accessor ?mutator) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax
			  (_              (?accessor ?thing))
			  ((set! _ ?expr) (?mutator ?thing ?expr)))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ ((?name ?thing ?accessor) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax (?accessor ?thing))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ () ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ ?name ?expr)
     (begin
       (define ghost ?expr)
       (define-syntax ?name
	 (identifier-syntax ghost))))))

(define-syntax define-syntax*
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?name ?stx) ?body0 ?body ...)
       (and (identifier? #'?name) (identifier? #'?stx))
       (with-syntax ((SYNNER (datum->syntax #'?name 'synner)))
	 #'(define-syntax ?name
	     (lambda (?stx)
	       (define SYNNER
		 (case-lambda
		  ((message)
		   (SYNNER message #f))
		  ((message subform)
		   (syntax-violation '?name message (syntax->datum ?stx) (syntax->datum subform)))))
	       ?body0 ?body ...)))))))

(define-syntax define-auxiliary-syntax
  (syntax-rules ()
    ((_ ?name)
     (define-syntax ?name (syntax-rules ())))
    ((_ ?name0 ?name ...)
     (begin
       (define-syntax ?name0 (syntax-rules ()))
       (define-auxiliary-syntax ?name ...)))
    ((_)	;allows this  syntax to be called with  no arguments and
		;still expand to a definition
     (define-syntax dummy (syntax-rules ())))
    ))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ . ?args)
     (define-auxiliary-syntax . ?args))))

(define-syntax define-for-expansion-evaluation
  (syntax-rules ()
    ((_ ?form ...)
     (begin
       (define-syntax the-macro
	 (lambda (stx)
	   ?form ...
	   #'(define dummy)))
       (the-macro)))))


;;;; done

)

;;; end of file
