;;;
;;;Part of: Nausicaa
;;;Contents: common stuff for the nausicaa language
;;;Date: Thu Jun 18, 2009
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


;;;; copyright notice for some SRFI implementations
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


(library (nausicaa common)
  (export

    ;; predicate extensions
    finite? infinite? nan? non-negative? non-positive?

    ;; syntactic abstractions
    and-let* begin0 receive recursion cut cute

    ;; shared structures
    read-with-shared-structure
    write-with-shared-structure

    ;; unimplemented condition
    &unimplemented
    make-unimplemented-condition
    unimplemented-condition?
    raise-unimplemented-error

    ;; simple syntaxes
    dotimes dolist loop-upon-list ensure

    ;; deferred exceptions
    with-deferred-exceptions-handler
    defer-exceptions run-deferred-exceptions-handler

    ;; compensations
    with-compensations with-compensations/on-error
    compensate run-compensations push-compensation

    ;; miscellaneous
    symbol*->string symbol->string/maybe)
  (import (rename (rnrs)
		  (finite?	rnrs:finite?)
		  (infinite?	rnrs:infinite?)
		  (nan?		rnrs:nan?))
    (parameters)
    (rnrs mutable-pairs)
    (rnrs mutable-strings))


;;;; extension for finite?, infinite?, nan?

(define (finite? num)
  (if (complex? num)
      (and (rnrs:finite? (real-part num))
	   (rnrs:finite? (imag-part num)))
    (rnrs:finite? num)))

(define-syntax cplx-or-pred
  (syntax-rules ()
    ((_ ?pred ?rnrs-pred)
     (define (?pred num)
       (if (complex? num)
	   (or (?rnrs-pred (real-part num))
	       (?rnrs-pred (imag-part num)))
	 (?rnrs-pred num))))))

(cplx-or-pred infinite?	rnrs:infinite?)
(cplx-or-pred nan?	rnrs:nan?)

(define (non-negative? n)
  (or (positive? n) (zero? n)))

(define (non-positive? n)
  (or (negative? n) (zero? n)))


;;;; syntactic absractions

(define-syntax and-let*
  (lambda (stx)
    (define (get-id c)
      (syntax-case c () [(var expr) #'var] [_ #f]))
    (syntax-case stx ()
      [(_ (clause* ...) body* ...)
       (for-all identifier? (filter values (map get-id #'(clause* ...))))
       #'(and-let*-core #t (clause* ...) body* ...)])))

(define-syntax and-let*-core
  (lambda (stx)
    (syntax-case stx ()
      [(kw _ ([var expr] clause* ...) body* ...)
       #'(let ([var expr])
	   (if var
               (kw var (clause* ...) body* ...)
	     #f))]
      [(kw _ ([expr] clause* ...) body* ...)
       #'(let ([t expr])
	   (if t
               (kw t (clause* ...) body* ...)
	     #f))]
      [(kw _ (id clause* ...) body* ...)
       (or (identifier? #'id)
	   (syntax-violation #f "invalid clause" stx #'id))
       #'(if id
             (kw id (clause* ...) body* ...)
	   #f)]
      [(kw last () body* ...)
       (if (positive? (length #'(body* ...)))
           #'(begin body* ...)
	 #'last)])))

;;; --------------------------------------------------------------------

;;;This  syntax  comes  from  the  R6RS original  document,  Appendix  A
;;;``Formal semantics''.
(define-syntax begin0
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda x
	 ?expr ...
	 (apply values x))))))

;;; --------------------------------------------------------------------

(define-syntax receive
  (syntax-rules ()
    [(_ formals expression b b* ...)
     (call-with-values
         (lambda () expression)
       (lambda formals b b* ...))]))

;;; --------------------------------------------------------------------

(define-syntax recursion
  (syntax-rules ()
    ((_ (?name . ?variables) . ?body)
     (letrec ((?name (lambda ?variables . ?body))) ?name))
    ((_ ?name ?expr)
     (letrec ((?name ?expr)) ?name))))

;;; --------------------------------------------------------------------

(define-syntax internal-cut
  (syntax-rules (<> <...>)
    ((internal-cut (?slot-name ...) (?proc ?arg ...))
     (lambda (?slot-name ...) ((begin ?proc) ?arg ...)))
    ((internal-cut (?slot-name ...) (?proc ?arg ...) <...>)
     (lambda (?slot-name ... . rest-slot) (apply ?proc ?arg ... rest-slot)))
    ((internal-cut (?slot-name ...)	(?position ...)		<>   . ?se)
     (internal-cut (?slot-name ... x)	(?position ... x)	     . ?se))
    ((internal-cut (?slot-name ...)	(?position ...)		?nse . ?se)
     (internal-cut (?slot-name ...)	(?position ... ?nse)	     . ?se))))

(define-syntax internal-cute
  (syntax-rules (<> <...>)
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...))
     (let ?nse-bindings (lambda (?slot-name ...) (?proc ?arg ...))))
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...) <...>)
     (let ?nse-bindings (lambda (?slot-name ... . x) (apply ?proc ?arg ... x))))
    ((internal-cute (?slot-name ...)   ?nse-bindings  (?position ...)   <>  . se)
     (internal-cute (?slot-name ... x) ?nse-bindings  (?position ... x)     . se))
    ((internal-cute ?slot-names        ?nse-bindings  (?position ...)   nse . se)
     (internal-cute ?slot-names ((x nse) . ?nse-bindings) (?position ... x) . se))))

(define-syntax cut
  (syntax-rules ()
    ((cut . slots-or-exprs)
     (internal-cut () () . slots-or-exprs))))

(define-syntax cute
  (syntax-rules ()
    ((cute . slots-or-exprs)
     (internal-cute () () () . slots-or-exprs))))


;;;; writing and reading shared structures

(define (write-with-shared-structure obj . optional-port)
  (define (acons key val alist)
    (cons (cons key val) alist))
  (define outport (if (eq? '() optional-port)
		      (current-output-port)
		    (car optional-port)))
  (define (interesting? obj)
    (or (pair? obj)
	(and (vector? obj) (not (zero? (vector-length obj))))
	(and (string? obj) (not (zero? (string-length obj))))))
  (define (write-obj obj alist)
    (define (write-interesting alist)
      (cond ((pair? obj)
	     (display "(" outport)
	     (let write-cdr ((obj (cdr obj)) (alist (write-obj (car obj) alist)))
	       (cond ((and (pair? obj) (not (cdr (assq obj alist))))
		      (display " " outport)
		      (write-cdr (cdr obj) (write-obj (car obj) alist)))
		     ((null? obj)
		      (display ")" outport)
		      alist)
		     (else
		      (display " . " outport)
		      (let ((alist (write-obj obj alist)))
			(display ")" outport)
			alist)))))
	    ((vector? obj)
	     (display "#(" outport)
	     (let ((len (vector-length obj)))
	       (let write-vec ((i 1) (alist (write-obj (vector-ref obj 0) alist)))
		 (cond ((= i len) (display ")" outport) alist)
		       (else (display " " outport)
			     (write-vec (+ i 1)
					(write-obj (vector-ref obj i) alist)))))))
	    (else (write obj outport) alist)))
    (cond ((interesting? obj)
	   (let ((val (cdr (assq obj alist))))
	     (cond ((not val) (write-interesting alist))
		   ((number? val)
		    (begin (display "#" outport)
			   (write val outport)
			   (display "#" outport) alist))
		   (else
		    (let ((n (+ 1 (cdar alist))))
		      (begin (display "#" outport)
			     (write n outport)
			     (display "=" outport))
		      (write-interesting (acons obj n alist)))))))
	  (else (write obj outport) alist)))
  (define (scan obj alist)
    (cond ((not (interesting? obj)) alist)
	  ((assq obj alist)
	   => (lambda (p) (if (cdr p) alist (acons obj #t alist))))
	  (else
	   (let ((alist (acons obj #f alist)))
	     (cond ((pair? obj) (scan (car obj) (scan (cdr obj) alist)))
		   ((vector? obj)
		    (let ((len (vector-length obj)))
		      (do ((i 0 (+ 1 i))
			   (alist alist (scan (vector-ref obj i) alist)))
			  ((= i len) alist))))
		   (else alist))))))
  (write-obj obj (acons 'dummy -1 (scan obj '())))
  (if #f #f))

(define (read-with-shared-structure . optional-port)
  (define port
    (if (null? optional-port) (current-input-port) (car optional-port)))
  (define (read-char*) (read-char port))
  (define (peek-char*) (peek-char port))
  (define (looking-at? c)
    (eqv? c (peek-char*)))
  (define (delimiter? c)
    (case c
      ((#\( #\) #\" #\;) #t)
      (else (or (eof-object? c)
		(char-whitespace? c)))))
  (define (not-delimiter? c) (not (delimiter? c)))
  (define (eat-intertoken-space)
    (define c (peek-char*))
    (cond ((eof-object? c))
	  ((char-whitespace? c) (read-char*) (eat-intertoken-space))
	  ((char=? c #\;)
	   (do ((c (read-char*) (read-char*)))
	       ((or (eof-object? c) (char=? c #\newline))))
	   (eat-intertoken-space))))
  (define (read-string)
    (read-char*)
    (let read-it ((chars '()))
      (let ((c (read-char*)))
	(if (eof-object? c)
	    (error 'read-string "EOF inside a string")
	  (case c
	    ((#\") (list->string (reverse chars)))
	    ((#\\) (read-it (cons (read-char*) chars)))
	    (else (read-it (cons c chars))))))))
  (define (read-some-chars pred)
    (let iter ((chars '()))
      (let ((c (peek-char*)))
	(if (or (eof-object? c) (not (pred c)))
	    (list->string (reverse chars))
	  (iter (cons (read-char*) chars))))))
  (define (read-character)
    (let ((c (peek-char*)))
      (cond ((eof-object? c) (error 'read-character "EOF inside a character"))
	    ((char-alphabetic? c)
	     (let ((name (read-some-chars char-alphabetic?)))
	       (cond ((= 1 (string-length name)) (string-ref name 0))
		     ((string-ci=? name "space") #\space)
		     ((string-ci=? name "newline") #\newline)
		     (else (error 'read-character "Unknown named character" name)))))
	    (else (read-char*)))))
  (define (read-number first-char)
    (let ((str (string-append (string first-char)
			      (read-some-chars not-delimiter?))))
      (or (string->number str)
	  (error 'read-number "Malformed number" str))))
  (define char-standard-case
    (if (char=? #\a (string-ref (symbol->string 'a) 0))
	char-downcase
      char-upcase))
  (define (string-standard-case str)
    (let* ((len (string-length str))
	   (new (make-string len)))
      (do ((i 0 (+ i 1)))
	  ((= i len) new)
	(string-set! new i (char-standard-case (string-ref str i))))))
  (define (read-identifier)
    (string->symbol (string-standard-case (read-some-chars not-delimiter?))))
  (define (read-part-spec)
    (let ((n (string->number (read-some-chars char-numeric?))))
      (let ((c (read-char*)))
	(case c
	  ((#\=) (cons 'decl n))
	  ((#\#) (cons 'use n))
	  (else (error 'read-part-spec "Malformed shared part specifier"))))))
  (define (read-optional-token)
    (eat-intertoken-space)
    (let ((c (peek-char*)))
      (case c
	((#\( #\) #\' #\`) (read-char*) (list c))
	((#\,)
	 (read-char*)
	 (if (looking-at? #\@)
	     (begin (read-char*) '(#\@))
	   '(#\,)))
	((#\") (read-string))
	((#\.)
	 (read-char*)
	 (cond ((delimiter? (peek-char*)) '(#\.))
	       ((not (looking-at? #\.)) (read-number #\.))
	       ((begin (read-char*) (looking-at? #\.)) (read-char*) '...)
	       (else (error 'read-char* "Malformed token starting with \"..\""))))
	((#\+) (read-char*) (if (delimiter? (peek-char*)) '+ (read-number c)))
	((#\-) (read-char*) (if (delimiter? (peek-char*)) '- (read-number c)))
	((#\#)
	 (read-char*)
	 (let ((c (peek-char*)))
	   (case c
	     ((#\() (read-char*) '(#\#))
	     ((#\\) (read-char*) (read-character))
	     ((#\t #\T) (read-char*) #t)
	     ((#\f #\F) (read-char*) #f)
	     (else (cond ((eof-object? c) (error 'read-char* "EOF inside a # token"))
			 ((char-numeric? c) (read-part-spec))
			 (else (read-number #\#)))))))
	(else (cond ((eof-object? c) c)
		    ((char-numeric? c) (read-char*) (read-number c))
		    (else (read-identifier)))))))
  (define (read-token)
    (let ((tok (read-optional-token)))
      (if (eof-object? tok)
	  (error 'read-token "EOF where token was required")
	tok)))
  (define parts-alist '())
  (define (add-part-to-alist! n thunk)
    (set! parts-alist (cons (cons n thunk) parts-alist)))
  (define (read-object)
    (finish-reading-object (read-token)))
  (define (read-optional-object)
    (finish-reading-object (read-optional-token)))
  (define (finish-reading-object first-token)
    (if (not (pair? first-token))
	first-token
      (if (char? (car first-token))
	  (case (car first-token)
	    ((#\() (read-list-tail))
	    ((#\#) (list->vector (read-list-tail)))
	    ((#\. #\)) (error 'read-with-shared-structure
			 (string-append "Unexpected \"" first-token "\"")))
	    (else
	     (list (caadr (assv (car first-token)
				'((#\' 'x) (#\, ,x) (#\` `x) (#\@ ,@x))))
		   (read-object))))
	(let ((starting-alist parts-alist))
	  (let read-decls ((token first-token))
	    (if (and (pair? token) (symbol? (car token)))
		(let ((n (cdr token)))
		  (case (car token)
		    ((use)
		     (cond ((assv n starting-alist) => cdr)
			   (else (error 'read-with-shared-structure
				   "Use of undeclared part " n))))
		    ((decl)
		     (if (assv n parts-alist)
			 (error 'read-with-shared-structure
			   "Double declaration of part" n))
		     (letrec ((obj (begin
				     (add-part-to-alist! n (lambda () obj))
				     (read-decls (read-token)))))
		       obj))))
	      (finish-reading-object token)))))))
  (define (read-list-tail)
    (let ((token (read-token)))
      (if (not (pair? token))
	  (cons token (read-list-tail))
	(case (car token)
	  ((#\)) '())
	  ((#\.) (let* ((obj (read-object))
			(tok (read-token)))
		   (if (and (pair? tok) (char=? #\) (car tok)))
		       obj
		     (error 'read-list-tail "Extra junk after a dot" token))))
	  (else (let ((obj (finish-reading-object token)))
		  (cons obj (read-list-tail))))))))
  (define (unthunk thunk)
    (let ((x (thunk)))
      (if (procedure? x) (unthunk x) x)))
  (let ((obj (read-optional-object)))
    (let fill-in-parts ((obj obj))
      (cond ((pair? obj)
	     (if (procedure? (car obj))
		 (set-car! obj (unthunk (car obj)))
	       (fill-in-parts (car obj)))
	     (if (procedure? (cdr obj))
		 (set-cdr! obj (unthunk (cdr obj)))
	       (fill-in-parts (cdr obj))))
	    ((vector? obj)
	     (let ((len (vector-length obj)))
	       (do ((i 0 (+ i 1)))
		   ((= i len))
		 (let ((elt (vector-ref obj i)))
		   (if (procedure? elt)
		       (vector-set! obj i (unthunk elt))
		     (fill-in-parts elt))))))))
    obj))


;;;; unimplemented exception

(define-condition-type &unimplemented &error
  make-unimplemented-condition
  unimplemented-condition?)

(define raise-unimplemented-error
  (case-lambda
   ((who)
    (raise-unimplemented-error who "feature not implemented or not available" #f))
   ((who message)
    (raise-unimplemented-error who message #f))
   ((who message . irritants)
    (raise (let ((c (condition (make-who-condition who)
			       (make-message-condition message)
			       (make-unimplemented-condition))))
	     (if irritants
		 (condition c (make-irritants-condition irritants))
	       c))))))


;;;; simple language extensions

(define-syntax dotimes
  (syntax-rules ()
    ((_ (?varname ?exclusive-count) ?form0 ?form ...)
     (dotimes (?varname ?exclusive-count #f) ?form0 ?form ...))
    ((_ (?varname ?exclusive-count ?result) ?form0 ?form ...)
     (do ((?varname 0 (+ 1 ?varname)))
	 ((>= ?varname ?exclusive-count)
	  ?result)
       ?form0 ?form ...))))

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

;;Define a macro with the function-like form.
;; (define-syntax define-as-syntax
;;   (syntax-rules ()
;;     ((_ (?name ?arg ...) ?form0 ?form ...)
;;      (define-syntax ?name
;;        (syntax-rules ()
;; 	 ((?name ?arg ...)
;; 	  ?form0 ?form ...))))))


;;;; deferred exceptions

(define deferred-exceptions
  (make-parameter #f))

(define deferred-exceptions-handler
  (make-parameter #f))

(define (run-deferred-exceptions-handler)
  (unless (null? (deferred-exceptions))
    (for-each
	(lambda (exc)
	  (guard (exc (else #f))
	    ((deferred-exceptions-handler) exc)))
      (deferred-exceptions))
    (deferred-exceptions '())))

(define-syntax defer-exceptions
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else
		  (let ((e (deferred-exceptions)))
		    (and e (deferred-exceptions (cons exc e))))))
       ?form0 ?form ...))))

(define-syntax with-deferred-exceptions-handler
  (syntax-rules ()
    ((_ ?handler ?thunk)
     (parameterize ((deferred-exceptions '())
		    (deferred-exceptions-handler ?handler))
       (dynamic-wind
	   (lambda () #f)
	   ?thunk
	   (lambda () (run-deferred-exceptions-handler)))))))



;;;; compensations

(define compensations
  (make-parameter #f))

(define (run-compensations)
  (when (compensations)
    (for-each
	(lambda (closure)
	  (defer-exceptions
	    (closure)))
      (compensations))
    (compensations '())))

(define-syntax with-compensations/on-error
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parameterize ((compensations '()))
       (with-exception-handler
	   (lambda (exc)
	     (run-compensations)
	     (raise exc))
	 (lambda ()
	   ?form0 ?form ...))))))

(define-syntax with-compensations
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parameterize ((compensations '()))
       (dynamic-wind
	   (lambda () #f)
	   (lambda () ?form0 ?form ...)
	   (lambda () (run-compensations)))))))

(define-syntax push-compensation
  (syntax-rules ()
    ((_ ?release0 ?release ...)
     (begin
       (compensations (cons (lambda () ?release0 ?release ...)
			    (compensations)))))))

(define-syntax compensate
  (syntax-rules (begin with)
    ((_ (begin ?alloc0 ?alloc ...) (with ?release0 ?release ...))
     (begin0
	 (begin ?alloc0 ?alloc ...)
       (push-compensation ?release0 ?release ...)))

    ((_ (begin ?alloc0 ?alloc ...) ?allocn ?form ...)
     (compensate (begin ?alloc0 ?alloc ... ?allocn) ?form ...))

    ((_ ?alloc ?form ...)
     (compensate (begin ?alloc) ?form ...))))


;;;; miscellaneous definitions

(define (symbol*->string obj)
  (if (symbol? obj)
      (symbol->string obj)
    obj))

(define (symbol->string/maybe thing)
  (cond ((symbol? thing) (symbol->string thing))
	((string? thing) thing)
	(else
	 (assertion-violation 'symbol->string/maybe
	   "expected symbol or string" thing))))


;;;; done

)

;;; end of file
