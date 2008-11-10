;; optargs.scm -- support for optional arguments
;;
;; Copyright (C) 1997, 1998, 1999, 2001, 2002, 2004, 2006 Free Software Foundation, Inc.
;; Copyright (C) 2008 Marco Maggi <marcomaggi@gna.org>
;;
;; Contributed by Maciej Stachowiak <mstachow@alum.mit.edu>
;; Assimilated into Nausicaa for Ikarus Scheme by Marco Maggi
;;
;; This library is free  software; you can redistribute it and/or
;; modify it  under the  terms of the  GNU Lesser  General Public
;; License as  published by the Free  Software Foundation; either
;; version  2.1 of  the License,  or (at  your option)  any later
;; version.
;; 
;; This  library is  distributed  in  the hope  that  it will  be
;; useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;; warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;; PURPOSE.  See  the GNU Lesser General Public  License for more
;; details.
;; 
;; You  should have  received a  copy of  the GNU  Lesser General
;; Public License along  with this library; if not,  write to the
;; Free  Software  Foundation, Inc.,  51  Franklin Street,  Fifth
;; Floor, Boston, MA 02110-1301 USA
;;

;;page
;; Optional Arguments
;;
;; The  C interface  for creating  Guile procedures  has  a very
;; handy  "optional argument" feature.  This module  attempts to
;; provide  similar  functionality  for  procedures  defined  in
;; Scheme with a convenient and attractive syntax.
;;
;; exported macros are:
;;   let-optional
;;   let-optional*
;;   let-keywords
;;   let-keywords*
;;   lambda*
;;   define*
;;   define*-public
;;   defmacro*
;;   defmacro*-public
;;
;;
;; Summary  of   the  lambda*  extended   parameter  list  syntax
;; (brackets are used to indicate grouping only):
;;
;; ext-param-list ::= [identifier]* [#:optional [ext-var-decl]+]?
;;   [#:key [ext-var-decl]+ [#:allow-other-keys]?]?
;;   [[#:rest identifier]|[. identifier]]?
;;
;; ext-var-decl ::= identifier | ( identifier expression )
;;
;; The characters `*', `+' and `?' are not to be taken literally;
;; they mean  respectively, zero or more occurences,  one or more
;; occurences, and one or zero occurences.
;;

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(library (ice-9 optargs)
	 (export let-optional let-optional*
		 let-keywords let-keywords*
		 define* lambda*)
	 (import (ikarus)
		 (ice-9 define-macro)
		 (for (ice-9 private optargs-expand-helpers) expand))

;;We cannot import (rnrs) because GENSYM is needed, so we rely on
;;(ikarus).

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Code.
;; ------------------------------------------------------------

;; let-optional rest-arg (binding ...) . body
;; let-optional* rest-arg (binding ...) . body
;;   macros used to bind optional arguments
;;
;;These two  macros give you an optional  argument interface that
;;is  very "Schemey" and  introduces no  fancy syntax.   They are
;;compatible  with the  scsh macros  of  the same  name, but  are
;;slightly extended.
;;
;;Each of  BINDING may  be of  one of the  forms <var>  or (<var>
;;<default-value>).  REST-ARG should  be the rest-argument of the
;;procedures  these are  used from.   The items  in  REST-ARG are
;;sequentially bound to the  given variable names.  When REST-ARG
;;runs out,  the remaining vars  are bound either to  the default
;;values or to `#f' if  no default value was specified.  REST-ARG
;;remains bound to whatever may have been left of REST-ARG.
;;

(define-macro (let-optional REST-ARG BINDINGS . BODY)
  (let-optional-template REST-ARG BINDINGS BODY 'let))

(defmacro let-optional* (REST-ARG BINDINGS . BODY)
  (let-optional-template REST-ARG BINDINGS BODY 'let*))

;; let-keywords rest-arg allow-other-keys? (binding ...) . body
;; let-keywords* rest-arg allow-other-keys? (binding ...) . body
;;   macros used to bind keyword arguments
;;
;;These macros  pick out keyword arguments from  REST-ARG, but do
;;not modify  it. This is  consistent at least with  Common Lisp,
;;which duplicates keyword args in the rest arg. More explanation
;;of what  keyword arguments  in a LAMBDA  list look like  can be
;;found  below in  the documentation  for LAMBDA*.
;;
;;Bindings  can  have  the  same  form as  for  LET-OPTIONAL.  If
;;ALLOW-OTHER-KEYS? is false, an error will be thrown if anything
;;that looks like  a keyword argument but does  not match a known
;;keyword parameter will result in an error.
;;

(defmacro let-keywords (REST-ARG ALLOW-OTHER-KEYS? BINDINGS . BODY)
  (let-keywords-template REST-ARG ALLOW-OTHER-KEYS? BINDINGS BODY 'let))

(defmacro let-keywords* (REST-ARG ALLOW-OTHER-KEYS? BINDINGS . BODY)
  (let-keywords-template REST-ARG ALLOW-OTHER-KEYS? BINDINGS BODY 'let*))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; High level macros.
;; ------------------------------------------------------------

;; lambda* args . body
;;   lambda extended for optional and keyword arguments
;;
;; lambda* creates a procedure that takes optional arguments. These
;; are specified by putting them inside brackets at the end of the
;; paramater list, but before any dotted rest argument. For example,
;;   (lambda* (a b #:optional c d . e) '())
;; creates a procedure with fixed arguments a and b, optional arguments c
;; and d, and rest argument e. If the optional arguments are omitted
;; in a call, the variables for them are bound to `#f'.
;;
;; lambda* can also take keyword arguments. For example, a procedure
;; defined like this:
;;   (lambda* (#:key xyzzy larch) '())
;; can be called with any of the argument lists (#:xyzzy 11)
;; (#:larch 13) (#:larch 42 #:xyzzy 19) (). Whichever arguments
;; are given as keywords are bound to values.
;;
;; Optional and keyword arguments can also be given default values
;; which they take on when they are not present in a call, by giving a
;; two-item list in place of an optional argument, for example in:
;;   (lambda* (foo #:optional (bar 42) #:key (baz 73)) (list foo bar baz))
;; foo is a fixed argument, bar is an optional argument with default
;; value 42, and baz is a keyword argument with default value 73.
;; Default value expressions are not evaluated unless they are needed
;; and until the procedure is called.
;;
;; lambda* now supports two more special parameter list keywords.
;;
;; lambda*-defined procedures now throw an error by default if a
;; keyword other than one of those specified is found in the actual
;; passed arguments. However, specifying #:allow-other-keys
;; immediately after the keyword argument declarations restores the
;; previous behavior of ignoring unknown keywords. lambda* also now
;; guarantees that if the same keyword is passed more than once, the
;; last one passed is the one that takes effect. For example,
;;   ((lambda* (#:key (heads 0) (tails 0)) (display (list heads tails)))
;;    #:heads 37 #:tails 42 #:heads 99)
;; would result in (99 47) being displayed.
;;
;; #:rest is also now provided as a synonym for the dotted syntax rest
;; argument. The argument lists (a . b) and (a #:rest b) are equivalent in
;; all respects to lambda*. This is provided for more similarity to DSSSL,
;; MIT-Scheme and Kawa among others, as well as for refugees from other
;; Lisp dialects.


(defmacro lambda* (ARGLIST . BODY)
  (parse-arglist
   ARGLIST
   (lambda (non-optional-args optionals keys aok? rest-arg)
     ;; Check for syntax errors.
     (if (not (every? symbol? non-optional-args))
	 (error "Syntax error in fixed argument declaration."))
     (if (not (every? ext-decl? optionals))
	 (error "Syntax error in optional argument declaration."))
     (if (not (every? ext-decl? keys))
	 (error "Syntax error in keyword argument declaration."))
     (if (not (or (symbol? rest-arg) (eq? #f rest-arg)))
	 (error "Syntax error in rest argument declaration."))
     ;; generate the code.
     (let ((rest-gensym (or rest-arg (gensym "lambda*:G")))
	   (lambda-gensym (gensym "lambda*:L")))
       (if (not (and (null? optionals) (null? keys)))
	   `(let ((,lambda-gensym
		   (lambda (,@non-optional-args . ,rest-gensym)
		     ;; Make sure that if the proc had a docstring, we put it
		     ;; here where it will be visible.
		     ,@(if (and (not (null? BODY))
				(string? (car BODY)))
			   (list (car BODY))
			   '())
		     (let-optional*
		      ,rest-gensym
		      ,optionals
		      (let-keywords* ,rest-gensym
				     ,aok?
				     ,keys
				     ,@(if (and (not rest-arg) (null? keys))
					   `((if (not (null? ,rest-gensym))
						 (error "Too many arguments.")))
					   '())
				     (let ()
				       ,@BODY))))))
	      (set-procedure-property! ,lambda-gensym 'arglist
				       '(,non-optional-args
					 ,optionals
					 ,keys
					 ,aok?
					 ,rest-arg))
	      ,lambda-gensym)
	   `(lambda (,@non-optional-args . ,(if rest-arg rest-arg '()))
	      ,@BODY))))))

;; ------------------------------------------------------------

;; define* args . body
;; define*-public args . body
;;   define and define-public extended for optional and keyword arguments
;;
;; define* and define*-public support optional arguments with
;; a similar syntax to lambda*. They also support arbitrary-depth
;; currying, just like Guile's define. Some examples:
;;   (define* (x y #:optional a (z 3) #:key w . u) (display (list y z u)))
;; defines a procedure x with a fixed argument y, an optional agument
;; a, another optional argument z with default value 3, a keyword argument w,
;; and a rest argument u.
;;   (define-public* ((foo #:optional bar) #:optional baz) '())
;; This illustrates currying. A procedure foo is defined, which,
;; when called with an optional argument bar, returns a procedure that
;; takes an optional argument baz.
;;
;; Of course, define*[-public] also supports #:rest and #:allow-other-keys
;; in the same way as lambda*.

(defmacro define* (ARGLIST . BODY)
  (define*-guts 'define ARGLIST BODY))

;; (defmacro define*-public (ARGLIST . BODY)
;;   (define*-guts 'define-public ARGLIST BODY))

;; ------------------------------------------------------------

;; defmacro* name args . body
;; defmacro*-public args . body
;;   defmacro and defmacro-public extended for optional and keyword arguments
;;
;; These are just like defmacro and defmacro-public except that they
;; take lambda*-style extended paramter lists, where #:optional,
;; #:key, #:allow-other-keys and #:rest are allowed with the usual
;; semantics. Here is an example of a macro with an optional argument:
;;   (defmacro* transmorgify (a #:optional b)

;; (defmacro defmacro* (NAME ARGLIST . BODY)
;;   (defmacro*-guts 'define NAME ARGLIST BODY))

;; (defmacro defmacro*-public (NAME ARGLIST . BODY)
;;   (defmacro*-guts 'define-public NAME ARGLIST BODY))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

) ;; end of library form


;;; optargs.scm ends here
