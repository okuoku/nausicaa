;;; 
;;; Part of: Ice-9 libraries for Ikarus Scheme
;;; Contents: helper functions for optargs
;;; Date: Mon Nov 10, 2008
;;; 
;;; Abstract
;;; 
;;;	This file  holds helper functions for the  expand time of
;;;	the (ice-9 optargs) library.  They  have to be put into a
;;;	separate  file  and loaded  with  the  (for ----  expand)
;;;	import spec.
;;; 
;;; Copyright (C) 1997, 1998, 1999, 2001, 2002, 2004, 2006
;;; Free Software Foundation, Inc.
;;; Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;; Contributed to Guile by Maciej Stachowiak <mstachow@alum.mit.edu>
;;; Assimilated into Nausicaa for Ikarus Scheme by Marco Maggi
;;; 
;;; This program is free software: you can redistribute it and/or
;;; modify it under  the terms of the GNU  General Public License
;;; as published by the  Free Software Foundation, either version
;;; 3 of the License, or (at your option) any later version.
;;; 
;;; This  program is  distributed in  the  hope that  it will  be
;;; useful, but  WITHOUT ANY  WARRANTY; without even  the implied
;;; warranty  of  MERCHANTABILITY  or  FITNESS FOR  A  PARTICULAR
;;; PURPOSE.   See  the  GNU  General  Public  License  for  more
;;; details.
;;; 
;;; You should  have received  a copy of  the GNU  General Public
;;; License   along    with   this   program.     If   not,   see
;;; <http://www.gnu.org/licenses/>.
;;; 

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(library (ice-9 private optargs-expand-helpers)
	 (export let-optional-template let-keywords-template
		 parse-arglist every? ext-decl?
		 define*-guts)
	 (import (ikarus))

;;We cannot import (rnrs) because GENSYM is needed, so we rely on
;;(ikarus).

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Utility procedures for implementing the various let-forms.
;; ------------------------------------------------------------

;;Return a LET or LET* form
(define (let-o-k-template REST-ARG BINDINGS BODY let-type proc)
  (let ((bindings (map (lambda (x)
			 (if (list? x)
			     x
			   (list x #f)))
		    BINDINGS)))
    `(,let-type ,(map proc bindings) ,@BODY)))

(define (let-optional-template REST-ARG BINDINGS BODY let-type)
  (if (null? BINDINGS)
      `(let () ,@BODY)
    (let-o-k-template REST-ARG BINDINGS BODY let-type
		      (lambda (optional)
			`(,(car optional)
			  (cond
			   ((not (null? ,REST-ARG))
			    (let ((result (car ,REST-ARG)))
			      ,(list 'set! REST-ARG
				     `(cdr ,REST-ARG))
			      result))
			   (else
			    ,(cadr optional))))))))

(define (let-keywords-template REST-ARG ALLOW-OTHER-KEYS? BINDINGS BODY let-type)
    (if (null? BINDINGS)
	`(let () ,@BODY)
	(let* ((kb-list-gensym (gensym "kb:G"))
	       (bindfilter (lambda (key)
			     `(,(car key)
			       (cond
				((assq ',(car key) ,kb-list-gensym)
				 => cdr)
				(else
				 ,(cadr key)))))))
	  `(let* ((ra->kbl ,rest-arg->keyword-binding-list)
		  (,kb-list-gensym (ra->kbl ,REST-ARG ',(map
							 (lambda (x) (if (pair? x) (car x) x))
							 BINDINGS)
					    ,ALLOW-OTHER-KEYS?)))
	     ,(let-o-k-template REST-ARG BINDINGS BODY let-type bindfilter)))))


(define (rest-arg->keyword-binding-list rest-arg keywords allow-other-keys?)
  (if (null? rest-arg)
      '()
    (let loop ((first (car rest-arg))
	       (rest (cdr rest-arg))
	       (accum '()))
      (let ((next (lambda (a)
		    (if (null? (cdr rest))
			a
		      (loop (cadr rest) (cddr rest) a)))))
	(if (symbol? first)
	    (cond
	     ((memq first keywords)
	      (if (null? rest)
		  (error "Keyword argument has no value.")
		(next (cons (cons first (car rest)) accum))))
	     ((not allow-other-keys?)
	      (error "Unknown keyword in arguments."))
	     (else (if (null? rest)
		       accum
		     (next accum))))
	  (if (null? rest)
	      accum
	    (loop (car rest) (cdr rest) accum)))))))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Functions to implement the DEFINE and LAMBDA wrappers.
;; ------------------------------------------------------------

(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
	   (every? pred (cdr lst)))))

(define (ext-decl? obj)
  (or (symbol? obj)
      (and (list? obj) (= 2 (length obj)) (symbol? (car obj)))))

;; XXX - not tail recursive
(define (improper-list-copy obj)
  (if (pair? obj)
      (cons (car obj) (improper-list-copy (cdr obj)))
      obj))

(define (parse-arglist arglist cont)
  (define (split-list-at val lst cont)
    (cond
     ((memq val lst)
      => (lambda (pos)
	   (if (memq val (cdr pos))
	       (error (with-output-to-string
			(lambda ()
			  (map display `(,val
					 " specified more than once in argument list.")))))
	       (cont (reverse (cdr (memq val (reverse lst)))) (cdr pos) #t))))
     (else (cont lst '() #f))))
  (define (parse-opt-and-fixed arglist keys aok? rest cont)
    (split-list-at
     'optional arglist
     (lambda (before after split?)
       (if (and split? (null? after))
	   (error "'optional specified but no optional arguments declared.")
	   (cont before after keys aok? rest)))))
  (define (parse-keys arglist rest cont)
    (split-list-at
     'allow-other-keys arglist
     (lambda (aok-before aok-after aok-split?)
       (if (and aok-split? (not (null? aok-after)))
	   (error "'allow-other-keys not at end of keyword argument declarations.")
	   (split-list-at
	    'key aok-before
	    (lambda (key-before key-after key-split?)
	      (cond
	       ((and aok-split? (not key-split?))
		(error "'allow-other-keys specified but no keyword arguments declared."))
	       (key-split?
		(cond
		 ((null? key-after) (error "'key specified but no keyword arguments declared."))
		 ((memq 'optional key-after) (error "'optional arguments declared after 'key arguments."))
		 (else (parse-opt-and-fixed key-before key-after aok-split? rest cont))))
	       (else (parse-opt-and-fixed arglist '() #f rest cont)))))))))
  (define (parse-rest arglist cont)
    (cond
     ((null? arglist) (cont '() '() '() #f #f))
     ((not (pair? arglist)) (cont '() '() '() #f arglist))
     ((not (list? arglist))
	  (let* ((copy (improper-list-copy arglist))
		 (lp (last-pair copy))
		 (ra (cdr lp)))
	    (set-cdr! lp '())
	    (if (memq 'rest copy)
		(error "Cannot specify both 'rest and dotted rest argument.")
		(parse-keys copy ra cont))))
     (else (split-list-at
	    'rest arglist
	    (lambda (before after split?)
	      (if split?
		  (case (length after)
		    ((0) (error "'rest not followed by argument."))
		    ((1) (parse-keys before (car after) cont))
		    (else (error "'rest argument must be declared last.")))
		  (parse-keys before #f cont)))))))

  (parse-rest arglist cont))

;; ------------------------------------------------------------

;; The guts of define* and define*-public.
(define (define*-guts DT ARGLIST BODY)
  (define (nest-lambda*s arglists)
    (if (null? arglists)
        BODY
        `((lambda* ,(car arglists) ,@(nest-lambda*s (cdr arglists))))))
  (define (define*-guts-helper ARGLIST arglists)
    (let ((first (car ARGLIST))
	  (al (cons (cdr ARGLIST) arglists)))
      (if (symbol? first)
	  `(,DT ,first ,@(nest-lambda*s al))
	  (define*-guts-helper first al))))
  (if (symbol? ARGLIST)
      `(,DT ,ARGLIST ,@BODY)
      (define*-guts-helper ARGLIST '())))

;; The guts of defmacro* and defmacro*-public
;; (define (defmacro*-guts DT NAME ARGLIST BODY)
;;   `(,DT ,NAME
;; 	(,(lambda (transformer) (defmacro:transformer transformer))
;; 	 (lambda* ,ARGLIST ,@BODY))))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

) ;; end of library form


;;; end of file
