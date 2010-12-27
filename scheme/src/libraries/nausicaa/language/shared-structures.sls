;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: reading and writing shared structures
;;;Date: Mon Apr  5, 2010
;;;
;;;Abstract
;;;
;;;
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


(library (shared-structures)
  (export
    read-with-shared-structure	write-with-shared-structure
    (rename (read-with-shared-structure		read/ss)
	    (write-with-shared-structure	write/ss)))
  (import (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings))


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


;;;; done

)

;;; end of file
