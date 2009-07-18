;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex 1.0 - Scheme Implementation of Lex
;;;Date: Fri Jul 17, 2009
;;;
;;;Abstract
;;;
;;;	SILex stands for "Scheme Implementation of Lex".  It generates a
;;;	Scheme  lexical analyser  from a  Lex--like  specification file.
;;;	Notice  that  the  original  distribution  of  SILex  should  be
;;;	included in  the Nausicaa  source tree, under  the "src/foreign"
;;;	directory.
;;;
;;;	  Quick guide to browsing the code:
;;;
;;;	LEX - This is the main function to produce a full lexer.
;;;
;;;	OUTPUT - This  function is the main output  function.  It parses
;;;	the  options given  to the  LEX and  LEX-TABLES  procedures, and
;;;	produces  one  among three  kinds  of  output:  a proper  Scheme
;;;	library, a
;;;
;;;	OUT-PRINT-TABLE  - This  function prints  the lexer  table  to a
;;;	given output port; it can be configured to read lexer input from
;;;	a file,  port or string.   This function is  the one to  read to
;;;	understand the basic format of lexer tables.
;;;
;;;Copyright (c) 2001 Danny Dube' <dube@iro.umontreal.ca>
;;;
;;;Original code  by Danny Dube'.   Port to R6RS Scheme  and integration
;;;into Nausicaa by Marco Maggi.
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


(library (silex)
  (export
    lex
    :input-file		:input-port	:input-string
    :output-file	:output-port
    :table-name		:library-spec
    :counters		:pretty-print	:lexer-format)
  (import (rnrs)
    (parameters)
    (keywords)
    (silex multilex)
    (rnrs mutable-pairs)
    (rnrs mutable-strings))


;;;; helpers

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


;;;; module util.scm.

;;Quelques definitions de constantes

(define eof-tok              0)
(define hblank-tok           1)
(define vblank-tok           2)
(define pipe-tok             3)
(define question-tok         4)
(define plus-tok             5)
(define star-tok             6)
(define lpar-tok             7)
(define rpar-tok             8)
(define dot-tok              9)
(define lbrack-tok          10)
(define lbrack-rbrack-tok   11)
(define lbrack-caret-tok    12)
(define lbrack-minus-tok    13)
(define subst-tok           14)
(define power-tok           15)
(define doublequote-tok     16)
(define char-tok            17)
(define caret-tok           18)
(define dollar-tok          19)
(define <<EOF>>-tok         20)
(define <<ERROR>>-tok       21)
(define percent-percent-tok 22)
(define id-tok              23)
(define rbrack-tok          24)
(define minus-tok           25)
(define illegal-tok         26)
; Tokens agreges
(define class-tok           27)
(define string-tok          28)

(define number-of-tokens 29)

(define newline-ch   (char->integer #\newline))
(define tab-ch       (char->integer #\	))
(define dollar-ch    (char->integer #\$))
(define minus-ch     (char->integer #\-))
(define rbrack-ch    (char->integer #\]))
(define caret-ch     (char->integer #\^))

(define dot-class (list (cons 'inf- (- newline-ch 1))
			(cons (+ newline-ch 1) 'inf+)))

(define default-action
  (string-append "        (yycontinue)" (string #\newline)))
(define default-<<EOF>>-action
  (string-append "       '(0)" (string #\newline)))
(define default-<<ERROR>>-action
  (string-append "       (begin"
		 (string #\newline)
		 "         (display \"Error: Invalid token.\")"
		 (string #\newline)
		 "         (newline)"
		 (string #\newline)
		 "         'error)"
		 (string #\newline)))

;;; Fabrication de tables de dispatch

(define (make-dispatch-table size alist default)
  (let ((v (make-vector size default)))
    (let loop ((alist alist))
      (if (null? alist)
	  v
	(begin
	  (vector-set! v (caar alist) (cdar alist))
	  (loop (cdr alist)))))))

;;; Fonctions de manipulation des tokens

(define (make-tok tok-type lexeme line column . attr)
  (cond ((null? attr)
	 (vector tok-type line column lexeme))
	((null? (cdr attr))
	 (vector tok-type line column lexeme (car attr)))
	(else
	 (vector tok-type line column lexeme (car attr) (cadr attr)))))

(define get-tok-type     (lambda (tok) (vector-ref tok 0)))
(define get-tok-line     (lambda (tok) (vector-ref tok 1)))
(define get-tok-column   (lambda (tok) (vector-ref tok 2)))
(define get-tok-lexeme   (lambda (tok) (vector-ref tok 3)))
(define get-tok-attr     (lambda (tok) (vector-ref tok 4)))
(define get-tok-2nd-attr (lambda (tok) (vector-ref tok 5)))

;;; Fonctions de manipulations des regles

(define (make-rule line eof? error? bol? eol? regexp action)
  (vector line eof? error? bol? eol? regexp action #f))

(define get-rule-line    (lambda (rule) (vector-ref rule 0)))
(define get-rule-eof?    (lambda (rule) (vector-ref rule 1)))
(define get-rule-error?  (lambda (rule) (vector-ref rule 2)))
(define get-rule-bol?    (lambda (rule) (vector-ref rule 3)))
(define get-rule-eol?    (lambda (rule) (vector-ref rule 4)))
(define get-rule-regexp  (lambda (rule) (vector-ref rule 5)))
(define get-rule-action  (lambda (rule) (vector-ref rule 6)))
(define get-rule-yytext? (lambda (rule) (vector-ref rule 7)))

(define set-rule-regexp  (lambda (rule regexp)  (vector-set! rule 5 regexp)))
(define set-rule-action  (lambda (rule action)  (vector-set! rule 6 action)))
(define set-rule-yytext? (lambda (rule yytext?) (vector-set! rule 7 yytext?)))

;;; Noeuds des regexp

(define epsilon-re  0)
(define or-re       1)
(define conc-re     2)
(define star-re     3)
(define plus-re     4)
(define question-re 5)
(define class-re    6)
(define char-re     7)

(define (make-re re-type . lattr)
  (cond ((null? lattr)
	 (vector re-type))
	((null? (cdr lattr))
	 (vector re-type (car lattr)))
	((null? (cddr lattr))
	 (vector re-type (car lattr) (cadr lattr)))))

(define get-re-type  (lambda (re) (vector-ref re 0)))
(define get-re-attr1 (lambda (re) (vector-ref re 1)))
(define get-re-attr2 (lambda (re) (vector-ref re 2)))

;;; Fonctions de manipulation des ensembles d'etats

; Intersection de deux ensembles d'etats
(define (ss-inter ss1 ss2)
  (cond ((null? ss1)
	 '())
	((null? ss2)
	 '())
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (ss-inter (cdr ss1) ss2))
		 ((= t1 t2)
		  (cons t1 (ss-inter (cdr ss1) (cdr ss2))))
		 (else
		  (ss-inter ss1 (cdr ss2))))))))

; Difference entre deux ensembles d'etats
(define (ss-diff ss1 ss2)
  (cond ((null? ss1)
	 '())
	((null? ss2)
	 ss1)
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (cons t1 (ss-diff (cdr ss1) ss2)))
		 ((= t1 t2)
		  (ss-diff (cdr ss1) (cdr ss2)))
		 (else
		  (ss-diff ss1 (cdr ss2))))))))

; Union de deux ensembles d'etats
(define (ss-union ss1 ss2)
  (cond ((null? ss1)
	 ss2)
	((null? ss2)
	 ss1)
	(else
	 (let ((t1 (car ss1))
	       (t2 (car ss2)))
	   (cond ((< t1 t2)
		  (cons t1 (ss-union (cdr ss1) ss2)))
		 ((= t1 t2)
		  (cons t1 (ss-union (cdr ss1) (cdr ss2))))
		 (else
		  (cons t2 (ss-union ss1 (cdr ss2)))))))))

; Decoupage de deux ensembles d'etats
(define (ss-sep ss1 ss2)
  (let loop ((ss1 ss1) (ss2 ss2) (l '()) (c '()) (r '()))
    (if (null? ss1)
	(if (null? ss2)
	    (vector (reverse l) (reverse c) (reverse r))
	  (loop ss1 (cdr ss2) l c (cons (car ss2) r)))
      (if (null? ss2)
	  (loop (cdr ss1) ss2 (cons (car ss1) l) c r)
	(let ((t1 (car ss1))
	      (t2 (car ss2)))
	  (cond ((< t1 t2)
		 (loop (cdr ss1) ss2 (cons t1 l) c r))
		((= t1 t2)
		 (loop (cdr ss1) (cdr ss2) l (cons t1 c) r))
		(else
		 (loop ss1 (cdr ss2) l c (cons t2 r)))))))))

;;; Fonctions de manipulation des classes de caracteres

;; Comparaisons de bornes d'intervalles
(define class-= eqv?)

(define class-<=
  (lambda (b1 b2)
    (cond ((eq? b1 'inf-) #t)
	  ((eq? b2 'inf+) #t)
	  ((eq? b1 'inf+) #f)
	  ((eq? b2 'inf-) #f)
	  (else (<= b1 b2)))))

(define class->=
  (lambda (b1 b2)
    (cond ((eq? b1 'inf+) #t)
	  ((eq? b2 'inf-) #t)
	  ((eq? b1 'inf-) #f)
	  ((eq? b2 'inf+) #f)
	  (else (>= b1 b2)))))

(define class-<
  (lambda (b1 b2)
    (cond ((eq? b1 'inf+) #f)
	  ((eq? b2 'inf-) #f)
	  ((eq? b1 'inf-) #t)
	  ((eq? b2 'inf+) #t)
	  (else (< b1 b2)))))

(define class->
  (lambda (b1 b2)
    (cond ((eq? b1 'inf-) #f)
	  ((eq? b2 'inf+) #f)
	  ((eq? b1 'inf+) #t)
	  ((eq? b2 'inf-) #t)
	  (else (> b1 b2)))))

; Complementation d'une classe
(define class-compl
  (lambda (c)
    (let loop ((c c) (start 'inf-))
      (if (null? c)
	  (list (cons start 'inf+))
	  (let* ((r (car c))
		 (rstart (car r))
		 (rend (cdr r)))
	    (if (class-< start rstart)
		(cons (cons start (- rstart 1))
		      (loop c rstart))
		(if (class-< rend 'inf+)
		    (loop (cdr c) (+ rend 1))
		    '())))))))

; Union de deux classes de caracteres
(define class-union
  (lambda (c1 c2)
    (let loop ((c1 c1) (c2 c2) (u '()))
      (if (null? c1)
	  (if (null? c2)
	      (reverse u)
	      (loop c1 (cdr c2) (cons (car c2) u)))
	  (if (null? c2)
	      (loop (cdr c1) c2 (cons (car c1) u))
	      (let* ((r1 (car c1))
		     (r2 (car c2))
		     (r1start (car r1))
		     (r1end (cdr r1))
		     (r2start (car r2))
		     (r2end (cdr r2)))
		(if (class-<= r1start r2start)
		    (cond ((class-= r1end 'inf+)
			   (loop c1 (cdr c2) u))
			  ((class-< (+ r1end 1) r2start)
			   (loop (cdr c1) c2 (cons r1 u)))
			  ((class-<= r1end r2end)
			   (loop (cdr c1)
				 (cons (cons r1start r2end) (cdr c2))
				 u))
			  (else
			   (loop c1 (cdr c2) u)))
		    (cond ((class-= r2end 'inf+)
			   (loop (cdr c1) c2 u))
			  ((class-> r1start (+ r2end 1))
			   (loop c1 (cdr c2) (cons r2 u)))
			  ((class->= r1end r2end)
			   (loop (cons (cons r2start r1end) (cdr c1))
				 (cdr c2)
				 u))
			  (else
			   (loop (cdr c1) c2 u))))))))))

; Decoupage de deux classes de caracteres
(define class-sep
  (lambda (c1 c2)
    (let loop ((c1 c1) (c2 c2) (l '()) (c '()) (r '()))
      (if (null? c1)
	  (if (null? c2)
	      (vector (reverse l) (reverse c) (reverse r))
	      (loop c1 (cdr c2) l c (cons (car c2) r)))
	  (if (null? c2)
	      (loop (cdr c1) c2 (cons (car c1) l) c r)
	      (let* ((r1 (car c1))
		     (r2 (car c2))
		     (r1start (car r1))
		     (r1end (cdr r1))
		     (r2start (car r2))
		     (r2end (cdr r2)))
		(cond ((class-< r1start r2start)
		       (if (class-< r1end r2start)
			   (loop (cdr c1) c2 (cons r1 l) c r)
			   (loop (cons (cons r2start r1end) (cdr c1)) c2
				 (cons (cons r1start (- r2start 1)) l) c r)))
		      ((class-> r1start r2start)
		       (if (class-> r1start r2end)
			   (loop c1 (cdr c2) l c (cons r2 r))
			   (loop c1 (cons (cons r1start r2end) (cdr c2))
				 l c (cons (cons r2start (- r1start 1)) r))))
		      (else
		       (cond ((class-< r1end r2end)
			      (loop (cdr c1)
				    (cons (cons (+ r1end 1) r2end) (cdr c2))
				    l (cons r1 c) r))
			     ((class-= r1end r2end)
			      (loop (cdr c1) (cdr c2) l (cons r1 c) r))
			     (else
			      (loop (cons (cons (+ r2end 1) r1end) (cdr c1))
				    (cdr c2)
				    l (cons r2 c) r)))))))))))

; Transformer une classe (finie) de caracteres en une liste de ...
(define class->char-list
  (lambda (c)
    (let loop1 ((c c))
      (if (null? c)
	  '()
	  (let* ((r (car c))
		 (rend (cdr r))
		 (tail (loop1 (cdr c))))
	    (let loop2 ((rstart (car r)))
	      (if (<= rstart rend)
		  (cons (integer->char rstart) (loop2 (+ rstart 1)))
		  tail)))))))

; Transformer une classe de caracteres en une liste poss. compl.
; 1er element = #t -> classe complementee
(define class->tagged-char-list
  (lambda (c)
    (let* ((finite? (or (null? c) (number? (caar c))))
	   (c2 (if finite? c (class-compl c)))
	   (c-l (class->char-list c2)))
      (cons (not finite?) c-l))))

;;; Fonction digraph

; Fonction "digraph".
; Etant donne un graphe dirige dont les noeuds comportent une valeur,
; calcule pour chaque noeud la "somme" des valeurs contenues dans le
; noeud lui-meme et ceux atteignables a partir de celui-ci.  La "somme"
; consiste a appliquer un operateur commutatif et associatif aux valeurs
; lorsqu'elles sont additionnees.
; L'entree consiste en un vecteur de voisinages externes, un autre de
; valeurs initiales et d'un operateur.
; La sortie est un vecteur de valeurs finales.
(define digraph
  (lambda (arcs init op)
    (let* ((nbnodes (vector-length arcs))
	   (infinity nbnodes)
	   (prio (make-vector nbnodes -1))
	   (stack (make-vector nbnodes #f))
	   (sp 0)
	   (final (make-vector nbnodes #f)))
      (letrec ((store-final
		(lambda (self-sp value)
		  (let loop ()
		    (if (> sp self-sp)
			(let ((voisin (vector-ref stack (- sp 1))))
			  (vector-set! prio voisin infinity)
			  (set! sp (- sp 1))
			  (vector-set! final voisin value)
			  (loop))))))
	       (visit-node
		(lambda (n)
		  (let ((self-sp sp))
		    (vector-set! prio n self-sp)
		    (vector-set! stack sp n)
		    (set! sp (+ sp 1))
		    (vector-set! final n (vector-ref init n))
		    (let loop ((vois (vector-ref arcs n)))
		      (if (pair? vois)
			  (let* ((v (car vois))
				 (vprio (vector-ref prio v)))
			    (if (= vprio -1)
				(visit-node v))
			    (vector-set! prio n (min (vector-ref prio n)
						     (vector-ref prio v)))
			    (vector-set! final n (op (vector-ref final n)
						     (vector-ref final v)))
			    (loop (cdr vois)))))
		    (if (= (vector-ref prio n) self-sp)
			(store-final self-sp (vector-ref final n)))))))
	(let loop ((n 0))
	  (if (< n nbnodes)
	      (begin
		(if (= (vector-ref prio n) -1)
		    (visit-node n))
		(loop (+ n 1)))))
	final))))

;;; Fonction de tri

(define merge-sort-merge
  (lambda (l1 l2 cmp-<=)
    (cond ((null? l1)
	   l2)
	  ((null? l2)
	   l1)
	  (else
	   (let ((h1 (car l1))
		 (h2 (car l2)))
	     (if (cmp-<= h1 h2)
		 (cons h1 (merge-sort-merge (cdr l1) l2 cmp-<=))
		 (cons h2 (merge-sort-merge l1 (cdr l2) cmp-<=))))))))

(define merge-sort
  (lambda (l cmp-<=)
    (if (null? l)
	l
	(let loop1 ((ll (map list l)))
	  (if (null? (cdr ll))
	      (car ll)
	      (loop1
	       (let loop2 ((ll ll))
		 (cond ((null? ll)
			ll)
		       ((null? (cdr ll))
			ll)
		       (else
			(cons (merge-sort-merge (car ll) (cadr ll) cmp-<=)
			      (loop2 (cddr ll))))))))))))


;;; Module action.l.scm.

(define action-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
          (make-tok eof-tok    yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          (make-tok hblank-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          (make-tok vblank-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          (make-tok char-tok   yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 4)
       ((#f #\;) . 3)
       ((#f #\newline) . 2)
       ((#t #\	 #\newline #\space #\;) . 1))
      (((#t #\newline) . 1))
      ()
      (((#t #\newline) . 3))
      (((#f #\	 #\space) . 4)
       ((#f #\;) . 3)
       ((#t #\	 #\newline #\space #\;) . 1)))
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))


;;; Module class.l.scm.

(define class-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
              (make-tok eof-tok    yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (make-tok rbrack-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (make-tok minus-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-spec-char     yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-quoted-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-ordinary-char yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\]) . 4) ((#f #\-) . 3) ((#f #\\) . 2) ((#t #\- #\\ #\]) . 1))
      ()
      (((#f #\n) . 8)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 7)
       ((#f #\-) . 6)
       ((#t #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\n) . 5))
      ()
      ()
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 10))
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 10)))
   '#((#f . #f) (6 . 6)   (6 . 6)   (1 . 1)   (0 . 0)   (5 . 5)   (5 . 5)
      (3 . 3)   (2 . 2)   (4 . 4)   (3 . 3))))


;;; Module macro.l.scm.

(define macro-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         (make-tok eof-tok             yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok hblank-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok vblank-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok percent-percent-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (parse-id                     yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok illegal-tok         yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 8)
       ((#f #\;) . 7)
       ((#f #\newline) . 6)
       ((#f #\%) . 5)
       ((#f  #\! #\$ #\& #\* #\/ #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E
         #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U
         #\V #\W #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i
         #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
         #\z #\~)
        .
        4)
       ((#f #\+ #\-) . 3)
       ((#f #\.) . 2)
       ((#t        #\	       #\newline #\space   #\!       #\$
         #\%       #\&       #\*       #\+       #\-       #\.
         #\/       #\:       #\;       #\<       #\=       #\>
         #\?       #\A       #\B       #\C       #\D       #\E
         #\F       #\G       #\H       #\I       #\J       #\K
         #\L       #\M       #\N       #\O       #\P       #\Q
         #\R       #\S       #\T       #\U       #\V       #\W
         #\X       #\Y       #\Z       #\^       #\_       #\a
         #\b       #\c       #\d       #\e       #\f       #\g
         #\h       #\i       #\j       #\k       #\l       #\m
         #\n       #\o       #\p       #\q       #\r       #\s
         #\t       #\u       #\v       #\w       #\x       #\y
         #\z       #\~)
        .
        1))
      ()
      (((#f #\.) . 9))
      ()
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#f #\%) . 11)
       ((#f  #\! #\$ #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6
         #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G #\H
         #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
         #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
         #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      ()
      (((#t #\newline) . 12))
      ()
      (((#f #\.) . 13))
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#t #\newline) . 12))
      ())
   '#((#f . #f) (4 . 4)   (4 . 4)   (3 . 3)   (3 . 3)   (3 . 3)   (1 . 1)
      (0 . 0)   (0 . 0)   (#f . #f) (3 . 3)   (2 . 2)   (0 . 0)   (3 . 3))))


;;; Module regexp.l.scm.

(define regexp-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok eof-tok           yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok hblank-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok vblank-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok pipe-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok question-tok      yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok plus-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok star-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lpar-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok rpar-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok dot-tok           yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-rbrack-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-caret-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-minus-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-id-ref               yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m              yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m-inf          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m-n            yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok illegal-tok       yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok doublequote-tok   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-spec-char            yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-digits-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-digits-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-quoted-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok caret-tok         yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok dollar-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-ordinary-char        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok <<EOF>>-tok       yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok <<ERROR>>-tok     yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 18)
       ((#f #\;) . 17)
       ((#f #\newline) . 16)
       ((#f #\|) . 15)
       ((#f #\?) . 14)
       ((#f #\+) . 13)
       ((#f #\*) . 12)
       ((#f #\() . 11)
       ((#f #\)) . 10)
       ((#f #\.) . 9)
       ((#f #\[) . 8)
       ((#f #\{) . 7)
       ((#f #\") . 6)
       ((#f #\\) . 5)
       ((#f #\^) . 4)
       ((#f #\$) . 3)
       ((#t        #\	       #\newline #\space   #\"       #\$
         #\(       #\)       #\*       #\+       #\.       #\;
         #\<       #\?       #\[       #\\       #\^       #\{
         #\|)
        .
        2)
       ((#f #\<) . 1))
      (((#f #\<) . 19))
      ()
      ()
      ()
      (((#f #\n) . 23)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 22)
       ((#f #\-) . 21)
       ((#t #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\n) . 20))
      ()
      (((#f  #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\A #\B #\C #\D
         #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
         #\U #\V #\W #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h
         #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
         #\y #\z #\~)
        .
        27)
       ((#f #\+ #\-) . 26)
       ((#f #\.) . 25)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 24))
      (((#f #\]) . 30) ((#f #\^) . 29) ((#f #\-) . 28))
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      (((#t #\newline) . 31))
      ()
      (((#f #\E) . 32))
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 33))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 34))
      ()
      (((#f #\}) . 36)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 24)
       ((#f #\,) . 35))
      (((#f #\.) . 37))
      (((#f #\}) . 38))
      (((#f #\}) . 38)
       ((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        27))
      ()
      ()
      ()
      (((#t #\newline) . 31))
      (((#f #\O) . 40) ((#f #\R) . 39))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 33))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 34))
      (((#f #\}) . 42) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 41))
      ()
      (((#f #\.) . 26))
      ()
      (((#f #\R) . 43))
      (((#f #\F) . 44))
      (((#f #\}) . 45) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 41))
      ()
      (((#f #\O) . 46))
      (((#f #\>) . 47))
      ()
      (((#f #\R) . 48))
      (((#f #\>) . 49))
      (((#f #\>) . 50))
      ()
      (((#f #\>) . 51))
      ())
   '#((#f . #f) (25 . 25) (25 . 25) (24 . 24) (23 . 23) (25 . 25) (18 . 18)
      (17 . 17) (9 . 9)   (8 . 8)   (7 . 7)   (6 . 6)   (5 . 5)   (4 . 4)
      (3 . 3)   (2 . 2)   (1 . 1)   (0 . 0)   (0 . 0)   (#f . #f) (22 . 22)
      (22 . 22) (20 . 20) (19 . 19) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
      (12 . 12) (11 . 11) (10 . 10) (0 . 0)   (#f . #f) (21 . 21) (20 . 20)
      (#f . #f) (14 . 14) (#f . #f) (13 . 13) (#f . #f) (#f . #f) (#f . #f)
      (15 . 15) (#f . #f) (#f . #f) (16 . 16) (#f . #f) (#f . #f) (#f . #f)
      (26 . 26) (#f . #f) (27 . 27))))


;;;; Module string.l.scm.

(define string-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
              (make-tok eof-tok         yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (make-tok doublequote-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-spec-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-quoted-char        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-ordinary-char      yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\") . 3) ((#f #\\) . 2) ((#t #\" #\\) . 1))
      ()
      (((#f #\n) . 7)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 6)
       ((#f #\-) . 5)
       ((#t #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\n) . 4))
      ()
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 8))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9))
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 8))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9)))
   '#((#f . #f) (5 . 5)   (5 . 5)   (0 . 0)   (4 . 4)   (4 . 4)   (2 . 2)
      (1 . 1)   (3 . 3)   (2 . 2))))


;;; Module lexparser.scm.

;
; Fonctions auxilliaires du lexer
;

(define parse-spec-char
  (lambda (lexeme line column)
    (make-tok char-tok lexeme line column newline-ch)))

(define parse-digits-char
  (lambda (lexeme line column)
    (let* ((num (substring lexeme 1 (string-length lexeme)))
	   (n (string->number num)))
      (make-tok char-tok lexeme line column n))))

(define parse-quoted-char
  (lambda (lexeme line column)
    (let ((c (string-ref lexeme 1)))
      (make-tok char-tok lexeme line column (char->integer c)))))

(define parse-ordinary-char
  (lambda (lexeme line column)
    (let ((c (string-ref lexeme 0)))
      (make-tok char-tok lexeme line column (char->integer c)))))

;;Already defined in R6RS.
;;
;; (define string-downcase
;;   (lambda (s)
;;     (let* ((l (string->list s))
;; 	   (ld (map char-downcase l)))
;;       (list->string ld))))

(define extract-id
  (lambda (s)
    (let ((len (string-length s)))
      (substring s 1 (- len 1)))))

(define parse-id
  (lambda (lexeme line column)
    (make-tok id-tok lexeme line column (string-downcase lexeme) lexeme)))

(define parse-id-ref
  (lambda (lexeme line column)
    (let* ((orig-name (extract-id lexeme))
	   (name (string-downcase orig-name)))
    (make-tok subst-tok lexeme line column name orig-name))))

(define parse-power-m
  (lambda (lexeme line column)
    (let* ((len (string-length lexeme))
	   (substr (substring lexeme 1 (- len 1)))
	   (m (string->number substr))
	   (range (cons m m)))
      (make-tok power-tok lexeme line column range))))

(define parse-power-m-inf
  (lambda (lexeme line column)
    (let* ((len (string-length lexeme))
	   (substr (substring lexeme 1 (- len 2)))
	   (m (string->number substr))
	   (range (cons m 'inf)))
      (make-tok power-tok lexeme line column range))))

(define parse-power-m-n
  (lambda (lexeme line column)
    (let ((len (string-length lexeme)))
      (let loop ((comma 2))
	(if (char=? (string-ref lexeme comma) #\,)
	    (let* ((sub1 (substring lexeme 1 comma))
		   (sub2 (substring lexeme (+ comma 1) (- len 1)))
		   (m (string->number sub1))
		   (n (string->number sub2))
		   (range (cons m n)))
	      (make-tok power-tok lexeme line column range))
	    (loop (+ comma 1)))))))

;
; Lexer generique
;

(define lexer-raw
  (make-parameter #f))

(define lexer-stack
  (make-parameter '()))

(define lexer-alist
  (make-parameter #f))

(define lexer-buffer
  (make-parameter #f))

(define lexer-buffer-empty?
  (make-parameter #t))

(define lexer-history
  (make-parameter '()))

(define lexer-history-interp
  (make-parameter #f))

; Lexer brut
; S'assurer qu'il n'y a pas de risque de changer de
; lexer quand le buffer est rempli
(define push-lexer
  (lambda (name)
    (lexer-stack (cons (lexer-raw) (lexer-stack)))
    (lexer-raw (cdr (assq name (lexer-alist))))))

(define pop-lexer
  (lambda ()
    (lexer-raw   (car (lexer-stack)))
    (lexer-stack (cdr (lexer-stack)))))

; Traite le "unget" (capacite du unget: 1)
(define lexer2
  (lambda ()
    (if (lexer-buffer-empty?)
	((lexer-raw))
      (begin
	(lexer-buffer-empty? #t)
	(lexer-buffer)))))

(define lexer2-unget
  (lambda (tok)
    (lexer-buffer tok)
    (lexer-buffer-empty? #f)))

; Traite l'historique
(define (lexer)
  (let* ((tok (lexer2))
	 (tok-lexeme (get-tok-lexeme tok))
	 (hist-lexeme (if (lexer-history-interp)
			  (blank-translate tok-lexeme)
			tok-lexeme)))
    (lexer-history (cons hist-lexeme (lexer-history)))
    tok))

(define (lexer-unget tok)
  (lexer-history (cdr (lexer-history)))
  (lexer2-unget tok))

(define (lexer-set-blank-history b)
  (lexer-history-interp b))

(define (blank-translate s)
  (let ((ss (string-copy s)))
    (let loop ((i (- (string-length ss) 1)))
      (cond ((< i 0)
	     ss)
	    ((char=? (string-ref ss i) (integer->char tab-ch))
	     (loop (- i 1)))
	    ((char=? (string-ref ss i) #\newline)
	     (loop (- i 1)))
	    (else
	     (string-set! ss i #\space)
	     (loop (- i 1)))))))

(define (lexer-get-history)
  (let* ((rightlist (reverse (lexer-history)))
	 (str (apply string-append rightlist))
	 (strlen (string-length str))
	 (str2 (if (and (> strlen 0)
			(char=? (string-ref str (- strlen 1)) #\newline))
		   str
		 (string-append str (string #\newline)))))
    (lexer-history '())
    str2))

;
; Traitement des listes de tokens
;

(define de-anchor-tokens
  (let ((not-anchor-toks (make-dispatch-table number-of-tokens
					      (list (cons caret-tok     #f)
						    (cons dollar-tok    #f)
						    (cons <<EOF>>-tok   #f)
						    (cons <<ERROR>>-tok #f))
					      #t)))
    (lambda (tok-list)
      (if (null? tok-list)
	  '()
	  (let* ((tok (car tok-list))
		 (tok-type (get-tok-type tok))
		 (toks (cdr tok-list))
		 (new-toks (de-anchor-tokens toks)))
	    (cond ((vector-ref not-anchor-toks tok-type)
		   (cons tok new-toks))
		  ((or (= tok-type caret-tok) (= tok-type dollar-tok))
		   (let* ((line (get-tok-line tok))
			  (column (get-tok-column tok))
			  (attr (if (= tok-type caret-tok) caret-ch dollar-ch))
			  (new-tok (make-tok char-tok "" line column attr)))
		     (cons new-tok new-toks)))
		  ((= tok-type <<EOF>>-tok)
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the <<EOF>> anchor must be used alone and only after %%."))
		  ((= tok-type <<ERROR>>-tok)
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the <<ERROR>> anchor must be used alone and only after %%."))))))))

(define strip-end
  (lambda (l)
    (if (null? (cdr l))
	'()
	(cons (car l) (strip-end (cdr l))))))

(define extract-anchors
  (lambda (tok-list)
    (let* ((tok1 (car tok-list))
	   (line (get-tok-line tok1))
	   (tok1-type (get-tok-type tok1)))
      (cond ((and (= tok1-type <<EOF>>-tok) (null? (cdr tok-list)))
	     (make-rule line #t #f #f #f '() #f))
	    ((and (= tok1-type <<ERROR>>-tok) (null? (cdr tok-list)))
	     (make-rule line #f #t #f #f '() #f))
	    (else
	     (let* ((bol? (= tok1-type caret-tok))
		    (tok-list2 (if bol? (cdr tok-list) tok-list)))
	       (if (null? tok-list2)
		   (make-rule line #f #f bol? #f tok-list2 #f)
		   (let* ((len (length tok-list2))
			  (tok2 (list-ref tok-list2 (- len 1)))
			  (tok2-type (get-tok-type tok2))
			  (eol? (= tok2-type dollar-tok))
			  (tok-list3 (if eol?
					 (strip-end tok-list2)
					 tok-list2)))
		     (make-rule line #f #f bol? eol? tok-list3 #f)))))))))

(define char-list->conc
  (lambda (char-list)
    (if (null? char-list)
	(make-re epsilon-re)
	(let loop ((cl char-list))
	  (let* ((c (car cl))
		 (cl2 (cdr cl)))
	    (if (null? cl2)
		(make-re char-re c)
		(make-re conc-re (make-re char-re c) (loop cl2))))))))

(define parse-tokens-atom
  (let ((action-table
	 (make-dispatch-table
	  number-of-tokens
	  (list (cons lpar-tok
		      (lambda (tok tok-list macros)
			(parse-tokens-sub tok-list macros)))
		(cons dot-tok
		      (lambda (tok tok-list macros)
			(cons (make-re class-re dot-class) (cdr tok-list))))
		(cons subst-tok
		      (lambda (tok tok-list macros)
			(let* ((name (get-tok-attr tok))
			       (ass (assoc name macros)))
			  (if ass
			      (cons (cdr ass) (cdr tok-list))
			    (lex-error (get-tok-line tok)
				       (get-tok-column tok)
				       "unknown macro \""
				       (get-tok-2nd-attr tok)
				       "\".")))))
		(cons char-tok
		      (lambda (tok tok-list macros)
			(let ((c (get-tok-attr tok)))
			  (cons (make-re char-re c) (cdr tok-list)))))
		(cons class-tok
		      (lambda (tok tok-list macros)
			(let ((class (get-tok-attr tok)))
			  (cons (make-re class-re class) (cdr tok-list)))))
		(cons string-tok
		      (lambda (tok tok-list macros)
			(let* ((char-list (get-tok-attr tok))
			       (re (char-list->conc char-list)))
			  (cons re (cdr tok-list))))))
	  (lambda (tok tok-list macros)
	    (lex-error (get-tok-line tok)
		       (get-tok-column tok)
		       "syntax error in regular expression.")))))
    (lambda (tok-list macros)
      (let* ((tok (car tok-list))
	     (tok-type (get-tok-type tok))
	     (action (vector-ref action-table tok-type)))
	(action tok tok-list macros)))))

(define check-power-tok
  (lambda (tok)
    (let* ((range (get-tok-attr tok))
	   (start (car range))
	   (end (cdr range)))
      (if (or (eq? 'inf end) (<= start end))
	  range
	  (lex-error (get-tok-line tok)
		     (get-tok-column tok)
		     "incorrect power specification.")))))

(define power->star-plus
  (lambda (re range)
    (power->star-plus-rec re (car range) (cdr range))))

(define power->star-plus-rec
  (lambda (re start end)
    (cond ((eq? end 'inf)
	   (cond ((= start 0)
		  (make-re star-re re))
		 ((= start 1)
		  (make-re plus-re re))
		 (else
		  (make-re conc-re
			   re
			   (power->star-plus-rec re (- start 1) 'inf)))))
	  ((= start 0)
	   (cond ((= end 0)
		  (make-re epsilon-re))
		 ((= end 1)
		  (make-re question-re re))
		 (else
		  (make-re question-re
			   (power->star-plus-rec re 1 end)))))
	  ((= start 1)
	   (if (= end 1)
	       re
	       (make-re conc-re re (power->star-plus-rec re 0 (- end 1)))))
	  (else
	   (make-re conc-re
		    re
		    (power->star-plus-rec re (- start 1) (- end 1)))))))

(define parse-tokens-fact
  (let ((not-op-toks (make-dispatch-table number-of-tokens
					  (list (cons question-tok #f)
						(cons plus-tok     #f)
						(cons star-tok     #f)
						(cons power-tok    #f))
					  #t)))
    (lambda (tok-list macros)
      (let* ((result (parse-tokens-atom tok-list macros))
	     (re (car result))
	     (tok-list2 (cdr result)))
	(let loop ((re re) (tok-list3 tok-list2))
	  (let* ((tok (car tok-list3))
		 (tok-type (get-tok-type tok)))
	    (cond ((vector-ref not-op-toks tok-type)
		   (cons re tok-list3))
		  ((= tok-type question-tok)
		   (loop (make-re question-re re) (cdr tok-list3)))
		  ((= tok-type plus-tok)
		   (loop (make-re plus-re re) (cdr tok-list3)))
		  ((= tok-type star-tok)
		   (loop (make-re star-re re) (cdr tok-list3)))
		  ((= tok-type power-tok)
		   (loop (power->star-plus re (check-power-tok tok))
			 (cdr tok-list3))))))))))

(define parse-tokens-conc
  (lambda (tok-list macros)
    (let* ((result1 (parse-tokens-fact tok-list macros))
	   (re1 (car result1))
	   (tok-list2 (cdr result1))
	   (tok (car tok-list2))
	   (tok-type (get-tok-type tok)))
      (cond ((or (= tok-type pipe-tok)
		 (= tok-type rpar-tok))
	     result1)
	    (else ; Autres facteurs
	     (let* ((result2 (parse-tokens-conc tok-list2 macros))
		    (re2 (car result2))
		    (tok-list3 (cdr result2)))
	       (cons (make-re conc-re re1 re2) tok-list3)))))))

(define parse-tokens-or
  (lambda (tok-list macros)
    (let* ((result1 (parse-tokens-conc tok-list macros))
	   (re1 (car result1))
	   (tok-list2 (cdr result1))
	   (tok (car tok-list2))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type pipe-tok)
	     (let* ((tok-list3 (cdr tok-list2))
		    (result2 (parse-tokens-or tok-list3 macros))
		    (re2 (car result2))
		    (tok-list4 (cdr result2)))
	       (cons (make-re or-re re1 re2) tok-list4)))
	    (else ; rpar-tok
	     result1)))))

(define parse-tokens-sub
  (lambda (tok-list macros)
    (let* ((tok-list2 (cdr tok-list)) ; Manger le lpar-tok
	   (result (parse-tokens-or tok-list2 macros))
	   (re (car result))
	   (tok-list3 (cdr result))
	   (tok-list4 (cdr tok-list3))) ; Manger le rpar-tok
      (cons re tok-list4))))

(define parse-tokens-match
  (lambda (tok-list line)
    (let loop ((tl tok-list) (count 0))
      (if (null? tl)
	  (if (> count 0)
	      (lex-error line
			 #f
			 "mismatched parentheses."))
	  (let* ((tok (car tl))
		 (tok-type (get-tok-type tok)))
	    (cond ((= tok-type lpar-tok)
		   (loop (cdr tl) (+ count 1)))
		  ((= tok-type rpar-tok)
		   (if (zero? count)
		       (lex-error line
				  #f
				  "mismatched parentheses."))
		   (loop (cdr tl) (- count 1)))
		  (else
		   (loop (cdr tl) count))))))))

; Ne traite pas les anchors
(define parse-tokens
  (lambda (tok-list macros)
    (if (null? tok-list)
	(make-re epsilon-re)
	(let ((line (get-tok-line (car tok-list))))
	  (parse-tokens-match tok-list line)
	  (let* ((begin-par (make-tok lpar-tok "" line 1))
		 (end-par (make-tok rpar-tok "" line 1)))
	    (let* ((tok-list2 (append (list begin-par)
				      tok-list
				      (list end-par)))
		   (result (parse-tokens-sub tok-list2 macros)))
	      (car result))))))) ; (cdr result) == () obligatoirement

(define tokens->regexp
  (lambda (tok-list macros)
    (let ((tok-list2 (de-anchor-tokens tok-list)))
      (parse-tokens tok-list2 macros))))

(define tokens->rule
  (lambda (tok-list macros)
    (let* ((rule (extract-anchors tok-list))
	   (tok-list2 (get-rule-regexp rule))
	   (tok-list3 (de-anchor-tokens tok-list2))
	   (re (parse-tokens tok-list3 macros)))
      (set-rule-regexp rule re)
      rule)))

; Retourne une paire: <<EOF>>-action et vecteur des regles ordinaires
(define (adapt-rules rules)
  (let loop ((r rules) (revr '()) (<<EOF>>-action #f) (<<ERROR>>-action #f))
    (if (null? r)
	(values (or <<EOF>>-action default-<<EOF>>-action)
		(or <<ERROR>>-action default-<<ERROR>>-action)
		(list->vector (reverse revr)))
      (let ((r1 (car r)))
	(cond ((get-rule-eof? r1)
	       (if <<EOF>>-action
		   (lex-error (get-rule-line r1)
			      #f
			      "the <<EOF>> anchor can be used at most once.")
		 (loop (cdr r) revr (get-rule-action r1) <<ERROR>>-action)))
	      ((get-rule-error? r1)
	       (if <<ERROR>>-action
		   (lex-error (get-rule-line r1)
			      #f
			      "the <<ERROR>> anchor can be used at most once.")
		 (loop (cdr r) revr <<EOF>>-action (get-rule-action r1))))
	      (else
	       (loop (cdr r) (cons r1 revr) <<EOF>>-action <<ERROR>>-action)))))))

;
; Analyseur de fichier lex
;

(define parse-hv-blanks
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (if (or (= tok-type hblank-tok)
	      (= tok-type vblank-tok))
	  (parse-hv-blanks)
	  (lexer-unget tok)))))

(define parse-class-range
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type char-tok)
	     (let* ((c (get-tok-attr tok))
		    (tok2 (lexer))
		    (tok2-type (get-tok-type tok2)))
	       (if (not (= tok2-type minus-tok))
		   (begin
		     (lexer-unget tok2)
		     (cons c c))
		   (let* ((tok3 (lexer))
			  (tok3-type (get-tok-type tok3)))
		     (cond ((= tok3-type char-tok)
			    (let ((c2 (get-tok-attr tok3)))
			      (if (> c c2)
				  (lex-error (get-tok-line tok3)
					     (get-tok-column tok3)
					     "bad range specification in "
					     "character class;"
					     #\newline
					     "the start character is "
					     "higher than the end one.")
				  (cons c c2))))
		           ((or (= tok3-type rbrack-tok)
				(= tok3-type minus-tok))
			    (lex-error (get-tok-line tok3)
				       (get-tok-column tok3)
				       "bad range specification in "
				       "character class; a specification"
				       #\newline
				       "like \"-x\", \"x--\" or \"x-]\" has "
				       "been used."))
			   ((= tok3-type eof-tok)
			    (lex-error (get-tok-line tok3)
				       #f
				       "eof of file found while parsing "
				       "a character class.")))))))
	    ((= tok-type minus-tok)
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"bad range specification in character class; a "
			"specification"
			#\newline
			"like \"-x\", \"x--\" or \"x-]\" has been used."))
            ((= tok-type rbrack-tok)
	     #f)
	    ((= tok-type eof-tok)
	     (lex-error (get-tok-line tok)
			#f
			"eof of file found while parsing "
			"a character class."))))))

(define parse-class
  (lambda (initial-class negative-class? line column)
    (push-lexer 'class)
    (let loop ((class initial-class))
      (let ((new-range (parse-class-range)))
	(if new-range
	    (loop (class-union (list new-range) class))
	    (let ((class (if negative-class?
			     (class-compl class)
			     class)))
	      (pop-lexer)
	      (make-tok class-tok "" line column class)))))))

(define parse-string
  (lambda (line column)
    (push-lexer 'string)
    (let ((char-list (let loop ()
		       (let* ((tok (lexer))
			      (tok-type (get-tok-type tok)))
			 (cond ((= tok-type char-tok)
				(cons (get-tok-attr tok) (loop)))
			       ((= tok-type doublequote-tok)
				(pop-lexer)
				'())
			       (else ; eof-tok
				(lex-error (get-tok-line tok)
					   #f
					   "end of file found while "
					   "parsing a string.")))))))
      (make-tok string-tok "" line column char-list))))

(define parse-regexp
  (let* ((end-action
	  (lambda (tok loop)
	    (lexer-unget tok)
	    (pop-lexer)
	    (lexer-set-blank-history #f)
	    `()))
	 (action-table
	  (make-dispatch-table
	   number-of-tokens
	   (list (cons eof-tok end-action)
		 (cons hblank-tok end-action)
		 (cons vblank-tok end-action)
		 (cons lbrack-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class (list)
						  #f
						  (get-tok-line tok)
						  (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-rbrack-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class
				      (list (cons rbrack-ch rbrack-ch))
				      #f
				      (get-tok-line tok)
				      (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-caret-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class (list)
						  #t
						  (get-tok-line tok)
						  (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-minus-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class
				      (list (cons minus-ch minus-ch))
				      #f
				      (get-tok-line tok)
				      (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons doublequote-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-string (get-tok-line tok)
						   (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons illegal-tok
		       (lambda (tok loop)
			 (lex-error (get-tok-line tok)
				    (get-tok-column tok)
				    "syntax error in macro reference."))))
	   (lambda (tok loop)
	     (cons tok (loop))))))
    (lambda ()
      (push-lexer 'regexp)
      (lexer-set-blank-history #t)
      (parse-hv-blanks)
      (let loop ()
	(let* ((tok (lexer))
	       (tok-type (get-tok-type tok))
	       (action (vector-ref action-table tok-type)))
	  (action tok loop))))))

(define parse-ws1-regexp  ; Exige un blanc entre le nom et la RE d'une macro
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((or (= tok-type hblank-tok) (= tok-type vblank-tok))
	     (parse-regexp))
	    (else  ; percent-percent-tok, id-tok ou illegal-tok
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"white space expected."))))))

(define parse-macro
  (lambda (macros)
    (push-lexer 'macro)
    (parse-hv-blanks)
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type id-tok)
	     (let* ((name (get-tok-attr tok))
		    (ass (assoc name macros)))
	       (if ass
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the macro \""
			      (get-tok-2nd-attr tok)
			      "\" has already been defined.")
		   (let* ((tok-list (parse-ws1-regexp))
			  (regexp (tokens->regexp tok-list macros)))
		     (pop-lexer)
		     (cons name regexp)))))
            ((= tok-type percent-percent-tok)
	     (pop-lexer)
	     #f)
	    ((= tok-type illegal-tok)
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"macro name expected."))
	    ((= tok-type eof-tok)
	     (lex-error (get-tok-line tok)
			#f
			"end of file found before %%."))))))

(define parse-macros
  (lambda ()
    (let loop ((macros '()))
      (let ((macro (parse-macro macros)))
	(if macro
	    (loop (cons macro macros))
	    macros)))))

(define parse-action-end
  (lambda (<<EOF>>-action? <<ERROR>>-action? action?)
    (let ((act (lexer-get-history)))
      (cond (action?
	     act)
	    (<<EOF>>-action?
	     (string-append act default-<<EOF>>-action))
	    (<<ERROR>>-action?
	     (string-append act default-<<ERROR>>-action))
	    (else
	     (string-append act default-action))))))

(define parse-action
  (lambda (<<EOF>>-action? <<ERROR>>-action?)
    (push-lexer 'action)
    (let loop ((action? #f))
      (let* ((tok (lexer))
	     (tok-type (get-tok-type tok)))
	(cond ((= tok-type char-tok)
	       (loop #t))
	      ((= tok-type hblank-tok)
	       (loop action?))
	      ((= tok-type vblank-tok)
	       (push-lexer 'regexp)
	       (let* ((tok (lexer))
		      (tok-type (get-tok-type tok))
		      (bidon (lexer-unget tok)))
		 (pop-lexer)
		 (if (or (= tok-type hblank-tok)
			 (= tok-type vblank-tok))
		     (loop action?)
		     (begin
		       (pop-lexer)
		       (parse-action-end <<EOF>>-action?
					 <<ERROR>>-action?
					 action?)))))
	      (else ; eof-tok
	       (lexer-unget tok)
	       (pop-lexer)
	       (parse-action-end <<EOF>>-action?
				 <<ERROR>>-action?
				 action?)))))))

(define parse-rule
  (lambda (macros)
    (let ((tok-list (parse-regexp)))
      (if (null? tok-list)
	  #f
	  (let* ((rule (tokens->rule tok-list macros))
		 (action
		  (parse-action (get-rule-eof? rule) (get-rule-error? rule))))
	    (set-rule-action rule action)
	    rule)))))

(define parse-rules
  (lambda (macros)
    (parse-action #f #f)
    (let loop ()
      (let ((rule (parse-rule macros)))
	(if rule
	    (cons rule (loop))
	    '())))))


;;;; Module re2nfa.scm.

; Le vecteur d'etats contient la table de transition du nfa.
; Chaque entree contient les arcs partant de l'etat correspondant.
; Les arcs sont stockes dans une liste.
; Chaque arc est une paire (class . destination).
; Les caracteres d'une classe sont enumeres par ranges.
; Les ranges sont donnes dans une liste,
;   chaque element etant une paire (debut . fin).
; Le symbole eps peut remplacer une classe.
; L'acceptation est decrite par une paire (acc-if-eol . acc-if-no-eol).

; Quelques variables globales
(define r2n-counter 0)
(define r2n-v-arcs '#(#f))
(define r2n-v-acc '#(#f))
(define r2n-v-len 1)

; Initialisation des variables globales
(define r2n-init
  (lambda ()
    (set! r2n-counter 0)
    (set! r2n-v-arcs (vector '()))
    (set! r2n-v-acc (vector #f))
    (set! r2n-v-len 1)))

; Agrandissement des vecteurs
(define r2n-extend-v
  (lambda ()
    (let* ((new-len (* 2 r2n-v-len))
	   (new-v-arcs (make-vector new-len '()))
	   (new-v-acc (make-vector new-len #f)))
      (let loop ((i 0))
	(if (< i r2n-v-len)
	    (begin
	      (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	      (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
	      (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len new-len))))

; Finalisation des vecteurs
(define r2n-finalize-v
  (lambda ()
    (let* ((new-v-arcs (make-vector r2n-counter))
	   (new-v-acc (make-vector r2n-counter)))
      (let loop ((i 0))
	(if (< i r2n-counter)
	    (begin
	      (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	      (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
	      (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len r2n-counter))))

; Creation d'etat
(define r2n-get-state
  (lambda (acc)
    (if (= r2n-counter r2n-v-len)
	(r2n-extend-v))
    (let ((state r2n-counter))
      (set! r2n-counter (+ r2n-counter 1))
      (vector-set! r2n-v-acc state (or acc (cons #f #f)))
      state)))

; Ajout d'un arc
(define r2n-add-arc
  (lambda (start chars end)
    (vector-set! r2n-v-arcs
		 start
		 (cons (cons chars end) (vector-ref r2n-v-arcs start)))))

; Construction de l'automate a partir des regexp
(define r2n-build-epsilon
  (lambda (re start end)
    (r2n-add-arc start 'eps end)))

(define r2n-build-or
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re))
	  (re2 (get-re-attr2 re)))
      (r2n-build-re re1 start end)
      (r2n-build-re re2 start end))))

(define r2n-build-conc
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (re2 (get-re-attr2 re))
	   (inter (r2n-get-state #f)))
      (r2n-build-re re1 start inter)
      (r2n-build-re re2 inter end))))

(define r2n-build-star
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter1 'eps inter2)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter2 inter1))))

(define r2n-build-plus
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter2 'eps inter1)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter1 inter2))))

(define r2n-build-question
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re)))
      (r2n-add-arc start 'eps end)
      (r2n-build-re re1 start end))))

(define r2n-build-class
  (lambda (re start end)
    (let ((class (get-re-attr1 re)))
      (r2n-add-arc start class end))))

(define r2n-build-char
  (lambda (re start end)
    (let* ((c (get-re-attr1 re))
	   (class (list (cons c c))))
      (r2n-add-arc start class end))))

(define r2n-build-re
  (let ((sub-function-v (vector r2n-build-epsilon
				r2n-build-or
				r2n-build-conc
				r2n-build-star
				r2n-build-plus
				r2n-build-question
				r2n-build-class
				r2n-build-char)))
    (lambda (re start end)
      (let* ((re-type (get-re-type re))
	     (sub-f (vector-ref sub-function-v re-type)))
	(sub-f re start end)))))

; Construction de l'automate relatif a une regle
(define r2n-build-rule
  (lambda (rule ruleno nl-start no-nl-start)
    (let* ((re (get-rule-regexp rule))
	   (bol? (get-rule-bol? rule))
	   (eol? (get-rule-eol? rule))
	   (rule-start (r2n-get-state #f))
	   (rule-end (r2n-get-state (if eol?
					(cons ruleno #f)
					(cons ruleno ruleno)))))
      (r2n-build-re re rule-start rule-end)
      (r2n-add-arc nl-start 'eps rule-start)
      (if (not bol?)
	  (r2n-add-arc no-nl-start 'eps rule-start)))))

; Construction de l'automate complet
(define (re2nfa rules)
  (let ((nb-of-rules (vector-length rules)))
    (r2n-init)
    (let* ((nl-start (r2n-get-state #f))
	   (no-nl-start (r2n-get-state #f)))
      (let loop ((i 0))
	(when (< i nb-of-rules)
	  (r2n-build-rule (vector-ref rules i)
			  i
			  nl-start
			  no-nl-start)
	  (loop (+ i 1))))
      (r2n-finalize-v)
      (let ((v-arcs r2n-v-arcs)
	    (v-acc r2n-v-acc))
	(r2n-init)
	(values nl-start no-nl-start v-arcs v-acc)))))


;;;; Module noeps.scm.

; Fonction "merge" qui elimine les repetitions
(define noeps-merge-1
  (lambda (l1 l2)
    (cond ((null? l1)
	   l2)
	  ((null? l2)
	   l1)
	  (else
	   (let ((t1 (car l1))
		 (t2 (car l2)))
	     (cond ((< t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) l2)))
		   ((= t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) (cdr l2))))
		   (else
		    (cons t2 (noeps-merge-1 l1 (cdr l2))))))))))

; Fabrication des voisinages externes
(define noeps-mkvois
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (begin
	      (let loop2 ((trans (vector-ref trans-v n)) (ends '()))
		(if (null? trans)
		    (vector-set! arcs n ends)
		    (let* ((tran (car trans))
			   (class (car tran))
			   (end (cdr tran)))
		      (loop2 (cdr trans) (if (eq? class 'eps)
					     (noeps-merge-1 ends (list end))
					     ends)))))
	      (loop1 (+ n 1)))))
      arcs)))

; Fabrication des valeurs initiales
(define noeps-mkinit
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (init (make-vector nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (begin
	      (vector-set! init n (list n))
	      (loop (+ n 1)))))
      init)))

; Traduction d'une liste d'arcs
(define noeps-trad-arcs
  (lambda (trans dict)
    (let loop ((trans trans))
      (if (null? trans)
	  '()
	  (let* ((tran (car trans))
		 (class (car tran))
		 (end (cdr tran)))
	    (if (eq? class 'eps)
		(loop (cdr trans))
		(let* ((new-end (vector-ref dict end))
		       (new-tran (cons class new-end)))
		  (cons new-tran (loop (cdr trans))))))))))

; Elimination des transitions eps
(define (noeps nl-start no-nl-start arcs acc)
  (let* ((digraph-arcs (noeps-mkvois arcs))
	 (digraph-init (noeps-mkinit arcs))
	 (dict (digraph digraph-arcs digraph-init noeps-merge-1))
	 (new-nl-start (vector-ref dict nl-start))
	 (new-no-nl-start (vector-ref dict no-nl-start)))
    (let loop ((i (- (vector-length arcs) 1)))
      (when (>= i 0)
	(vector-set! arcs i (noeps-trad-arcs (vector-ref arcs i) dict))
	(loop (- i 1))))
    (values new-nl-start new-no-nl-start arcs acc)))

; Module sweep.scm.

; Preparer les arcs pour digraph
(define sweep-mkarcs
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs-v (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (let loop2 ((trans (vector-ref trans-v n)) (arcs '()))
	      (if (null? trans)
		  (begin
		    (vector-set! arcs-v n arcs)
		    (loop1 (+ n 1)))
		  (loop2 (cdr trans) (noeps-merge-1 (cdar trans) arcs))))
	    arcs-v)))))

; Preparer l'operateur pour digraph
(define sweep-op
  (let ((acc-min (lambda (rule1 rule2)
		   (cond ((not rule1)
			  rule2)
			 ((not rule2)
			  rule1)
			 (else
			  (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
	    (acc-min (cdr acc1) (cdr acc2))))))

; Renumerotation des etats (#f pour etat a eliminer)
; Retourne (new-nbnodes . dict)
(define sweep-renum
  (lambda (dist-acc-v)
    (let* ((nbnodes (vector-length dist-acc-v))
	   (dict (make-vector nbnodes)))
      (let loop ((n 0) (new-n 0))
	(if (< n nbnodes)
	    (let* ((acc (vector-ref dist-acc-v n))
		   (dead? (equal? acc '(#f . #f))))
	      (if dead?
		  (begin
		    (vector-set! dict n #f)
		    (loop (+ n 1) new-n))
		  (begin
		    (vector-set! dict n new-n)
		    (loop (+ n 1) (+ new-n 1)))))
	    (cons new-n dict))))))

; Elimination des etats inutiles d'une liste d'etats
(define sweep-list
  (lambda (ss dict)
    (if (null? ss)
	'()
	(let* ((olds (car ss))
	       (news (vector-ref dict olds)))
	  (if news
	      (cons news (sweep-list (cdr ss) dict))
	      (sweep-list (cdr ss) dict))))))

; Elimination des etats inutiles d'une liste d'arcs
(define sweep-arcs
  (lambda (arcs dict)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (class (car arc))
	       (ss (cdr arc))
	       (new-ss (sweep-list ss dict)))
	  (if (null? new-ss)
	      (sweep-arcs (cdr arcs) dict)
	      (cons (cons class new-ss) (sweep-arcs (cdr arcs) dict)))))))

; Elimination des etats inutiles dans toutes les transitions
(define sweep-all-arcs
  (lambda (arcs-v dict)
    (let loop ((n (- (vector-length arcs-v) 1)))
      (if (>= n 0)
	  (begin
	    (vector-set! arcs-v n (sweep-arcs (vector-ref arcs-v n) dict))
	    (loop (- n 1)))
	  arcs-v))))

; Elimination des etats inutiles dans un vecteur
(define sweep-states
  (lambda (v new-nbnodes dict)
    (let ((nbnodes (vector-length v))
	  (new-v (make-vector new-nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (let ((new-n (vector-ref dict n)))
	      (if new-n
		  (vector-set! new-v new-n (vector-ref v n)))
	      (loop (+ n 1)))
	    new-v)))))

; Elimination des etats inutiles
(define (sweep nl-start no-nl-start arcs-v acc-v)
  (let* ((digraph-arcs (sweep-mkarcs arcs-v))
	 (digraph-init acc-v)
	 (digraph-op sweep-op)
	 (dist-acc-v (digraph digraph-arcs digraph-init digraph-op))
	 (result (sweep-renum dist-acc-v))
	 (new-nbnodes (car result))
	 (dict (cdr result))
	 (new-nl-start (sweep-list nl-start dict))
	 (new-no-nl-start (sweep-list no-nl-start dict))
	 (new-arcs-v (sweep-states (sweep-all-arcs arcs-v dict)
				   new-nbnodes
				   dict))
	 (new-acc-v (sweep-states acc-v new-nbnodes dict)))
    (values new-nl-start new-no-nl-start new-arcs-v new-acc-v)))


;;;; Module nfa2dfa.scm.

; Recoupement de deux arcs
(define n2d-2arcs
  (lambda (arc1 arc2)
    (let* ((class1 (car arc1))
	   (ss1 (cdr arc1))
	   (class2 (car arc2))
	   (ss2 (cdr arc2))
	   (result (class-sep class1 class2))
	   (classl (vector-ref result 0))
	   (classc (vector-ref result 1))
	   (classr (vector-ref result 2))
	   (ssl ss1)
	   (ssc (ss-union ss1 ss2))
	   (ssr ss2))
      (vector (if (or (null? classl) (null? ssl)) #f (cons classl ssl))
	      (if (or (null? classc) (null? ssc)) #f (cons classc ssc))
	      (if (or (null? classr) (null? ssr)) #f (cons classr ssr))))))

; Insertion d'un arc dans une liste d'arcs a classes distinctes
(define n2d-insert-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
	(list new-arc)
	(let* ((arc (car arcs))
	       (others (cdr arcs))
	       (result (n2d-2arcs new-arc arc))
	       (arcl (vector-ref result 0))
	       (arcc (vector-ref result 1))
	       (arcr (vector-ref result 2))
	       (list-arcc (if arcc (list arcc) '()))
	       (list-arcr (if arcr (list arcr) '())))
	  (if arcl
	      (append list-arcc list-arcr (n2d-insert-arc arcl others))
	      (append list-arcc list-arcr others))))))

; Regroupement des arcs qui aboutissent au meme sous-ensemble d'etats
(define n2d-factorize-arcs
  (lambda (arcs)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (arc-ss (cdr arc))
	       (others-no-fact (cdr arcs))
	       (others (n2d-factorize-arcs others-no-fact)))
	  (let loop ((o others))
	    (if (null? o)
		(list arc)
		(let* ((o1 (car o))
		       (o1-ss (cdr o1)))
		  (if (equal? o1-ss arc-ss)
		      (let* ((arc-class (car arc))
			     (o1-class (car o1))
			     (new-class (class-union arc-class o1-class))
			     (new-arc (cons new-class arc-ss)))
			(cons new-arc (cdr o)))
		      (cons o1 (loop (cdr o)))))))))))

; Transformer une liste d'arcs quelconques en des arcs a classes distinctes
(define n2d-distinguish-arcs
  (lambda (arcs)
    (let loop ((arcs arcs) (n-arcs '()))
      (if (null? arcs)
	  n-arcs
	  (loop (cdr arcs) (n2d-insert-arc (car arcs) n-arcs))))))

; Transformer une liste d'arcs quelconques en des arcs a classes et a
; destinations distinctes
(define n2d-normalize-arcs
  (lambda (arcs)
    (n2d-factorize-arcs (n2d-distinguish-arcs arcs))))

; Factoriser des arcs a destination unique (~deterministes)
(define n2d-factorize-darcs
  (lambda (arcs)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (arc-end (cdr arc))
	       (other-arcs (cdr arcs))
	       (farcs (n2d-factorize-darcs other-arcs)))
	  (let loop ((farcs farcs))
	    (if (null? farcs)
		(list arc)
		(let* ((farc (car farcs))
		       (farc-end (cdr farc)))
		  (if (= farc-end arc-end)
		      (let* ((arc-class (car arc))
			     (farc-class (car farc))
			     (new-class (class-union farc-class arc-class))
			     (new-arc (cons new-class arc-end)))
			(cons new-arc (cdr farcs)))
		      (cons farc (loop (cdr farcs)))))))))))

; Normaliser un vecteur de listes d'arcs
(define n2d-normalize-arcs-v
  (lambda (arcs-v)
    (let* ((nbnodes (vector-length arcs-v))
	   (new-v (make-vector nbnodes)))
      (let loop ((n 0))
	(if (= n nbnodes)
	    new-v
	    (begin
	      (vector-set! new-v n (n2d-normalize-arcs (vector-ref arcs-v n)))
	      (loop (+ n 1))))))))

; Inserer un arc dans une liste d'arcs a classes distinctes en separant
; les arcs contenant une partie de la classe du nouvel arc des autres arcs
; Retourne: (oui . non)
(define n2d-ins-sep-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
	(cons (list new-arc) '())
	(let* ((arc (car arcs))
	       (others (cdr arcs))
	       (result (n2d-2arcs new-arc arc))
	       (arcl (vector-ref result 0))
	       (arcc (vector-ref result 1))
	       (arcr (vector-ref result 2))
	       (l-arcc (if arcc (list arcc) '()))
	       (l-arcr (if arcr (list arcr) '()))
	       (result (if arcl
			   (n2d-ins-sep-arc arcl others)
			   (cons '() others)))
	       (oui-arcs (car result))
	       (non-arcs (cdr result)))
	  (cons (append l-arcc oui-arcs) (append l-arcr non-arcs))))))

; Combiner deux listes d'arcs a classes distinctes
; Ne tente pas de combiner les arcs qui ont nec. des classes disjointes
; Conjecture: les arcs crees ont leurs classes disjointes
; Note: envisager de rajouter un "n2d-factorize-arcs" !!!!!!!!!!!!
(define n2d-combine-arcs
  (lambda (arcs1 arcs2)
    (let loop ((arcs1 arcs1) (arcs2 arcs2) (dist-arcs2 '()))
      (if (null? arcs1)
	  (append arcs2 dist-arcs2)
	  (let* ((arc (car arcs1))
		 (result (n2d-ins-sep-arc arc arcs2))
		 (oui-arcs (car result))
		 (non-arcs (cdr result)))
	    (loop (cdr arcs1) non-arcs (append oui-arcs dist-arcs2)))))))

; ;
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation naive
; ; des arcs d'un ensemble d'etats.
; ;
;
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
;
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
;
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
;
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
;
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
;
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
;
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
;
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
; 	  (n2d-add-state ss)
; 	  (let* ((dentry (vector-ref n2d-state-dict n))
; 		 (dentry-ss (get-dentry-ss dentry)))
; 	    (if (equal? dentry-ss ss)
; 		n
; 		(loop (+ n 1))))))))
;
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
;
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
;
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
;
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
;
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
;
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcss (map (lambda (s) (vector-ref arcs-v s)) ss))
; 		   (arcs (apply append arcss))
; 		   (dist-arcs (n2d-distinguish-arcs arcs))
; 		   (darcs (n2d-translate-arcs dist-arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ;
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ;
;
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
;
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
;
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
;
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
;
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
;
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
;
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
;
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
; 	  (n2d-add-state ss)
; 	  (let* ((dentry (vector-ref n2d-state-dict n))
; 		 (dentry-ss (get-dentry-ss dentry)))
; 	    (if (equal? dentry-ss ss)
; 		n
; 		(loop (+ n 1))))))))
;
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
; 	'()
; 	(let* ((arcs (car arcs-l))
; 	       (other-arcs-l (cdr arcs-l))
; 	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
; 	  (n2d-combine-arcs arcs other-arcs)))))
;
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
;
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
;
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
;
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
;
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
;
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start))
; 	   (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
; 		   (arcs (n2d-combine-arcs-l arcs-l))
; 		   (darcs (n2d-translate-arcs arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ;
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Arbre de recherche.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ;
;
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; (define n2d-state-tree '#(#f ()))
;
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
;
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
;
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
;
; ; Fonctions de gestion de l'arbre de recherche
; (define make-snode (lambda () (vector #f '())))
;
; (define get-snode-dstate   (lambda (snode) (vector-ref snode 0)))
; (define get-snode-children (lambda (snode) (vector-ref snode 1)))
;
; (define set-snode-dstate
;   (lambda (snode dstate)   (vector-set! snode 0 dstate)))
; (define set-snode-children
;   (lambda (snode children) (vector-set! snode 1 children)))
;
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)
;     (set! n2d-state-tree (make-snode))))
;
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
;
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
;
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((s-l ss) (snode n2d-state-tree))
;       (if (null? s-l)
; 	  (or (get-snode-dstate snode)
; 	      (let ((s (n2d-add-state ss)))
; 		(set-snode-dstate snode s)
; 		s))
; 	  (let* ((next-s (car s-l))
; 		 (alist (get-snode-children snode))
; 		 (ass (or (assv next-s alist)
; 			  (let ((ass (cons next-s (make-snode))))
; 			    (set-snode-children snode (cons ass alist))
; 			    ass))))
; 	    (loop (cdr s-l) (cdr ass)))))))
;
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
; 	'()
; 	(let* ((arcs (car arcs-l))
; 	       (other-arcs-l (cdr arcs-l))
; 	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
; 	  (n2d-combine-arcs arcs other-arcs)))))
;
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
;
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
;
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
;
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
;
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
;
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start))
; 	   (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
; 		   (arcs (n2d-combine-arcs-l arcs-l))
; 		   (darcs (n2d-translate-arcs arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

;
; Section temporaire: vieille facon de generer le dfa
; Dictionnaire d'etat det.  Table de hashage.  Creation des
; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; classes distinctes.
;

; Quelques variables globales
(define n2d-state-dict '#(#f))
(define n2d-state-len 1)
(define n2d-state-count 0)
(define n2d-state-hash '#())

; Fonctions de gestion des entrees du dictionnaire
(define make-dentry (lambda (ss) (vector ss #f #f)))

(define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
(define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
(define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))

(define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
(define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))

; Initialisation des variables globales
(define n2d-init-glob-vars
  (lambda (hash-len)
    (set! n2d-state-dict (vector #f))
    (set! n2d-state-len 1)
    (set! n2d-state-count 0)
    (set! n2d-state-hash (make-vector hash-len '()))))

; Extension du dictionnaire
(define n2d-extend-dict
  (lambda ()
    (let* ((new-len (* 2 n2d-state-len))
	   (v (make-vector new-len #f)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (begin
	      (set! n2d-state-dict v)
	      (set! n2d-state-len new-len))
	    (begin
	      (vector-set! v n (vector-ref n2d-state-dict n))
	      (loop (+ n 1))))))))

; Ajout d'un etat
(define n2d-add-state
  (lambda (ss)
    (let* ((s n2d-state-count)
	   (dentry (make-dentry ss)))
      (if (= n2d-state-count n2d-state-len)
	  (n2d-extend-dict))
      (vector-set! n2d-state-dict s dentry)
      (set! n2d-state-count (+ n2d-state-count 1))
      s)))

; Recherche d'un etat
(define n2d-search-state
  (lambda (ss)
    (let* ((hash-no (if (null? ss) 0 (car ss)))
	   (alist (vector-ref n2d-state-hash hash-no))
	   (ass (assoc ss alist)))
      (if ass
	  (cdr ass)
	  (let* ((s (n2d-add-state ss))
		 (new-ass (cons ss s)))
	    (vector-set! n2d-state-hash hash-no (cons new-ass alist))
	    s)))))

; Combiner des listes d'arcs a classes dictinctes
(define n2d-combine-arcs-l
  (lambda (arcs-l)
    (if (null? arcs-l)
	'()
	(let* ((arcs (car arcs-l))
	       (other-arcs-l (cdr arcs-l))
	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
	  (n2d-combine-arcs arcs other-arcs)))))

; Transformer un arc non-det. en un arc det.
(define n2d-translate-arc
  (lambda (arc)
    (let* ((class (car arc))
	   (ss (cdr arc))
	   (s (n2d-search-state ss)))
      (cons class s))))

; Transformer une liste d'arcs non-det. en ...
(define n2d-translate-arcs
  (lambda (arcs)
    (map n2d-translate-arc arcs)))

; Trouver le minimum de deux acceptants
(define n2d-acc-min2
  (let ((acc-min (lambda (rule1 rule2)
		   (cond ((not rule1)
			  rule2)
			 ((not rule2)
			  rule1)
			 (else
			  (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
	    (acc-min (cdr acc1) (cdr acc2))))))

; Trouver le minimum de plusieurs acceptants
(define n2d-acc-mins
  (lambda (accs)
    (if (null? accs)
	(cons #f #f)
	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))

; Fabriquer les vecteurs d'arcs et d'acceptance
(define n2d-extract-vs
  (lambda ()
    (let* ((arcs-v (make-vector n2d-state-count))
	   (acc-v (make-vector n2d-state-count)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (cons arcs-v acc-v)
	    (begin
	      (vector-set! arcs-v n (get-dentry-darcs
				     (vector-ref n2d-state-dict n)))
	      (vector-set! acc-v n (get-dentry-acc
				    (vector-ref n2d-state-dict n)))
	      (loop (+ n 1))))))))

; Effectuer la transformation de l'automate de non-det. a det.
(define (nfa2dfa nl-start no-nl-start arcs-v acc-v)
  (n2d-init-glob-vars (vector-length arcs-v))
  (let* ((nl-d (n2d-search-state nl-start))
	 (no-nl-d (n2d-search-state no-nl-start))
	 (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
    (let loop ((n 0))
      (if (< n n2d-state-count)
	  (let* ((dentry (vector-ref n2d-state-dict n))
		 (ss (get-dentry-ss dentry))
		 (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
		 (arcs (n2d-combine-arcs-l arcs-l))
		 (darcs (n2d-translate-arcs arcs))
		 (fact-darcs (n2d-factorize-darcs darcs))
		 (accs (map (lambda (s) (vector-ref acc-v s)) ss))
		 (acc (n2d-acc-mins accs)))
	    (set-dentry-darcs dentry fact-darcs)
	    (set-dentry-acc   dentry acc)
	    (loop (+ n 1)))))
    (let* ((result (n2d-extract-vs))
	   (new-arcs-v (car result))
	   (new-acc-v (cdr result)))
      (n2d-init-glob-vars 0)
      (values nl-d no-nl-d new-arcs-v new-acc-v))))


;;;; Module prep.scm.

;
; Divers pre-traitements avant l'ecriture des tables
;

; Passe d'un arc multi-range a une liste d'arcs mono-range
(define prep-arc->sharcs
  (lambda (arc)
    (let* ((range-l (car arc))
	   (dest (cdr arc))
	   (op (lambda (range) (cons range dest))))
      (map op range-l))))

; Compare des arcs courts selon leur premier caractere
(define prep-sharc-<=
  (lambda (sharc1 sharc2)
    (class-<= (caar sharc1) (caar sharc2))))

; Remplit les trous parmi les sharcs avec des arcs "erreur"
(define prep-fill-error
  (lambda (sharcs)
    (let loop ((sharcs sharcs) (start 'inf-))
      (cond ((class-= start 'inf+)
	     '())
	    ((null? sharcs)
	     (cons (cons (cons start 'inf+) 'err) (loop sharcs 'inf+)))
	    (else
	     (let* ((sharc (car sharcs))
		    (h (caar sharc))
		    (t (cdar sharc)))
	       (if (class-< start h)
		   (cons (cons (cons start (- h 1)) 'err) (loop sharcs h))
		   (cons sharc (loop (cdr sharcs)
				     (if (class-= t 'inf+)
					 'inf+
					 (+ t 1)))))))))))

; ; Passe d'une liste d'arcs a un arbre de decision
; ; 1ere methode: seulement des comparaisons <
; (define prep-arcs->tree
;   (lambda (arcs)
;     (let* ((sharcs-l (map prep-arc->sharcs arcs))
; 	   (sharcs (apply append sharcs-l))
; 	   (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
; 	   (sorted (prep-fill-error sorted-with-holes))
; 	   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
; 	   (table (list->vector (map op sorted))))
;       (let loop ((left 0) (right (- (vector-length table) 1)))
; 	(if (= left right)
; 	    (cdr (vector-ref table left))
; 	    (let ((mid (div (+ left right 1) 2)))
; 	      (list (car (vector-ref table mid))
; 		    (loop left (- mid 1))
; 		    (loop mid right))))))))

; Passe d'une liste d'arcs a un arbre de decision
; 2eme methode: permettre des comparaisons = quand ca adonne
(define prep-arcs->tree
  (lambda (arcs)
    (let* ((sharcs-l (map prep-arc->sharcs arcs))
	   (sharcs (apply append sharcs-l))
	   (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
	   (sorted (prep-fill-error sorted-with-holes))
	   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
	   (table (list->vector (map op sorted))))
      (let loop ((left 0) (right (- (vector-length table) 1)))
	(if (= left right)
	    (cdr (vector-ref table left))
	    (let ((mid (div (+ left right 1) 2)))
	      (if (and (= (+ left 2) right)
		       (= (+ (car (vector-ref table mid)) 1)
			  (car (vector-ref table right)))
		       (eqv? (cdr (vector-ref table left))
			     (cdr (vector-ref table right))))
		  (list '=
			(car (vector-ref table mid))
			(cdr (vector-ref table mid))
			(cdr (vector-ref table left)))
		  (list (car (vector-ref table mid))
			(loop left (- mid 1))
			(loop mid right)))))))))

; Determine si une action a besoin de calculer yytext
(define prep-detect-yytext
  (lambda (s)
    (let loop1 ((i (- (string-length s) 6)))
      (cond ((< i 0)
	     #f)
	    ((char-ci=? (string-ref s i) #\y)
	     (let loop2 ((j 5))
	       (cond ((= j 0)
		      #t)
		     ((char-ci=? (string-ref s (+ i j))
				 (string-ref "yytext" j))
		      (loop2 (- j 1)))
		     (else
		      (loop1 (- i 1))))))
	    (else
	     (loop1 (- i 1)))))))

; Note dans une regle si son action a besoin de yytext
(define prep-set-rule-yytext?
  (lambda (rule)
    (let ((action (get-rule-action rule)))
      (set-rule-yytext? rule (prep-detect-yytext action)))))

; Note dans toutes les regles si leurs actions ont besoin de yytext
(define prep-set-rules-yytext?
  (lambda (rules)
    (let loop ((n (- (vector-length rules) 1)))
      (if (>= n 0)
	  (begin
	    (prep-set-rule-yytext? (vector-ref rules n))
	    (loop (- n 1)))))))


;;;; Module output.scm.

;
; Nettoie les actions en enlevant les lignes blanches avant et apres
;

(define out-split-in-lines
  (lambda (s)
    (let ((len (string-length s)))
      (let loop ((i 0) (start 0))
	(cond ((= i len)
	       '())
	      ((char=? (string-ref s i) #\newline)
	       (cons (substring s start (+ i 1))
		     (loop (+ i 1) (+ i 1))))
	      (else
	       (loop (+ i 1) start)))))))

(define out-empty-line?
  (lambda (s)
    (let ((len (- (string-length s) 1)))
      (let loop ((i 0))
	(cond ((= i len)
	       #t)
	      ((char-whitespace? (string-ref s i))
	       (loop (+ i 1)))
	      (else
	       #f))))))

; Enleve les lignes vides dans une liste avant et apres l'action
(define out-remove-empty-lines
  (lambda (lines)
    (let loop ((lines lines) (top? #t))
      (if (null? lines)
	  '()
	  (let ((line (car lines)))
	    (cond ((not (out-empty-line? line))
		   (cons line (loop (cdr lines) #f)))
		  (top?
		   (loop (cdr lines) #t))
		  (else
		   (let ((rest (loop (cdr lines) #f)))
		     (if (null? rest)
			 '()
			 (cons line rest))))))))))

; Enleve les lignes vides avant et apres l'action
(define out-clean-action
  (lambda (s)
    (let* ((lines (out-split-in-lines s))
	   (clean-lines (out-remove-empty-lines lines)))
      (apply string-append clean-lines))))




;
; Pretty-printer pour les booleens, la liste vide, les nombres,
; les symboles, les caracteres, les chaines, les listes et les vecteurs
;

; Colonne limite pour le pretty-printer (a ne pas atteindre)
(define out-max-col 76)

(define out-flatten-list
  (lambda (ll)
    (let loop ((ll ll) (part-out '()))
      (if (null? ll)
	  part-out
	  (let* ((new-part-out (loop (cdr ll) part-out))
		 (head (car ll)))
	    (cond ((null? head)
		   new-part-out)
		  ((pair? head)
		   (loop head new-part-out))
		  (else
		   (cons head new-part-out))))))))

(define out-force-string
  (lambda (obj)
    (if (char? obj)
	(string obj)
	obj)))

; Transforme une liste impropre en une liste propre qui s'ecrit
; de la meme facon
(define out-regular-list
  (let ((symbolic-dot (string->symbol ".")))
    (lambda (p)
      (let ((tail (cdr p)))
	(cond ((null? tail)
	       p)
	      ((pair? tail)
	       (cons (car p) (out-regular-list tail)))
	      (else
	       (list (car p) symbolic-dot tail)))))))

; Cree des chaines d'espaces de facon paresseuse
(define out-blanks
  (let ((cache-v (make-vector 80 #f)))
    (lambda (n)
      (or (vector-ref cache-v n)
	  (let ((result (make-string n #\space)))
	    (vector-set! cache-v n result)
	    result)))))

; Insere le separateur entre chaque element d'une liste non-vide
(define out-separate
  (lambda (text-l sep)
    (if (null? (cdr text-l))
	text-l
	(cons (car text-l) (cons sep (out-separate (cdr text-l) sep))))))

; Met des donnees en colonnes.  Retourne comme out-pp-aux-list
(define out-pp-columns
  (lambda (left right wmax txt&lens)
    (let loop1 ((tls txt&lens) (lwmax 0) (lwlast 0) (lines '()))
      (if (null? tls)
	  (vector #t 0 lwmax lwlast (reverse lines))
	  (let loop2 ((tls tls) (len 0) (first? #t) (prev-pad 0) (line '()))
	    (cond ((null? tls)
		   (loop1 tls
			  (max len lwmax)
			  len
			  (cons (reverse line) lines)))
		  ((> (+ left len prev-pad 1 wmax) out-max-col)
		   (loop1 tls
			  (max len lwmax)
			  len
			  (cons (reverse line) lines)))
		  (first?
		   (let ((text     (caar tls))
			 (text-len (cdar tls)))
		     (loop2 (cdr tls)
			    (+ len text-len)
			    #f
			    (- wmax text-len)
			    (cons text line))))
		  ((pair? (cdr tls))
		   (let* ((prev-pad-s (out-blanks prev-pad))
			  (text     (caar tls))
			  (text-len (cdar tls)))
		     (loop2 (cdr tls)
			    (+ len prev-pad 1 text-len)
			    #f
			    (- wmax text-len)
			    (cons text (cons " " (cons prev-pad-s line))))))
		  (else
		   (let ((prev-pad-s (out-blanks prev-pad))
			 (text     (caar tls))
			 (text-len (cdar tls)))
		     (if (> (+ left len prev-pad 1 text-len) right)
			 (loop1 tls
				(max len lwmax)
				len
				(cons (reverse line) lines))
			 (loop2 (cdr tls)
				(+ len prev-pad 1 text-len)
				#f
				(- wmax text-len)
				(append (list text " " prev-pad-s)
					line)))))))))))

; Retourne un vecteur #( multiline? width-all width-max width-last text-l )
(define out-pp-aux-list
  (lambda (l left right)
    (let loop ((l l) (multi? #f) (wall -1) (wmax -1) (wlast -1) (txt&lens '()))
      (if (null? l)
	  (cond (multi?
		 (vector #t wall wmax wlast (map car (reverse txt&lens))))
		((<= (+ left wall) right)
		 (vector #f wall wmax wlast (map car (reverse txt&lens))))
		((<= (+ left wmax 1 wmax) out-max-col)
		 (out-pp-columns left right wmax (reverse txt&lens)))
		(else
		 (vector #t wall wmax wlast (map car (reverse txt&lens)))))
	  (let* ((obj (car l))
		 (last? (null? (cdr l)))
		 (this-right (if last? right out-max-col))
		 (result (out-pp-aux obj left this-right))
		 (obj-multi? (vector-ref result 0))
		 (obj-wmax   (vector-ref result 1))
		 (obj-wlast  (vector-ref result 2))
		 (obj-text   (vector-ref result 3)))
	    (loop (cdr l)
		  (or multi? obj-multi?)
		  (+ wall obj-wmax 1)
		  (max wmax obj-wmax)
		  obj-wlast
		  (cons (cons obj-text obj-wmax) txt&lens)))))))

; Retourne un vecteur #( multiline? wmax wlast text )
(define out-pp-aux
  (lambda (obj left right)
    (cond ((boolean? obj)
	   (vector #f 2 2 (if obj '("#t") '("#f"))))
	  ((null? obj)
	   (vector #f 2 2 '("()")))
	  ((number? obj)
	   (let* ((s (number->string obj))
		  (len (string-length s)))
	     (vector #f len len (list s))))
	  ((symbol? obj)
	   (let* ((s (symbol->string obj))
		  (len (string-length s)))
	     (vector #f len len (list s))))
	  ((char? obj)
	   (cond ((char=? obj #\space)
		  (vector #f 7 7 (list "#\\space")))
		 ((char=? obj #\newline)
		  (vector #f 9 9 (list "#\\newline")))
		 (else
		  (vector #f 3 3 (list "#\\" obj)))))
	  ((string? obj)
	   (let loop ((i (- (string-length obj) 1))
		      (len 1)
		      (text '("\"")))
	     (if (= i -1)
		 (vector #f (+ len 1) (+ len 1) (cons "\"" text))
		 (let ((c (string-ref obj i)))
		   (cond ((char=? c #\\)
			  (loop (- i 1) (+ len 2) (cons "\\\\" text)))
			 ((char=? c #\")
			  (loop (- i 1) (+ len 2) (cons "\\\"" text)))
			 (else
			  (loop (- i 1) (+ len 1) (cons (string c) text))))))))
	  ((pair? obj)
	   (let* ((l (out-regular-list obj))
		  (result (out-pp-aux-list l (+ left 1) (- right 1)))
		  (multiline? (vector-ref result 0))
		  (width-all  (vector-ref result 1))
		  (width-max  (vector-ref result 2))
		  (width-last (vector-ref result 3))
		  (text-l     (vector-ref result 4)))
	     (if multiline?
		 (let* ((sep (list #\newline (out-blanks left)))
			(formatted-text (out-separate text-l sep))
			(text (list "(" formatted-text ")")))
		   (vector #t
			   (+ (max width-max (+ width-last 1)) 1)
			   (+ width-last 2)
			   text))
		 (let* ((sep (list " "))
			(formatted-text (out-separate text-l sep))
			(text (list "(" formatted-text ")")))
		   (vector #f (+ width-all 2) (+ width-all 2) text)))))
	  ((and (vector? obj) (zero? (vector-length obj)))
	   (vector #f 3 3 '("#()")))
	  ((vector? obj)
	   (let* ((l (vector->list obj))
		  (result (out-pp-aux-list l (+ left 2) (- right 1)))
		  (multiline? (vector-ref result 0))
		  (width-all  (vector-ref result 1))
		  (width-max  (vector-ref result 2))
		  (width-last (vector-ref result 3))
		  (text-l     (vector-ref result 4)))
	     (if multiline?
		 (let* ((sep (list #\newline (out-blanks (+ left 1))))
			(formatted-text (out-separate text-l sep))
			(text (list "#(" formatted-text ")")))
		   (vector #t
			   (+ (max width-max (+ width-last 1)) 2)
			   (+ width-last 3)
			   text))
		 (let* ((sep (list " "))
			(formatted-text (out-separate text-l sep))
			(text (list "#(" formatted-text ")")))
		   (vector #f (+ width-all 3) (+ width-all 3) text)))))
	  (else
	   (display "Internal error: out-pp")
	   (newline)))))

; Retourne la chaine a afficher
(define out-pp
  (lambda (obj col)
    (let* ((list-rec-of-strings-n-chars
	    (vector-ref (out-pp-aux obj col out-max-col) 3))
	   (list-of-strings-n-chars
	    (out-flatten-list list-rec-of-strings-n-chars))
	   (list-of-strings
	    (map out-force-string list-of-strings-n-chars)))
      (apply string-append list-of-strings))))




;
; Nice-printer, plus rapide mais moins beau que le pretty-printer
;

(define out-np
  (lambda (obj start)
    (letrec ((line-pad
	      (string-append (string #\newline)
			     (out-blanks (- start 1))))
	     (step-line
	      (lambda (p)
		(set-car! p line-pad)))
	     (p-bool
	      (lambda (obj col objw texts hole cont)
		(let ((text (if obj "#t" "#f")))
		  (cont (+ col 2) (+ objw 2) (cons text texts) hole))))
	     (p-number
	      (lambda (obj col objw texts hole cont)
		(let* ((text (number->string obj))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-symbol
	      (lambda (obj col objw texts hole cont)
		(let* ((text (symbol->string obj))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-char
	      (lambda (obj col objw texts hole cont)
		(let* ((text
			(cond ((char=? obj #\space) "#\\space")
			      ((char=? obj #\newline) "#\\newline")
			      (else (string-append "#\\" (string obj)))))
		       (len (string-length text)))
		  (cont (+ col len) (+ objw len) (cons text texts) hole))))
	     (p-list
	      (lambda (obj col objw texts hole cont)
		(p-tail obj (+ col 1) (+ objw 1) (cons "(" texts) hole cont)))
	     (p-vector
	      (lambda (obj col objw texts hole cont)
		(p-list (vector->list obj)
			(+ col 1) (+ objw 1) (cons "#" texts) hole cont)))
	     (p-tail
	      (lambda (obj col objw texts hole cont)
		(if (null? obj)
		    (cont (+ col 1) (+ objw 1) (cons ")" texts) hole)
		    (p-obj (car obj) col objw texts hole
			   (make-cdr-cont obj cont)))))
	     (make-cdr-cont
	      (lambda (obj cont)
		(lambda (col objw texts hole)
		  (cond ((null? (cdr obj))
			 (cont (+ col 1) (+ objw 1) (cons ")" texts) hole))
			((> col out-max-col)
			 (step-line hole)
			 (let ((hole2 (cons " " texts)))
			   (p-cdr obj (+ start objw 1) 0 hole2 hole2 cont)))
			(else
			 (let ((hole2 (cons " " texts)))
			   (p-cdr obj (+ col 1) 0 hole2 hole2 cont)))))))
	     (p-cdr
	      (lambda (obj col objw texts hole cont)
		(if (pair? (cdr obj))
		    (p-tail (cdr obj) col objw texts hole cont)
		    (p-dot col objw texts hole
			   (make-cdr-cont (list #f (cdr obj)) cont)))))
	     (p-dot
	      (lambda (col objw texts hole cont)
		(cont (+ col 1) (+ objw 1) (cons "." texts) hole)))
	     (p-obj
	      (lambda (obj col objw texts hole cont)
		(cond ((boolean? obj)
		       (p-bool obj col objw texts hole cont))
		      ((number? obj)
		       (p-number obj col objw texts hole cont))
		      ((symbol? obj)
		       (p-symbol obj col objw texts hole cont))
		      ((char? obj)
		       (p-char obj col objw texts hole cont))
		      ((or (null? obj) (pair? obj))
		       (p-list obj col objw texts hole cont))
		      ((vector? obj)
		       (p-vector obj col objw texts hole cont))))))
      (p-obj obj start 0 '() (cons #f #f)
	     (lambda (col objw texts hole)
	       (if (> col out-max-col)
		   (step-line hole))
	       (apply string-append (reverse texts)))))))


;;;; output table functions

(define (out-print-table options
			 <<EOF>>-action <<ERROR>>-action rules
			 nl-start no-nl-start arcs-v acc-v
			 output-port)
  ;;Print the lexer table.
  ;;
  (let-keywords options #t ((input-file		:input-file	#f)
			    (table-name		:table-name	#f)
			    (pretty?		:pretty-print	#f)
			    (counters-type	:counters	'line)
			    (lexer-format	:lexer-format	'decision-tree))
    (let* ((counters-param-list	(case  counters-type
		;NOTE: The leading space in the result is important.
				  ((none)	")")
				  ((line)	" yyline)")
				  (else		" yyline yycolumn yyoffset)")))
	   (counters-param-list-short
	    (if (char=? (string-ref counters-param-list 0) #\space)
		(substring counters-param-list
			   1
			   (string-length counters-param-list))
	      counters-param-list))
	   (clean-eof-action	(out-clean-action <<EOF>>-action))
	   (clean-error-action	(out-clean-action <<ERROR>>-action))
	   (rule-op		(lambda (rule)
				  (out-clean-action (get-rule-action rule))))
	   (rules-l		(vector->list rules))
	   (clean-actions-l	(map rule-op rules-l))
	   (yytext?-l		(map get-rule-yytext? rules-l)))

      ;;Preamble of comments.
      (display ";\n" output-port)
      (display "; Table generated from the file " output-port)
      (display input-file output-port)
      (display " by SILex 1.0" output-port)
      (newline output-port)
      (display ";\n\n" output-port)

      ;;Print the opening of the table.
      (display "(define " output-port)
      (display table-name output-port)
      (newline output-port)
      (display "  (vector\n" output-port)

      ;;Print the description of the selected counters.  This is the value
      ;;of the "counters" option.
      (display "   '" output-port)
      (write counters-type output-port)
      (newline output-port)

      ;;Print  the  action function  to  call  when  the lexer  finds  the
      ;;end-of-file.
      (display "   (lambda (yycontinue yygetc yyungetc)\n" output-port)
      (display "     (lambda (yytext" output-port)
      (display counters-param-list output-port)
      (newline output-port)
      (display clean-eof-action output-port)
      (display "       ))\n" output-port)

      ;;Print the action function to call when the lexer finds an error in
      ;;the input.
      (display "   (lambda (yycontinue yygetc yyungetc)\n" output-port)
      (display "     (lambda (yytext" output-port)
      (display counters-param-list output-port)
      (newline output-port)
      (display clean-error-action output-port)
      (display "       ))\n" output-port)

      ;;Print the subvector of action  functions for the lexer rules which
      ;;is terminated by the automaton itself, in the selected format.
      (display "   (vector\n" output-port)
      (let loop ((al clean-actions-l) (yyl yytext?-l))
	(if (pair? al)
	    (let ((yytext? (car yyl)))
	      (display "    " output-port)
	      (write yytext? output-port)
	      (newline output-port)
	      (display "    (lambda (yycontinue yygetc yyungetc)\n" output-port)
	      (if yytext?
		  (begin
		    (display "      (lambda (yytext" output-port)
		    (display counters-param-list output-port))
		(begin
		  (display "      (lambda (" output-port)
		  (display counters-param-list-short output-port)))
	      (newline output-port)
	      (display (car al) output-port)
	      (display "        ))" output-port)
	      (when (pair? (cdr al))
		(newline output-port))
	      (loop (cdr al) (cdr yyl)))))
      (display ")\n" output-port)

      ;;Print  the  automaton  in  one  of the  three  supported  formats:
      ;;portable, scheme code, raw data.
      (case lexer-format
	((portable)
	 (out-print-table-chars pretty?
				nl-start no-nl-start arcs-v acc-v
				output-port))
	((code)
	 (out-print-table-code counters-type (vector-length rules) yytext?-l
			       nl-start no-nl-start arcs-v acc-v
			       output-port))
	((decision-tree)
	 (out-print-table-data pretty?
			       nl-start no-nl-start arcs-v acc-v
			       output-port))
	(else
	 (assertion-violation 'lex
	   "unknown lexer output format" lexer-format)))
      (display "))\n" output-port)))) ;terminate the subvector and the vector

(define (out-print-table-data pretty? nl-start no-nl-start arcs-v acc-v port)
  ;;Print the table  in the decision tree format, which  is the raw data
  ;;format.
  (let* ((len (vector-length arcs-v))
	 (trees-v (make-vector len)))
    (let loop ((i 0))
      (when (< i len)
	(vector-set! trees-v i (prep-arcs->tree (vector-ref arcs-v i)))
	(loop (+ i 1))))

		; Decrire le format de l'automate
    (display "   'decision-trees" port)
    (newline port)

		; Ecrire l'etat de depart pour le cas "debut de la ligne"
    (display "   " port)
    (write nl-start port)
    (newline port)

		; Ecrire l'etat de depart pour le cas "pas au debut de la ligne"
    (display "   " port)
    (write no-nl-start port)
    (newline port)

		; Ecrire la table de transitions
    (display "   '" port)
    (if pretty?
	(display (out-pp trees-v 5) port)
      (display (out-np trees-v 5) port))
    (newline port)

		; Ecrire la table des acceptations
    (display "   '" port)
    (if pretty?
	(display (out-pp acc-v 5) port)
      (display (out-np acc-v 5) port))))

(define (out-print-table-chars pretty? nl-start no-nl-start arcs-v acc-v port)
  ;;Print the automation in the portable format.
  (let* ((len		(vector-length arcs-v))
	 (portable-v	(make-vector len))
	 (arc-op	(lambda (arc)
			  (cons (class->tagged-char-list (car arc))
				(cdr arc)))))
    (let loop ((s 0))
      (when (< s len)
	(let* ((arcs	  (vector-ref arcs-v s))
	       (port-arcs (map arc-op arcs)))
	  (vector-set! portable-v s port-arcs)
	  (loop (+ s 1)))))

		; Decrire le format de l'automate
    (display "   'tagged-chars-lists" port)
    (newline port)

		; Ecrire l'etat de depart pour le cas "debut de la ligne"
    (display "   " port)
    (write nl-start port)
    (newline port)

		; Ecrire l'etat de depart pour le cas "pas au debut de la ligne"
    (display "   " port)
    (write no-nl-start port)
    (newline port)

		; Ecrire la table de transitions
    (display "   '" port)
    (display ((if pretty? out-pp out-np) portable-v 5) port)
    (newline port)

		; Ecrire la table des acceptations
    (display "   '" port)
    (display ((if pretty? out-pp out-np) acc-v 5) port)))

(define (out-print-code-trans3 margin tree action-var port)
  ;;Generate the automaton in  Scheme code form.
  (newline port)
  (display (out-blanks margin) port)
  (cond ((eq? tree 'err)
	 (display action-var port))
	((number? tree)
	 (display "(state-" port)
	 (display tree port)
	 (display " " port)
	 (display action-var port)
	 (display ")" port))
	((eq? (car tree) '=)
	 (display "(if (= c " port)
	 (display (list-ref tree 1) port)
	 (display ")" port)
	 (out-print-code-trans3 (+ margin 4)
				(list-ref tree 2)
				action-var
				port)
	 (out-print-code-trans3 (+ margin 4)
				(list-ref tree 3)
				action-var
				port)
	 (display ")" port))
	(else
	 (display "(if (< c " port)
	 (display (list-ref tree 0) port)
	 (display ")" port)
	 (out-print-code-trans3 (+ margin 4)
				(list-ref tree 1)
				action-var
				port)
	 (out-print-code-trans3 (+ margin 4)
				(list-ref tree 2)
				action-var
				port)
	 (display ")" port))))

(define (out-print-code-trans2 margin tree action-var port)
  (display (string-append
	    "\n"
	    (out-blanks margin)
	    "(if c")
	   port)
  (out-print-code-trans3 (+ margin 4) tree action-var port)
  (display (string-append
	    "\n"
	    (out-blanks (+ margin 4))
	    action-var ")")
	   port))

(define (out-print-code-trans1 margin tree action-var port)
  (newline port)
  (display (out-blanks margin) port)
  (if (eq? tree 'err)
      (display action-var port)
    (begin
      (display "(let ((c (read-char)))" port)
      (out-print-code-trans2 (+ margin 2) tree action-var port)
      (display ")" port))))

(define (out-print-table-code counters nbrules yytext?-l
			      nl-start no-nl-start arcs-v acc-v
			      port)
  (let-values (((counters-params counters-params-short)
		(case counters
		  ((none) (values ")" ")"))
		  ((line) (values " yyline)"
				  "yyline)"))
		  ((all)  (values " yyline yycolumn yyoffset)"
				  "yyline yycolumn yyoffset)")))))
    (let* ((nbstates (vector-length arcs-v))
	   (trees-v (make-vector nbstates)))
      (let loop ((s 0))
	(if (< s nbstates)
	    (begin
	      (vector-set! trees-v s (prep-arcs->tree (vector-ref arcs-v s)))
	      (loop (+ s 1)))))

      ;;Print the format of the automaton.
      (display "   'code\n" port)

      (display (string-append
		;;Ecrire l'entete de la fonction
		"   (lambda (<<EOF>>-pre-action\n"
		"            <<ERROR>>-pre-action\n"
		"            rules-pre-action\n"
		"            IS)\n"
		;;Ecrire le  debut du letrec et  les variables d'actions
		;;brutes.
		"     (letrec\n"
		"         ((user-action-<<EOF>> #f)\n"
		"          (user-action-<<ERROR>> #f)\n")
	       port)
      (let loop ((i 0))
	(when (< i nbrules)
	  (display "          (user-action-" port)
	  (write i port)
	  (display " #f)" port)
	  (newline port)
	  (loop (+ i 1))))

      (display (string-append
		;;Ecrire l'extraction des fonctions du IS.
		"          (start-go-to-end    (:lexer-start-go-to-end		IS))\n"
		"          (end-go-to-point    (:lexer-end-go-to-point		IS))\n"
		"          (init-lexeme        (:lexer-init-lexeme		IS))\n"
		"          (get-start-line     (:lexer-get-start-line		IS))\n"
		"          (get-start-column   (:lexer-get-start-column		IS))\n"
		"          (get-start-offset   (:lexer-get-start-offset		IS))\n"
		"          (peek-left-context  (:lexer-peek-left-context	IS))\n"
		"          (peek-char          (:lexer-peek-char		IS))\n"
		"          (read-char          (:lexer-read-char		IS))\n"
		"          (get-start-end-text (:lexer-get-start-end-text	IS))\n"
		"          (user-getc          (:lexer-user-getc		IS))\n"
		"          (user-ungetc        (:lexer-user-ungetc		IS))\n"
		;;Ecrire les variables d'actions.
		"          (action-<<EOF>>\n"
		"           (lambda (" counters-params-short "\n"
		"             (user-action-<<EOF>> \"\"" counters-params "))\n"
		"          (action-<<ERROR>>\n"
		"           (lambda (" counters-params-short "\n"
		"             (user-action-<<ERROR>> \"\"" counters-params "))\n")
	       port)

      (let loop ((i 0)
		 (yyl yytext?-l))
	(when (< i nbrules)
	  (display (string-append
		    "          (action-" (number->string i) "\n"
		    "           (lambda (" counters-params-short "\n"
		    (if (car yyl)
			(string-append
			 "             (let ((yytext (get-start-end-text)))\n"
			 "               (start-go-to-end)\n"
			 "               (user-action-" (number->string i) " yytext"
			 counters-params ")))\n")
		      (string-append
		       "             (start-go-to-end)\n"
		       "             (user-action-" (number->string i) counters-params "))\n")))
		   port)
	  (loop (+ i 1) (cdr yyl))))

      ;; Ecrire les variables d'etats
      (let loop ((s 0))
	(if (< s nbstates)
	    (let* ((tree (vector-ref trees-v s))
		   (acc (vector-ref acc-v s))
		   (acc-eol (car acc))
		   (acc-no-eol (cdr acc)))
	      (display (string-append
			"          (state-" (number->string s) "\n"
			"           (lambda (action)")
		       port)
	      (cond ((not acc-eol)
		     (out-print-code-trans1 13 tree "action" port))
		    ((not acc-no-eol)
		     (display (string-append
			       "\n"
			       (if (eq? tree 'err)
				   "             (let* ((c (peek-char))"
				 "             (let* ((c (read-char))")
			       "\n"
			       "                    (new-action (if (o"
			       "r (not c) (= c lexer-integer-newline))\n"
			       "                                  "
			       "  (begin (end-go-to-point) action-" (number->string acc-eol) ")\n"
			       "                       "
			       "             action)))")
			      port)
		     ((if (eq? tree 'err)
			  out-print-code-trans1
			out-print-code-trans2) 15 tree "new-action" port)
		     (display ")" port))
		    ((< acc-eol acc-no-eol)
		     (display
		      (string-append
		       "\n"
		       "             (end-go-to-point)\n"
		       "             (let* ((c (" (if (eq? tree 'err) "peek-char" "read-char") "))\n"
		       "                    (new-action (if (or (not c) (= c lexer-integer-newline))\n"
		       "                      "
		       "              action-" (number->string acc-eol) "\n"
		       "                      "
		       "              action-" (number->string acc-no-eol) ")))")
		      port)
		     ((if (eq? tree 'err)
			  out-print-code-trans1
			out-print-code-trans2) 15 tree "new-action" port)
		     (display ")" port))
		    (else
		     (let ((action-var (string-append "action-" (number->string acc-eol))))
		       (display "\n             (end-go-to-point)" port)
		       (out-print-code-trans1 13 tree action-var port))))
	      (display "))\n" port)
	      (loop (+ s 1)))))

		; Ecrire la variable de lancement de l'automate
      (display
       (string-append
	"          (start-automaton\n"
	"           (lambda ()\n"
	(if (= nl-start no-nl-start)
	    (string-append
	     "             (if (peek-char)\n"
	     "                 (state-" (number->string nl-start) " action-<<ERROR>>)\n"
	     "               action-<<EOF>>)")
	  (string-append
	   "             (cond ((not (peek-char))\n"
	   "                    action-<<EOF>>)\n"
	   "                   ((= (peek-left-context) lexer-integer-newline)\n"
	   "                    (state-" (number->string nl-start) " action-<<ERROR>>))\n"
	   "                   (else\n"
	   "                    (state-" (number->string no-nl-start) " action-<<ERROR>>)))"))
	"))\n"
	;; Ecrire la fonction principale de lexage
	"          (final-lexer\n"
	"           (lambda ()\n"
	"             (init-lexeme)\n"
	(cond ((eq? counters 'none)
	       "             ((start-automaton))")
	      ((eq? counters 'line)
	       (string-append
		"             (let ((yyline (get-start-line)))\n"
		"               ((start-automaton) yyline))"))
	      ((eq? counters 'all)
	       (string-append
		"             (let ((yyline (get-start-line))\n"
		"                   (yycolumn (get-start-column))\n"
		"                   (yyoffset (get-start-offset)))\n"
		"               ((start-automaton) yyline yycolumn yyoffset))")))
	"))"
	;; Fermer les bindings du grand letrec
	")\n"

	;;Initialiser les variables user-action-XX
	"       (set! user-action-<<EOF>>"
	" (<<EOF>>-pre-action\n"
	"                                  final-lexer user-getc user-ungetc))\n"
	"       (set! user-action-<<ERROR>>"
	" (<<ERROR>>-pre-action\n"
	"                                    final-lexer user-getc user-ungetc))\n")
       port)

      (let loop ((r 0))
	(when (< r nbrules)
	  (let* ((str-r  (number->string r))
		 (blanks (out-blanks (string-length str-r))))
	    (display (string-append
		      "       (set! user-action-" str-r " ((vector-ref rules-pre-action "
		      (number->string (+ (* 2 r) 1)) ")\n"
		      blanks
		      "                           final-lexer user-getc user-ungetc))\n")
		     port)
	    (loop (+ r 1)))))

      ;; Faire retourner le lexer final
      (display "       final-lexer))" port))))


;;;; output functions

(define (output options
		<<EOF>>-action <<ERROR>>-action rules
		nl-start no-nl-start arcs acc)
  ;;Print the output code.  This is invoked both when producing the full
  ;;lexer and when producing only the tables.
  ;;
  (define (library-spec->string-spec spec)
    ;;We allow the library specification  to be: a string, including the
    ;;parentheses; a symbol, to which  parentheses will be added; a list
    ;;of whatever, simple converted to string.
    ;;
    (cond ((string? spec)
	   spec)
	  ((symbol? spec)
	   (string-append "(" (symbol->string spec) ")"))
	  ((list? spec)
	   (string-append "(" (let-values (((port getter)
					    (open-string-output-port)))
				(write spec port)
				(getter))
			  ")"))
	  (else
	   (assertion-violation 'library-spec->string-spec
	     "invalid library name specification" spec))))
  (define (table-name->export-name table-name)
    ;;We allow the table name to be specified as string or symbol.
    ;;
    (cond ((string? table-name)
	   table-name)
	  ((symbol? table-name)
	   (symbol->string table-name))
	  (else
	   (assertion-violation 'table-name->export-name
	     "invalid table name specification" table-name))))

  (let-keywords options #t ((library-spec	:library-spec #f)
			    (output-file	:output-file  #f)
			    (output-port	:output-port  #f)
			    (table-name		:table-name   #f))
    (let ((opened-file? #f))
      (dynamic-wind
	  (lambda ()
	    (when (and output-file (not output-port))
	      (set! output-port (open-file-output-port output-file
						       (file-options no-fail)
						       (buffer-mode block)
						       (native-transcoder)))
	      (set! opened-file? #t)))
	  (lambda ()
	    (when library-spec
	      (display (string-append "(library "
				      (library-spec->string-spec library-spec)
				      "\n"
				      "  (export\n"
				      "    " (table-name->export-name table-name) ")\n"
				      "  (import (rnrs) (silex multilex))\n"
				      "\n")
		       output-port))
	    (out-print-table options
			     <<EOF>>-action <<ERROR>>-action rules
			     nl-start no-nl-start arcs acc
			     output-port)
	    (when library-spec
	      (display "\n) ; end of library\n\n" output-port)))
	  (lambda ()
	    ((if opened-file? close-output-port flush-output-port) output-port))))))


;;;; Module main.scm.

(define (lex . options)
  (let-keywords options #t ((input-file		:input-file	#f)
			    (input-port		:input-port	#f)
			    (input-string	:input-string	#f))
    (let ((close-port? #f))
      (dynamic-wind
	  (lambda ()
	    (set! input-port
		  (cond (input-string (open-string-input-port input-string))
			(input-port   input-port)
			(input-file   (begin0
					  (open-input-file input-file)
					(set! close-port? #t)))
			(else
			 (assertion-violation 'lex
			   "missing input method for lexer")))))
	  (lambda ()
	    (let* ((IS	       (lexer-make-IS :port input-port :counters 'all))
		   (action-lexer (lexer-make-lexer action-tables IS))
		   (class-lexer  (lexer-make-lexer class-tables  IS))
		   (macro-lexer  (lexer-make-lexer macro-tables  IS))
		   (regexp-lexer (lexer-make-lexer regexp-tables IS))
		   (string-lexer (lexer-make-lexer string-tables IS)))
	      (parameterize ((lexer-raw		#f)
			     (lexer-stack	'())
			     (lexer-alist	(list (cons 'action action-lexer)
						      (cons 'class  class-lexer)
						      (cons 'macro  macro-lexer)
						      (cons 'regexp regexp-lexer)
						      (cons 'string string-lexer)))
			     (lexer-buffer-empty? #t)
			     (lexer-buffer	#f)
			     (lexer-history	'())
			     (lexer-history-interp #f))
		(let-values (((<<EOF>>-action <<ERROR>>-action rules)
			      (adapt-rules (parse-rules (parse-macros)))))

		  (let-values (((nl-start no-nl-start arcs acc)
				(re2nfa rules)))

		    (let-values (((nl-start no-nl-start arcs acc)
				  (noeps nl-start no-nl-start arcs acc)))

		      (let-values (((nl-start no-nl-start arcs acc)
				    (sweep nl-start no-nl-start arcs acc)))

			(let-values (((nl-start no-nl-start arcs acc)
				      (nfa2dfa nl-start no-nl-start arcs acc)))
			  (prep-set-rules-yytext? rules)
			  (output options
				  <<EOF>>-action <<ERROR>>-action
				  rules nl-start no-nl-start arcs acc)
			  #t))))))))
	  (lambda ()
	    (when close-port?
	      (close-input-port input-port)))))))


;;;; helpers

(define (lex-error line column . message-strings)
  (assertion-violation #f
    (apply string-append "lex error: "
	   "line "   (if line   (number->string line)   "?")
	   "column " (if column (number->string column) "?")
	   ": " message-strings)))

(define-keyword :input-file)
(define-keyword :input-port)
(define-keyword :input-string)
(define-keyword :output-file)
(define-keyword :output-port)
(define-keyword :table-name)
(define-keyword :library-spec)
(define-keyword :pretty-print)
(define-keyword :lexer-format)

;;Already defined in (silex multilex)
;;(define-keyword :counters)

(define (lex-parse-options args)
  (let ((lex-recognized-options '(input-file input-port input-string
				  output-file output-port
				  table-name library-spec
				  counters portable code pretty-print))
	(lex-options-with-value '(input-file input-port input-string
				  output-file output-port
				  table-name library-spec
				  counters)))
    (let loop ((args args)
	       (ops  '()))
      (if (null? args)
	  ops
	(let ((sym (car args)))
	  (cond ((not (symbol? sym))
		 (lex-error #f #f "bad option list."))
		((not (memq sym lex-recognized-options))
		 (lex-error #f #f "unrecognized option \"" sym "\"."))
		((not (memq sym lex-options-with-value))
		 (loop (cdr args)
		       (cons (cons sym '()) ops)))
		((null? (cdr args))
		 (lex-error #f #f "the value of \"" sym "\" not specified."))
		(else
		 (loop (cddr args)
		       (cons (cons sym (cadr args)) ops)))))))))


;;;; done

)

;;; end of file
