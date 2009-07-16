; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;
; Quelques definitions de constantes
;

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




;
; Fabrication de tables de dispatch
;

(define make-dispatch-table
  (lambda (size alist default)
    (let ((v (make-vector size default)))
      (let loop ((alist alist))
	(if (null? alist)
	    v
	    (begin
	      (vector-set! v (caar alist) (cdar alist))
	      (loop (cdr alist))))))))




;
; Fonctions de manipulation des tokens
;

(define make-tok
  (lambda (tok-type lexeme line column . attr)
    (cond ((null? attr)
	   (vector tok-type line column lexeme))
	  ((null? (cdr attr))
	   (vector tok-type line column lexeme (car attr)))
	  (else
	   (vector tok-type line column lexeme (car attr) (cadr attr))))))

(define get-tok-type     (lambda (tok) (vector-ref tok 0)))
(define get-tok-line     (lambda (tok) (vector-ref tok 1)))
(define get-tok-column   (lambda (tok) (vector-ref tok 2)))
(define get-tok-lexeme   (lambda (tok) (vector-ref tok 3)))
(define get-tok-attr     (lambda (tok) (vector-ref tok 4)))
(define get-tok-2nd-attr (lambda (tok) (vector-ref tok 5)))




;
; Fonctions de manipulations des regles
;

(define make-rule
  (lambda (line eof? error? bol? eol? regexp action)
    (vector line eof? error? bol? eol? regexp action #f)))

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




;
; Noeuds des regexp
;

(define epsilon-re  0)
(define or-re       1)
(define conc-re     2)
(define star-re     3)
(define plus-re     4)
(define question-re 5)
(define class-re    6)
(define char-re     7)

(define make-re
  (lambda (re-type . lattr)
    (cond ((null? lattr)
	   (vector re-type))
	  ((null? (cdr lattr))
	   (vector re-type (car lattr)))
	  ((null? (cddr lattr))
	   (vector re-type (car lattr) (cadr lattr))))))

(define get-re-type  (lambda (re) (vector-ref re 0)))
(define get-re-attr1 (lambda (re) (vector-ref re 1)))
(define get-re-attr2 (lambda (re) (vector-ref re 2)))




;
; Fonctions de manipulation des ensembles d'etats
;

; Intersection de deux ensembles d'etats
(define ss-inter
  (lambda (ss1 ss2)
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
		    (ss-inter ss1 (cdr ss2)))))))))

; Difference entre deux ensembles d'etats
(define ss-diff
  (lambda (ss1 ss2)
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
		    (ss-diff ss1 (cdr ss2)))))))))

; Union de deux ensembles d'etats
(define ss-union
  (lambda (ss1 ss2)
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
		    (cons t2 (ss-union ss1 (cdr ss2))))))))))

; Decoupage de deux ensembles d'etats
(define ss-sep
  (lambda (ss1 ss2)
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
		       (loop ss1 (cdr ss2) l c (cons t2 r))))))))))




;
; Fonctions de manipulation des classes de caracteres
;

; Comparaisons de bornes d'intervalles
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




;
; Fonction digraph
;

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




;
; Fonction de tri
;

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
