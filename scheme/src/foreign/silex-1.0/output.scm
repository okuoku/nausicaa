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




;
; Fonction pour afficher une table
; Appelle la sous-routine adequate pour le type de fin de table
;

; Affiche la table d'un driver
(define out-print-table
  (lambda (args-alist
	   <<EOF>>-action <<ERROR>>-action rules
	   nl-start no-nl-start arcs-v acc-v
	   port)
    (let* ((filein
	    (cdr (assq 'filein args-alist)))
	   (table-name
	    (cdr (assq 'table-name args-alist)))
	   (pretty?
	    (assq 'pp args-alist))
	   (counters-type
	    (let ((a (assq 'counters args-alist)))
	      (if a (cdr a) 'line)))
	   (counters-param-list
	    (cond ((eq? counters-type 'none)
		   ")")
		  ((eq? counters-type 'line)
		   " yyline)")
		  (else ; 'all
		   " yyline yycolumn yyoffset)")))
	   (counters-param-list-short
	    (if (char=? (string-ref counters-param-list 0) #\space)
		(substring counters-param-list
			   1
			   (string-length counters-param-list))
		counters-param-list))
	   (clean-eof-action
	    (out-clean-action <<EOF>>-action))
	   (clean-error-action
	    (out-clean-action <<ERROR>>-action))
	   (rule-op
	    (lambda (rule) (out-clean-action (get-rule-action rule))))
	   (rules-l
	    (vector->list rules))
	   (clean-actions-l
	    (map rule-op rules-l))
	   (yytext?-l
	    (map get-rule-yytext? rules-l)))

      ; Commentaires prealables
      (display ";" port)
      (newline port)
      (display "; Table generated from the file " port)
      (display filein port)
      (display " by SILex 1.0" port)
      (newline port)
      (display ";" port)
      (newline port)
      (newline port)

      ; Ecrire le debut de la table
      (display "(define " port)
      (display table-name port)
      (newline port)
      (display "  (vector" port)
      (newline port)

      ; Ecrire la description du type de compteurs
      (display "   '" port)
      (write counters-type port)
      (newline port)

      ; Ecrire l'action pour la fin de fichier
      (display "   (lambda (yycontinue yygetc yyungetc)" port)
      (newline port)
      (display "     (lambda (yytext" port)
      (display counters-param-list port)
      (newline port)
      (display clean-eof-action port)
      (display "       ))" port)
      (newline port)

      ; Ecrire l'action pour le cas d'erreur
      (display "   (lambda (yycontinue yygetc yyungetc)" port)
      (newline port)
      (display "     (lambda (yytext" port)
      (display counters-param-list port)
      (newline port)
      (display clean-error-action port)
      (display "       ))" port)
      (newline port)

      ; Ecrire le vecteur des actions des regles ordinaires
      (display "   (vector" port)
      (newline port)
      (let loop ((al clean-actions-l) (yyl yytext?-l))
	(if (pair? al)
	    (let ((yytext? (car yyl)))
	      (display "    " port)
	      (write yytext? port)
	      (newline port)
	      (display "    (lambda (yycontinue yygetc yyungetc)" port)
	      (newline port)
	      (if yytext?
		  (begin
		    (display "      (lambda (yytext" port)
		    (display counters-param-list port))
		  (begin
		    (display "      (lambda (" port)
		    (display counters-param-list-short port)))
	      (newline port)
	      (display (car al) port)
	      (display "        ))" port)
	      (if (pair? (cdr al))
		  (newline port))
	      (loop (cdr al) (cdr yyl)))))
      (display ")" port)
      (newline port)

      ; Ecrire l'automate
      (cond ((assq 'portable args-alist)
	     (out-print-table-chars
	      pretty?
	      nl-start no-nl-start arcs-v acc-v
	      port))
	    ((assq 'code args-alist)
	     (out-print-table-code
	      counters-type (vector-length rules) yytext?-l
	      nl-start no-nl-start arcs-v acc-v
	      port))
	    (else
	     (out-print-table-data
	      pretty?
	      nl-start no-nl-start arcs-v acc-v
	      port))))))

;
; Affiche l'automate sous forme d'arbres de decision
; Termine la table du meme coup
;

(define out-print-table-data
  (lambda (pretty? nl-start no-nl-start arcs-v acc-v port)
    (let* ((len (vector-length arcs-v))
	   (trees-v (make-vector len)))
      (let loop ((i 0))
	(if (< i len)
	    (begin
	      (vector-set! trees-v i (prep-arcs->tree (vector-ref arcs-v i)))
	      (loop (+ i 1)))))

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
	  (display (out-np acc-v 5) port))

      ; Ecrire la fin de la table
      (display "))" port)
      (newline port))))

;
; Affiche l'automate sous forme de listes de caracteres taggees
; Termine la table du meme coup
;

(define out-print-table-chars
  (lambda (pretty? nl-start no-nl-start arcs-v acc-v port)
    (let* ((len (vector-length arcs-v))
	   (portable-v (make-vector len))
	   (arc-op (lambda (arc)
		     (cons (class->tagged-char-list (car arc)) (cdr arc)))))
      (let loop ((s 0))
	(if (< s len)
	    (let* ((arcs (vector-ref arcs-v s))
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
      (if pretty?
	  (display (out-pp portable-v 5) port)
	  (display (out-np portable-v 5) port))
      (newline port)

      ; Ecrire la table des acceptations
      (display "   '" port)
      (if pretty?
	  (display (out-pp acc-v 5) port)
	  (display (out-np acc-v 5) port))

      ; Ecrire la fin de la table
      (display "))" port)
      (newline port))))

;
; Genere l'automate en code Scheme
; Termine la table du meme coup
;

(define out-print-code-trans3
  (lambda (margin tree action-var port)
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
	   (display ")" port)))))

(define out-print-code-trans2
  (lambda (margin tree action-var port)
    (newline port)
    (display (out-blanks margin) port)
    (display "(if c" port)
    (out-print-code-trans3 (+ margin 4) tree action-var port)
    (newline port)
    (display (out-blanks (+ margin 4)) port)
    (display action-var port)
    (display ")" port)))

(define out-print-code-trans1
  (lambda (margin tree action-var port)
    (newline port)
    (display (out-blanks margin) port)
    (if (eq? tree 'err)
	(display action-var port)
	(begin
	  (display "(let ((c (read-char)))" port)
	  (out-print-code-trans2 (+ margin 2) tree action-var port)
	  (display ")" port)))))

(define out-print-table-code
  (lambda (counters nbrules yytext?-l
	   nl-start no-nl-start arcs-v acc-v
	   port)
    (let* ((counters-params
	    (cond ((eq? counters 'none) ")")
		  ((eq? counters 'line) " yyline)")
		  ((eq? counters 'all)  " yyline yycolumn yyoffset)")))
	   (counters-params-short
	    (cond ((eq? counters 'none) ")")
		  ((eq? counters 'line) "yyline)")
		  ((eq? counters 'all)  "yyline yycolumn yyoffset)")))
	   (nbstates (vector-length arcs-v))
	   (trees-v (make-vector nbstates)))
      (let loop ((s 0))
	(if (< s nbstates)
	    (begin
	      (vector-set! trees-v s (prep-arcs->tree (vector-ref arcs-v s)))
	      (loop (+ s 1)))))

      ; Decrire le format de l'automate
      (display "   'code" port)
      (newline port)

      ; Ecrire l'entete de la fonction
      (display "   (lambda (<<EOF>>-pre-action" port)
      (newline port)
      (display "            <<ERROR>>-pre-action" port)
      (newline port)
      (display "            rules-pre-action" port)
      (newline port)
      (display "            IS)" port)
      (newline port)

      ; Ecrire le debut du letrec et les variables d'actions brutes
      (display "     (letrec" port)
      (newline port)
      (display "         ((user-action-<<EOF>> #f)" port)
      (newline port)
      (display "          (user-action-<<ERROR>> #f)" port)
      (newline port)
      (let loop ((i 0))
	(if (< i nbrules)
	    (begin
	      (display "          (user-action-" port)
	      (write i port)
	      (display " #f)" port)
	      (newline port)
	      (loop (+ i 1)))))

      ; Ecrire l'extraction des fonctions du IS
      (display "          (start-go-to-end    " port)
      (display "(cdr (assq 'start-go-to-end IS)))" port)
      (newline port)
      (display "          (end-go-to-point    " port)
      (display "(cdr (assq 'end-go-to-point IS)))" port)
      (newline port)
      (display "          (init-lexeme        " port)
      (display "(cdr (assq 'init-lexeme IS)))" port)
      (newline port)
      (display "          (get-start-line     " port)
      (display "(cdr (assq 'get-start-line IS)))" port)
      (newline port)
      (display "          (get-start-column   " port)
      (display "(cdr (assq 'get-start-column IS)))" port)
      (newline port)
      (display "          (get-start-offset   " port)
      (display "(cdr (assq 'get-start-offset IS)))" port)
      (newline port)
      (display "          (peek-left-context  " port)
      (display "(cdr (assq 'peek-left-context IS)))" port)
      (newline port)
      (display "          (peek-char          " port)
      (display "(cdr (assq 'peek-char IS)))" port)
      (newline port)
      (display "          (read-char          " port)
      (display "(cdr (assq 'read-char IS)))" port)
      (newline port)
      (display "          (get-start-end-text " port)
      (display "(cdr (assq 'get-start-end-text IS)))" port)
      (newline port)
      (display "          (user-getc          " port)
      (display "(cdr (assq 'user-getc IS)))" port)
      (newline port)
      (display "          (user-ungetc        " port)
      (display "(cdr (assq 'user-ungetc IS)))" port)
      (newline port)

      ; Ecrire les variables d'actions
      (display "          (action-<<EOF>>" port)
      (newline port)
      (display "           (lambda (" port)
      (display counters-params-short port)
      (newline port)
      (display "             (user-action-<<EOF>> \"\"" port)
      (display counters-params port)
      (display "))" port)
      (newline port)
      (display "          (action-<<ERROR>>" port)
      (newline port)
      (display "           (lambda (" port)
      (display counters-params-short port)
      (newline port)
      (display "             (user-action-<<ERROR>> \"\"" port)
      (display counters-params port)
      (display "))" port)
      (newline port)
      (let loop ((i 0) (yyl yytext?-l))
	(if (< i nbrules)
	    (begin
	      (display "          (action-" port)
	      (display i port)
	      (newline port)
	      (display "           (lambda (" port)
	      (display counters-params-short port)
	      (newline port)
	      (if (car yyl)
		  (begin
		    (display "             (let ((yytext" port)
		    (display " (get-start-end-text)))" port)
		    (newline port)
		    (display "               (start-go-to-end)" port)
		    (newline port)
		    (display "               (user-action-" port)
		    (display i port)
		    (display " yytext" port)
		    (display counters-params port)
		    (display ")))" port)
		    (newline port))
		  (begin
		    (display "             (start-go-to-end)" port)
		    (newline port)
		    (display "             (user-action-" port)
		    (display i port)
		    (display counters-params port)
		    (display "))" port)
		    (newline port)))
	      (loop (+ i 1) (cdr yyl)))))

      ; Ecrire les variables d'etats
      (let loop ((s 0))
	(if (< s nbstates)
	    (let* ((tree (vector-ref trees-v s))
		   (acc (vector-ref acc-v s))
		   (acc-eol (car acc))
		   (acc-no-eol (cdr acc)))
	      (display "          (state-" port)
	      (display s port)
	      (newline port)
	      (display "           (lambda (action)" port)
	      (cond ((not acc-eol)
		     (out-print-code-trans1 13 tree "action" port))
		    ((not acc-no-eol)
		     (newline port)
		     (if (eq? tree 'err)
			 (display "             (let* ((c (peek-char))" port)
			 (display "             (let* ((c (read-char))" port))
		     (newline port)
		     (display "                    (new-action (if (o" port)
		     (display "r (not c) (= c lexer-integer-newline))" port)
		     (newline port)
		     (display "                                  " port)
		     (display "  (begin (end-go-to-point) action-" port)
		     (display acc-eol port)
		     (display ")" port)
		     (newline port)
		     (display "                       " port)
		     (display "             action)))" port)
		     (if (eq? tree 'err)
			 (out-print-code-trans1 15 tree "new-action" port)
			 (out-print-code-trans2 15 tree "new-action" port))
		     (display ")" port))
		    ((< acc-eol acc-no-eol)
		     (newline port)
		     (display "             (end-go-to-point)" port)
		     (newline port)
		     (if (eq? tree 'err)
			 (display "             (let* ((c (peek-char))" port)
			 (display "             (let* ((c (read-char))" port))
		     (newline port)
		     (display "                    (new-action (if (o" port)
		     (display "r (not c) (= c lexer-integer-newline))" port)
		     (newline port)
		     (display "                      " port)
		     (display "              action-" port)
		     (display acc-eol port)
		     (newline port)
		     (display "                      " port)
		     (display "              action-" port)
		     (display acc-no-eol port)
		     (display ")))" port)
		     (if (eq? tree 'err)
			 (out-print-code-trans1 15 tree "new-action" port)
			 (out-print-code-trans2 15 tree "new-action" port))
		     (display ")" port))
		    (else
		     (let ((action-var
			    (string-append "action-"
					   (number->string acc-eol))))
		       (newline port)
		       (display "             (end-go-to-point)" port)
		       (out-print-code-trans1 13 tree action-var port))))
	      (display "))" port)
	      (newline port)
	      (loop (+ s 1)))))

      ; Ecrire la variable de lancement de l'automate
      (display "          (start-automaton" port)
      (newline port)
      (display "           (lambda ()" port)
      (newline port)
      (if (= nl-start no-nl-start)
	  (begin
	    (display "             (if (peek-char)" port)
	    (newline port)
	    (display "                 (state-" port)
	    (display nl-start port)
	    (display " action-<<ERROR>>)" port)
	    (newline port)
	    (display "                 action-<<EOF>>)" port))
	  (begin
	    (display "             (cond ((not (peek-char))" port)
	    (newline port)
	    (display "                    action-<<EOF>>)" port)
	    (newline port)
	    (display "                   ((= (peek-left-context)" port)
	    (display " lexer-integer-newline)" port)
	    (newline port)
	    (display "                    (state-" port)
	    (display nl-start port)
	    (display " action-<<ERROR>>))" port)
	    (newline port)
	    (display "                   (else" port)
	    (newline port)
	    (display "                    (state-" port)
	    (display no-nl-start port)
	    (display " action-<<ERROR>>)))" port)))
      (display "))" port)
      (newline port)

      ; Ecrire la fonction principale de lexage
      (display "          (final-lexer" port)
      (newline port)
      (display "           (lambda ()" port)
      (newline port)
      (display "             (init-lexeme)" port)
      (newline port)
      (cond ((eq? counters 'none)
	     (display "             ((start-automaton))" port))
	    ((eq? counters 'line)
	     (display "             (let ((yyline (get-start-line)))" port)
	     (newline port)
	     (display "               ((start-automaton) yyline))" port))
	    ((eq? counters 'all)
	     (display "             (let ((yyline (get-start-line))" port)
	     (newline port)
	     (display "                   (yycolumn (get-start-column))" port)
	     (newline port)
	     (display "                   (yyoffset (get-start-offset)))" port)
	     (newline port)
	     (display "               ((start-automat" port)
	     (display "on) yyline yycolumn yyoffset))" port)))
      (display "))" port)

      ; Fermer les bindings du grand letrec
      (display ")" port)
      (newline port)

      ; Initialiser les variables user-action-XX
      (display "       (set! user-action-<<EOF>>" port)
      (display " (<<EOF>>-pre-action" port)
      (newline port)
      (display "                                  final-lexer" port)
      (display " user-getc user-ungetc))" port)
      (newline port)
      (display "       (set! user-action-<<ERROR>>" port)
      (display " (<<ERROR>>-pre-action" port)
      (newline port)
      (display "                                    final-lexer" port)
      (display " user-getc user-ungetc))" port)
      (newline port)
      (let loop ((r 0))
	(if (< r nbrules)
	    (let* ((str-r (number->string r))
		   (blanks (out-blanks (string-length str-r))))
	      (display "       (set! user-action-" port)
	      (display str-r port)
	      (display " ((vector-ref rules-pre-action " port)
	      (display (number->string (+ (* 2 r) 1)) port)
	      (display ")" port)
	      (newline port)
	      (display blanks port)
	      (display "                           final-lexer " port)
	      (display "user-getc user-ungetc))" port)
	      (newline port)
	      (loop (+ r 1)))))

      ; Faire retourner le lexer final et fermer la table au complet
      (display "       final-lexer))))" port)
      (newline port))))

;
; Fonctions necessaires a l'initialisation automatique du lexer
;

(define out-print-driver-functions
  (lambda (args-alist port)
    (let ((counters   (cdr (or (assq 'counters args-alist) '(z . line))))
	  (table-name (cdr (assq 'table-name args-alist))))
      (display ";" port)
      (newline port)
      (display "; User functions" port)
      (newline port)
      (display ";" port)
      (newline port)
      (newline port)
      (display "(define lexer #f)" port)
      (newline port)
      (newline port)
      (if (not (eq? counters 'none))
	  (begin
	    (display "(define lexer-get-line   #f)" port)
	    (newline port)
	    (if (eq? counters 'all)
		(begin
		  (display "(define lexer-get-column #f)" port)
		  (newline port)
		  (display "(define lexer-get-offset #f)" port)
		  (newline port)))))
      (display "(define lexer-getc       #f)" port)
      (newline port)
      (display "(define lexer-ungetc     #f)" port)
      (newline port)
      (newline port)
      (display "(define lexer-init" port)
      (newline port)
      (display "  (lambda (input-type input)" port)
      (newline port)
      (display "    (let ((IS (lexer-make-IS input-type input '" port)
      (write counters port)
      (display ")))" port)
      (newline port)
      (display "      (set! lexer (lexer-make-lexer " port)
      (display table-name port)
      (display " IS))" port)
      (newline port)
      (if (not (eq? counters 'none))
	  (begin
	    (display "      (set! lexer-get-line   (lexer-get-func-line IS))"
		     port)
	    (newline port)
	    (if (eq? counters 'all)
		(begin
		  (display
		   "      (set! lexer-get-column (lexer-get-func-column IS))"
		   port)
		  (newline port)
		  (display
		   "      (set! lexer-get-offset (lexer-get-func-offset IS))"
		   port)
		  (newline port)))))
      (display "      (set! lexer-getc       (lexer-get-func-getc IS))" port)
      (newline port)
      (display "      (set! lexer-ungetc     (lexer-get-func-ungetc IS)))))"
	       port)
      (newline port))))

;
; Fonction principale
; Affiche une table ou un driver complet
;

(define output
  (lambda (args-alist
	   <<EOF>>-action <<ERROR>>-action rules
	   nl-start no-nl-start arcs acc)
    (let* ((fileout          (cdr (assq 'fileout args-alist)))
	   (port             (open-output-file fileout))
	   (complete-driver? (cdr (assq 'complete-driver? args-alist))))
      (if complete-driver?
	  (begin
	    (out-print-run-time-lib port)
	    (newline port)))
      (out-print-table args-alist
		       <<EOF>>-action <<ERROR>>-action rules
		       nl-start no-nl-start arcs acc
		       port)
      (if complete-driver?
	  (begin
	    (newline port)
	    (out-print-driver-functions args-alist port)))
      (close-output-port port))))
