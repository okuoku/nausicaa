;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2001 Danny Dube'
;;;Port to R6RS and Nausicaa integration by Marco Maggi
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


#!r6rs
(library (silex lexer)
  (export
    lexer-make-lexer		lexer-make-IS
    lexer-get-func-getc		lexer-get-func-ungetc
    lexer-get-func-line		lexer-get-func-column
    lexer-get-func-offset

    :port :procedure :string :counters

    ;;Low level getters for the ":input-system" record fields.  They are
    ;;needed in the output tables with the "code" format.
    :input-system-start-go-to-end
    :input-system-end-go-to-point
    :input-system-init-lexeme
    :input-system-get-start-line
    :input-system-get-start-column
    :input-system-get-start-offset
    :input-system-peek-left-context
    :input-system-peek-char
    :input-system-read-char
    :input-system-get-start-end-text
    :input-system-get-user-line
    :input-system-get-user-column
    :input-system-get-user-offset
    :input-system-user-getc
    :input-system-user-ungetc)
  (import (rnrs)
    (keywords)
    (rnrs mutable-strings))


(define-keyword :port)
(define-keyword :procedure)
(define-keyword :string)
		;input methods
(define-keyword :counters)

(define lexer-init-buffer-len 1024)
(define lexer-integer-newline (char->integer #\newline))

(define-record-type (:input-system :input-system-make lexer?)
  (fields (immutable start-go-to-end)
	  (immutable end-go-to-point)
	  (immutable init-lexeme)
	  (immutable get-start-line)
	  (immutable get-start-column)
	  (immutable get-start-offset)
	  (immutable peek-left-context)
	  (immutable peek-char)
	  (immutable read-char)
	  (immutable get-start-end-text)
	  (immutable get-user-line)
	  (immutable get-user-column)
	  (immutable get-user-offset)
	  (immutable user-getc)
	  (immutable user-ungetc)))


(define (lexer-make-IS . options)
  (let-keywords options #f ((counters-type	:counters	'line)
			    (input-port		:port		#f)
			    (input-procedure	:procedure	#f)
			    (input-string	:string		#f))
    (let-values (((buffer read-ptr input-function)
		  (cond ((and input-string (string? input-string))
			 (values (string-append (string #\newline) input-string)
				 (+ 1 (string-length input-string))
				 (lambda () (eof-object))))
			((and input-port (input-port? input-port))
			 (values (make-string lexer-init-buffer-len #\newline)
				 1
				 (lambda () (read-char input-port))))
			((and input-procedure (procedure? input-procedure))
			 (values (make-string lexer-init-buffer-len #\newline)
				 1
				 input-procedure))
			(else
			 (assertion-violation 'lexer-make-IS
			   "input source was not specified")))))
      (lexer-raw-IS-maker buffer read-ptr input-function
			  (if (memq counters-type '(none line all))
			      counters-type
			    (assertion-violation 'lexer-make-IS
			      "invalid selection of counters type"
			      counters-type))))))

(define (lexer-make-lexer tables IS)
  (case (vector-ref tables 4) ; automaton type
    ((decision-trees)
     (lexer-make-tree-lexer tables IS))
    ((tagged-chars-lists)
     (lexer-make-char-lexer tables IS))
    ((code)
     (lexer-make-code-lexer tables IS))))


(define (lexer-raw-IS-maker buffer read-ptr input-f counters)
  (let ((input-f          input-f) ; Entree reelle
	(buffer           buffer)  ; Buffer
	(buflen           (string-length buffer))
	(read-ptr         read-ptr)
	(start-ptr        1) ; Marque de debut de lexeme
	(start-line       1)
	(start-column     1)
	(start-offset     0)
	(end-ptr          1) ; Marque de fin de lexeme
	(point-ptr        1) ; Le point
	(user-ptr         1) ; Marque de l'usager
	(user-line        1)
	(user-column      1)
	(user-offset      0)
	(user-up-to-date? #t)) ; Concerne la colonne seul.
    (letrec
	((start-go-to-end-none ; Fonctions de depl. des marques
	  (lambda ()
	    (set! start-ptr end-ptr)))
	 (start-go-to-end-line
	  (lambda ()
	    (let loop ((ptr start-ptr) (line start-line))
	      (if (= ptr end-ptr)
		  (begin
		    (set! start-ptr ptr)
		    (set! start-line line))
		(if (char=? (string-ref buffer ptr) #\newline)
		    (loop (+ ptr 1) (+ line 1))
		  (loop (+ ptr 1) line))))))
	 (start-go-to-end-all
	  (lambda ()
	    (set! start-offset (+ start-offset (- end-ptr start-ptr)))
	    (let loop ((ptr start-ptr)
		       (line start-line)
		       (column start-column))
	      (if (= ptr end-ptr)
		  (begin
		    (set! start-ptr ptr)
		    (set! start-line line)
		    (set! start-column column))
		(if (char=? (string-ref buffer ptr) #\newline)
		    (loop (+ ptr 1) (+ line 1) 1)
		  (loop (+ ptr 1) line (+ column 1)))))))
	 (start-go-to-user-none
	  (lambda ()
	    (set! start-ptr user-ptr)))
	 (start-go-to-user-line
	  (lambda ()
	    (set! start-ptr user-ptr)
	    (set! start-line user-line)))
	 (start-go-to-user-all
	  (lambda ()
	    (set! start-line user-line)
	    (set! start-offset user-offset)
	    (if user-up-to-date?
		(begin
		  (set! start-ptr user-ptr)
		  (set! start-column user-column))
	      (let loop ((ptr start-ptr) (column start-column))
		(if (= ptr user-ptr)
		    (begin
		      (set! start-ptr ptr)
		      (set! start-column column))
		  (if (char=? (string-ref buffer ptr) #\newline)
		      (loop (+ ptr 1) 1)
		    (loop (+ ptr 1) (+ column 1))))))))
	 (end-go-to-point
	  (lambda ()
	    (set! end-ptr point-ptr)))
	 (point-go-to-start
	  (lambda ()
	    (set! point-ptr start-ptr)))
	 (user-go-to-start-none
	  (lambda ()
	    (set! user-ptr start-ptr)))
	 (user-go-to-start-line
	  (lambda ()
	    (set! user-ptr start-ptr)
	    (set! user-line start-line)))
	 (user-go-to-start-all
	  (lambda ()
	    (set! user-ptr start-ptr)
	    (set! user-line start-line)
	    (set! user-column start-column)
	    (set! user-offset start-offset)
	    (set! user-up-to-date? #t)))
	 (init-lexeme-none ; Debute un nouveau lexeme
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-none))
	    (point-go-to-start)))
	 (init-lexeme-line
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-line))
	    (point-go-to-start)))
	 (init-lexeme-all
	  (lambda ()
	    (if (< start-ptr user-ptr)
		(start-go-to-user-all))
	    (point-go-to-start)))
	 (get-start-line ; Obtention des stats du debut du lxm
	  (lambda ()
	    start-line))
	 (get-start-column
	  (lambda ()
	    start-column))
	 (get-start-offset
	  (lambda ()
	    start-offset))
	 (peek-left-context ; Obtention de caracteres (#f si EOF)
	  (lambda ()
	    (char->integer (string-ref buffer (- start-ptr 1)))))
	 (peek-char
	  (lambda ()
	    (if (< point-ptr read-ptr)
		(char->integer (string-ref buffer point-ptr))
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer point-ptr c)
		      (set! read-ptr (+ point-ptr 1))
		      (char->integer c))
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    #f))))))
	 (read-char
	  (lambda ()
	    (if (< point-ptr read-ptr)
		(let ((c (string-ref buffer point-ptr)))
		  (set! point-ptr (+ point-ptr 1))
		  (char->integer c))
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer point-ptr c)
		      (set! read-ptr (+ point-ptr 1))
		      (set! point-ptr read-ptr)
		      (char->integer c))
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    #f))))))
	 (get-start-end-text ; Obtention du lexeme
	  (lambda ()
	    (substring buffer start-ptr end-ptr)))
	 (get-user-line-line ; Fonctions pour l'usager
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-line))
	    user-line))
	 (get-user-line-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    user-line))
	 (get-user-column-all
	  (lambda ()
	    (cond ((< user-ptr start-ptr)
		   (user-go-to-start-all)
		   user-column)
		  (user-up-to-date?
		   user-column)
		  (else
		   (let loop ((ptr start-ptr) (column start-column))
		     (if (= ptr user-ptr)
			 (begin
			   (set! user-column column)
			   (set! user-up-to-date? #t)
			   column)
		       (if (char=? (string-ref buffer ptr) #\newline)
			   (loop (+ ptr 1) 1)
			 (loop (+ ptr 1) (+ column 1)))))))))
	 (get-user-offset-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    user-offset))
	 (user-getc-none
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-none))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-getc-line
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-line))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  (if (char=? c #\newline)
		      (set! user-line (+ user-line 1)))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      (if (char=? c #\newline)
			  (set! user-line (+ user-line 1)))
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-getc-all
	  (lambda ()
	    (if (< user-ptr start-ptr)
		(user-go-to-start-all))
	    (if (< user-ptr read-ptr)
		(let ((c (string-ref buffer user-ptr)))
		  (set! user-ptr (+ user-ptr 1))
		  (if (char=? c #\newline)
		      (begin
			(set! user-line (+ user-line 1))
			(set! user-column 1))
		    (set! user-column (+ user-column 1)))
		  (set! user-offset (+ user-offset 1))
		  c)
	      (let ((c (input-f)))
		(if (char? c)
		    (begin
		      (if (= read-ptr buflen)
			  (reorganize-buffer))
		      (string-set! buffer user-ptr c)
		      (set! read-ptr (+ read-ptr 1))
		      (set! user-ptr read-ptr)
		      (if (char=? c #\newline)
			  (begin
			    (set! user-line (+ user-line 1))
			    (set! user-column 1))
			(set! user-column (+ user-column 1)))
		      (set! user-offset (+ user-offset 1))
		      c)
		  (begin
		    (set! input-f (lambda () (eof-object)))
		    (eof-object)))))))
	 (user-ungetc-none
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1)))))
	 (user-ungetc-line
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1))
	      (let ((c (string-ref buffer user-ptr)))
		(if (char=? c #\newline)
		    (set! user-line (- user-line 1)))))))
	 (user-ungetc-all
	  (lambda ()
	    (when (> user-ptr start-ptr)
	      (set! user-ptr (- user-ptr 1))
	      (let ((c (string-ref buffer user-ptr)))
		(if (char=? c #\newline)
		    (begin
		      (set! user-line (- user-line 1))
		      (set! user-up-to-date? #f))
		  (set! user-column (- user-column 1)))
		(set! user-offset (- user-offset 1))))))
	 (reorganize-buffer ; Decaler ou agrandir le buffer
	  (lambda ()
	    (if (< (* 2 start-ptr) buflen)
		(let* ((newlen (* 2 buflen))
		       (newbuf (make-string newlen))
		       (delta (- start-ptr 1)))
		  (let loop ((from (- start-ptr 1)))
		    (if (< from buflen)
			(begin
			  (string-set! newbuf
				       (- from delta)
				       (string-ref buffer from))
			  (loop (+ from 1)))))
		  (set! buffer    newbuf)
		  (set! buflen    newlen)
		  (set! read-ptr  (- read-ptr delta))
		  (set! start-ptr (- start-ptr delta))
		  (set! end-ptr   (- end-ptr delta))
		  (set! point-ptr (- point-ptr delta))
		  (set! user-ptr  (- user-ptr delta)))
	      (let ((delta (- start-ptr 1)))
		(let loop ((from (- start-ptr 1)))
		  (if (< from buflen)
		      (begin
			(string-set! buffer
				     (- from delta)
				     (string-ref buffer from))
			(loop (+ from 1)))))
		(set! read-ptr  (- read-ptr delta))
		(set! start-ptr (- start-ptr delta))
		(set! end-ptr   (- end-ptr delta))
		(set! point-ptr (- point-ptr delta))
		(set! user-ptr  (- user-ptr delta)))))))
      (:input-system-make
       (case counters
	 ((none) start-go-to-end-none)
	 ((line) start-go-to-end-line)
	 ((all)  start-go-to-end-all))
       end-go-to-point
       (case counters
	 ((none) init-lexeme-none)
	 ((line) init-lexeme-line)
	 ((all)  init-lexeme-all))
       get-start-line
       get-start-column
       get-start-offset
       peek-left-context
       peek-char
       read-char
       get-start-end-text
       (case counters
	 ((none) #f)
	 ((line) get-user-line-line)
	 ((all)  get-user-line-all))
       (case counters
	 ((none) #f)
	 ((line) #f)
	 ((all)  get-user-column-all))
       (case counters
	 ((none) #f)
	 ((line) #f)
	 ((all)  get-user-offset-all))
       (case counters
	 ((none) user-getc-none)
	 ((line) user-getc-line)
	 ((all)  user-getc-all))
       (case counters
	 ((none) user-ungetc-none)
	 ((line) user-ungetc-line)
	 ((all)  user-ungetc-all))))))


(define (lexer-get-func-getc IS)
  (:input-system-user-getc IS))

(define (lexer-get-func-ungetc IS)
  (:input-system-user-ungetc IS))

(define (lexer-get-func-line IS)
  (:input-system-get-user-line IS))

(define (lexer-get-func-column IS)
  (:input-system-get-user-column IS))

(define (lexer-get-func-offset IS)
  (:input-system-get-user-offset IS))


(define (lexer-make-tree-lexer tables IS)
  ;;Fabrication de lexer a partir d'arbres de decision.
  ;;
  (letrec
      (		; Contenu de la table
       (counters-type        (vector-ref tables 0))
       (<<EOF>>-pre-action   (vector-ref tables 1))
       (<<ERROR>>-pre-action (vector-ref tables 2))
       (rules-pre-actions    (vector-ref tables 3))
       (table-nl-start       (vector-ref tables 5))
       (table-no-nl-start    (vector-ref tables 6))
       (trees-v              (vector-ref tables 7))
       (acc-v                (vector-ref tables 8))

		; Contenu du IS
       (IS-start-go-to-end    (:input-system-start-go-to-end	IS))
       (IS-end-go-to-point    (:input-system-end-go-to-point	IS))
       (IS-init-lexeme        (:input-system-init-lexeme	IS))
       (IS-get-start-line     (:input-system-get-start-line	IS))
       (IS-get-start-column   (:input-system-get-start-column	IS))
       (IS-get-start-offset   (:input-system-get-start-offset	IS))
       (IS-peek-left-context  (:input-system-peek-left-context	IS))
       (IS-peek-char          (:input-system-peek-char		IS))
       (IS-read-char          (:input-system-read-char		IS))
       (IS-get-start-end-text (:input-system-get-start-end-text IS))
       (IS-get-user-line      (:input-system-get-user-line	IS))
       (IS-get-user-column    (:input-system-get-user-column	IS))
       (IS-get-user-offset    (:input-system-get-user-offset	IS))
       (IS-user-getc          (:input-system-user-getc		IS))
       (IS-user-ungetc        (:input-system-user-ungetc	IS))

		; Resultats
       (<<EOF>>-action   #f)
       (<<ERROR>>-action #f)
       (rules-actions    #f)
       (states           #f)
       (final-lexer      #f)

		; Gestion des hooks
       (hook-list '())
       (add-hook
	(lambda (thunk)
	  (set! hook-list (cons thunk hook-list))))
       (apply-hooks
	(lambda ()
	  (let loop ((l hook-list))
	    (if (pair? l)
		(begin
		  ((car l))
		  (loop (cdr l)))))))

		; Preparation des actions
       (set-action-statics
	(lambda (pre-action)
	  (pre-action final-lexer IS-user-getc IS-user-ungetc)))
       (prepare-special-action-none
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda ()
		     (action "")))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action-line
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda (yyline)
		     (action "" yyline)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action-all
	(lambda (pre-action)
	  (let ((action #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (action "" yyline yycolumn yyoffset)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-special-action
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-special-action-none pre-action))
	    ((line) (prepare-special-action-line pre-action))
	    ((all)  (prepare-special-action-all  pre-action)))))
       (prepare-action-yytext-none
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda ()
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext-line
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline)
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext yyline))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext-all
	(lambda (pre-action)
	  (let ((get-start-end-text IS-get-start-end-text)
		(start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (let ((yytext (get-start-end-text)))
		       (start-go-to-end)
		       (action yytext yyline yycolumn yyoffset))))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-yytext
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-action-yytext-none pre-action))
	    ((line) (prepare-action-yytext-line pre-action))
	    ((all)  (prepare-action-yytext-all  pre-action)))))
       (prepare-action-no-yytext-none
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda ()
		     (start-go-to-end)
		     (action)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext-line
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline)
		     (start-go-to-end)
		     (action yyline)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext-all
	(lambda (pre-action)
	  (let ((start-go-to-end    IS-start-go-to-end)
		(action             #f))
	    (let ((result
		   (lambda (yyline yycolumn yyoffset)
		     (start-go-to-end)
		     (action yyline yycolumn yyoffset)))
		  (hook
		   (lambda ()
		     (set! action (set-action-statics pre-action)))))
	      (add-hook hook)
	      result))))
       (prepare-action-no-yytext
	(lambda (pre-action)
	  (case counters-type
	    ((none) (prepare-action-no-yytext-none pre-action))
	    ((line) (prepare-action-no-yytext-line pre-action))
	    ((all)  (prepare-action-no-yytext-all  pre-action)))))

		; Fabrique les fonctions de dispatch
       (prepare-dispatch-err
	(lambda (leaf)
	  (lambda (c)
	    #f)))
       (prepare-dispatch-number
	(lambda (leaf)
	  (let ((state-function #f))
	    (let ((result
		   (lambda (c)
		     state-function))
		  (hook
		   (lambda ()
		     (set! state-function (vector-ref states leaf)))))
	      (add-hook hook)
	      result))))
       (prepare-dispatch-leaf
	(lambda (leaf)
	  (if (eq? leaf 'err)
	      (prepare-dispatch-err leaf)
	    (prepare-dispatch-number leaf))))
       (prepare-dispatch-<
	(lambda (tree)
	  (let ((left-tree  (list-ref tree 1))
		(right-tree (list-ref tree 2)))
	    (let ((bound      (list-ref tree 0))
		  (left-func  (prepare-dispatch-tree left-tree))
		  (right-func (prepare-dispatch-tree right-tree)))
	      (lambda (c)
		(if (< c bound)
		    (left-func c)
		  (right-func c)))))))
       (prepare-dispatch-=
	(lambda (tree)
	  (let ((left-tree  (list-ref tree 2))
		(right-tree (list-ref tree 3)))
	    (let ((bound      (list-ref tree 1))
		  (left-func  (prepare-dispatch-tree left-tree))
		  (right-func (prepare-dispatch-tree right-tree)))
	      (lambda (c)
		(if (= c bound)
		    (left-func c)
		  (right-func c)))))))
       (prepare-dispatch-tree
	(lambda (tree)
	  (cond ((not (pair? tree))
		 (prepare-dispatch-leaf tree))
		((eq? (car tree) '=)
		 (prepare-dispatch-= tree))
		(else
		 (prepare-dispatch-< tree)))))
       (prepare-dispatch
	(lambda (tree)
	  (let ((dicho-func (prepare-dispatch-tree tree)))
	    (lambda (c)
	      (and c (dicho-func c))))))

		; Fabrique les fonctions de transition (read & go) et (abort)
       (prepare-read-n-go
	(lambda (tree)
	  (let ((dispatch-func (prepare-dispatch tree))
		(read-char     IS-read-char))
	    (lambda ()
	      (dispatch-func (read-char))))))
       (prepare-abort
	(lambda (tree)
	  (lambda ()
	    #f)))
       (prepare-transition
	(lambda (tree)
	  (if (eq? tree 'err)
	      (prepare-abort     tree)
	    (prepare-read-n-go tree))))

		; Fabrique les fonctions d'etats ([set-end] & trans)
       (prepare-state-no-acc
	(lambda (s r1 r2)
	  (let ((trans-func (prepare-transition (vector-ref trees-v s))))
	    (lambda (action)
	      (let ((next-state (trans-func)))
		(if next-state
		    (next-state action)
		  action))))))
       (prepare-state-yes-no
	(lambda (s r1 r2)
	  (let ((peek-char       IS-peek-char)
		(end-go-to-point IS-end-go-to-point)
		(new-action1     #f)
		(trans-func (prepare-transition (vector-ref trees-v s))))
	    (let ((result
		   (lambda (action)
		     (let* ((c (peek-char))
			    (new-action
			     (if (or (not c) (= c lexer-integer-newline))
				 (begin
				   (end-go-to-point)
				   new-action1)
			       action))
			    (next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action1 (vector-ref rules-actions r1)))))
	      (add-hook hook)
	      result))))
       (prepare-state-diff-acc
	(lambda (s r1 r2)
	  (let ((end-go-to-point IS-end-go-to-point)
		(peek-char       IS-peek-char)
		(new-action1     #f)
		(new-action2     #f)
		(trans-func (prepare-transition (vector-ref trees-v s))))
	    (let ((result
		   (lambda (action)
		     (end-go-to-point)
		     (let* ((c (peek-char))
			    (new-action
			     (if (or (not c) (= c lexer-integer-newline))
				 new-action1
			       new-action2))
			    (next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action1 (vector-ref rules-actions r1))
		     (set! new-action2 (vector-ref rules-actions r2)))))
	      (add-hook hook)
	      result))))
       (prepare-state-same-acc
	(lambda (s r1 r2)
	  (let ((end-go-to-point IS-end-go-to-point)
		(trans-func (prepare-transition (vector-ref trees-v s)))
		(new-action #f))
	    (let ((result
		   (lambda (action)
		     (end-go-to-point)
		     (let ((next-state (trans-func)))
		       (if next-state
			   (next-state new-action)
			 new-action))))
		  (hook
		   (lambda ()
		     (set! new-action (vector-ref rules-actions r1)))))
	      (add-hook hook)
	      result))))
       (prepare-state
	(lambda (s)
	  (let* ((acc (vector-ref acc-v s))
		 (r1 (car acc))
		 (r2 (cdr acc)))
	    (cond ((not r1)  (prepare-state-no-acc   s r1 r2))
		  ((not r2)  (prepare-state-yes-no   s r1 r2))
		  ((< r1 r2) (prepare-state-diff-acc s r1 r2))
		  (else      (prepare-state-same-acc s r1 r2))))))

		; Fabrique la fonction de lancement du lexage a l'etat de depart
       (prepare-start-same
	(lambda (s1 s2)
	  (let ((peek-char    IS-peek-char)
		(eof-action   #f)
		(start-state  #f)
		(error-action #f))
	    (let ((result
		   (lambda ()
		     (if (not (peek-char))
			 eof-action
		       (start-state error-action))))
		  (hook
		   (lambda ()
		     (set! eof-action   <<EOF>>-action)
		     (set! start-state  (vector-ref states s1))
		     (set! error-action <<ERROR>>-action))))
	      (add-hook hook)
	      result))))
       (prepare-start-diff
	(lambda (s1 s2)
	  (let ((peek-char         IS-peek-char)
		(eof-action        #f)
		(peek-left-context IS-peek-left-context)
		(start-state1      #f)
		(start-state2      #f)
		(error-action      #f))
	    (let ((result
		   (lambda ()
		     (cond ((not (peek-char))
			    eof-action)
			   ((= (peek-left-context) lexer-integer-newline)
			    (start-state1 error-action))
			   (else
			    (start-state2 error-action)))))
		  (hook
		   (lambda ()
		     (set! eof-action <<EOF>>-action)
		     (set! start-state1 (vector-ref states s1))
		     (set! start-state2 (vector-ref states s2))
		     (set! error-action <<ERROR>>-action))))
	      (add-hook hook)
	      result))))
       (prepare-start
	(lambda ()
	  (let ((s1 table-nl-start)
		(s2 table-no-nl-start))
	    (if (= s1 s2)
		(prepare-start-same s1 s2)
	      (prepare-start-diff s1 s2)))))

		; Fabrique la fonction principale
       (prepare-lexer-none
	(lambda ()
	  (let ((init-lexeme IS-init-lexeme)
		(start-func  (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      ((start-func))))))
       (prepare-lexer-line
	(lambda ()
	  (let ((init-lexeme    IS-init-lexeme)
		(get-start-line IS-get-start-line)
		(start-func     (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      (let ((yyline (get-start-line)))
		((start-func) yyline))))))
       (prepare-lexer-all
	(lambda ()
	  (let ((init-lexeme      IS-init-lexeme)
		(get-start-line   IS-get-start-line)
		(get-start-column IS-get-start-column)
		(get-start-offset IS-get-start-offset)
		(start-func       (prepare-start)))
	    (lambda ()
	      (init-lexeme)
	      (let ((yyline   (get-start-line))
		    (yycolumn (get-start-column))
		    (yyoffset (get-start-offset)))
		((start-func) yyline yycolumn yyoffset))))))
       (prepare-lexer
	(lambda ()
	  (case counters-type
	    ((none) (prepare-lexer-none))
	    ((line) (prepare-lexer-line))
	    ((all)  (prepare-lexer-all))))))

		; Calculer la valeur de <<EOF>>-action et de <<ERROR>>-action
    (set! <<EOF>>-action   (prepare-special-action <<EOF>>-pre-action))
    (set! <<ERROR>>-action (prepare-special-action <<ERROR>>-pre-action))

		; Calculer la valeur de rules-actions
    (let* ((len (div (vector-length rules-pre-actions) 2))
	   (v (make-vector len)))
      (let loop ((r (- len 1)))
	(if (< r 0)
	    (set! rules-actions v)
	  (let* ((yytext? (vector-ref rules-pre-actions (* 2 r)))
		 (pre-action (vector-ref rules-pre-actions (+ (* 2 r) 1)))
		 (action (if yytext?
			     (prepare-action-yytext    pre-action)
			   (prepare-action-no-yytext pre-action))))
	    (vector-set! v r action)
	    (loop (- r 1))))))

		; Calculer la valeur de states
    (let* ((len (vector-length trees-v))
	   (v (make-vector len)))
      (let loop ((s (- len 1)))
	(if (< s 0)
	    (set! states v)
	  (begin
	    (vector-set! v s (prepare-state s))
	    (loop (- s 1))))))

		; Calculer la valeur de final-lexer
    (set! final-lexer (prepare-lexer))

		; Executer les hooks
    (apply-hooks)

		; Resultat
    final-lexer))


(define lexer-make-char-lexer
  ;; Fabrication de lexer a partir de listes de caracteres taggees
  ;;
  (let* ((char->class
	  (lambda (c)
	    (let ((n (char->integer c)))
	      (list (cons n n)))))
	 (merge-sort
	  (lambda (l combine zero-elt)
	    (if (null? l)
		zero-elt
		(let loop1 ((l l))
		  (if (null? (cdr l))
		      (car l)
		      (loop1
		       (let loop2 ((l l))
			 (cond ((null? l)
				l)
			       ((null? (cdr l))
				l)
			       (else
				(cons (combine (car l) (cadr l))
				      (loop2 (cddr l))))))))))))
	 (finite-class-union
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
			(if (<= r1start r2start)
			    (cond ((< (+ r1end 1) r2start)
				   (loop (cdr c1) c2 (cons r1 u)))
				  ((<= r1end r2end)
				   (loop (cdr c1)
					 (cons (cons r1start r2end) (cdr c2))
					 u))
				  (else
				   (loop c1 (cdr c2) u)))
			    (cond ((> r1start (+ r2end 1))
				   (loop c1 (cdr c2) (cons r2 u)))
				  ((>= r1end r2end)
				   (loop (cons (cons r2start r1end) (cdr c1))
					 (cdr c2)
					 u))
				  (else
				   (loop (cdr c1) c2 u))))))))))
	 (char-list->class
	  (lambda (cl)
	    (let ((classes (map char->class cl)))
	      (merge-sort classes finite-class-union '()))))
	 (class-<
	  (lambda (b1 b2)
	    (cond ((eq? b1 'inf+) #f)
		  ((eq? b2 'inf-) #f)
		  ((eq? b1 'inf-) #t)
		  ((eq? b2 'inf+) #t)
		  (else (< b1 b2)))))
	 (finite-class-compl
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
			(loop (cdr c) (+ rend 1))))))))
	 (tagged-chars->class
	  (lambda (tcl)
	    (let* ((inverse? (car tcl))
		   (cl (cdr tcl))
		   (class-tmp (char-list->class cl)))
	      (if inverse? (finite-class-compl class-tmp) class-tmp))))
	 (charc->arc
	  (lambda (charc)
	    (let* ((tcl (car charc))
		   (dest (cdr charc))
		   (class (tagged-chars->class tcl)))
	      (cons class dest))))
	 (arc->sharcs
	  (lambda (arc)
	    (let* ((range-l (car arc))
		   (dest (cdr arc))
		   (op (lambda (range) (cons range dest))))
	      (map op range-l))))
	 (class-<=
	  (lambda (b1 b2)
	    (cond ((eq? b1 'inf-) #t)
		  ((eq? b2 'inf+) #t)
		  ((eq? b1 'inf+) #f)
		  ((eq? b2 'inf-) #f)
		  (else (<= b1 b2)))))
	 (sharc-<=
	  (lambda (sharc1 sharc2)
	    (class-<= (caar sharc1) (caar sharc2))))
	 (merge-sharcs
	  (lambda (l1 l2)
	    (let loop ((l1 l1) (l2 l2))
	      (cond ((null? l1)
		     l2)
		    ((null? l2)
		     l1)
		    (else
		     (let ((sharc1 (car l1))
			   (sharc2 (car l2)))
		       (if (sharc-<= sharc1 sharc2)
			   (cons sharc1 (loop (cdr l1) l2))
			   (cons sharc2 (loop l1 (cdr l2))))))))))
	 (class-= eqv?)
	 (fill-error
	  (lambda (sharcs)
	    (let loop ((sharcs sharcs) (start 'inf-))
	      (cond ((class-= start 'inf+)
		     '())
		    ((null? sharcs)
		     (cons (cons (cons start 'inf+) 'err)
			   (loop sharcs 'inf+)))
		    (else
		     (let* ((sharc (car sharcs))
			    (h (caar sharc))
			    (t (cdar sharc)))
		       (if (class-< start h)
			   (cons (cons (cons start (- h 1)) 'err)
				 (loop sharcs h))
			   (cons sharc (loop (cdr sharcs)
					     (if (class-= t 'inf+)
						 'inf+
						 (+ t 1)))))))))))
	 (charcs->tree
	  (lambda (charcs)
	    (let* ((op (lambda (charc) (arc->sharcs (charc->arc charc))))
		   (sharcs-l (map op charcs))
		   (sorted-sharcs (merge-sort sharcs-l merge-sharcs '()))
		   (full-sharcs (fill-error sorted-sharcs))
		   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
		   (table (list->vector (map op full-sharcs))))
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
				(loop mid right))))))))))
    (lambda (tables IS)
      (let ((counters         (vector-ref tables 0))
	    (<<EOF>>-action   (vector-ref tables 1))
	    (<<ERROR>>-action (vector-ref tables 2))
	    (rules-actions    (vector-ref tables 3))
	    (nl-start         (vector-ref tables 5))
	    (no-nl-start      (vector-ref tables 6))
	    (charcs-v         (vector-ref tables 7))
	    (acc-v            (vector-ref tables 8)))
	(let* ((len (vector-length charcs-v))
	       (v (make-vector len)))
	  (let loop ((i (- len 1)))
	    (if (>= i 0)
		(begin
		  (vector-set! v i (charcs->tree (vector-ref charcs-v i)))
		  (loop (- i 1)))
		(lexer-make-tree-lexer
		 (vector counters
			 <<EOF>>-action
			 <<ERROR>>-action
			 rules-actions
			 'decision-trees
			 nl-start
			 no-nl-start
			 v
			 acc-v)
		 IS))))))))


(define (lexer-make-code-lexer tables IS)
  ;; Fabrication d'un lexer a partir de code pre-genere
  (let ((<<EOF>>-pre-action   (vector-ref tables 1))
	(<<ERROR>>-pre-action (vector-ref tables 2))
	(rules-pre-action     (vector-ref tables 3))
	(code                 (vector-ref tables 5)))
    (code <<EOF>>-pre-action <<ERROR>>-pre-action rules-pre-action IS)))


;;;; done

)

;;; end of file
