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
; Gestion d'erreurs
;

(define lex-exit-continuation #f)
(define lex-unwind-protect-list '())
(define lex-error-filename #f)

(define lex-unwind-protect
  (lambda (proc)
    (set! lex-unwind-protect-list (cons proc lex-unwind-protect-list))))

(define lex-error
  (lambda (line column . l)
    (let* ((linestr (if line   (number->string line)   #f))
	   (colstr  (if column (number->string column) #f))
	   (namelen (string-length lex-error-filename))
	   (linelen (if line   (string-length linestr) -1))
	   (collen  (if column (string-length colstr)  -1))
	   (totallen (+ namelen 1 linelen 1 collen 2)))
      (display "Lex error:")
      (newline)
      (display lex-error-filename)
      (if line
	  (begin
	    (display ":")
	    (display linestr)))
      (if column
	  (begin
	    (display ":")
	    (display colstr)))
      (display ": ")
      (let loop ((l l))
	(if (null? l)
	    (newline)
	    (let ((item (car l)))
	      (display item)
	      (if (equal? '#\newline item)
		  (let loop2 ((i totallen))
		    (if (> i 0)
			(begin
			  (display #\space)
			  (loop2 (- i 1))))))
	      (loop (cdr l)))))
      (newline)
      (let loop ((l lex-unwind-protect-list))
	(if (pair? l)
	    (begin
	      ((car l))
	      (loop (cdr l)))))
      (lex-exit-continuation #f))))




;
; Decoupage des arguments
;

(define lex-recognized-args
  '(complete-driver?
    filein
    table-name
    fileout
    counters
    portable
    code
    pp))

(define lex-valued-args
  '(complete-driver?
    filein
    table-name
    fileout
    counters))

(define lex-parse-args
  (lambda (args)
    (let loop ((args args))
      (if (null? args)
	  '()
	  (let ((sym (car args)))
	    (cond ((not (symbol? sym))
		   (lex-error #f #f "bad option list."))
		  ((not (memq sym lex-recognized-args))
		   (lex-error #f #f "unrecognized option \"" sym "\"."))
		  ((not (memq sym lex-valued-args))
		   (cons (cons sym '()) (loop (cdr args))))
		  ((null? (cdr args))
		   (lex-error #f #f "the value of \"" sym "\" not specified."))
		  (else
		   (cons (cons sym (cadr args)) (loop (cddr args))))))))))




;
; Differentes etapes de la fabrication de l'automate
;

(define lex1
  (lambda (filein)
;     (display "lex1: ") (write (get-internal-run-time)) (newline)
    (parser filein)))

(define lex2
  (lambda (filein)
    (let* ((result (lex1 filein))
	   (<<EOF>>-action (car result))
	   (<<ERROR>>-action (cadr result))
	   (rules (cddr result)))
;       (display "lex2: ") (write (get-internal-run-time)) (newline)
      (append (list <<EOF>>-action <<ERROR>>-action rules)
	      (re2nfa rules)))))

(define lex3
  (lambda (filein)
    (let* ((result (lex2 filein))
	   (<<EOF>>-action   (list-ref result 0))
	   (<<ERROR>>-action (list-ref result 1))
	   (rules            (list-ref result 2))
	   (nl-start         (list-ref result 3))
	   (no-nl-start      (list-ref result 4))
	   (arcs             (list-ref result 5))
	   (acc              (list-ref result 6)))
;       (display "lex3: ") (write (get-internal-run-time)) (newline)
      (append (list <<EOF>>-action <<ERROR>>-action rules)
	      (noeps nl-start no-nl-start arcs acc)))))

(define lex4
  (lambda (filein)
    (let* ((result (lex3 filein))
	   (<<EOF>>-action   (list-ref result 0))
	   (<<ERROR>>-action (list-ref result 1))
	   (rules            (list-ref result 2))
	   (nl-start         (list-ref result 3))
	   (no-nl-start      (list-ref result 4))
	   (arcs             (list-ref result 5))
	   (acc              (list-ref result 6)))
;       (display "lex4: ") (write (get-internal-run-time)) (newline)
      (append (list <<EOF>>-action <<ERROR>>-action rules)
	      (sweep nl-start no-nl-start arcs acc)))))

(define lex5
  (lambda (filein)
    (let* ((result (lex4 filein))
	   (<<EOF>>-action   (list-ref result 0))
	   (<<ERROR>>-action (list-ref result 1))
	   (rules            (list-ref result 2))
	   (nl-start         (list-ref result 3))
	   (no-nl-start      (list-ref result 4))
	   (arcs             (list-ref result 5))
	   (acc              (list-ref result 6)))
;       (display "lex5: ") (write (get-internal-run-time)) (newline)
      (append (list <<EOF>>-action <<ERROR>>-action rules)
	      (nfa2dfa nl-start no-nl-start arcs acc)))))

(define lex6
  (lambda (args-alist)
    (let* ((filein           (cdr (assq 'filein args-alist)))
	   (result           (lex5 filein))
	   (<<EOF>>-action   (list-ref result 0))
	   (<<ERROR>>-action (list-ref result 1))
	   (rules            (list-ref result 2))
	   (nl-start         (list-ref result 3))
	   (no-nl-start      (list-ref result 4))
	   (arcs             (list-ref result 5))
	   (acc              (list-ref result 6)))
;       (display "lex6: ") (write (get-internal-run-time)) (newline)
      (prep-set-rules-yytext? rules)
      (output args-alist
	      <<EOF>>-action <<ERROR>>-action
	      rules nl-start no-nl-start arcs acc)
      #t)))

(define lex7
  (lambda (args)
    (call-with-current-continuation
     (lambda (exit)
       (set! lex-exit-continuation exit)
       (set! lex-unwind-protect-list '())
       (set! lex-error-filename (cadr (memq 'filein args)))
       (let* ((args-alist (lex-parse-args args))
	      (result (lex6 args-alist)))
; 	 (display "lex7: ") (write (get-internal-run-time)) (newline)
	 result)))))




;
; Fonctions principales
;

(define lex
  (lambda (filein fileout . options)
    (lex7 (append (list 'complete-driver? #t
			'filein filein
			'table-name "lexer-default-table"
			'fileout fileout)
		  options))))

(define lex-tables
  (lambda (filein table-name fileout . options)
    (lex7 (append (list 'complete-driver? #f
			'filein filein
			'table-name table-name
			'fileout fileout)
		  options))))
