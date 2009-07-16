;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lalr
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme
;;;
;;;	  This  program  illustrates  the  use of  the  lalr-scm  parser
;;;	generator for Scheme. It is NOT robust, since calling a function
;;;	with the  wrong number of  arguments may generate an  error that
;;;	will cause the calculator to crash.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2004 Dominique Boucher
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


(import (nausicaa)
  (lalr)
  (checks)
  (rnrs mutable-pairs))

(check-set-mode! 'report-failed)
(display "*** testing lalr\n")



(define calc-parser
  (lalr-parser
     ;; --- Options
     ;; output a parser, called calc-parser, in a separate file - calc.yy.scm,
     (output:    calc-parser "calc.yy.scm")
     ;; output the LALR table to calc.out
     (out-table: "calc.out")
     ;; there should be no conflict
     (expect:    5)

     ;; --- token definitions
     (ID NUM = LPAREN RPAREN NEWLINE COMMA
	 (left: + -)
	 (left: * /)
	 (nonassoc: uminus))

     (lines    (lines line) : (display-result $2)
	       (line)       : (display-result $1))


     ;; --- rules
     (line     (assign NEWLINE)        : $1
	       (expr   NEWLINE)        : $1
	       (error  NEWLINE)        : #f)

     (assign   (ID = expr)             : (add-binding $1 $3))

     (expr     (expr + expr)           : (+ $1 $3)
	       (expr - expr)           : (- $1 $3)
	       (expr * expr)           : (* $1 $3)
	       (expr / expr)           : (/ $1 $3)
	       (- expr (prec: uminus)) : (- $2)
	       (ID)                    : (get-binding $1)
	       (ID LPAREN args RPAREN) : (invoke-proc $1 $3)
	       (NUM)                   : $1
	       (LPAREN expr RPAREN)    : $2)

     (args     ()                      : '()
	       (expr arg-rest)         : (cons $1 $2))

     (arg-rest (COMMA expr arg-rest)   : (cons $2 $3)
	       ()                      : '())))


(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(when (file-exists? "calc.out")
  (delete-file "calc.out"))
(when (file-exists? "calc.yy.scm")
  (delete-file "calc.yy.scm"))


;;;
;;;;   The lexer
;;;

(define (port-line port) '??)
(define (port-column port) '??)

(define (make-lexer errorp)
  (lambda ()
    (letrec ((skip-spaces
              (lambda ()
                (let loop ((c (peek-char)))
                  (if (and (not (eof-object? c))
                           (or (char=? c #\space) (char=? c #\tab)))
                      (begin
                        (read-char)
                        (loop (peek-char)))))))
             (read-number
              (lambda (l)
                (let ((c (peek-char)))
                  (if (char-numeric? c)
                      (read-number (cons (read-char) l))
		    (string->number (apply string (reverse l)))))))
             (read-id
              (lambda (l)
                (let ((c (peek-char)))
                  (if (char-alphabetic? c)
                      (read-id (cons (read-char) l))
		    (string->symbol (apply string (reverse l))))))))

      ;; -- skip spaces
      (skip-spaces)
      ;; -- read the next token
      (let loop ()
        (let* ((location (make-source-location "*stdin*"
					       (port-line (current-input-port))
					       (port-column (current-input-port)) -1 -1))
               (c (read-char)))
          (cond ((eof-object? c)      '*eoi*)
                ((char=? c #\newline) (make-lexical-token 'NEWLINE location #f))
                ((char=? c #\+)       (make-lexical-token '+       location #f))
                ((char=? c #\-)       (make-lexical-token '-       location #f))
                ((char=? c #\*)       (make-lexical-token '*       location #f))
                ((char=? c #\/)       (make-lexical-token '/       location #f))
                ((char=? c #\=)       (make-lexical-token '=       location #f))
                ((char=? c #\,)       (make-lexical-token 'COMMA   location #f))
                ((char=? c #\()       (make-lexical-token 'LPAREN  location #f))
                ((char=? c #\))       (make-lexical-token 'RPAREN  location #f))
                ((char-numeric? c)    (make-lexical-token 'NUM     location (read-number (list c))))
                ((char-alphabetic? c) (make-lexical-token 'ID      location (read-id (list c))))
                (else
                 (errorp "PARSE ERROR : illegal character: " c)
                 (skip-spaces)
                 (loop))))))))


(define (read-line)
  (let loop ((c (read-char)))
    (if (and (not (eof-object? c))
             (not (char=? c #\newline)))
        (loop (read-char)))))


;;;
;;;;   Environment management
;;;


(define *env* (list (cons '$$ 0)))


(define (init-bindings)
  (set-cdr! *env* '())
  (add-binding 'cos cos)
  (add-binding 'sin sin)
  (add-binding 'tan tan)
  (add-binding 'expt expt)
  (add-binding 'sqrt sqrt))


(define (add-binding var val)
  (set! *env* (cons (cons var val) *env*))
  val)


(define (get-binding var)
  (let ((p (assq var *env*)))
    (if p
        (cdr p)
        0)))


(define (invoke-proc proc-name args)
  (let ((proc (get-binding proc-name)))
    (if (procedure? proc)
        (apply proc args)
        (begin
          (display "ERROR: invalid procedure:")
          (display proc-name)
          (newline)
          0))))


;;;
;;;;   The main program
;;;




(define calc
  (lambda ()
    (call-with-current-continuation
     (lambda (k)
       (display "********************************") (newline)
       (display "*  Mini calculator in Scheme   *") (newline)
       (display "*                              *") (newline)
       (display "* Enter expressions followed   *") (newline)
       (display "* by [RETURN] or 'quit()' to   *") (newline)
       (display "* exit.                        *") (newline)
       (display "********************************") (newline)
       (init-bindings)
       (add-binding 'quit (lambda () (k #t)))
       (letrec ((errorp
                 (lambda (message . args)
                   (display message)
                   (if (and (pair? args)
                            (lexical-token? (car args)))
                       (let ((token (car args)))
                         (display (or (lexical-token-value token)
                                      (lexical-token-category token)))
                         (let ((source (lexical-token-source token)))
                           (if (source-location? source)
                               (begin
                                 (display " (at line ")
                                 (display (source-location-line source))
                                 (display ", column ")
                                 (display (+ 1 (source-location-column source)))
                                 (display ")")))))
                       (for-each display args))
                   (newline)))
                (start
                 (lambda ()
                   (calc-parser (make-lexer errorp) errorp))))
         (start))))))

(calc)


;;;; done

(check-report)

;;; end of file
