;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: semantic action bindings for SILex
;;;Date: Mon Jan 17, 2011
;;;
;;;Abstract
;;;
;;;	This library  exports bindings used  in the semantic  actions of
;;;	the lexer tables of SILex itself.
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


#!r6rs
(library (nausicaa silex semantic)
  (export
    ;; token record
    :tok		:tok-make
    tok?		make-tok
    get-tok-type	get-tok-line
    get-tok-column	get-tok-lexeme
    get-tok-attr	get-tok-2nd-attr

    ;; fonctions auxilliaires du lexer
    parse-spec-char		parse-digits-char
    parse-hex-digits-char	parse-quoted-char
    parse-ordinary-char		extract-id
    parse-id			parse-id-ref
    parse-power-m		parse-power-m-inf
    parse-power-m-n

    ;; constants
    eof-tok			hblank-tok
    vblank-tok			pipe-tok
    question-tok		plus-tok
    star-tok			lpar-tok
    rpar-tok			dot-tok
    lbrack-tok			lbrack-rbrack-tok
    lbrack-caret-tok		lbrack-minus-tok
    subst-tok			power-tok
    doublequote-tok		char-tok
    caret-tok			dollar-tok
    <<EOF>>-tok			<<ERROR>>-tok
    percent-percent-tok		id-tok
    rbrack-tok			minus-tok
    illegal-tok			class-tok
    string-tok			number-of-tokens
    newline-ch			tab-ch
    dollar-ch			minus-ch
    rbrack-ch			caret-ch
    dot-class			default-action
    default-<<EOF>>-action	default-<<ERROR>>-action
    )
  (import (rnrs))


;;; Fonctions de manipulation des tokens

(define-record-type (:tok :tok-make tok?)
  (nongenerative nausicaa:silex::tok)
  (fields (immutable type		get-tok-type)
	  (immutable line		get-tok-line)
	  (immutable column		get-tok-column)
	  (immutable lexeme		get-tok-lexeme)
	  (immutable attr		get-tok-attr)
	  (immutable second-attr	get-tok-2nd-attr)))

(define (make-tok tok-type lexeme line column . attr)
  (cond ((null? attr)
	 (:tok-make tok-type line column lexeme #f         #f))
	((null? (cdr attr))
	 (:tok-make tok-type line column lexeme (car attr) #f))
	(else
	 (:tok-make tok-type line column lexeme (car attr) (cadr attr)))))


;;;; module util.scm
;;
;;Quelques definitions de constantes
;;

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

(define dot-class
  (list (cons 'inf- (- newline-ch 1))
	(cons (+ newline-ch 1) 'inf+)))

(define default-action
  (string-append "        (yycontinue)" "\n"))

(define default-<<EOF>>-action
  (string-append "       (eof-object)" "\n"))

(define default-<<ERROR>>-action
  (string-append "       (assertion-violation #f \"invalid token\")\n"))



;;;; module lexparser.scm
;;
;;Fonctions auxilliaires du lexer.
;;

(define (parse-spec-char lexeme line column)
  (make-tok char-tok lexeme line column newline-ch))

(define (parse-digits-char lexeme line column)
  (let* ((num (substring lexeme 1 (string-length lexeme)))
	 (n (string->number num)))
    (make-tok char-tok lexeme line column n)))

(define (parse-hex-digits-char lexeme line column)
  (let ((n (string->number lexeme)))
    (make-tok char-tok lexeme line column n)))

(define (parse-quoted-char lexeme line column)
  (let ((c (string-ref lexeme 1)))
    (make-tok char-tok lexeme line column (char->integer c))))

(define (parse-ordinary-char lexeme line column)
  (let ((c (string-ref lexeme 0)))
    (make-tok char-tok lexeme line column (char->integer c))))

(define (extract-id s)
  (let ((len (string-length s)))
    (substring s 1 (- len 1))))

(define (parse-id lexeme line column)
  (make-tok id-tok lexeme line column (string-downcase lexeme) lexeme))

(define (parse-id-ref lexeme line column)
  (let* ((orig-name (extract-id lexeme))
	 (name (string-downcase orig-name)))
    (make-tok subst-tok lexeme line column name orig-name)))

(define (parse-power-m lexeme line column)
  (let* ((len    (string-length lexeme))
	 (substr (substring lexeme 1 (- len 1)))
	 (m      (string->number substr))
	 (range  (cons m m)))
    (make-tok power-tok lexeme line column range)))

(define (parse-power-m-inf lexeme line column)
  (let* ((len (string-length lexeme))
	 (substr (substring lexeme 1 (- len 2)))
	 (m (string->number substr))
	 (range (cons m 'inf)))
    (make-tok power-tok lexeme line column range)))

(define (parse-power-m-n lexeme line column)
  (let ((len (string-length lexeme)))
    (let loop ((comma 2))
      (if (char=? (string-ref lexeme comma) #\,)
	  (let* ((sub1  (substring lexeme 1 comma))
		 (sub2  (substring lexeme (+ comma 1) (- len 1)))
		 (m     (string->number sub1))
		 (n     (string->number sub2))
		 (range (cons m n)))
	    (make-tok power-tok lexeme line column range))
	(loop (+ comma 1))))))


;;;; done

)

;;; end of file
