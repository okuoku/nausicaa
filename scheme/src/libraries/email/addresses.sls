;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: email address parser
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
(library (email address)
  (export a)
  (import (rnrs))


(define (make-lexer arg)
  (let ((in-port (cond ((string? arg)
			(open-string-input-port arg))
		       ((and (input-port?   arg)
			     (textual-port? arg))
			arg)
		       (else
			(assertion-violation 'make-lexer
			  "expected string or textual input port" arg))))
	(offset  0))
    (lambda ()
      (let-syntax ((is-char	(syntax-rules () ((_ ?c) (<= ?c #\127))))
		   (is-control	(syntax-rules () ((_ ?c) (<= ?c #\31))))
		   (is-space	(syntax-rules () ((_ ?c) (or (char=? ?c #\space)
							     (char=? ?c #\tab)))))
		   (is-space	(syntax-rules () ((_ ?c) (char<=? #\160 ?c #\255))))
		   (is-special	(syntax-rules () ((_ ?c) (memv ?c
							       '(#\( #\)
								 #\< #\>
								 #\@ #\,
								 #\; #\:
								 #\backspace
								 #\" #\.
								 #\[ #\])))))
		   (is-atom	(syntax-rules () ((_ ?c) (and (is-char ?c)
							      (not (is-special ?c))
							      (not (is-space   ?c))
							      (not (is-control ?c))))))
		   (is-ctext	(syntax-rules () ((_ ?c) (and (or (is-char    ?c)
								  (is-bigchar ?c))
							      (not (char=? #\( ?c))
							      (not (char=? #\) ?c))
							      (not (char=? #\backslash ?c))))))
		   (is-dtext	(syntax-rules () ((_ ?c) (and (is-char    ?c)
							      (not (char=? #\[ ?c))
							      (not (char=? #\] ?c))
							      (not (char=? #\backslash ?c))))))
		   (is-qtext	(syntax-rules () ((_ ?c) (and (is-char    ?c)
							      (not (char=? #\" ?c))
							      (not (char=? #\backslash ?c)))))))

	(define (next-char)
	  (set! offset (+ 1 offset))
	  (get-char in-port))

	(define (error message)
	  (assertion-violation 'lexer message address-string))

	(define (eat-comment ou-port)
	  (define error-message/incomplete "incomplete comment in email address")
	  (let loop ((c (next-char)))
	    (cond ((eof-object? c)
		   (error error-message/incomplete))

		  ((is-ctext c)
		   (put-char c ou-port)
		   (loop (next-char)))

		  (else
		   (case c
		     ((#\backslash)
		      (let ((c1 (next-char)))
			(cond ((eof-object? c1)
			       (error error-message/incomplete))
			      (else
			       (put-char c  ou-port)
			       (put-char c1 ou-port)
			       (loop (next-char))))))

		     ((#\()
		      (eat-comment ou-port)
		      (loop (next-char)))

		     ((#\))
		      (put-char c ou-port)
		      (loop (next-char)))

		     (else
		      (error (string-append "forbidden character \""
					    (list->string (list c))
					    "\" in email address comment offset "
					    (number->string offset)))))))))

	(define (eat-spaces-after-newline)
	  ;;There must be at least one space after a cr+lf sequence.
	  ;;
	  (let loop ((c (next-char)))
	    (case c
	      ((#\space #\tab)
	       (loop (next-char)))
	      (else
	       (error "newline without proper continuation of spaces")))))

	(let loop ((c (next-char)))
	  (cond ((eof-object? c)
		 )

		((is-atom c)
		 (let-values (((ou-port getter) (open-string-output-port)))
		   (put-char c ou-port)
		   (let loop2 ((c (next-char)))
		     (cond ((eof-object? c))))))

		(else (case c
			((#\()
			 (let-values (((ou-port getter) (open-string-output-port)))
			   (eat-comment ou-port)
			   (cons 'comment (string-append "(" (getter)))))

			((#\< #\> #\@ #\, #\; #\: #\.')
			 (cons 'character . c))

			((#\newline)
			 (eat-spaces-aftger-newline)
			 (loop (next-char)))

			((#\return)
			 (if (char=? #\newline (next-char))
			     (eat-spaces-aftger-newline)
			   (error "return character not followed by newline character")))

			((#\space #\tab)
			 (loop (next-char)))



			)))))))


;;;; done

)

;;; end of file
