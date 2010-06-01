;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: default error handler for SILex lexers
;;;Date: Tue Jun  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (silex default-error-handler)
  (export silex-default-error-handler)
  (import (rnrs)
    (parser-tools lexical-token)
    (parser-tools source-location))


(define-syntax silex-default-error-handler
  (lambda (stx)
    (syntax-case stx ()
      ((?key)
       #`(%silex-default-error-handler #,(datum->syntax #'?key 'yyline)
				       #,(datum->syntax #'?key 'yycolumn)
				       #,(datum->syntax #'?key 'yyoffset)
				       #,(datum->syntax #'?key 'yygetc)
				       #,(datum->syntax #'?key 'yyungetc)
				       )))))

(define (%silex-default-error-handler yyline yycolumn yyoffset yygetc yyungetc)
  (let ((text (letrec ((unget (lambda (count)
				(unless (zero? count)
				  (yyungetc)
				  (unget (- count 1)))))
		       (done  (lambda (count chars)
				(unget count)
				(apply string
				       (reverse (cons* #\. #\. #\. chars))))))
		(let loop ((count 0) (chars '()))
		  (if (= 10 count)
		      (done count chars)
		    (let ((ch (yygetc)))
		      (if (eof-object? ch)
			  (done count chars)
			(loop (+ 1 count) (cons ch chars)))))))))
    (make-<lexical-token>
     '*lexer-error*
     (make-<source-location> #f yyline yycolumn yyoffset)
     text (string-length text))))


;;;; done

)

;;; end of file
