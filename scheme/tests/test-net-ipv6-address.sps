;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for IPv6 address object
;;;Date: Wed Jun  9, 2010
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


(import (nausicaa)
  (net ipv6-address)
  (silex lexer)
  (parser-tools lexical-token)
  (net helpers ipv6-address-lexer)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing net IPv6 address\n")


(parametrise ((check-test-name	'lexing))

  (define (tokenise-address string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(lexer-make-lexer ipv6-address-lexer-table IS))
	   (out		'()))
      (define (push-token! (T <lexical-token>))
	(set-cons! out (cons T.category T.value)))
      (do ((token (lexer) (lexer)))
	  ((<lexical-token>?/special token)
	   (push-token! token)
	   (reverse out))
;;;(write token)(newline)
	(push-token! token))
      ))

  (define eoi `(*eoi* . ,(eof-object)))

;;; --------------------------------------------------------------------


  (check
      (tokenise-address "1:2:3:4:5:6:7:8")
    => `((HEXINT . "1")
	 (COLON  . #\:)
	 (HEXINT . "2")
	 (COLON  . #\:)
	 (HEXINT . "3")
	 (COLON  . #\:)
	 (HEXINT . "4")
	 (COLON  . #\:)
	 (HEXINT . "5")
	 (COLON  . #\:)
	 (HEXINT . "6")
	 (COLON  . #\:)
	 (HEXINT . "7")
	 (COLON  . #\:)
	 (HEXINT . "8")
	 ,eoi))

  (check
      (tokenise-address "1:ciao")
    => '((HEXINT . "1")
	 (COLON  . #\:)
	 (*lexer-error* . "ciao")))


  #t)


;;;; done

(check-report)

;;; end of file
