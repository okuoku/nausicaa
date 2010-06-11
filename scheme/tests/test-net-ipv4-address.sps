;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for IPv4 address object
;;;Date: Fri Jun 11, 2010
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
  (net ipv4-addresses)
  (silex lexer)
  (parser-tools lexical-token)
  (parser-tools source-location)
  (net helpers ipv4-address-lexer)
  (prefix (net helpers ipv4-address-parser) parser:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing net Ipv4 address\n")


(parametrise ((check-test-name	'lexing))

  (define (tokenise-address string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(lexer-make-lexer ipv4-address-lexer-table IS))
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
      (tokenise-address "1.2.3.4")
    => `((NUMBER . 1)
	 (DOT  . #\.)
	 (NUMBER . 2)
	 (DOT  . #\.)
	 (NUMBER . 3)
	 (DOT  . #\.)
	 (NUMBER . 4)
	 ,eoi))

  (check
      (tokenise-address "1.Zciao")
    => '((NUMBER . 1)
	 (DOT    . #\.)
	 (*lexer-error* . "Zciao")))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "1")
    => `((NUMBER . 1)
	 ,eoi))

  (check
      (tokenise-address "10")
    => `((NUMBER . 10)
	 ,eoi))

  (check
      (tokenise-address "100")
    => `((NUMBER . 100)
	 ,eoi))

  (check
      (tokenise-address "190")
    => `((NUMBER . 190)
	 ,eoi))

  (check
      (tokenise-address "210")
    => `((NUMBER . 210)
	 ,eoi))

  (check
      (tokenise-address "250")
    => `((NUMBER . 250)
	 ,eoi))

  (check
      (tokenise-address "255")
    => `((NUMBER . 255)
	 ,eoi))

  (check
      (tokenise-address "256")
    => `((NUMBER . 25)
	 (NUMBER . 6)
	 ,eoi))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "0xa")
    => `((NUMBER . 10)
	 ,eoi))

  (check
      (tokenise-address "0xFE")
    => `((NUMBER . 254)
	 ,eoi))

;;; --------------------------------------------------------------------

  (check
      (tokenise-address "02")
    => `((NUMBER . 2)
	 ,eoi))

  (check
      (tokenise-address "012")
    => `((NUMBER . 10)
	 ,eoi))

  (check
      (tokenise-address "0123")
    => `((NUMBER . 83)
	 ,eoi))

  #t)


(parametrise ((check-test-name	'parsing))

  (define-condition &parser-error
    (parent &assertion))

  (define (make-ipv4-address-parser-error-handler who string)
    (lambda (message (token <lexical-token>))
      (raise
       (condition
	(make-parser-error-condition)
	(make-who-condition who)
	(make-message-condition
	 (let (((pos <source-location>) token.location))
	   (string-append "invalid Ipv4 address input at column "
	 		  pos.column-string ": " message)))
	(make-irritants-condition (list string token.value))))))

  (define (parse-address string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(lexer-make-lexer ipv4-address-lexer-table IS))
	   (parser	(parser:make-ipv4-address-parser)))
      (parser lexer (make-ipv4-address-parser-error-handler 'parse-address string))))

;;; --------------------------------------------------------------------
;;; plain addresses

  (check
      (parse-address "1.2.3.4")
    => '(1 2 3 4))

  (check
      (parse-address "0x1.0x2.0x3.0x4")
    => '(1 2 3 4))

  (check
      (parse-address "01.02.03.04")
    => '(1 2 3 4))

  (check
      (parse-address "192.168.99.1")
    => '(192 168 99 1))

;;; --------------------------------------------------------------------
;;; prefix

  (check
      (parse-address "1.2.3.4/8")
    => '(1 2 3 4 8))

  (check
      (parse-address "0x1.0x2.0x3.0x4/8")
    => '(1 2 3 4 8))

  (check
      (parse-address "01.02.03.04/8")
    => '(1 2 3 4 8))

  (check
      (parse-address "192.168.99.1/8")
    => '(192 168 99 1 8))

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1.2.3.4.5"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1,"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1..2..3"))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "1..2.."))
    => #t)

  (check
      (guard (E ((parser-error-condition? E)
;;;(display (condition-message E))(newline)
		 #t)
		(else #f))
	(parse-address "..2..3"))
    => #t)

  #t)


(parametrise ((check-test-name	'class))

  (check
      (let (((o <ipv4-address>)
	     (make <ipv4-address> (ipv4-address-parse "1.2.3.4"))))
	(list o.third o.second o.first o.zeroth))
    => '(1 2 3 4))

  (check
      (let (((o <ipv4-address>)
	     (make <ipv4-address> (ipv4-address-parse "0x1.0x2.0x3.0x4"))))
	o.bignum)
    => #x01020304)

  (check
      (let (((o <ipv4-address>)
	     (make <ipv4-address> (ipv4-address-parse "1.2.3.4"))))
	o.string)
    => "1.2.3.4")

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::0"))))
  ;; 	o.unspecified?)
  ;;   => #t)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::1"))))
  ;; 	o.unspecified?)
  ;;   => #f)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::0"))))
  ;; 	o.loopback?)
  ;;   => #f)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::1"))))
  ;; 	o.loopback?)
  ;;   => #t)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "FF00::"))))
  ;; 	o.multicast?)
  ;;   => #t)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::1"))))
  ;; 	o.multicast?)
  ;;   => #f)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "FE80::"))))
  ;; 	o.link-local-unicast?)
  ;;   => #t)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "::1"))))
  ;; 	o.link-local-unicast?)
  ;;   => #f)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "FF80::"))))
  ;; 	o.global-unicast?)
  ;;   => #f)

  ;; (check
  ;;     (let (((o <ipv4-address>)
  ;; 	     (make <ipv4-address> (ipv4-address-parse "1:2::"))))
  ;; 	o.global-unicast?)
  ;;   => #t)

;;; --------------------------------------------------------------------

  (check
      (let (((o <ipv4-address-prefix>)
	     (receive (addr len)
		 (ipv4-address-prefix-parse "1.2.3.4/10")
	       (make <ipv4-address-prefix> addr len))))
	(list o.third o.second o.first o.zeroth o.prefix-length))
    => '(1 2 3 4 10))

  (check
      (let (((o <ipv4-address-prefix>)
	     (receive (addr len)
		 (ipv4-address-prefix-parse "1.2.3.4/8")
	       (make <ipv4-address-prefix> addr len))))
	o.string)
    => "1.2.3.4/8")

  #t)


;;;; done

(check-report)

;;; end of file
