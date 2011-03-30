;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for URI library
;;;Date: Wed Jun  2, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa uri)
  (prefix (nausicaa uri low) low.)
  (prefix (nausicaa uri conditions) low.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing URI\n")


;;;; helpers

(define (make-lexer-port obj)
  (cond ((string? obj)
	 (open-bytevector-input-port (low.to-bytevector obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation 'make-lexer-port "expecting string or bytevector" obj))))


(parametrise ((check-test-name	'string/bytevector))

  (check (low.to-string (low.to-bytevector ""))			=> "")
  (check (low.to-string (low.to-bytevector "ciao"))		=> "ciao")
  (check (low.to-string (low.to-bytevector "ci%3fa%3do"))	=> "ci%3fa%3do")
  (check (low.to-string (low.to-bytevector "ci%3Fa%3Do"))	=> "ci%3Fa%3Do")

  (check
      (guard (E ((low.parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(low.to-bytevector "ciaoλ"))
    => #t)

  #t)


(parametrise ((check-test-name	'percent-encoding))

  (let ()

    (define-inline (doit ch str)
      (check (low.percent-encode ch  (low.string-result? #t)) => str)
      (check (low.percent-decode str (low.string-result? #t)) => (string ch)))

    (doit #\. ".")
    (doit #\- "-")
    (doit #\_ "_")
    (doit #\~ "~")
    (doit #\% "%25")
    (doit #\? "%3F")
    (doit #\= "%3D")
    (doit #\# "%23")

    #f)

  (let ()

    (define-inline (doit ch str)
      (check
	  (low.percent-encode ch
			      (low.string-result? #t)
			      (low.char-selector (lambda (chi)
						   (memv (integer->char chi)
							 '(#\. #\- #\_ #\~ #\%
							   #\: #\/ #\?
							   #\# #\[ #\]
							   #\@ #\\ #\!
							   #\$ #\& #\'
							   #\( #\) #\*
							   #\+ #\, #\;
							   #\=))
						   )))
	=> str)
      (check (low.percent-decode str (low.string-result? #t)) => (string ch)))

    (doit #\. "%2E")
    (doit #\- "%2D")
    (doit #\_ "%5F")
    (doit #\~ "%7E")
    (doit #\% "%25")
    (doit #\? "%3F")
    (doit #\= "%3D")
    (doit #\# "%23")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-inline (doit dec enc)
      (check (low.percent-encode dec (low.string-result? #t)) => enc)
      (check (low.percent-decode enc (low.string-result? #t)) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3Do")
    (doit "ci?a=o" "ci%3Fa%3Do")

    #f)

  (check
      (low.percent-encode "ciao")
    => '#vu8(99 105 97 111))

  (check
      (low.percent-decode '#vu8(99 105 97 111))
    => '#vu8(99 105 97 111))

  (check
      (low.percent-decode '#vu8(99 105 97 111) (low.string-result? #t))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (low.normalise-percent-encoded-string "")
    => "")

  (check
      (low.normalise-percent-encoded-string "ciao")
    => "ciao")

  (check
      (low.normalise-percent-encoded-string "cia%3do")
    => "cia%3Do")

  (check
      (low.normalise-percent-encoded-string "cia%3Do")
    => "cia%3Do")

  (check
      (low.normalise-percent-encoded-string "ci%3fa%3do")
    => "ci%3Fa%3Do")

  (check
      (low.normalise-percent-encoded-string "ci%3Fa%3Do")
    => "ci%3Fa%3Do")

  (check
      (low.normalise-percent-encoded-string "%7Eciao")
    => "~ciao")

  (check
      (low.normalise-percent-encoded-string "ci%5Fao")
    => "ci_ao")

;;; --------------------------------------------------------------------

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector ""))
    => (low.to-bytevector ""))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "ciao"))
    => (low.to-bytevector "ciao"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "cia%3do"))
    => (low.to-bytevector "cia%3Do"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "cia%3Do"))
    => (low.to-bytevector "cia%3Do"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "ci%3fa%3do"))
    => (low.to-bytevector "ci%3Fa%3Do"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "ci%3Fa%3Do"))
    => (low.to-bytevector "ci%3Fa%3Do"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "%7eciao"))
    => (low.to-bytevector "~ciao"))

  (check
      (low.normalise-percent-encoded-bytevector (low.to-bytevector "ci%5fao"))
    => (low.to-bytevector "ci_ao"))

  #t)


(parametrise ((check-test-name	'parsing-misc))

;;; valid component

  (let-syntax ((doit	(syntax-rules ()
			  ((_ ?expected ?input)
			   (check
			       (receive (bool pos)
				   (low.valid-component? (make-lexer-port ?input))
				 (list bool pos))
			     => ?expected)))))

    (doit '(#t  4) "ciao")
    (doit '(#t  4) "ciao")
    (doit '(#t  3) "%3d")
    (doit '(#t  9) "%3d%3d%3d")
    (doit '(#t 11) "ciao%3dciao")
    (doit '(#f  1) "?")
    (doit '(#f  5) "ciao?")

    #f)

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri))

;;; scheme

  (check
      (low.parse-scheme (make-lexer-port ""))
    => #f)

  (check
      (low.parse-scheme (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-scheme (make-lexer-port "hel/lo:"))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "http://ciao"))
	     (scheme	(low.to-string (low.parse-scheme in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list scheme rest))
    => '("http" "//ciao"))

  (check
      (let* ((in-port	(make-lexer-port "A123+-.://ciao"))
	     (scheme	(low.to-string (low.parse-scheme in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list scheme rest))
    => '("A123+-." "//ciao"))

;;; --------------------------------------------------------------------
;;; hier-part

  (check
      (low.collect-hier-part (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "//"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//" #t))

  (check
      (let* ((in-port	(make-lexer-port "//ciao"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//ciao" #t))

  (check
      (let* ((in-port	(make-lexer-port "//ciao/salut"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//ciao/salut" #t))

  (check
      (let* ((in-port	(make-lexer-port "//ciao?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao/salut?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao/salut" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao/salut#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao/salut" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "/"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/" #t))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/ciao" #t))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/salut"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/ciao/salut" #t))

  (check
      (let* ((in-port	(make-lexer-port "/ciao?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/salut?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao/salut" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/salut#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao/salut" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "."))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("." #t))

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("ciao" #t))

  (check
      (let* ((in-port	(make-lexer-port "ciao/salut"))
  	     (part	(low.to-string (low.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("ciao/salut" #t))

  (check
      (let* ((in-port	(make-lexer-port "ciao?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "ciao/salut?query"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao/salut" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "ciao#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "ciao/salut#fragment"))
  	     (part	(low.to-string (low.collect-hier-part in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao/salut" "#fragment"))

;;; --------------------------------------------------------------------
;;; relative-part

  (check
      (low.collect-relative-part (make-lexer-port ""))
    => #f)

  (check
      (low.to-string (low.collect-relative-part (make-lexer-port "//ciao")))
    => "//ciao")

  (check
      (let* ((p (make-lexer-port "//ciao?query"))
  	     (r (low.to-string (low.collect-relative-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\?)))

  (check
      (let* ((p (make-lexer-port "//ciao#fragment"))
  	     (r (low.to-string (low.collect-relative-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\#)))

;;; --------------------------------------------------------------------
;;; query

  (check
      (low.parse-query (make-lexer-port ""))
    => #f)

  (check
      (low.parse-query (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-query (make-lexer-port "#hello"))
    => #f)

  (check
      (low.to-string (low.parse-query (make-lexer-port "?")))
    => "")

  (check
      (low.to-string (low.parse-query (make-lexer-port "?the-query???")))
    => "the-query???")

  (check
      (let* ((in-port	(make-lexer-port "?ciao%3dciao#fragment"))
	     (query	(low.to-string (low.parse-query in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list query rest))
    => '("ciao%3dciao" "#fragment"))

;;; --------------------------------------------------------------------
;;; fragment

  (check
      (low.parse-fragment (make-lexer-port ""))
    => #f)

  (check
      (low.parse-fragment (make-lexer-port "#hello#"))
    => #f)

  (check
      (low.parse-fragment (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-fragment (make-lexer-port "?hello"))
    => #f)

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#the-fragment???")))
    => "the-fragment???")

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#")))
    => "")

  #t)


(parametrise ((check-test-name	'parsing-authority))

;;; authority

  (check
      (low.parse-authority (make-lexer-port ""))
    => #f)

  (check
      (low.parse-authority (make-lexer-port "ciao"))
    => #f)

  (check
      (low.parse-authority (make-lexer-port "/ciao"))
    => #f)

  (check
      (low.parse-authority (make-lexer-port "?ciao"))
    => #f)

  (check
      (low.parse-authority (make-lexer-port "#ciao"))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "//"))
	     (authority	(low.to-string (low.parse-authority in-port))))
	(list authority (eof-object? (lookahead-u8 in-port))))
    => '("" #t))

  (check
      (let* ((in-port	(make-lexer-port "//?query"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "//#fragment"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "#fragment"))

  (check
      (let* ((in-port	(make-lexer-port "///"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "/"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao/salut"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao" "/salut"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao:8080/salut"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao:8080" "/salut"))

  (check
      (let* ((in-port	(make-lexer-port "//ciao.it:8080/salut"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao.it:8080" "/salut"))

  (check
      (let* ((in-port	(make-lexer-port "//marco@ciao.it:8080/salut"))
	     (authority	(low.to-string (low.parse-authority in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list authority rest))
    => '("marco@ciao.it:8080" "/salut"))

;;; --------------------------------------------------------------------
;;; userinfo

  (check
      (low.parse-userinfo (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao.it"))
	     (info	(low.parse-userinfo in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "ciao.it"))

  (check
      (let* ((in-port	(make-lexer-port ":8080"))
	     (info	(low.parse-userinfo in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f ":8080"))

  (check
      (let* ((in-port	(make-lexer-port "/hello"))
	     (info	(low.parse-userinfo in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "/hello"))

  (check
      (let* ((in-port	(make-lexer-port "?hello"))
	     (info	(low.parse-userinfo in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "?hello"))

  (check
      (let* ((in-port	(make-lexer-port "#hello"))
	     (info	(low.parse-userinfo in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "#hello"))

  (check
      (let* ((in-port	(make-lexer-port "@"))
	     (userinfo	(low.to-string (low.parse-userinfo in-port)))
	     (eof?	(eof-object? (lookahead-u8 in-port))))
	(list userinfo eof?))
    => '("" #t))

  (check
      (let* ((in-port	(make-lexer-port "@host"))
	     (userinfo	(low.to-string (low.parse-userinfo in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("" "host"))

  (check
      (let* ((in-port	(make-lexer-port "userinfo@host"))
	     (userinfo	(low.to-string (low.parse-userinfo in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("userinfo" "host"))

  (check
      (let* ((in-port	(make-lexer-port "ciao%3dciao@host"))
	     (userinfo	(low.to-string (low.parse-userinfo in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("ciao%3dciao" "host"))

;;; --------------------------------------------------------------------
;;; IP-literal

  (check
      (low.parse-ip-literal (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (ip	(low.parse-ip-literal in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list ip rest))
    => '(#f "ciao"))

  (check
      (low.to-string (low.parse-ip-literal (make-lexer-port "[]")))
    => "")

  (check
      (low.to-string (low.parse-ip-literal (make-lexer-port "[::0:1:2]")))
    => "::0:1:2")

  (check
      (let* ((in-port	(make-lexer-port "[::0:1:2]:8080"))
	     (ip	(low.to-string (low.parse-ip-literal in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list ip rest))
    => '("::0:1:2" ":8080"))

;;; --------------------------------------------------------------------
;;; IPvFuture

  (check
      (call-with-values
	  (lambda ()
	    (low.parse-ipvfuture (make-lexer-port "")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (low.parse-ipvfuture (make-lexer-port "ciao")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (low.parse-ipvfuture (make-lexer-port "v1")))
	list)
    => '(49 #vu8()))

  (check
      (call-with-values
	  (lambda ()
	    (low.parse-ipvfuture (make-lexer-port "v9ciao")))
	(lambda (version bv)
	  (list version (low.to-string bv))))
    => '(57 "ciao"))

;;; --------------------------------------------------------------------
;;; reg-name

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "")))
    => "")

  (check
      (let* ((in-port	(make-lexer-port ":80"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" ":80"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "/ciao"))

  (check
      (let* ((in-port	(make-lexer-port "?query"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "?query"))

  (check
      (let* ((in-port	(make-lexer-port "#fragment"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "#fragment"))

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "the-reg-name")))
    => "the-reg-name")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "the.reg.name")))
    => "the.reg.name")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (let* ((in-port	(make-lexer-port "the-reg-name:80"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("the-reg-name" ":80"))

  (check
      (let* ((in-port	(make-lexer-port "the-reg-name/ciao"))
	     (reg	(low.to-string (low.parse-reg-name in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list reg rest))
    => '("the-reg-name" "/ciao"))

;;; --------------------------------------------------------------------
;;; port

  (check
      (low.parse-port (make-lexer-port ""))
    => #f)

  (check
      (low.to-string (low.parse-port (make-lexer-port ":")))
    => "")

  (check
      (low.to-string (low.parse-port (make-lexer-port ":2")))
    => "2")

  (check
      (low.to-string (low.parse-port (make-lexer-port ":8080")))
    => "8080")

  (check
      (let* ((in-port	(make-lexer-port ":8080ciao"))
	     (port	(low.to-string (low.parse-port in-port)))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list port rest))
    => '("8080" "ciao"))

  #t)


(parametrise ((check-test-name	'parsing-path-segments))

;;; path segment

  (check
      (low.to-string (low.parse-segment (make-lexer-port "")))
    => "")

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (segment	(low.to-string (low.parse-segment in-port)))
	     (eof	(lookahead-u8 in-port)))
	(list segment eof))
    => `("ciao" ,(eof-object)))

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao!$&'()*+,;=:@-._~")))
    => "ciao!$&'()*+,;=:@-._~")

  (check
      (let* ((in-port	(make-lexer-port "/hello"))
	     (segment1	(low.to-string (low.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(low.to-string (low.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (segment1	(low.to-string (low.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(low.to-string (low.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (segment	(low.parse-segment in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#vu8() "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello?ciao"))
	     (segment	(low.to-string (low.parse-segment in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (segment	(low.parse-segment in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#vu8() "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello#ciao"))
	     (segment	(low.to-string (low.parse-segment in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else E))
	(low.to-string (low.parse-segment (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else E))
	(low.to-string (low.parse-segment (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (low.parse-segment-nz (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "{"))
	     (segment	(low.parse-segment-nz in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\{))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (segment	(low.parse-segment-nz in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (segment	(low.to-string (low.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao:ciao"))
	     (segment	(low.to-string (low.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao:ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (segment1	(low.to-string (low.parse-segment-nz in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(low.to-string (low.parse-segment-nz in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao%3dciao"))
	     (segment	(low.to-string (low.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (let ((S "ciao%3d%3dciao"))
    (check
	(let* ((in-port	(make-lexer-port S))
	       (segment	(low.to-string (low.parse-segment-nz in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (segment	(low.parse-segment-nz in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (segment	(low.parse-segment-nz in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello?ciao"))
	     (segment	(low.to-string (low.parse-segment-nz in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello#ciao"))
	     (segment	(low.to-string (low.parse-segment-nz in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz-nc

  (check
      (low.parse-segment-nz-nc (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "{"))
	     (segment	(low.parse-segment-nz-nc in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\{))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (segment	(low.parse-segment-nz-nc in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (segment	(low.to-string (low.parse-segment-nz-nc in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (let ((S "ciao:ciao"))
    (check
	(let* ((in-port	(make-lexer-port S))
	       (segment	(low.to-string (low.parse-segment-nz-nc in-port)))
	       (rest	(low.to-string (get-bytevector-some in-port))))
	  (list segment rest))
      => '("ciao" ":ciao")))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (segment1	(low.to-string (low.parse-segment-nz-nc in-port)))
	     (char	(integer->char (get-u8 in-port)))
	     (segment2	(low.to-string (low.parse-segment-nz-nc in-port))))
	(list segment1 char segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (let ((S "ciao%3dciao"))
    (check
	(let* ((in-port	(make-lexer-port S))
	       (segment	(low.to-string (low.parse-segment-nz-nc in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (let ((S "ciao%3d%3dciao"))
    (check
	(let* ((in-port	(make-lexer-port S))
	       (segment	(low.to-string (low.parse-segment-nz-nc in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (segment	(low.parse-segment-nz-nc in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello?ciao"))
	     (segment	(low.to-string (low.parse-segment-nz-nc in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (segment	(low.parse-segment-nz-nc in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "hello#ciao"))
	     (segment	(low.to-string (low.parse-segment-nz-nc in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port ":ciao"))
	     (segment	(low.parse-segment-nz-nc in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list segment rest))
    => '(#f ":ciao"))

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (low.parse-slash-and-segment (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (segment	(low.parse-slash-and-segment in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list segment rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (segment	(low.parse-slash-and-segment in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (segment	(low.parse-slash-and-segment in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (segment	(low.parse-slash-and-segment in-port)))
	(list segment (lookahead-u8 in-port)))
    => `(#vu8() ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello"))
	     (segment1	(low.to-string (low.parse-slash-and-segment in-port)))
	     (segment2	(low.to-string (low.parse-slash-and-segment in-port))))
	(list segment1 segment2 (lookahead-u8 in-port)))
    => `("ciao" "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/"))
	     (segment1	(low.to-string (low.parse-slash-and-segment in-port)))
	     (segment2	(low.to-string (low.parse-slash-and-segment in-port)))
	     (segment3	(low.to-string (low.parse-slash-and-segment in-port))))
	(list segment1 segment2 segment3 (lookahead-u8 in-port)))
    => `("ciao" "hello" "" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao%3dciao"))
	     (segment	(low.to-string (low.parse-slash-and-segment in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao%3d%3dciao"))
	     (segment	(low.to-string (low.parse-slash-and-segment in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3d%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/?ciao"))
	     (segment	(low.to-string (low.parse-slash-and-segment in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list segment query))
    => '("" "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "/#ciao"))
	     (segment	(low.to-string (low.parse-slash-and-segment in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list segment fragment))
    => '("" "ciao"))

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao%,3%3dciao"))))
    => #t)

  #t)


(parametrise ((check-test-name	'parsing-path))

;;; path-empty

  (check
      (low.parse-path-empty (make-lexer-port ""))
    => '())

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (path	(low.parse-path-empty in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(vector path query))
    => '#(() "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
  	     (path	(low.parse-path-empty in-port))
  	     (fragment	(low.to-string (low.parse-fragment in-port))))
  	(vector path fragment))
    => '#(() "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
  	     (path	(low.parse-path-empty in-port))
  	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
  	     (path	(low.parse-path-empty in-port))
  	     (rest	(low.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "/ciao"))

;;; --------------------------------------------------------------------
;;; path-abempty

  (check
      (low.parse-path-abempty (make-lexer-port ""))
    => '())

  (check
      (let* ((in-port	(make-lexer-port "?query"))
	     (path	(low.parse-path-abempty in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(() "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "#fragment"))
	     (path	(low.parse-path-abempty in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(() "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
	     (path	(map low.to-string (low.parse-path-abempty in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao?query"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("ciao") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao#fragment"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello"))
	     (path	(map low.to-string (low.parse-path-abempty in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut"))
	     (path	(map low.to-string (low.parse-path-abempty in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/"))
	     (path	(map low.to-string (low.parse-path-abempty in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut?query"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut/?query"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut" "") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut#fragment"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut/#fragment"))
	     (path	(map low.to-string (low.parse-path-abempty in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut" "") "fragment" ,(eof-object)))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "///")))
    => '("" "" ""))

;;; --------------------------------------------------------------------
;;; path-absolute

  (check
      (low.parse-path-absolute (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (path	(low.parse-path-absolute in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list path rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (path	(map low.to-string (low.parse-path-absolute in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "//"))
	     (path	(low.parse-path-absolute in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list path rest))
    => `(#f "//"))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
	     (path	(map low.to-string (low.parse-path-absolute in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello"))
	     (path	(map low.to-string (low.parse-path-absolute in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/salut"))
	     (path	(map low.to-string (low.parse-path-absolute in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/"))
	     (path	(map low.to-string (low.parse-path-absolute in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/?query"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello?query"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/?query"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") "query" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/#fragment"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello#fragment"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/#fragment"))
	     (path	(map low.to-string (low.parse-path-absolute in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") "fragment" ,(eof-object)))

;;; --------------------------------------------------------------------
;;; path-noscheme

  (check
      (low.parse-path-noscheme (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (path	(low.parse-path-noscheme in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list path char))
    => '(#f #\/))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
	     (path	(low.parse-path-noscheme in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list path rest))
    => '(#f "/ciao"))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello/salut"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello/"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/he:llo"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "he:llo") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ci:ao/hello"))
	     (path	(low.parse-path-noscheme in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list path rest))
    => `(#f "ci:ao/hello"))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (path	(low.parse-path-noscheme in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello?ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut?ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut/?ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello" "salut" "") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (path	(low.parse-path-noscheme in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello#ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut#ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut/#ciao"))
	     (path	(map low.to-string (low.parse-path-noscheme in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello" "salut" "") "ciao"))

;;; --------------------------------------------------------------------
;;; path-rootless

  (check
      (low.parse-path-rootless (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/"))
	     (path	(low.parse-path-rootless in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list path char))
    => '(#f #\/))

  (check
      (let* ((in-port	(make-lexer-port "/ciao"))
	     (path	(low.parse-path-rootless in-port))
	     (rest	(low.to-string (get-bytevector-some in-port))))
	(list path rest))
    => '(#f "/ciao"))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello/salut"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hel:lo"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hel:lo") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ci:ao/hel:lo"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ci:ao" "hel:lo") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello/"))
	     (path	(map low.to-string (low.parse-path-rootless in-port))))
	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "?ciao"))
	     (path	(low.parse-path-rootless in-port))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello?ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut?ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut/?ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (query	(low.to-string (low.parse-query in-port))))
	(list path query))
    => '(("hello" "salut" "") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "#ciao"))
	     (path	(low.parse-path-rootless in-port))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello#ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut#ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "hello/salut/#ciao"))
	     (path	(map low.to-string (low.parse-path-rootless in-port)))
	     (fragment	(low.to-string (low.parse-fragment in-port))))
	(list path fragment))
    => '(("hello" "salut" "") "ciao"))

;;; --------------------------------------------------------------------
;;; path

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port ""))
	(vector type (map low.to-string segments)))
    => '#(path-empty ()))

  (check
      (let*-values (((in-port)		(make-lexer-port "?query"))
		    ((type segments)	(low.parse-path in-port))
		    ((query)		(low.to-string (low.parse-query in-port))))
	(vector type segments query))
    => '#(path-empty () "query"))

  (check
      (let*-values (((in-port)		(make-lexer-port "#fragment"))
		    ((type segments)	(low.parse-path in-port))
		    ((fragment)		(low.to-string (low.parse-fragment in-port))))
	(vector type segments fragment))
    => '#(path-empty () "fragment"))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "/ciao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-absolute ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "/"))
	(vector type segments))
    => '#(path-absolute (#vu8())))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "//"))
	(vector type (map low.to-string segments)))
    => '#(path-abempty ("" "")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "///"))
	(vector type (map low.to-string segments)))
    => '#(path-abempty ("" "" "")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "//ciao/"))
	(vector type (map low.to-string segments)))
    => '#(path-abempty ("" "ciao" "")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "ciao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-noscheme ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "ci:ao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-rootless ("ci:ao" "hello" "salut")))

  #t)


(parametrise ((check-test-name	'parse-uri-reference))

;;; relative-part

  (let-syntax
      ((doit (syntax-rules ()
	       ((_ ?input ?expected-vector)
		(check
		    (let*-values (((in-port)
				   (make-lexer-port ?input))
				  ((authority path-kind segments)
				   (low.parse-relative-part in-port))
				  ((rest)
				   (get-bytevector-some in-port)))
		      (vector (and authority (low.to-string authority))
			      path-kind
			      (map low.to-string segments)
			      (if (eof-object? rest)
				  ""
				(low.to-string rest))))
		  => '?expected-vector)))))

;;; with authority

    (doit "//"
	  #("" path-abempty () ""))

    (doit "//?query" ;empty authority
	  #("" path-abempty () "?query"))

    (doit "//#fragment"	;empty authority
	  #("" path-abempty () "#fragment"))

    (doit "///"	;empty authority
	  #("" path-abempty ("") ""))

    (doit "///?query" ;empty authority
	  #("" path-abempty ("") "?query"))

    (doit "///#fragment" ;empty authority
	  #("" path-abempty ("") "#fragment"))
    (doit "///ciao" ;empty authority
	  #("" path-abempty ("ciao") ""))

    (doit "//ciao.com"
	  #("ciao.com" path-abempty () ""))

    (doit "//ciao.com:8080"
	  #("ciao.com:8080" path-abempty () ""))

    (doit "//marco@ciao.com:8080"
	  #("marco@ciao.com:8080" path-abempty () ""))

    (doit "//ciao.com:8080/"
	  #("ciao.com:8080" path-abempty ("") ""))

    (doit "//ciao.com:8080/a"
	  #("ciao.com:8080" path-abempty ("a") ""))

    (doit "//ciao.com/a/b/c"
	  #("ciao.com" path-abempty ("a" "b" "c") ""))

    (doit "//ciao.com:8080/a/b/c"
	  #("ciao.com:8080" path-abempty ("a" "b" "c") ""))

;;; no authority, emtpy path

    (doit ""
	  #(#f path-empty () ""))

;;; no authority, absolute path

    (doit "/"
	  #(#f path-absolute ("") ""))

    (doit "/a///"
	  #(#f path-absolute ("a" "" "" "") ""))

    (doit "/ciao"
	  #(#f path-absolute ("ciao") ""))

    (doit "/ciao/hello/salut"
	  #(#f path-absolute ("ciao" "hello" "salut") ""))

;;; no authority relative path

    (doit "./"
	  #(#f path-noscheme ("." "") ""))

    (doit "./a///"
	  #(#f path-noscheme ("." "a" "" "" "") ""))

    (doit "./ciao"
	  #(#f path-noscheme ("." "ciao") ""))

    (doit "./ciao/hello/salut"
	  #(#f path-noscheme ("." "ciao" "hello" "salut") ""))

    #f)

  #t)


(parametrise ((check-test-name	'class-output))

  (define scheme	(string->utf8 "http"))
  (define authority	(string->utf8 "www.spiffy.org"))
  (define path		(map string->utf8 '("the" "path" "name")))
  (define query		(string->utf8 "question=answer"))
  (define fragment	(string->utf8 "anchor-point"))

  (define uri-string	"http://www.spiffy.org/the/path/name?question%3Danswer#anchor-point")
  (define uri-bv	(string->utf8 uri-string))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?var ?expected . ?body)
       (check
	   (let (((?var <uri>)
		  (make <uri>
		    (:decoded-scheme	scheme)
		    (:decoded-authority	authority)
		    (:decoded-path	path)
		    (:decoded-query	query)
		    (:decoded-fragment	fragment))))
	     . ?body)
	 => ?expected))))

;;; --------------------------------------------------------------------

  (doit o uri-string
	o.string)

  (doit o uri-bv
	o.bytevector)

  #t)


;;;; done

(check-report)

;;; end of file
