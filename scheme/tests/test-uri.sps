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
	 #f)))


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
      (low.to-string (low.parse-scheme (make-lexer-port "http://ciao")))
    => "http")

  (check
      (low.parse-scheme (make-lexer-port ""))
    => #f)

  (check
      (low.parse-scheme (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-scheme (make-lexer-port "hel/lo:"))
    => #f)

;;; --------------------------------------------------------------------
;;; hier-part

  (check
      (low.parse-hier-part (make-lexer-port ""))
    => #f)

  (check
      (low.to-string (low.parse-hier-part (make-lexer-port "//ciao")))
    => "//ciao")

  (check
      (let* ((p (make-lexer-port "//ciao?query"))
  	     (r (low.to-string (low.parse-hier-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\?)))

  (check
      (let* ((p (make-lexer-port "//ciao#fragment"))
  	     (r (low.to-string (low.parse-hier-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\#)))

;;; --------------------------------------------------------------------
;;; query

  (check
      (low.to-string (low.parse-query (make-lexer-port "?the-query???")))
    => "the-query???")

  (check
      (low.to-string (low.parse-query (make-lexer-port "?ciao%3dciao#fragment")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-query (make-lexer-port "?")))
    => "")

  (check
      (low.parse-query (make-lexer-port ""))
    => #f)

  (check
      (low.parse-query (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-query (make-lexer-port "#hello"))
    => #f)

;;; --------------------------------------------------------------------
;;; fragment

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#the-fragment???")))
    => "the-fragment???")

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-fragment (make-lexer-port "#")))
    => "")

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
      (low.to-string (low.parse-authority (make-lexer-port "//")))
    => "")

  (check
      (low.to-string (low.parse-authority (make-lexer-port "///")))
    => "")

  (check
      (low.to-string (low.parse-authority (make-lexer-port "//ciao/")))
    => "ciao")

  (check
      (low.to-string (low.parse-authority (make-lexer-port "//ciao:8080/")))
    => "ciao:8080")

  (check
      (low.to-string (low.parse-authority (make-lexer-port "//ciao.it:8080/")))
    => "ciao.it:8080")

  (check
      (low.to-string (low.parse-authority (make-lexer-port "//marco@ciao.it:8080/")))
    => "marco@ciao.it:8080")

;;; --------------------------------------------------------------------
;;; userinfo

  (check
      (low.to-string (low.parse-userinfo (make-lexer-port "the-userinfo@")))
    => "the-userinfo")

  (check
      (low.to-string (low.parse-userinfo (make-lexer-port "ciao%3dciao@")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-userinfo (make-lexer-port "@")))
    => "")

  (check
      (low.parse-userinfo (make-lexer-port ""))
    => #f)

  (check
      (low.parse-userinfo (make-lexer-port "#hello#"))
    => #f)

  (check
      (low.parse-userinfo (make-lexer-port "hello"))
    => #f)

  (check
      (low.parse-userinfo (make-lexer-port "?hello"))
    => #f)

;;; --------------------------------------------------------------------
;;; IP-literal

  (check
      (low.parse-ip-literal (make-lexer-port ""))
    => #f)

  (check
      (low.parse-ip-literal (make-lexer-port "ciao"))
    => #f)

  (check
      (low.to-string (low.parse-ip-literal (make-lexer-port "[]")))
    => "")

  (check
      (low.to-string (low.parse-ip-literal (make-lexer-port "[::0:1:2]")))
    => "::0:1:2")

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
      (low.to-string (low.parse-reg-name (make-lexer-port ":80")))
    => "")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "/ciao")))
    => "")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "the-reg-name")))
    => "the-reg-name")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "the-reg-name:80")))
    => "the-reg-name")

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "the-reg-name/ciao")))
    => "the-reg-name")

  (check
      (low.parse-reg-name (make-lexer-port "#hello#"))
    => #f)

  (check
      (low.to-string (low.parse-reg-name (make-lexer-port "hello")))
    => "hello")

  (check
      (low.parse-reg-name (make-lexer-port "?hello"))
    => #f)

;;; --------------------------------------------------------------------
;;; port

  (check
      (let ((result (low.parse-port (make-lexer-port ""))))
	(and result (low.to-string result)))
    => #f)

  (check
      (let ((result (low.parse-port (make-lexer-port ":"))))
	(and result (low.to-string result)))
    => "")

  (check
      (let ((result (low.parse-port (make-lexer-port ":2"))))
	(and result (low.to-string result)))
    => "2")

  (check
      (let ((result (low.parse-port (make-lexer-port ":8080"))))
	(and result (low.to-string result)))
    => "8080")

  #t)


(parametrise ((check-test-name	'parsing-path-segments))

;;; path segment

  (check
      (low.to-string (low.parse-segment (make-lexer-port "")))
    => "")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-segment (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (low.parse-segment (make-lexer-port "?ciao"))
    => '#vu8())

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (low.parse-segment-nz (make-lexer-port ""))
    => #f)

  (check
      (low.parse-segment-nz (make-lexer-port "{"))
    => #f)

  (check
      (low.parse-segment-nz (make-lexer-port "/"))
    => #f)

  (check
      (low.to-string (low.parse-segment-nz (make-lexer-port "ciao")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment-nz (make-lexer-port "ciao:ciao")))
    => "ciao:ciao")

  (check
      (low.to-string (low.parse-segment-nz (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment-nz (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-segment-nz (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (low.parse-segment-nz (make-lexer-port "?ciao"))
    => #f)

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
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
      (low.parse-segment-nz-nc (make-lexer-port "{"))
    => #f)

  (check
      (low.parse-segment-nz-nc (make-lexer-port "/"))
    => #f)

  (check
      (low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao:ciao")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (low.parse-segment-nz-nc (make-lexer-port "?ciao"))
    => #f)

  (check
      (low.parse-segment-nz-nc (make-lexer-port ":ciao"))
    => #f)

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-segment-nz-nc (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (low.parse-slash-and-segment (make-lexer-port ""))
    => (eof-object))

  (check
      (low.parse-slash-and-segment (make-lexer-port "ciao"))
    => #f)

  (check
      (low.parse-slash-and-segment (make-lexer-port "?ciao"))
    => '#f)

  (check
      (low.parse-slash-and-segment (make-lexer-port "/"))
    => '#vu8())

  (check
      (low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao/hello")))
    => "ciao")

  (check
      (let* ((p (make-lexer-port "/ciao/hello"))
	     (a (low.to-string (low.parse-slash-and-segment p)))
	     (b (low.to-string (low.parse-slash-and-segment p))))
	(list a b))
    => '("ciao" "hello"))

  (check
      (let* ((p (make-lexer-port "/ciao/hello/"))
	     (a (low.to-string (low.parse-slash-and-segment p)))
	     (b (low.to-string (low.parse-slash-and-segment p)))
	     (c (low.to-string (low.parse-slash-and-segment p))))
	(list a b c (low.parse-slash-and-segment p)))
    => `("ciao" "hello" "" ,(eof-object)))

  (check
      (low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (guard (E ((low.parser-error-condition? E)
		 #t)
		(else #f))
	(low.to-string (low.parse-slash-and-segment (make-lexer-port "/ciao%3d%3,ciao"))))
    => #t)

  (check
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
    => (eof-object))

  (check
      (low.parse-path-empty (make-lexer-port "?ciao"))
    => '())

  (check
      (low.parse-path-empty (make-lexer-port "#ciao"))
    => '())

  (check
      (low.parse-path-empty (make-lexer-port "ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-abempty

  (check
      (low.parse-path-abempty (make-lexer-port ""))
    => '())

  (check
      (low.parse-path-abempty (make-lexer-port "?query"))
    => '())

  (check
      (low.parse-path-abempty (make-lexer-port "#fragment"))
    => '())

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao")))
    => '("ciao"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao?query")))
    => '("ciao"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao#fragment")))
    => '("ciao"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao/hello/salut?query")))
    => '("ciao" "hello" "salut"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "/ciao/hello/salut#fragment")))
    => '("ciao" "hello" "salut"))

  (check
      (map low.to-string (low.parse-path-abempty (make-lexer-port "///")))
    => '("" "" ""))

;;; --------------------------------------------------------------------
;;; path-absolute

  (check
      (low.parse-path-absolute (make-lexer-port ""))
    => #f)

  (check
      (low.parse-path-absolute (make-lexer-port "ciao"))
    => #f)

  (check
      (low.parse-path-absolute (make-lexer-port "/"))
    => '())

  (check
      (map low.to-string (low.parse-path-absolute (make-lexer-port "/ciao")))
    => '("ciao"))

  (check
      (map low.to-string (low.parse-path-absolute (make-lexer-port "/ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map low.to-string (low.parse-path-absolute (make-lexer-port "/ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (low.parse-path-absolute (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-noscheme

  (check
      (low.parse-path-noscheme (make-lexer-port ""))
    => #f)

  (check
      (map low.to-string (low.parse-path-noscheme (make-lexer-port "ciao")))
    => '("ciao"))

  (check
      (low.parse-path-noscheme (make-lexer-port "/"))
    => #f)

  (check
      (low.parse-path-noscheme (make-lexer-port "/ciao"))
    => #f)

  (check
      (map low.to-string (low.parse-path-noscheme (make-lexer-port "ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map low.to-string (low.parse-path-noscheme (make-lexer-port "ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map low.to-string (low.parse-path-noscheme (make-lexer-port "ciao/he:llo")))
    => '("ciao" "he:llo"))

  (check
      (map low.to-string (low.parse-path-noscheme (make-lexer-port "ci:ao/hello")))
    => '("ci"))

  (check
      (low.parse-path-noscheme (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-rootless

  (check
      (low.parse-path-rootless (make-lexer-port ""))
    => #f)

  (check
      (map low.to-string (low.parse-path-rootless (make-lexer-port "ciao")))
    => '("ciao"))

  (check
      (low.parse-path-rootless (make-lexer-port "/"))
    => #f)

  (check
      (low.parse-path-rootless (make-lexer-port "/ciao"))
    => #f)

  (check
      (map low.to-string (low.parse-path-rootless (make-lexer-port "ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map low.to-string (low.parse-path-rootless (make-lexer-port "ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map low.to-string (low.parse-path-rootless (make-lexer-port "ciao/he:llo")))
    => '("ciao" "he:llo"))

  (check
      (low.parse-path-rootless (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port ""))
	(vector type (map low.to-string segments)))
    => '#(#f ()))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "/ciao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-absolute ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "ciao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-rootless ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "ci:ao/hello/salut"))
	(vector type (map low.to-string segments)))
    => '#(path-rootless ("ci:ao" "hello" "salut")))

  (check
      (receive (type segments)
	  (low.parse-path (make-lexer-port "/"))
	(vector type segments))
    => '#(path-absolute ()))

  #t)


(parametrise ((check-test-name	'parse-uri-reference))

  (define (make-lexer-port obj)
    (cond ((string? obj)
	   (open-bytevector-input-port (low.to-bytevector obj)))
	  ((bytevector? obj)
	   (open-bytevector-input-port obj))
	  (else
	   #f)))

;;; --------------------------------------------------------------------
;;; relative-part

  (let-syntax ((doit	(syntax-rules ()
			  ((_ ?input ?expected-vector)
			   (check
			       (receive (authority path-kind segments)
				   (low.parse-relative-part (make-lexer-port ?input))
				 (vector (and authority (low.to-string authority))
					 path-kind (map low.to-string segments)))
			     => '?expected-vector)))))

;;; with authority

    (doit "//"
	  #("" path-abempty ()))

    (doit "//ciao.com"
	  #("ciao.com" path-abempty ()))

    (doit "//ciao.com:8080"
	  #("ciao.com:8080" path-abempty ()))

    (doit "//marco@ciao.com:8080"
	  #("marco@ciao.com:8080" path-abempty ()))

    (doit "//ciao.com:8080/"
	  #("ciao.com:8080" path-abempty ("")))

    (doit "//ciao.com:8080/a"
	  #("ciao.com:8080" path-abempty ("a")))

    (doit "//ciao.com/a/b/c"
	  #("ciao.com" path-abempty ("a" "b" "c")))

    (doit "//ciao.com:8080/a/b/c"
	  #("ciao.com:8080" path-abempty ("a" "b" "c")))

;;; no authority, emtpy path

    (doit ""
	  #(#f path-empty ()))

;;; no authority, absolute path

    (doit "/"
	  #(#f path-absolute ()))

    (doit "/a///"
	  #(#f path-absolute ("a" "" "" "")))

    (doit "/ciao"
	  #(#f path-absolute ("ciao")))

    (doit "/ciao/hello/salut"
	  #(#f path-absolute ("ciao" "hello" "salut")))

;;; no authority relative path

    (doit "./"
	  #(#f path-noscheme ("." "")))

    (doit "./a///"
	  #(#f path-noscheme ("." "a" "" "" "")))

    (doit "./ciao"
	  #(#f path-noscheme ("." "ciao")))

    (doit "./ciao/hello/salut"
	  #(#f path-noscheme ("." "ciao" "hello" "salut")))

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
