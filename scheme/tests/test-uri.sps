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
  (prefix (nausicaa uri low) uri:)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing URI\n")


;;;; helpers

(define (make-lexer-port obj)
  (cond ((string? obj)
	 (open-bytevector-input-port (uri:to-bytevector obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 #f)))


(parametrise ((check-test-name	'string/bytevector))

  (check (uri:to-string (uri:to-bytevector ""))			=> "")
  (check (uri:to-string (uri:to-bytevector "ciao"))		=> "ciao")
  (check (uri:to-string (uri:to-bytevector "ci%3fa%3do"))	=> "ci%3fa%3do")

  #t)


(parametrise ((check-test-name	'percent-encoding))

  (let ()

    (define-inline (doit ch str)
      (check (uri:percent-encode ch  (:string-result? #t)) => str)
      (check (uri:percent-decode str (:string-result? #t)) => (string ch)))

    (doit #\. ".")
    (doit #\- "-")
    (doit #\_ "_")
    (doit #\~ "~")
    (doit #\% "%25")
    (doit #\? "%3f")
    (doit #\= "%3d")
    (doit #\# "%23")

    #f)

  (let ()

    (define-inline (doit ch str)
      (check
	  (uri:percent-encode ch
			      (:string-result? #t)
			      (:char-selector (lambda (chi)
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
      (check (uri:percent-decode str (:string-result? #t)) => (string ch)))

    (doit #\. "%2e")
    (doit #\- "%2d")
    (doit #\_ "%5f")
    (doit #\~ "%7e")
    (doit #\% "%25")
    (doit #\? "%3f")
    (doit #\= "%3d")
    (doit #\# "%23")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-inline (doit dec enc)
      (check (uri:percent-encode dec (:string-result? #t)) => enc)
      (check (uri:percent-decode enc (:string-result? #t)) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3do")
    (doit "ci?a=o" "ci%3fa%3do")

    #f)

  (check
      (uri:percent-encode "ciao")
    => '#vu8(99 105 97 111))

  (check
      (uri:percent-decode '#vu8(99 105 97 111))
    => '#vu8(99 105 97 111))

  (check
      (uri:percent-decode '#vu8(99 105 97 111) (:string-result? #t))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (uri:normalise-percent-encoded-string "")
    => "")

  (check
      (uri:normalise-percent-encoded-string "ciao")
    => "ciao")

  (check
      (uri:normalise-percent-encoded-string "cia%3do")
    => "cia%3do")

  (check
      (uri:normalise-percent-encoded-string "ci%3fa%3do")
    => "ci%3fa%3do")

  (check
      (uri:normalise-percent-encoded-string "%7eciao")
    => "~ciao")

  (check
      (uri:normalise-percent-encoded-string "ci%5fao")
    => "ci_ao")

;;; --------------------------------------------------------------------

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector ""))
    => (uri:to-bytevector ""))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ciao"))
    => (uri:to-bytevector "ciao"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "cia%3do"))
    => (uri:to-bytevector "cia%3do"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ci%3fa%3do"))
    => (uri:to-bytevector "ci%3fa%3do"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "%7eciao"))
    => (uri:to-bytevector "~ciao"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ci%5fao"))
    => (uri:to-bytevector "ci_ao"))

  #t)


(parametrise ((check-test-name	'parsing-misc))

;;; valid component

  (let-syntax ((doit	(syntax-rules ()
			  ((_ ?expected ?input)
			   (check
			       (receive (bool pos)
				   (uri:valid-component? (make-lexer-port ?input))
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
      (uri:to-string (uri:parse-scheme (make-lexer-port "http://ciao")))
    => "http")

  (check
      (uri:parse-scheme (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-scheme (make-lexer-port "hello"))
    => #f)

  (check
      (uri:parse-scheme (make-lexer-port "hel/lo:"))
    => #f)

;;; --------------------------------------------------------------------
;;; hier-part

  (check
      (uri:parse-hier-part (make-lexer-port ""))
    => #f)

  (check
      (uri:to-string (uri:parse-hier-part (make-lexer-port "//ciao")))
    => "//ciao")

  (check
      (let* ((p (make-lexer-port "//ciao?query"))
  	     (r (uri:to-string (uri:parse-hier-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\?)))

  (check
      (let* ((p (make-lexer-port "//ciao#fragment"))
  	     (r (uri:to-string (uri:parse-hier-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\#)))

;;; --------------------------------------------------------------------
;;; query

  (check
      (uri:to-string (uri:parse-query (make-lexer-port "?the-query???")))
    => "the-query???")

  (check
      (uri:to-string (uri:parse-query (make-lexer-port "?ciao%3dciao#fragment")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-query (make-lexer-port "?")))
    => "")

  (check
      (uri:parse-query (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-query (make-lexer-port "hello"))
    => #f)

  (check
      (uri:parse-query (make-lexer-port "#hello"))
    => #f)

;;; --------------------------------------------------------------------
;;; fragment

  (check
      (uri:to-string (uri:parse-fragment (make-lexer-port "#the-fragment???")))
    => "the-fragment???")

  (check
      (uri:to-string (uri:parse-fragment (make-lexer-port "#ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-fragment (make-lexer-port "#")))
    => "")

  (check
      (uri:parse-fragment (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-fragment (make-lexer-port "#hello#"))
    => #f)

  (check
      (uri:parse-fragment (make-lexer-port "hello"))
    => #f)

  (check
      (uri:parse-fragment (make-lexer-port "?hello"))
    => #f)

  #t)


(parametrise ((check-test-name	'parsing-authority))

;;; authority

  (check
      (uri:parse-authority (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-authority (make-lexer-port "ciao"))
    => #f)

  (check
      (uri:parse-authority (make-lexer-port "/ciao"))
    => #f)

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "//")))
    => "")

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "///")))
    => "")

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "//ciao/")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "//ciao:8080/")))
    => "ciao:8080")

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "//ciao.it:8080/")))
    => "ciao.it:8080")

  (check
      (uri:to-string (uri:parse-authority (make-lexer-port "//marco@ciao.it:8080/")))
    => "marco@ciao.it:8080")

;;; --------------------------------------------------------------------
;;; userinfo

  (check
      (uri:to-string (uri:parse-userinfo (make-lexer-port "the-userinfo@")))
    => "the-userinfo")

  (check
      (uri:to-string (uri:parse-userinfo (make-lexer-port "ciao%3dciao@")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-userinfo (make-lexer-port "@")))
    => "")

  (check
      (uri:parse-userinfo (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-userinfo (make-lexer-port "#hello#"))
    => #f)

  (check
      (uri:parse-userinfo (make-lexer-port "hello"))
    => #f)

  (check
      (uri:parse-userinfo (make-lexer-port "?hello"))
    => #f)

;;; --------------------------------------------------------------------
;;; IP-literal

  (check
      (uri:parse-ip-literal (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-ip-literal (make-lexer-port "ciao"))
    => #f)

  (check
      (uri:to-string (uri:parse-ip-literal (make-lexer-port "[]")))
    => "")

  (check
      (uri:to-string (uri:parse-ip-literal (make-lexer-port "[::0:1:2]")))
    => "::0:1:2")

;;; --------------------------------------------------------------------
;;; IPvFuture

  (check
      (call-with-values
	  (lambda ()
	    (uri:parse-ipvfuture (make-lexer-port "")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (uri:parse-ipvfuture (make-lexer-port "ciao")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (uri:parse-ipvfuture (make-lexer-port "v1")))
	list)
    => '(49 #vu8()))

  (check
      (call-with-values
	  (lambda ()
	    (uri:parse-ipvfuture (make-lexer-port "v9ciao")))
	(lambda (version bv)
	  (list version (uri:to-string bv))))
    => '(57 "ciao"))

;;; --------------------------------------------------------------------
;;; reg-name

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "the-reg-name")))
    => "the-reg-name")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "the-reg-name:80")))
    => "the-reg-name")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "the-reg-name/ciao")))
    => "the-reg-name")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port ":80")))
    => "")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "/ciao")))
    => "")

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "")))
    => "")

  (check
      (uri:parse-reg-name (make-lexer-port "#hello#"))
    => #f)

  (check
      (uri:to-string (uri:parse-reg-name (make-lexer-port "hello")))
    => "hello")

  (check
      (uri:parse-reg-name (make-lexer-port "?hello"))
    => #f)

  #t)


(parametrise ((check-test-name	'parsing-path-segments))

;;; path segment

  (check
      (uri:to-string (uri:parse-segment (make-lexer-port "")))
    => "")

  (check
      (uri:to-string (uri:parse-segment (make-lexer-port "ciao")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-segment (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (uri:parse-segment (make-lexer-port "?ciao"))
    => '#vu8())

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (uri:parse-segment-nz (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-segment-nz (make-lexer-port "{"))
    => #f)

  (check
      (uri:parse-segment-nz (make-lexer-port "/"))
    => #f)

  (check
      (uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao:ciao")))
    => "ciao:ciao")

  (check
      (uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (uri:parse-segment-nz (make-lexer-port "?ciao"))
    => #f)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment-nz (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz-nc

  (check
      (uri:parse-segment-nz-nc (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-segment-nz-nc (make-lexer-port "{"))
    => #f)

  (check
      (uri:parse-segment-nz-nc (make-lexer-port "/"))
    => #f)

  (check
      (uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao:ciao")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao/hello")))
    => "ciao")

  (check
      (uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (uri:parse-segment-nz-nc (make-lexer-port "?ciao"))
    => #f)

  (check
      (uri:parse-segment-nz-nc (make-lexer-port ":ciao"))
    => #f)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-segment-nz-nc (make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (uri:parse-slash-and-segment (make-lexer-port ""))
    => (eof-object))

  (check
      (uri:parse-slash-and-segment (make-lexer-port "ciao"))
    => #f)

  (check
      (uri:parse-slash-and-segment (make-lexer-port "?ciao"))
    => '#f)

  (check
      (uri:parse-slash-and-segment (make-lexer-port "/"))
    => '#vu8())

  (check
      (uri:to-string (uri:parse-slash-and-segment (make-lexer-port "/ciao/hello")))
    => "ciao")

  (check
      (let* ((p (make-lexer-port "/ciao/hello"))
	     (a (uri:to-string (uri:parse-slash-and-segment p)))
	     (b (uri:to-string (uri:parse-slash-and-segment p))))
	(list a b))
    => '("ciao" "hello"))

  (check
      (let* ((p (make-lexer-port "/ciao/hello/"))
	     (a (uri:to-string (uri:parse-slash-and-segment p)))
	     (b (uri:to-string (uri:parse-slash-and-segment p)))
	     (c (uri:to-string (uri:parse-slash-and-segment p))))
	(list a b c (uri:parse-slash-and-segment p)))
    => `("ciao" "hello" "" ,(eof-object)))

  (check
      (uri:to-string (uri:parse-slash-and-segment (make-lexer-port "/ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uri:to-string (uri:parse-slash-and-segment (make-lexer-port "/ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-slash-and-segment (make-lexer-port "/ciao%3d%3,ciao"))))
    => #t)

  (check
      (guard (E ((uri:parser-error-condition? E)
		 #t)
		(else #f))
	(uri:to-string (uri:parse-slash-and-segment (make-lexer-port "/ciao%,3%3dciao"))))
    => #t)

  #t)


(parametrise ((check-test-name	'parsing-path))

;;; path-empty

  (check
      (uri:parse-path-empty (make-lexer-port ""))
    => (eof-object))

  (check
      (uri:parse-path-empty (make-lexer-port "?ciao"))
    => '())

  (check
      (uri:parse-path-empty (make-lexer-port "#ciao"))
    => '())

  (check
      (uri:parse-path-empty (make-lexer-port "ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-abempty

  (check
      (uri:parse-path-abempty (make-lexer-port ""))
    => '())

  (check
      (uri:parse-path-abempty (make-lexer-port "?query"))
    => '())

  (check
      (uri:parse-path-abempty (make-lexer-port "#fragment"))
    => '())

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao")))
    => '("ciao"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao?query")))
    => '("ciao"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao#fragment")))
    => '("ciao"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao/hello/salut?query")))
    => '("ciao" "hello" "salut"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "/ciao/hello/salut#fragment")))
    => '("ciao" "hello" "salut"))

  (check
      (map uri:to-string (uri:parse-path-abempty (make-lexer-port "///")))
    => '("" "" ""))

;;; --------------------------------------------------------------------
;;; path-absolute

  (check
      (uri:parse-path-absolute (make-lexer-port ""))
    => #f)

  (check
      (uri:parse-path-absolute (make-lexer-port "ciao"))
    => #f)

  (check
      (uri:parse-path-absolute (make-lexer-port "/"))
    => '())

  (check
      (map uri:to-string (uri:parse-path-absolute (make-lexer-port "/ciao")))
    => '("ciao"))

  (check
      (map uri:to-string (uri:parse-path-absolute (make-lexer-port "/ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map uri:to-string (uri:parse-path-absolute (make-lexer-port "/ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (uri:parse-path-absolute (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-noscheme

  (check
      (uri:parse-path-noscheme (make-lexer-port ""))
    => #f)

  (check
      (map uri:to-string (uri:parse-path-noscheme (make-lexer-port "ciao")))
    => '("ciao"))

  (check
      (uri:parse-path-noscheme (make-lexer-port "/"))
    => #f)

  (check
      (uri:parse-path-noscheme (make-lexer-port "/ciao"))
    => #f)

  (check
      (map uri:to-string (uri:parse-path-noscheme (make-lexer-port "ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map uri:to-string (uri:parse-path-noscheme (make-lexer-port "ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map uri:to-string (uri:parse-path-noscheme (make-lexer-port "ciao/he:llo")))
    => '("ciao" "he:llo"))

  (check
      (map uri:to-string (uri:parse-path-noscheme (make-lexer-port "ci:ao/hello")))
    => '("ci"))

  (check
      (uri:parse-path-noscheme (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path-rootless

  (check
      (uri:parse-path-rootless (make-lexer-port ""))
    => #f)

  (check
      (map uri:to-string (uri:parse-path-rootless (make-lexer-port "ciao")))
    => '("ciao"))

  (check
      (uri:parse-path-rootless (make-lexer-port "/"))
    => #f)

  (check
      (uri:parse-path-rootless (make-lexer-port "/ciao"))
    => #f)

  (check
      (map uri:to-string (uri:parse-path-rootless (make-lexer-port "ciao/hello")))
    => '("ciao" "hello"))

  (check
      (map uri:to-string (uri:parse-path-rootless (make-lexer-port "ciao/hello/salut")))
    => '("ciao" "hello" "salut"))

  (check
      (map uri:to-string (uri:parse-path-rootless (make-lexer-port "ciao/he:llo")))
    => '("ciao" "he:llo"))

  (check
      (uri:parse-path-rootless (make-lexer-port "?ciao"))
    => #f)

;;; --------------------------------------------------------------------
;;; path

  (check
      (receive (type segments)
	  (uri:parse-path (make-lexer-port ""))
	(vector type (map uri:to-string segments)))
    => '#(#f ()))

  (check
      (receive (type segments)
	  (uri:parse-path (make-lexer-port "/ciao/hello/salut"))
	(vector type (map uri:to-string segments)))
    => '#(path-absolute ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (uri:parse-path (make-lexer-port "ciao/hello/salut"))
	(vector type (map uri:to-string segments)))
    => '#(path-rootless ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (uri:parse-path (make-lexer-port "ci:ao/hello/salut"))
	(vector type (map uri:to-string segments)))
    => '#(path-rootless ("ci:ao" "hello" "salut")))

  (check
      (receive (type segments)
	  (uri:parse-path (make-lexer-port "/"))
	(vector type segments))
    => '#(path-absolute ()))

  #t)


(parametrise ((check-test-name	'parse-uri-reference))

  (define (make-lexer-port obj)
    (cond ((string? obj)
	   (open-bytevector-input-port (uri:to-bytevector obj)))
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
				   (uri:parse-relative-part (make-lexer-port ?input))
				 (vector authority path-kind (map uri:to-string segments)))
			     => '?expected-vector)))))

    (doit ""
	  #(#f path-empty ()))

    (doit "//"
	  #(#f path-abempty ()))

    #f)


  #t)


(parametrise ((check-test-name	'class-output))

  (define scheme	(string->utf8 "http"))
  (define authority	(string->utf8 "www.spiffy.org"))
  (define path		(map string->utf8 '("the" "path" "name")))
  (define query		(string->utf8 "question=answer"))
  (define fragment	(string->utf8 "anchor-point"))

  (define uri-string	"http://www.spiffy.org/the/path/name?question%3danswer#anchor-point")
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
