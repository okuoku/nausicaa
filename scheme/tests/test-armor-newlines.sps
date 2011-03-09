;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for newlines filter
;;;Date: Fri Mar 12, 2010
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
  (nausicaa armor newlines)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing armor newlines\n")


(parametrise ((check-test-name	'insert-update))

  (check	;empty bytevectors
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 "\r\n") 0))
	    (in.bv	(string->utf8 ""))
	    (ou.bv	(make-bytevector 0)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next)))
    => '(0 0))

  (check	;short line
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 "\r\n") 0))
	    (in.bv	(string->utf8 "012"))
	    (ou.bv	(make-bytevector 3)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next (utf8->string ou.bv))))
    => '(3 3 "012"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "01234"))
	    (ou.bv	(make-bytevector 7)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next (utf8->string ou.bv))))
    => '(7 5 "01234.,"))

  (check 	;exactly one line with too small output bytevector
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "01234"))
	    (ou.bv	(make-bytevector 6 0)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next (utf8->string ou.bv))))
    => '(5 5 "01234\x0;"))

  (check	;two lines
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "012345"))
	    (ou.bv	(make-bytevector 8)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next (utf8->string ou.bv))))
    => '(8 6 "01234.,5"))

  (check	;three lines
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "012345678901"))
	    (ou.bv	(make-bytevector 16)))
	(receive (dst-next src-next)
	    (newlines-encode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list dst-next src-next (utf8->string ou.bv))))
    => '(16 12 "01234.,56789.,01"))

  #t)


(parametrise ((check-test-name	'insert-final))

  (check	;empty bytevectors
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 "\r\n") 0))
	    (in.bv	(string->utf8 ""))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#t 0 0))

  (check	;empty output bytevector
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 "\r\n") 0))
	    (in.bv	(string->utf8 "ciao"))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#f 0 0))

  (check	;short line
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 "\r\n") 0))
	    (in.bv	(string->utf8 "012"))
	    (ou.bv	(make-bytevector 3)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 3 3 "012"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "01234"))
	    (ou.bv	(make-bytevector 7)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 7 5 "01234.,"))

  (check 	;exactly one line with too small output bytevector
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "01234"))
	    (ou.bv	(make-bytevector 6 0)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 5 5 "01234\x0;"))

  (check	;two lines
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "012345"))
	    (ou.bv	(make-bytevector 8)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 8 6 "01234.,5"))

  (check	;three lines
      (let ((ctx	(make-<newlines-encode-ctx> 5 (string->utf8 ".,") 0))
	    (in.bv	(string->utf8 "012345678901"))
	    (ou.bv	(make-bytevector 16)))
	(receive (result dst-next src-next)
	    (newlines-encode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 16 12 "01234.,56789.,01"))

  #t)


(parametrise ((check-test-name	'remove-update))

  (check	;empty bytevectors
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 "\r\n")))
	    (in.bv	(string->utf8 ""))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#f 0 0))

  (check	;short line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 "\r\n")))
	    (in.bv	(string->utf8 "012"))
	    (ou.bv	(make-bytevector 3 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 2 2 "01\x0;"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,"))
	    (ou.bv	(make-bytevector 5)))
	(receive (result dst-next src-next)
	    (newlines-decode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 5 7 "01234"))

  (check	;two lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,5"))
	    (ou.bv	(make-bytevector 6 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 5 7 "01234\x0;"))

  (check	;three lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,56789.,01"))
	    (ou.bv	(make-bytevector 12 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-update! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 11 15 "01234567890\x0;"))

  #t)


(parametrise ((check-test-name	'remove-final))

;;; sequence with two characters

  (check	;empty bytevectors
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 ""))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#t 0 0))

  (check	;empty output bytevector
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "ciao"))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#f 0 0))

  (check	;short line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "012"))
	    (ou.bv	(make-bytevector 3 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 3 3 "012"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,"))
	    (ou.bv	(make-bytevector 5)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 5 7 "01234"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,5"))
	    (ou.bv	(make-bytevector 5)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 5 7 "01234"))

  (check 	;half sequence
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234."))
	    (ou.bv	(make-bytevector 6)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 6 6 "01234."))

  (check	;two lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,5"))
	    (ou.bv	(make-bytevector 6 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 6 8 "012345"))

  (check	;three lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".,")))
	    (in.bv	(string->utf8 "01234.,56789.,01"))
	    (ou.bv	(make-bytevector 12 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 12 16 "012345678901"))

;;; --------------------------------------------------------------------
;;; sequence with one character

  (check	;empty bytevectors
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 ""))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#t 0 0))

  (check	;empty output bytevector
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "ciao"))
	    (ou.bv	(make-bytevector 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next)))
    => '(#f 0 0))

  (check	;short line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "012"))
	    (ou.bv	(make-bytevector 3 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 3 3 "012"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "01234."))
	    (ou.bv	(make-bytevector 5)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 5 6 "01234"))

  (check 	;exactly one line
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "01234.5"))
	    (ou.bv	(make-bytevector 5)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#f 5 6 "01234"))

  (check	;two lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "01234.5"))
	    (ou.bv	(make-bytevector 6 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 6 7 "012345"))

  (check	;three lines
      (let ((ctx	(make-<newlines-decode-ctx> (string->utf8 ".")))
	    (in.bv	(string->utf8 "01234.56789.01"))
	    (ou.bv	(make-bytevector 12 0)))
	(receive (result dst-next src-next)
	    (newlines-decode-final! ctx ou.bv 0 in.bv 0 (bytevector-length in.bv))
	  (list result dst-next src-next (utf8->string ou.bv))))
    => '(#t 12 14 "012345678901"))

  #t)


;;;; done

(check-report)

;;; end of file
