;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for csv
;;;Date: Mon Jul 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks)
  (nausicaa silex lexer)
  (nausicaa csv strings-lexer)
  (nausicaa csv unquoted-data-lexer)
  (nausicaa csv unquoted-data-comma-lexer)
  (nausicaa csv)
  (nausicaa strings))

(check-set-mode! 'report-failed)
(display "*** testing csv\n")


(parameterise ((check-test-name 'strings-lexer))

  (define (tokenise/list string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer csv-strings-table IS)))
      (do ((token (lexer) (lexer))
	   (ell   '()))
	  ((not token)
	   (reverse ell))
	(set! ell (cons token ell)))))

  (define (tokenise/string string)
    (let* ((IS		(lexer-make-IS (string: string)))
	   (lexer	(lexer-make-lexer csv-strings-table IS)))
      (let-values (((port the-string) (open-string-output-port)))
	(do ((token (lexer) (lexer)))
	    ((not token)
	     (the-string))
	  (write-char token port)))))

;;;All the test strings  must end with a \" to signal  the end of string
;;;to the strings lexer.

;;; --------------------------------------------------------------------

  (check
      (tokenise/list "\"")
    => '())

  (check
      (tokenise/list "abcd\"")
    => '(#\a #\b #\c #\d))

  (check	;quoted double-quote
      (tokenise/list "ab\"\"cd\"")
    => '(#\a #\b #\" #\c #\d))

  (check	;nested string
      (tokenise/list "ab \"\"ciao\"\" cd\"")
    => '(#\a #\b #\space #\" #\c #\i #\a #\o #\" #\space #\c #\d))

  (check	;stop reading at the ending double-quote
      (tokenise/list "ab\"cd")
    => '(#\a #\b))

  (check	;end of input before end of string
      (guard (exc (else (condition-message exc)))
	(tokenise/list "abcd"))
    => "while parsing string, found end of input before closing double-quote")

;;; --------------------------------------------------------------------

  (check
      (tokenise/string "\"")
    => "")

  (check
      (tokenise/string "abcd\"")
    => "abcd")

  (check	;quoted double-quote
      (tokenise/string "ab\"\"cd\"")
    => "ab\"cd")

  (check	;quoted double-quote
      (tokenise/string "ab\"\"\"\"\"\"cd\"")
    => "ab\"\"\"cd")

  (check	;nested string
      (tokenise/string "ab \"\"ciao\"\" cd\"")
    => "ab \"ciao\" cd")

  (check	;stop reading at the ending double-quote
      (tokenise/string "ab\"cd")
    => "ab")

  (check	;end of input before end of string
      (guard (exc (else (condition-message exc)))
	(tokenise/string "abcd"))
    => "while parsing string, found end of input before closing double-quote")

  )


(parameterise ((check-test-name 'unquoted-data-lexer))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer csv-unquoted-data-table IS)))
      (do ((token (lexer) (lexer))
	   (ell   '()))
	  ((or (not token) (eq? token 'string))
	   (reverse ell))
	(set! ell (cons token ell)))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "alpha, beta")
    => '(#\a #\l #\p #\h #\a #\, #\space #\b #\e #\t #\a))

  (check
      (tokenise "alpha\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\n\n\n\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\r\r\r\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\r\n\n\n\r\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

;;; --------------------------------------------------------------------

  (check ;read until the string opening
      (tokenise "alpha \"beta")
    => '(#\a #\l #\p #\h #\a #\space))


)


(parameterise ((check-test-name 'unquoted-data-lexer/comma))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer csv-unquoted-data-table/comma IS)))
      (do ((token (lexer) (lexer))
	   (ell   '()))
	  ((or (not token) (eq? token 'string))
	   (reverse ell))
	(set! ell (cons token ell)))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "alpha, beta")
    => '(#\a #\l #\p #\h #\a field #\space #\b #\e #\t #\a))

  (check
      (tokenise "alpha\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\n\n\n\nbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\r\r\r\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

  (check
      (tokenise "alpha\n\r\n\n\n\r\r\rbeta")
    => '(#\a #\l #\p #\h #\a eol #\b #\e #\t #\a))

;;; --------------------------------------------------------------------

  (check ;read until the string opening
      (tokenise "alpha \"beta")
    => '(#\a #\l #\p #\h #\a #\space))


)


(parameterise ((check-test-name 'csv-comma))

  (check
      (csv->list/comma (open-string-input-port "alpha, beta, delta
one, two, three"))
    => '(("alpha" " beta" " delta")
	 ("one" " two" " three")))

  (check
      (csv->list/comma (open-string-input-port "alpha, \"beta\", de\"\"\"\"\"\"lta
one, two, three"))
    => '(("alpha" " \"beta\"" " de\"\"\"\"lta")
	 ("one" " two" " three")))

  (check
      (csv->list/comma (open-string-input-port "alpha, beta, delta
one, two, three")
		       (lambda (field)
			 (string-trim-both field #\space)))
    => '(("alpha" "beta" "delta")
	 ("one" "two" "three")))

;;; --------------------------------------------------------------------

  (check
      (csv->list (open-string-input-port "alpha| beta| delta
one| two| three") '(#\|))
    => '(("alpha" " beta" " delta")
	 ("one" " two" " three")))

  (check
      (csv->list (open-string-input-port "alpha| beta| delta
one| two| three") '(#\|)
		       (lambda (field)
			 (string-trim-both field #\space)))
    => '(("alpha" "beta" "delta")
	 ("one" "two" "three")))


  )


;;;; done

(check-report)

;;; end of file
