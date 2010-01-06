;;;
;;;Part of: Nausicaa/Expat
;;;Contents: expat high-level API tests
;;;Date: Wed Jan  6, 2010
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
  (checks)
  (debugging)
  (compensations)
  (foreign memory)
  (foreign cstrings)
  (foreign xml expat)
  (foreign xml expat compensated))

(check-set-mode! 'report-failed)
(display "*** testing Expat high-level library\n")


(parametrise ((check-test-name 'string)
	      (debugging	#f))

  (check
      (with-compensations
	(define document "
<stuff>
 <thing>
  <alpha a1=\"1\" a2=\"2\">one</alpha>
  <beta>two</beta>
 </thing>
 <thing>
  <alpha>123</alpha>
  <beta>456</beta>
 </thing>
</stuff>")

	(let ((parser (xml-parser-create/c pointer-null))
	      (stack '()))

	  (define (start-callback element attributes)
	    (debug "start ~s ~s" element attributes)
	    (set! stack (cons (cons element attributes) stack)))

	  (define (end-callback element)
	    (debug "end ~s" element)
	    (set! stack (cons element stack)))

	  (xml-set-element-handler parser
				   (make-xml-start-callback start-callback)
				   (make-xml-end-callback   end-callback))
	  (xml-parse/string parser document #t)
	  (reverse stack)))
    => '((stuff)
	 (thing)
	 (alpha (a1 . "1") (a2 . "2")) alpha
	 (beta) beta
	 thing
	 (thing)
	 (alpha) alpha
	 (beta) beta
	 thing
	 stuff))

  (check
      (with-compensations
	(define document-1 "<stuff><thing><alpha a1=\"1\" a2=\"2\">one</alpha><beta>tw")
	(define document-2 "o</beta></thing><thing><alpha>123</alpha><beta>456</beta></thing></stuff>")

	(let ((parser (xml-parser-create/c pointer-null))
	      (stack '()))

	  (define (start-callback element attributes)
	    (debug "start ~s ~s" element attributes)
	    (set! stack (cons (cons element attributes) stack)))

	  (define (end-callback element)
	    (debug "end ~s" element)
	    (set! stack (cons element stack)))

	  (define (data-callback data)
	    (debug "data ~s" data)
	    (set! stack (cons data stack)))

	  (xml-set-element-handler parser
				   (make-xml-start-callback start-callback)
				   (make-xml-end-callback   end-callback))
	  (xml-set-character-data-handler parser (make-xml-data-callback data-callback))
	  (xml-parse/string parser document-1 #f)
	  (xml-parse/string parser document-2 #t)
	  (reverse stack)))
    => '((stuff)
	 (thing)
	 (alpha (a1 . "1") (a2 . "2")) "one" alpha
	 (beta) "tw" "o" beta
	 thing
	 (thing)
	 (alpha) "123" alpha
	 (beta) "456" beta
	 thing
	 stuff))

  #t)


;;;; done

(check-report)

;;; end of file
