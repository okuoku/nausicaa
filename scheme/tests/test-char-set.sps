;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for char-set library
;;;Date: Thu Jan 22, 2009
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


;;;; setup

(import (scheme)
  (checks)
  (char-sets))

(check-set-mode! 'report-failed)
(display "*** testing char-set\n")




(parameterise ((check-test-name	'create))

  (check
      (char-set? (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set? (char-set-copy (char-set #\a #\b #\c)))
    => #t)


  )



(parameterise ((check-test-name 'preds))

  (check
      (char-set? char-set:lower-case)
    => #t)

  (check
      (char-set? char-set:upper-case)
    => #t)

  (check
      (char-set? char-set:title-case)
    => #t)

  (check
      (char-set? char-set:letter)
    => #t)

  (check
      (char-set? char-set:digit)
    => #t)

  (check
      (char-set? char-set:letter+digit)
    => #t)

  (check
      (char-set? char-set:graphic)
    => #t)

  (check
      (char-set? char-set:printing)
    => #t)

  (check
      (char-set? char-set:whitespace)
    => #t)

  (check
      (char-set? char-set:iso-control)
    => #t)

  (check
      (char-set? char-set:punctuation)
    => #t)

  (check
      (char-set? char-set:symbol)
    => #t)

  (check
      (char-set? char-set:hex-digit)
    => #t)

  (check
      (char-set? char-set:blank)
    => #t)

  (check
      (char-set? char-set:ascii)
    => #t)

  (check
      (char-set? char-set:empty)
    => #t)

  (check
      (char-set? char-set:full)
    => #t)


  )



(parameterise ((check-test-name	'iteration))

  (check
      (with-result
       (let ((cs (char-set #\a #\b #\c)))
	 (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
	     ((end-of-char-set? cursor)
	      #t)
	   (add-result (char-set-ref cs cursor)))))
    => '(#t (#\c #\b #\a)))

  (check
      (with-result
       (let ((cs (char-set #\a)))
	 (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
	     ((end-of-char-set? cursor)
	      #t)
	   (add-result (char-set-ref cs cursor)))))
    => '(#t (#\a)))



  )


;;;; done

(check-report)

;;; end of file
