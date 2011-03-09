;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for keywords
;;;Date: Sun Jul  5, 2009
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


(import (nausicaa)
  (nausicaa checks)
  (nausicaa keywords))

(check-set-mode! 'report-failed)
(display "*** testing keywords\n")



(define :alpha   (keyword alpha))
(define :alpha1  (keyword alpha))
(define-keywords :beta :delta)

(check (keyword? :alpha) => #t)
(check (keyword->symbol :alpha) => 'alpha)
(check (keyword->string :alpha) => "alpha")
(check (symbol->keyword 'alpha) => :alpha)
(check (string->keyword "alpha") => :alpha)

(check (keyword? :beta) => #t)
(check (keyword->symbol :beta) => ':beta)

(let ((:alpha 123))
  (check (keyword? :alpha) => #f))

(check (eq? :alpha :alpha1) => #t)

(with-keywords (:a :b :c :d)
  (check (keyword? :a) => #t)
  (check (eq? :a :b)   => #f))

(check
    (let-keywords (list :a 1 :b 2 :d 4) #f
		  ((a :a #\a)
		   (b :b #\b)
		   (c :c #\c)
		   (d :d #\d))
      (list a b c d))
  => '(1 2 #\c 4))

(check
    (let-keywords '() #f
		  ((a :a #\a)
		   (b :b #\b)
		   (c :c #\c)
		   (d :d #\d))
      (list a b c d))
  => '(#\a #\b #\c #\d))

(check
    (with-keywords (:k)
      (let-keywords (list :a 1 :b 2 :k 999 :d 4) #t
		    ((a :a #\a)
		     (b :b #\b)
		     (c :c #\c)
		     (d :d #\d))
	(list a b c d)))
  => '(1 2 #\c 4))

(check
    (let-keywords '() #t
		  ((a :a #\a)
		   (b :b #\b)
		   (c :c #\c)
		   (d :d #\d))
      (list a b c d))
  => '(#\a #\b #\c #\d))

(check
    (guard (exc (else (condition-message exc)))
      (with-keywords (:k)
	(let-keywords (list :a 1 :b 2 :k 999 :d 4) #f
		      ((a :a #\a)
		       (b :b #\b)
		       (c :c #\c)
		       (d :d #\d))
	  (list a b c d))))
  => "unrecognised option")

(check
    (guard (exc (else (list (condition-message exc)
			    (condition-irritants exc))))
      (let-keywords (list :a 1 2 :d 4) #f
		    ((a :a #\a)
		     (b :b #\b)
		     (c :c #\c)
		     (d :d #\d))
	(list a b c d)))
  => '("expected keyword option" (2)))

(check
    (guard (exc (else (condition-message exc)))
      (let-keywords (list :a 1 :d) #t
		    ((a :a #\a)
		     (b :b #\b)
		     (c :c #\c)
		     (d :d #\d))
	(list a b c d)))
  => "keyword option requires value")


;;;; done

(check-report)

;;; end of file
