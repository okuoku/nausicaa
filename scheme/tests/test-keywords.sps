;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for keywords
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;




(import (nausicaa)
  (checks)
  (keywords))

(check-set-mode! 'report-failed)
(display "*** testing keywords\n")



(define :alpha   (make-keyword alpha))
(define :alpha1  (make-keyword alpha))
(define-keyword :beta)

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


;;;; done

(check-report)

;;; end of file
