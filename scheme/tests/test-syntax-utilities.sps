;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for syntax utility functions
;;;Date: Wed May 26, 2010
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


#!r6rs
(import (for (nausicaa) run expand (meta -1))
  (nausicaa language syntax-utilities)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing syntax-utilities\n")


(parametrise ((check-test-name	'unwrap))

  (check
      (unwrap-syntax-object #'(1 (2 3) 4))
    => '(1 (2 3) 4))

  (check
      (unwrap-syntax-object #'(1 #(2 3) 4))
    => '(1 #(2 3) 4))

  ;; (check
  ;;     (unwrap-syntax-object #'(1 '#(2 3) 4))
  ;;   => `(1 (,(syntax quote) #(2 3)) 4))

  #t)


(parametrise ((check-test-name	'predicates))

  (check
      (quoted-syntax-object? #'(quote alpha))
    => #t)

  (check
      (quoted-syntax-object? #'(alpha))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
