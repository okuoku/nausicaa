;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for submodules library
;;;Date: Thu Oct 14, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (submodules)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing submodules\n")


(parametrise ((check-test-name	'basic))

  (let ()	;explicit prefix
    (submodule this
        (export a b c)
	(prefix that.)
      (define a 1)
      (define b 2)
      (define d 3)
      (define (c)
	(+ a b d)))

    (check (that.c) => 6)
    (check (list that.a that.b) => '(1 2))
    ;;(write d)(newline)
    #f)

  (let ()	;implicit prefix
    (submodule this
        (export a b c)
      (define a 1)
      (define b 2)
      (define d 3)
      (define (c)
	(+ a b d)))

    (check (this.c) => 6)
    (check (list this.a this.b) => '(1 2))
    ;;(write d)(newline)
    #f)

  (let ()	;empty string prefix
    (submodule this
        (export a b c)
	(prefix "")
      (define a 1)
      (define b 2)
      (define d 3)
      (define (c)
	(+ a b d)))

    (check (c) => 6)
    (check (list a b) => '(1 2))
    ;;(write d)(newline)
    #f)

  (let ()	;empty prefix
    (submodule this
        (export a b c)
	(prefix)
      (define a 1)
      (define b 2)
      (define d 3)
      (define (c)
	(+ a b d)))

    (check (c) => 6)
    (check (list a b) => '(1 2))
    ;;(write d)(newline)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
