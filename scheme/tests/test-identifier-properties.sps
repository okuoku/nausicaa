;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for expand-time identifier properties
;;;Date: Mon Nov  8, 2010
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
(import (nausicaa)
  (nausicaa language identifier-properties)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing expand-time identifier properties\n")


(parametrise ((check-test-name	'basic))

  (let ((a "ciao")
	(b 123))

    (define-syntax get-type
      (lambda (stx)
	(syntax-case stx ()
	  ((_ ?id)
	   (lookup-identifier-property #'?id #'type)))))

    (define-syntax get-spiffy
      (lambda (stx)
	(syntax-case stx ()
	  ((_ ?id)
	   (lookup-identifier-property #'?id #'spiffy)))))

    (define-identifier-property a type 'int)

    (check (get-type a) => 'int)
    (check (get-type b) => #f)

    (check (get-spiffy a) => #f)
    (check (get-spiffy b) => #f)

    #f)
  #t)


;;;; done

(check-report)

;;; end of file
