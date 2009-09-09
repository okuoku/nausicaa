;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for parser-tools libraries
;;;Date: Tue Sep  8, 2009
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


(import (nausicaa)
  (checks)
  (parser-tools lexical-token)
  (parser-tools source-location))

(check-set-mode! 'report-failed)
(display "*** testing parser-tools\n")


(parametrise ((check-test-name 'predicates))

  (check
      (source-location? (make-source-location/start "this"))
    => #t)

  (check
      (source-location? #f)
    => #f)

  (check
      (source-location? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (source-location?/or-false (make-source-location/start "this"))
    => #t)

  (check
      (source-location?/or-false #f)
    => #t)

  (check
      (source-location?/or-false 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (source-location?/start (make-source-location "this" 1 2 3))
    => #f)

  (check
      (source-location?/start (make-source-location/start "this"))
    => #t)

  (check
      (source-location?/start #f)
    => #f)

  (check
      (source-location?/start 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (source-location?/start/or-false (make-source-location "this" 1 2 3))
    => #f)

  (check
      (source-location?/start/or-false (make-source-location/start "this"))
    => #t)

  (check
      (source-location?/start/or-false #f)
    => #t)

  (check
      (source-location?/start/or-false 123)
    => #f)

;;; --------------------------------------------------------------------


  #t)


;;;; done

(check-report)

;;; end of file
