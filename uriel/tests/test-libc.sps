;;;
;;;Part of: Uriel libraries
;;;Contents: tests for the GNU C library interface
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-11-30 17:32:14 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (uriel posix)
  (uriel glibc))

(check-set-mode! 'report-failed)


;;;; environment variables

(check
    (let ()
      (setenv 'CIAO 'pasta 1)
      (clearenv)
      (getenv 'CIAO))
  => "")



;;;; done

(check-report)

;;; end of file
