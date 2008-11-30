;;;
;;;Part of: Uriel libraries
;;;Contents: interface to POSIX functions for Ikarus
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-30 15:57:20 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (uriel posix compat)
  (export
    (rename (getenv getenv-stub)))
  (import (rnrs)
    (only (ikarus) getenv)))

;;; end of file
