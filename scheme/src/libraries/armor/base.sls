;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: base library for ASCII armor modules
;;;Date: Sun Jan 24, 2010
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


(library (armor base)
  (export
    <armor-ctx>
    make-<armor-ctx>		<armor-ctx>?
    <armor-ctx>-update-function
    <armor-ctx>-finished-function)
  (import (rnrs))
  (define-record-type <armor-ctx>
    (fields (immutable update		<armor-ctx>-update-function)
	    (immutable finished?	<armor-ctx>-finished-function))))

;;; end of file
