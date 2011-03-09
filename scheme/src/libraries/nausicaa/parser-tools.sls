;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: top library for common parser tools
;;;Date: Wed Jan 12, 2011
;;;
;;;Abstract
;;;
;;;	This library just reexports the most commonly used bindings from
;;;	(nausicaa parser-tools lexical-token) and (nausicaa parser-tools
;;;	source-location).
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa parser-tools)
  (export
    <lexical-token> <source-location>)
  (import
      (only (nausicaa parser-tools lexical-token) <lexical-token>)
    (only (nausicaa parser-tools source-location) <source-location>)))

;;; end of file
