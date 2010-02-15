;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: type definitions
;;;Date: Mon Feb 15, 2010
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


(library (foreign databases postgresql typedefs)
  (export
    <connection>		<connection-rtd>
    <connection>?
    pointer-><connection>	<connection>->pointer

    <fd>			<fd>?
    integer-><fd>		<fd>->integer

    )
  (import (rnrs))


(define-record-type (<connection> pointer-><connection> <connection>?)
  (fields (immutable pointer <connection>->pointer)))

(define <connection-rtd>
  (record-type-descriptor <connection>))

;;; --------------------------------------------------------------------

(define-record-type (<fd> integer-><fd> <fd>?)
  (nongenerative nausicaa:posix:<fd>)
  (fields (immutable object <fd>->integer)))


;;;; done

)

;;; end of file
