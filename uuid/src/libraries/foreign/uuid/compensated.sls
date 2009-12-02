;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/UUID
;;;Contents: compensated constructors
;;;Date: Wed Dec  2, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign uuid compensated)
  (export
    uuid-create/c uuid-clone/c uuid-load/c uuid-make/c
    uuid-import/c)
  (import (rnrs)
    (compensations)
    (foreign uuid))

  (define-syntax define-compensated
    (syntax-rules ()
      ((_ ?name ?func)
       (define (?name . args)
	 (letrec ((uuid (compensate
			    (apply ?func args)
			  (with
			   (uuid-destroy)))))
	   uuid)))))

  (define-compensated uuid-create/c	uuid-create)
  (define-compensated uuid-clone/c	uuid-clone)
  (define-compensated uuid-load/c	uuid-load)
  (define-compensated uuid-make/c	uuid-make)
  (define-compensated uuid-import/c	uuid-import))

;;; end of file
