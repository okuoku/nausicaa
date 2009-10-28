;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/UUID
;;;Contents: high level API for UUID
;;;Date: Tue Oct 27, 2009
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


(library (foreign uuid)
  (export
    ;; UUID object handling
    uuid-create			uuid-create/c
    uuid-destroy
    uuid-clone			uuid-clone/c

    ;; UUID generation
    uuid-load			uuid-load/c
    uuid-make			uuid-make/c

    ;; UUID comparison
    uuid-isnil?
    uuid-compare

    ;; UUID import/export
    uuid-import			uuid-import/c
    uuid-export

    ;; library utilities
    uuid-error
    uuid-version)
  (import (rnrs)
    (foreign uuid primitives)
    (foreign uuid sizeof))


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
(define-compensated uuid-load/c		uuid-load)
(define-compensated uuid-make/c		uuid-make)
(define-compensated uuid-import/c	uuid-import)



;;;; done

)

;;; end of file
