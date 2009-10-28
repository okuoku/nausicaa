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
    uuid-equal?

    ;; UUID import/export
    uuid-import			uuid-import/c
    uuid-export

    ;; library utilities
    uuid-error
    uuid-version

    ;; type inspection
    uuid_rc_t
    sizeof-uuid_rc_t
    strideof-uuid_rc_t
    alignof-uuid_rc_t

    uuid_fmt_t
    sizeof-uuid_fmt_t
    strideof-uuid_fmt_t
    alignof-uuid_fmt_t

    UUID_VERSION

    ;; encoding octet stream lengths
    UUID_LEN_BIN
    UUID_LEN_STR
    UUID_LEN_SIV

    ;; enum uuid_rc_t
    UUID_RC_OK
    UUID_RC_ARG
    UUID_RC_MEM
    UUID_RC_SYS
    UUID_RC_INT
    UUID_RC_IMP

    ;; UUID make modes
    UUID_MAKE_V1
    UUID_MAKE_V3
    UUID_MAKE_V4
    UUID_MAKE_V5
    UUID_MAKE_MC

    ;; enum uuid_fmt_t
    UUID_FMT_BIN
    UUID_FMT_STR
    UUID_FMT_SIV
    UUID_FMT_TXT)
  (import (rnrs)
    (compensations)
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
