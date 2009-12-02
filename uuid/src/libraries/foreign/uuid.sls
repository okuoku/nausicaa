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


(library (foreign uuid)
  (export
    ;; UUID object handling
    uuid-create			uuid-destroy
    uuid-clone

    ;; UUID generation
    uuid-load			uuid-make

    ;; UUID comparison
    uuid-isnil?			uuid-compare
    uuid-equal?

    ;; UUID import/export
    uuid-import			uuid-export

    ;; library utilities
    uuid-error			uuid-version

    ;; version number
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
    (foreign uuid primitives)
    (foreign uuid sizeof)))

;;; end of file
