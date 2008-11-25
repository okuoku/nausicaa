;;;
;;;Part of: Nausicaa/MP
;;;Contents: foreign functions interface to GMP
;;;Date: Tue Nov 25, 2008
;;;Time-stamp: <2008-11-25 22:18:00 marco>
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

(library (mp gmp)
  (export
    mpf_init)
  (import (rnrs)
;;    (uriel printing)
    (uriel ffi))


;;;; code

(define gmp-lib
  (open-shared-object 'libgmp.so))

(shared-object gmp-lib)
;(with-shared-object gmp-lib

  (define-c-function mpf_set_default_prec
    (void mpf_set_default_prec (ulong)))

  (define-c-function mpf_set_default_prec
    (ulong mpf_get_default_prec (void)))

  (define-c-function mpf_init
    (void mpf_init (pointer)))

  (define-c-function mpf_clear
    (void mpf_init (pointer)))

;  )



;;;; done

)

;;; end of file
