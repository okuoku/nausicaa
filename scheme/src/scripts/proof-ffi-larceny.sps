;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proof script for Larceny's FFI
;;;Date: Fri Jul  3, 2009
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



(import (rnrs)
  (primitives
   foreign-procedure
   %peek8 %peek8u %peek16 %peek16u %peek32 %peek32u %peek-pointer
   %poke8 %poke8u %poke16 %poke16u %poke32 %poke32u %poke-pointer
   void*-double-set! void*-double-ref void*-float-set! void*-float-ref
   void*? void*-rt record-constructor void*->address))

(define platform-malloc
  (foreign-procedure "malloc" '(unsigned) 'void*))

(define p (platform-malloc 4096))
(define a (void*->address p))

(let ((v (- (expt 2 7) 1)))
  (%poke8 a v)
  (write (list v (%peek8 a)))
  (newline))

(let ((v (- (expt 2 15) 1)))
  (%poke16 a v)
  (write (list v (%peek16 a)))
  (newline))

(let ((v (- (expt 2 31) 1)))
  (%poke32 a v)
  (write (list v (%peek32 a)))
  (newline))

(newline)

(let ((v (- (expt 2 8) 1)))
  (%poke8u a v)
  (write (list v (%peek8u a)))
  (newline))

(let ((v (- (expt 2 16) 1)))
  (%poke16u a v)
  (write (list v (%peek16u a)))
  (newline))

(let ((v (- (expt 2 32) 1)))
  (%poke32u a v)
  (write (list v (%peek32u a)))
  (newline))

(newline)

(let ((v (- (expt 2 15) 2)))
  (%poke16 a v)
  (write (list v (%peek16 a)))
  (newline))

;;; end of file
