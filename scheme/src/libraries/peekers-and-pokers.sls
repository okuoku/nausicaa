;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: aliases for the peekers and pokers
;;;Date: Tue Jul  7, 2009
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


(library (peekers-and-pokers)
  (export
    (rename (pointer-ref-c-signed-char		peek-signed-char)
	    (pointer-ref-c-signed-short		peek-signed-short)
	    (pointer-ref-c-signed-int		peek-signed-int)
	    (pointer-ref-c-signed-long		peek-signed-long)
	    (pointer-ref-c-signed-long-long	peek-signed-long-long)
	    (pointer-ref-c-unsigned-char	peek-unsigned-char)
	    (pointer-ref-c-unsigned-short	peek-unsigned-short)
	    (pointer-ref-c-unsigned-int		peek-unsigned-int)
	    (pointer-ref-c-unsigned-long	peek-unsigned-long)
	    (pointer-ref-c-unsigned-long-long	peek-unsigned-long-long)
	    (pointer-ref-c-float		peek-float)
	    (pointer-ref-c-double		peek-double)
	    (pointer-ref-c-pointer		peek-pointer)

	    (pointer-set-c-signed-char!		poke-signed-char!)
	    (pointer-set-c-signed-short!	poke-signed-short!)
	    (pointer-set-c-signed-int!		poke-signed-int!)
	    (pointer-set-c-signed-long!		poke-signed-long!)
	    (pointer-set-c-signed-long-long!	poke-signed-long-long!)
	    (pointer-set-c-unsigned-char!	poke-unsigned-char!)
	    (pointer-set-c-unsigned-short!	poke-unsigned-short!)
	    (pointer-set-c-unsigned-int!	poke-unsigned-int!)
	    (pointer-set-c-unsigned-long!	poke-unsigned-long!)
	    (pointer-set-c-unsigned-long-long!	poke-unsigned-long-long!)
	    (pointer-set-c-float!		poke-float!)
	    (pointer-set-c-double!		poke-double!)
	    (pointer-set-c-pointer!		poke-pointer!)

	    (array-ref-c-signed-char		peek-array-signed-char)
	    (array-ref-c-signed-short		peek-array-signed-short)
	    (array-ref-c-signed-int		peek-array-signed-int)
	    (array-ref-c-signed-long		peek-array-signed-long)
	    (array-ref-c-signed-long-long	peek-array-signed-long-long)
	    (array-ref-c-unsigned-char		peek-array-unsigned-char)
	    (array-ref-c-unsigned-short		peek-array-unsigned-short)
	    (array-ref-c-unsigned-int		peek-array-unsigned-int)
	    (array-ref-c-unsigned-long		peek-array-unsigned-long)
	    (array-ref-c-unsigned-long-long	peek-array-unsigned-long-long)
	    (array-ref-c-float			peek-array-float)
	    (array-ref-c-double			peek-array-double)
	    (array-ref-c-pointer		peek-array-pointer)

	    (array-set-c-signed-char!		poke-array-signed-char!)
	    (array-set-c-signed-short!		poke-array-signed-short!)
	    (array-set-c-signed-int!		poke-array-signed-int!)
	    (array-set-c-signed-long!		poke-array-signed-long!)
	    (array-set-c-signed-long-long!	poke-array-signed-long-long!)
	    (array-set-c-unsigned-char!		poke-array-unsigned-char!)
	    (array-set-c-unsigned-short!	poke-array-unsigned-short!)
	    (array-set-c-unsigned-int!		poke-array-unsigned-int!)
	    (array-set-c-unsigned-long!		poke-array-unsigned-long!)
	    (array-set-c-unsigned-long-long!	poke-array-unsigned-long-long!)
	    (array-set-c-float!			poke-array-float!)
	    (array-set-c-double!		poke-array-double!)
	    (array-set-c-pointer!		poke-array-pointer!)))
  (import (foreign memory)))

;;; end of file
