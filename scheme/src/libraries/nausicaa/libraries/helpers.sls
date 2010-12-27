;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for libraries libraries
;;;Date: Tue Apr 20, 2010
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


(library (libraries helpers)
  (export
    %list-of-symbols?
    %list-of-renamings?
    %renaming? )
  (import (rnrs))

  (define (%list-of-symbols? obj)
    ;;Return true if OBJ is a list of symbols.
    ;;
    (and (list? obj)
	 (for-all symbol? obj)))

  (define (%list-of-renamings? obj)
    ;;Return true if  OBJ is a list of lists,  each holding two symbols.
    ;;It is meant to be the last part of a RENAME import specification.
    ;;
    (and (list? obj)
	 (for-all %renaming? obj)))

  (define (%renaming? obj)
    ;;Return true of OBJ is a  list holding two symbols.  It is meant to
    ;;be an elements in the last part of a RENAME import specification.
    ;;
    (and (list? obj)
	 (= 2 (length obj))
	 (symbol? (car obj))
	 (symbol? (cadr obj))))

  )

;;; end of file
