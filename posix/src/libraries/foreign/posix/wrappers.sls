;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: entity record wrappers
;;;Date: Sun Dec  6, 2009
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


(library (foreign posix wrappers)
  (export

    <posix-wrapper>
    make-<posix-wrapper>	<posix-wrapper>?
    <posix-wrapper>-object

    file-descriptor		file-descriptor?
    integer->file-descriptor	(rename (<posix-wrapper>-object file-descriptor->integer))

    <FILE*>			FILE*?
    pointer->FILE*		(rename (<posix-wrapper>-object FILE*->pointer))
    )
  (import (rnrs))


(define-record-type <posix-wrapper>
  (fields (immutable object)))

(define-record-type (file-descriptor integer->file-descriptor file-descriptor?)
  (parent <posix-wrapper>))

(define-record-type (<FILE*> pointer->FILE* FILE*?)
  (parent <posix-wrapper>))



;;;; done

)

;;; end of file
