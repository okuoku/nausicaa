;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: condition objects for gpg-error
;;;Date: Sun Dec 27, 2009
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


(library (foreign crypto gpg-error conditions)
  (export

    &gpg-error				gpg-error-condition?
    make-gpg-error-condition		raise-gpg-error
    condition-gpg-error-code

    )
  (import (rnrs)
    (foreign crypto gpg-error sizeof)
    (foreign crypto gpg-error primitives))


(define-condition-type &gpg-error &error
  make-gpg-error-condition gpg-error-condition?
  (code		condition-gpg-error-code))

(define-syntax raise-gpg-error
  (syntax-rules ()
    ((_ ?who ?code ?irr ...)
     (let ((who  ?who)
	   (code ?code)
	   (irrs (list ?irr ...)))
       (if (null? irrs)
	   (raise (condition (make-who-condition who)
			     (make-message-condition (gpg-strerror code))
			     (make-gpg-error-condition code)))
	 (raise (condition (make-who-condition who)
			   (make-message-condition (gpg-strerror code))
			   (make-gpg-error-condition code)
			   (make-irritants-condition irrs))))))))


;;;; done

)

;;; end of file
