;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: feature-based conditional expansion
;;;Date: Mon Apr  5, 2010
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

;;;Copyright notice for the implementation of COND-EXPAND.
;;;
;;;Copyright (c) 2008-2009 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(library (cond-expand)
  (export cond-expand)
  (import (rnrs)
    (for (cond-expand registry) expand))


(define-syntax cond-expand
  (lambda (stx)
    (syntax-case stx (and or not else)
      ((_)
       (syntax-violation #f "unfulfilled cond-expand" stx))

      ((_ (else ?body ...))
       (syntax (begin ?body ...)))

      ((_ ((and) ?body ...) ?more-clauses ...)
       (syntax (begin ?body ...)))

      ((_ ((and ?req1 ?req2 ...) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		;;It is right to put ?more-clauses in both the branches,
		;;because only one will be evaluated: the inner if ?req1
		;;is found, the outer if ?req1 is not found.
		(?req1 (cond-expand
			((and ?req2 ...) ?body ...)
			?more-clauses ...))
		?more-clauses ...)))

      ((_ ((or) ?body ...) ?more-clauses ...)
       (syntax (cond-expand ?more-clauses ...)))

      ((_ ((or ?req1 ?req2 ...) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		(?req1	(begin ?body ...))
		(else	(cond-expand
			 ((or ?req2 ...) ?body ...)
			 ?more-clauses ...)))))

      ((_ ((not ?req) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		(?req	(cond-expand ?more-clauses ...))
		(else	?body ...))))

      ((_ (?feature-id ?body ...) ?more-clauses ...)
       (if (member (syntax->datum (syntax ?feature-id)) (cons 'ypsilon registry-features))
           (syntax (begin ?body ...))
	 (syntax (cond-expand ?more-clauses ...)))))))


;;;; done

)

;;; end of file
