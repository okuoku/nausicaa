;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: miscellaneous helper functions
;;;Date: Tue Nov  2, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (classes helpers)
  (export
    %variable-name->Setter-name		%variable-name->Getter-name)
  (import (rnrs))


(define (%variable-name->Setter-name variable-name-stx)
  (datum->syntax variable-name-stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name-stx))
				 ".__nausicaa_private_Setter_identifier_syntax"))))

(define (%variable-name->Getter-name variable-name-stx)
  (datum->syntax variable-name-stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name-stx))
				 ".__nausicaa_private_Getter_identifier_syntax"))))


;;;; done

)

;;; end of file
