;; 
;; Part of: Uriel libraries for Ikarus
;; Contents: test macros
;; Date: Sun Nov  9, 2008
;; 
;; Abstract
;; 
;; 
;; 
;; Copyright (c) 2008 Marco Maggi
;; 
;; This  program  is free  software:  you  can redistribute  it
;; and/or modify it  under the terms of the  GNU General Public
;; License as published by the Free Software Foundation, either
;; version  3 of  the License,  or (at  your option)  any later
;; version.
;; 
;; This  program is  distributed in  the hope  that it  will be
;; useful, but  WITHOUT ANY WARRANTY; without  even the implied
;; warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;; PURPOSE.   See  the  GNU  General Public  License  for  more
;; details.
;; 
;; You should  have received a  copy of the GNU  General Public
;; License   along   with    this   program.    If   not,   see
;; <http://www.gnu.org/licenses/>.
;; 

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(library (macros-test)
	 (export the-macro-1 the-macro-2)
	 (import (rnrs)
		 (uriel define-macro))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Code.
;; ------------------------------------------------------------

(define-macro (the-macro-1 a b c)
  `(list ,a ,b ,c))

(defmacro the-macro-2 (a b c)
  `(list ,a ,b ,c))


;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

) ;; end of library form


;;; end of file
