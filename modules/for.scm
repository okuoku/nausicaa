;;
;; Part of: Mykarus
;; Contents: for syntax
;; Date: Wed Oct 22, 2008
;; 
;; Abstract
;; 
;;	The FOR iteration syntax.
;; 
;; Copyright (c) 2008 Marco Maggi
;; 
;; This program is free  software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;; 
;; This  program is  distributed  in  the hope  that  it will  be
;; useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;; warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;; 
;; You  should have  received a  copy of  the GNU  General Public
;; License    along   with   this    program.    If    not,   see
;; <http://www.gnu.org/licenses/>.
;; 


;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(library (mykarus for)
	 (import (rnrs))
	 (export (for))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Code.
;; ------------------------------------------------------------

;;Example:
;;
;;  (for (i 0) (< i 5) (set! i (+ 1 i))
;;    (display i)
;;    (newline))
(define-syntax for
  (syntax-rules ()
    [(_ (?init-vars ...) (?clause ...) (?step ...) ?body ...)
     (let loop (?init-vars ...)
       (when (?clause ...)
	 (begin ?body ...)
	 (begin ?step ...)
	 (loop)))]))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; End of LIBRARY form.
;; ------------------------------------------------------------

)

;;; end of file
