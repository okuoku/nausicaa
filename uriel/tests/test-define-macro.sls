;; 
;; Part of: Uriel libraries for Ikarus
;; Contents: tests for Common Lisp style macros
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

(import (ikarus)
	(srfi lightweight-testing)
	(uriel define-macro)
	(srfi receive))

(check-set-mode! 'report-failed)

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Tests.
;; ------------------------------------------------------------

(check
 (let ()
   (define-macro (proof a b c)
     `(list ,a ,b ,c))
   (proof 'one 'two 'three))
 => '(one two three))

(check
 (let ()
   (define-macro (proof a b c)
     `(,a ,(+ 1 b) ,c))
   (proof list 123 'three))
 => '(124 three))

(let ()
  (define-macro (false-if-exception expr)
    `(guard (exc (else #f))
	    ,expr))

  (check
   (false-if-exception (list 1 2 3))
   => '(1 2 3))

  (check
   (false-if-exception (raise 'slap))
   => #f))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

(check-report)

;;; end of file
