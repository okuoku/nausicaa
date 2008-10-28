;;
;;Part of: Uriel libraries
;;Contents: CLOS-like object system
;;Date: Sun Oct 26, 2008
;;
;;Abstract
;;
;;
;;
;;Copyright (c) 2008 Marco Maggi
;;
;;This  program  is free  software:  you  can redistribute  it
;;and/or modify it  under the terms of the  GNU General Public
;;License as published by the Free Software Foundation, either
;;version  3 of  the License,  or (at  your option)  any later
;;version.
;;
;;This  program is  distributed in  the hope  that it  will be
;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;details.
;;
;;You should  have received a  copy of the GNU  General Public
;;License   along   with    this   program.    If   not,   see
;;<http://www.gnu.org/licenses/>.
;;

(import (rnrs))

(export

 ;; Class record functions.
 make-class class? class-name list-of-supers list-of-slots

 ;; Instance record functions.
 make-instance instance? class-of

 )

(define-record-type class
  (fields class-name list-of-supers list-of-slots))

(define-record-type instance
  (fields class-of))


(define <object>
  (make-class '<object> '() '()))

(define (make class . args)
  (make-instance class))

(define a (make <object>))



;; (define (class-of obj)
;;   (cond [(<object>? obj)
;; 	 #t]
;; 	<object>))


;;; end of file
