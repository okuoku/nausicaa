;;;
;;;Part of: Nausicaa/ScmObj
;;;Contents: object layout tests
;;;Date: Sun Nov 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


;;;page
;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(import (rnrs)
        (only (ikarus) printf pretty-print)
        (srfi lightweight-testing)
	(scmobj))

(check-set-mode! 'report-failed)

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Code.
;;; ------------------------------------------------------------

(define-class <biologic>)
(define-class <fruit> (<biologic>))
(define-class <apple> (<fruit>) (:variety :colour :quality))
(define-class <price> () (:tag))
(define-class <priced-apple> (<apple> <price>))

(define o (make <apple>
            ':variety 'renetta
            ':colour  'green
            ':quality 'high))

(define p (make <priced-apple>
            ':variety 'renetta
            ':colour  'green
            ':quality 'high
            ':tag 100))


;; (printf "\nthis is <class>\n")
;; (pretty-print <class>)

;; (printf "\nthis is <biologic>\n")
;; (pretty-print <biologic>)

;; (printf "\nthis is <fruit>\n")
;; (pretty-print <fruit>)

;; (printf "\nthis is <apple>\n")
;; (pretty-print <apple>)

;; (printf "\nthis is o\n")
;; (pretty-print o)

;; (printf "\nthis is <priced-apple>\n")
;; (pretty-print <priced-apple>)

;; (printf "\nthis is p\n")
;; (pretty-print p)


;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

(check-report)

;;; end of file
