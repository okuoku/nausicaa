;; q.test --- test (ice-9 q) module -*- scheme -*-
;;
;; Copyright 2008 Marco Maggi <marcomaggi@gna.org>
;; Copyright 2004, 2006 Free Software Foundation, Inc.
;;
;; This library is free  software; you can redistribute it and/or
;; modify it  under the  terms of the  GNU Lesser  General Public
;; License as  published by the Free  Software Foundation; either
;; version  2.1 of  the License,  or (at  your option)  any later
;; version.
;;
;; This  library is  distributed  in  the hope  that  it will  be
;; useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;; warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;; PURPOSE.  See  the GNU Lesser General Public  License for more
;; details.
;;
;; You  should have  received a  copy of  the GNU  Lesser General
;; Public License along  with this library; if not,  write to the
;; Free  Software  Foundation, Inc.,  51  Franklin Street,  Fifth
;; Floor, Boston, MA 02110-1301 USA



;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(import (rnrs)
  (uriel test)
  (uriel queue))

(check-set-mode! 'report-failed)

;; ------------------------------------------------------------

(define-syntax check-queue-is-empty-error
  (syntax-rules ()
    [(_ ?form ...)
     (check
      (guard (exc (else exc))
	     ?form ...)
      => 'queue-is-empty)]))

;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Tests.
;; ------------------------------------------------------------

(let ((q (make-q)))
  (check-queue-is-empty-error (q-pop! q))
  (check-for-true (q? q)))

(let ((x (cons 1 2))
      (q (make-q)))
  (q-push! q x)
  (check-for-true (eq? x (q-pop! q)))
  (check-for-true (q? q))
  (check-queue-is-empty-error (q-pop! q))
  (check-for-true (q? q)))

(let ((x (cons 1 2))
      (y (cons 3 4))
      (q (make-q)))
  (q-push! q x)
  (q-push! q y)

  (check-for-true (eq? y (q-pop! q)))
  (check-for-true (q? q))
  (check-for-true (eq? x (q-pop! q)))
  (check-for-true (q? q))
  (check-queue-is-empty-error (q-pop! q))
  (check-for-true (q? q)))

(let ((x (cons 1 2))
      (y (cons 3 4))
      (z (cons 5 6))
      (q (make-q)))
  (q-push! q x)
  (q-push! q y)
  (q-push! q z)

  (check-for-true (eq? z (q-pop! q)))
  (check-for-true (q? q))
  (check-for-true (eq? y (q-pop! q)))
  (check-for-true (q? q))
  (check-for-true (eq? x (q-pop! q)))
  (check-for-true (q? q))
  (check-queue-is-empty-error (q-pop! q))
  (check-for-true (q? q)))

;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

(check-report)

;;; end of file
