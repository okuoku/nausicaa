;;;
;;;Part of: Uriel libraries
;;;Contents: tests for compensation stacks
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(import (rnrs)
  (only (ikarus) printf pretty-print)
  (srfi parameters)
  (srfi lightweight-testing)
  (uriel test)
  (uriel lang)
  (uriel define-macro))

(check-set-mode! 'report-failed)

;;; --------------------------------------------------------------------


;;; ------------------------------------------------------------
;;; No error evaluation.
;;; ------------------------------------------------------------

;;; No error.
(check
    (with-result
     (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       4))
  => '(4 (1 2 3 0)))

;;; No error, explicit compensations invocation.
(check
    (with-result
     (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       (run-compensations))
     4)
  => '(4 (1 2 3 0 -3 -2 -1)))

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Evaluations with error in body.
;;; ------------------------------------------------------------

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0)
	(raise 'misc-error))))
  => '(misc-error (1 2 3 0 -3 -2 -1)))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(raise 'misc-error)
	(add-result 0))))
  => '(misc-error (1 2 3 -3 -2 -1)))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	(raise 'misc-error)
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0))))
  => '(misc-error (1 2 -2 -1)))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	(raise 'misc-error)
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0))))
  => '(misc-error (1 -1)))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	(raise 'misc-error)
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0))))
  => '(misc-error (1 -1)))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-compensations
	(raise 'misc-error)
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0))))
  => '(misc-error ()))

;;; Error the body.
(check
    (with-result
     (catch-exception
      (with-deferred-exception-handler
	  (lambda (exc)
	    (add-result -4))
	(with-compensations
	    (compensate
		(add-result 1)
	      (with
	       (add-result -1)))
	    (compensate
		(add-result 2)
	      (with
	       (add-result -2)))
	    (compensate
		(add-result 3)
	      (with
	       (add-result -3)))
	  (add-result 0)
	  (raise 'misc-error)))))
  => '(misc-error (1 2 3 0 -3 -2 -1)))

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Error in resource allocation.
;;; ------------------------------------------------------------

;;; Error in resource allocation.
(check
    (with-result
     (catch-exception
      (with-deferred-exception-handler
	  (lambda (exc)
	    (add-result -4))
	(with-compensations
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
		(raise 'misc-error)
	      (with
	       (add-result -2)))
	    (compensate (add-result 3) (with (add-result -3)))
	  (add-result 0)))))
  => '(misc-error (1 2 -1)))

;;; Error in compensation form.
(check
  (with-result
   (catch-exception
    (with-deferred-exception-handler
	(lambda (exc)
	  (add-result -4)
	  (add-result exc))
      (with-compensations #t
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2)
	     (raise 'nested-error)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0)
	9))))
  => '(9 (1 2 3 0 -3 -2 -1 -4 nested-error)))

;;; Raising from the body and error in compensation form.
(check
  (with-result
   (catch-exception
    (with-deferred-exception-handler
	(lambda (exc)
	  (add-result -4)
	  (add-result exc))
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2)
	     (raise 'nested-error)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 0)
	(raise 'first-error)))))
  => '(first-error (1 2 3 0 -3 -2 -1 -4 nested-error)))

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Let form, no error.
;;; ------------------------------------------------------------

(check
    (with-result
     (let-compensations*
	 ((a (compensate (add-result 1) (with (add-result -1))))
	  (b (compensate (add-result 2) (with (add-result -2))))
	  (c (compensate (add-result 3) (with (add-result -3)))))
       (add-result 0)
       4))
  => '(4 (1 2 3 0 -3 -2 -1)))

;;; Nested COMPENSATE.
(check
    (with-result
     (let-compensations*
	 ((a (compensate (add-result 1) (with (add-result -1))))
	  (b (compensate (add-result 2) (with (add-result -2))))
	  (c (compensate (add-result 3) (with (add-result -3)))))
       (add-result 4)
       (compensate (add-result 5) (with (add-result -5)))
       (add-result 6)
       7))
  => '(7 (1 2 3 4 5 6 -5 -3 -2 -1)))

;;; Using bindings in binding forms.
(check
    (with-result
     (let-compensations*
	 ((a (compensate (add-result 1) 1 (with (add-result -1))))
	  (b (compensate (add-result 2) (+ a 2) (with (add-result -2))))
	  (c (compensate (add-result 3) (+ b 3) (with (add-result -3)))))
       (compensate (add-result 4) (with (add-result -4)))
       (add-result (list a b c))
       5))
  => '(5 (1 2 3 4 (1 3 6) -4 -3 -2 -1)))

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Let form with errors.
;;; ------------------------------------------------------------

;;; Error in body.
;; (check
;;     (with-result
;;      (catch-exception (with-deferred-exception-handler
;; 	      (lambda ()
;; 		(let-compensations
;; 		    ((a (compensate (add-result 1) (with (add-result -1))))
;; 		     (b (compensate (add-result 2) (with (add-result -2))))
;; 		     (c (compensate (add-result 3) (with (add-result -3)))))
;; 		  (add-result 0)
;; 		  (raise 'misc-error)
;; 		  (add-result 'a)))
;; 	      (lambda (key . args)
;; 		(add-result -4)
;; 		(add-result key))
;; 	      (lambda (key . args)
;; 		(add-result -5)))))
;;   => '(1 2 3 0 -3 -2 -1 -4 misc-error))

;;; Error in resource allocation.
;; (check
;;     (with-result
;;      (catch-exception (with-deferred-exception-handler
;; 	      (lambda ()
;; 		(let-compensations*
;; 		    ((a (compensate (add-result 1) (with (add-result -1))))
;; 		     (b (compensate (add-result 2)
;; 			    (raise 'misc-error)
;; 			  (with (add-result -2))))
;; 		     (c (compensate (add-result 3) (with (add-result -3)))))
;; 		  (add-result 0)))
;; 	      (lambda (key . args)
;; 		(add-result -4)
;; 		(add-result key))
;; 	      (lambda (key . args)
;; 		(add-result -5)
;; 		(add-result key)))))
;;   => '(1 2 -1 -4 misc-error))

;;; Raising from body and error in compensation form.
;; (check
;;     (with-result
;;      (catch-exception (with-deferred-exception-handler
;; 	      (lambda ()
;; 		(let-compensations*
;; 		    ((a (compensate (add-result 1) (with (add-result -1))))
;; 		     (b (compensate (add-result 2) (with
;; 						    (add-result -2)
;; 						    (raise 'nested-error))))
;; 		     (c (compensate (add-result 3) (with (add-result -3)))))
;; 		  (add-result 0)
;; 		  (raise 'misc-error)))
;; 	      (lambda (key . args)
;; 		(add-result -4)
;; 		(add-result key))
;; 	      (lambda (key . args)
;; 		(add-result -5)
;; 		(add-result key)))))
;;   => '(1 2 3 0 -3 -2 -1 -4 misc-error -5 nested-error))

;;; ------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

(check-report)

;;; end of file
