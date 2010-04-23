;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for compensation stacks
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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



(import (nausicaa)
  (checks)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing compensations\n")

;;; --------------------------------------------------------------------

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))


;;;; on error compensations: no error evaluation

;;;No COMPENSATE forms.
(check
    (with-result
     (with-compensations/on-error
       (add-result 0)
       1))
  => '(1 (0)))

(check
    (with-result
     (with-compensations/on-error
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       4))
  => '(4 (1 2 3 0)))

;;; No error, explicit compensations invocation.
(check
    (with-result
     (with-compensations/on-error
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       (run-compensations))
     4)
  => '(4 (1 2 3 0 -3 -2 -1)))


;;;; on return compensations: no error evaluation

;;;No COMPENSATE forms.
(check
    (with-result
     (with-compensations
       (add-result 0)
       1))
  => '(1 (0)))

(check
    (with-result
     (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       4))
  => '(4 (1 2 3 0 -3 -2 -1)))

;;; Explicit compensations invocation.
(check
    (with-result
     (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
       (add-result 0)
       (run-compensations)
       4))
  => '(4 (1 2 3 0 -3 -2 -1)))


;;;; on error compensations: error in the body

;;;After the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations/on-error
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	0)))
  => '(misc-error (1 2 3 4 -3 -2 -1)))

;;;Between the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations/on-error
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	  (compensate (add-result 3) (with (add-result -3)))
	0)))
  => '(misc-error (1 2 4 -2 -1)))

;;;Before the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations/on-error
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	0)))
  => '(misc-error (4)))



;;;; on return compensations: error in the body

;;;After the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	0)))
  => '(misc-error (1 2 3 4 -3 -2 -1)))

;;; Between the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	  (compensate (add-result 3) (with (add-result -3)))
	0)))
  => '(misc-error (1 2 4 -2 -1)))

;;;Before the COMPENSATE forms.
(check
    (with-result
     (catch-exception
      (with-compensations
	(add-result 4)
	(raise 'misc-error)
	(add-result 5)
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	0)))
  => '(misc-error (4)))


(parametrise ((check-test-name	'allocation-form-error))

  (check
      (with-result
       (catch-exception
	(with-compensations/on-error
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
		(raise 'alloc-error)
		(add-result 3)
	      (with
	       (add-result -2)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  0)))
    => '(alloc-error (1 2 -1)))

  (check
      (with-result
       (catch-exception
	(with-compensations
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
		(raise 'alloc-error)
		(add-result 3)
	      (with
	       (add-result -2)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  0)))
    => '(alloc-error (1 2 -1)))

  #t)


(parametrise ((check-test-name	 'release-form-error))

  (check
      (with-result
       (catch-exception
	(with-compensations/on-error
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
	      (with
	       (add-result -2)
	       (raise 'release-error)
	       (add-result -3)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  0)))
    => '(0 (1 2 4 5)))

  (check
      (with-result
       (catch-exception
	(with-compensations/on-error
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
	      (with
	       (add-result -2)
	       (raise 'release-error)
	       (add-result -3)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  (raise 'body-error)
	  (add-result 6)
	  0)))
    => '(body-error (1 2 4 5 -4 -2 -1)))



  (check
      (with-result
       (catch-exception
	(with-compensations
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
	      (with
	       (add-result -2)
	       (raise 'release-error)
	       (add-result -3)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  0)))
    => '(0 (1 2 4 5 -4 -2 -1)))

  (check
      (with-result
       (catch-exception
	(with-compensations
	    (compensate (add-result 1) (with (add-result -1)))
	    (compensate
		(add-result 2)
	      (with
	       (add-result -2)
	       (raise 'release-error)
	       (add-result -3)))
	    (compensate (add-result 4) (with (add-result -4)))
	  (add-result 5)
	  (raise 'body-error)
	  (add-result 6)
	  0)))
    => '(body-error (1 2 4 5 -4 -2 -1)))

  #t)


(parametrise ((check-test-name	'resources))

  (check
      (with-result
       (with-compensations
	 (letrec
	     ((item (compensate
			123
		      (with
		       (add-result item)))))
	   (add-result 1)
	   0)))
    => '(0 (1 123)))

  (check
      (with-result
       (with-compensations
	 (letrec*
	     ((item1 (compensate
			 123
		       (with
			(add-result item2))))
	      (item2 (compensate
			 456
		       (with
			(add-result item1)))))
	   (add-result 1)
	   0)))
    => '(0 (1 123 456)))

  (check
      (with-result
       (with-compensations/on-error
	 (letrec
	     ((item (compensate
			123
		      (with
		       (add-result item)))))
	   (add-result 1)
	   0)))
    => '(0 (1)))

  (check
      (with-result
       (with-compensations/on-error
	 (letrec
	     ((item1 (compensate
			 123
		       (with
			(add-result item2))))
	      (item2 (compensate
			 456
		       (with
			(add-result item1)))))
	   (add-result 1)
	   0)))
    => '(0 (1)))

  #t)


(parametrise ((check-test-name	'syntax-errors))

  (check	;missing WITH
      (guard (E ((syntax-violation? E)
		 #t))
	(eval '(with-compensations
		 (letrec ((a (compensate 123)))
		   #t))
	      (environment '(rnrs) '(compensations))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
