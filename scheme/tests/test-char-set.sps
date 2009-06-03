;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for char-set library
;;;Date: Thu Jan 22, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


;;;; setup

(import (scheme)
  (checks)
  (char-sets)
  (lists))

(check-set-mode! 'report-failed)
(display "*** testing char-sets\n")




(parameterise ((check-test-name	'create))

  (check
      (char-set? (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set? (char-set))
    => #t)

  (check
      (char-set? (char-set-copy (char-set #\a #\b #\c)))
    => #t)

  (check
      (char-set? (char-set-copy (char-set)))
    => #t)

  (let ((l '(#\a #\b #\a #\c #\c)))
    (check
	(char-set->list (apply char-set l))
      (=> list=?) l))

  (check
      (char-set->list (char-set/unique #\a #\b #\a #\c #\c))
    => '(#\a #\b #\c))

  )



(parameterise ((check-test-name 'inspection))

  (check
      (char-set-size (char-set))
    => 0)

  (check
      (char-set-size (char-set #\a))
    => 1)

  (check
      (char-set-size (char-set #\a #\b))
    => 2)

  )



(parameterise ((check-test-name 'predicates))

  (check
      (char-set? 123)
    => #f)

;;; --------------------------------------------------------------------

  (let ((cs (char-set '(#\a #\b))))
    (check
	(assert-char-set cs 'it)
      => cs))

  (check
      (guard (exc (else (condition-who exc)))
	(assert-char-set 123 'it))
    => 'it)

;;; --------------------------------------------------------------------

  (let ((cs (char-set '(#\a #\b))))
    (check
	(assert-char-set/or-false cs 'it)
      => cs))

  (check
      (guard (exc (else (condition-who exc)))
	(assert-char-set/or-false 123 'it))
    => 'it)

  (check
      (assert-char-set/or-false #f 'it)
    => #f)

;;; --------------------------------------------------------------------

  (let ((l '(#\a #\b)))
    (check
	(assert-list-of-chars l 'it)
      => l))

  (let ((l '()))
    (check
	(assert-list-of-chars l 'it)
      => l))

  (let ((l '(#\a 2 #\b)))
    (check
	(false-if-exception (assert-list-of-chars l 'it))
      => #f))

  (let ((l '(#\a 2 #\b)))
    (check
	(guard (exc (else (condition-who exc)))
	  (assert-list-of-chars l 'it))
      => 'it))

  (let ((l 0))
    (check
	(guard (exc (else (condition-who exc)))
	  (assert-list-of-chars l 'it))
      => 'it))

  )



(parameterise ((check-test-name 'conversion))

  (check
      (char-set? (list->char-set '(#\a #\b #\c)))
    => #t)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (list->char-set '(#\a #\b #\c)))
    => #t)

  (check
      (list=? '(#\a #\b #\c)
	      (char-set->list (char-set #\a #\b #\c)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (char-set? (string->char-set "abc"))
    => #t)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (string->char-set "abc"))
    => #t)

  (check
      (string=? (char-set->string (char-set #\a #\b #\c))
		"abc")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (->char-set '(#\a #\b #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

  (check
      (->char-set "abc")
    (=> char-set=?)
    (char-set #\a #\b #\c))

  (check
      (->char-set #\a)
    (=> char-set=?)
    (char-set #\a))

  (check
      (->char-set (char-set #\a #\b #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

  )



(parameterise ((check-test-name 'searching))

  (check
      (char-set-contains? (char-set) #\a)
    => #f)

  (check
      (char-set-contains? (char-set #\a) #\a)
    => #t)

  (check
      (char-set-contains? (char-set #\b #\a #\c) #\a)
    => #t)

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (char=? #\a ch))

    (check
	(char-set-count it? (char-set))
      => 0)

    (check
	(char-set-count it? (char-set #\a))
      => 1)

    (check
	(char-set-count it? (char-set #\b #\a #\c))
      => 1)

    (check
	(char-set-count it? (char-set #\b #\a #\c #\a))
      => 2)

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (char=? #\a ch))

    (check
	(char-set-find it? (char-set))
      => #f)

    (check
	(char-set-find it? (char-set #\a))
      => #\a)

    (check
	(char-set-find it? (char-set #\b #\a #\c))
      => #\a)

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (char=? #\a ch))

    (check
	(char-set-every it? (char-set))
      => #t)

    (check
	(char-set-every it? (char-set #\a))
      => #t)

    (check
	(char-set-every it? (char-set #\b #\a #\c))
      => #f)

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (char=? #\a ch))

    (check
	(char-set-any it? (char-set))
      => #f)

    (check
	(char-set-any it? (char-set #\a))
      => #t)

    (check
	(char-set-any it? (char-set #\b #\a #\c))
      => #t)

    (check
	(char-set-any it? (char-set #\b #\d #\c))
      => #f)

    )
  )



(parameterise ((check-test-name 'filter))

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c) char=?))

    (check
	(char-set-filter it? (char-set #\a #\b #\F #\G))
      (=> char-set=?)
      (char-set #\a #\b))

    (check
	(char-set-filter it? (char-set  #\F #\G))
      (=> char-set=?)
      (char-set))

    (check
	(char-set-filter it?
			 (char-set #\a #\b #\F #\G)
			 (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set #\a #\b))

    )

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c #\f #\g) char=?))

    (check
	(char-set-filter it?
			 (char-set #\a #\b #\F #\G)
			 (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set #\a #\b #\f #\g))

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c) char=?))

    (check
	(char-set-filter! it? (char-set #\a #\b #\F #\G)
			  (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set #\a #\b))

    (check
	(char-set-filter! it? (char-set  #\F #\G)
			  (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set))

    (check
	(char-set-filter! it?
			  (char-set #\a #\b #\F #\G)
			  (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set #\a #\b))

    )

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c #\f #\g) char=?))

    (check
	(char-set-filter! it?
			  (char-set #\a #\b #\F #\G)
			  (char-set #\f #\g #\h))
      (=> char-set=?)
      (char-set #\a #\b #\f #\g))

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c) char=?))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\a #\b #\F #\G))
	  in)
      (=> char-set=?)
      (char-set #\a #\b))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\a #\b #\F #\G))
	  ou)
      (=> char-set=?)
      (char-set #\F #\G))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\a #\b))
	  in)
      (=> char-set=?)
      (char-set #\a #\b))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\a #\b))
	  ou)
      (=> char-set=?)
      (char-set))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\F #\G))
	  in)
      (=> char-set=?)
      (char-set))

    (check
	(receive (in ou)
	    (char-set-partition it? (char-set #\F #\G))
	  ou)
      (=> char-set=?)
      (char-set #\F #\G))

    )

;;; --------------------------------------------------------------------

  (check
      (char-set-remove #\b (char-set #\a #\b #\c))
    (=> char-set=?)
    (char-set #\a #\c))

  (check
      (char-set-remove #\D (char-set #\a #\b #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

;;; --------------------------------------------------------------------

  (let ()

    (define (it? ch)
      (member* ch '(#\a #\b #\c) char=?))

    (check
	(char-set-remove* it? (char-set #\a #\b #\D #\E))
      (=> char-set=?)
      (char-set #\D #\E))

    (check
	(char-set-remove*! it? (char-set #\a #\b #\D #\E))
      (=> char-set=?)
      (char-set #\D #\E))

    )

;;; --------------------------------------------------------------------

  (check
      (char-set-delete-duplicates (char-set #\a #\b #\a #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

  (check
      (char-set-delete-duplicates! (char-set #\a #\b #\a #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

  )



(parameterise ((check-test-name	'iteration))

  (check
      (with-result
       (let ((cs (char-set #\a #\b #\c)))
	 (do ((cursor (char-set-cursor cs) (char-set-cursor-next cursor)))
	     ((end-of-char-set? cursor)
	      #t)
	   (add-result (char-set-cursor-ref cursor)))))
    => '(#t (#\a #\b #\c)))

  (check
      (with-result
       (let ((cs (char-set #\a)))
	 (do ((cursor (char-set-cursor cs) (char-set-cursor-next cursor)))
	     ((end-of-char-set? cursor)
	      #t)
	   (add-result (char-set-cursor-ref cursor)))))
    => '(#t (#\a)))

;;; --------------------------------------------------------------------

  (check
      (char-set-fold (lambda (c i)
		       (+ i 1))
		     0
		     (char-set #\a #\b #\c))
    => 3)

  (check
      (char-set-fold (lambda (c i)
		       (+ i 1))
		     0
		     (char-set))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (char-set-unfold null? car cdr '(#\a #\b #\c))
    (=> char-set=?)
    (char-set #\a #\b #\c))

  (check
      (let ((p (open-string-input-port "abc")))
	(char-set-unfold eof-object?		;; stop?
			 values			;; map-to-char
			 (lambda (ch)
			   (read-char p))	;; seed-step
			 (read-char p)))	;; seed
    (=> char-set=?)
    (char-set #\a #\b #\c))

  (check
      (char-set-unfold null? car cdr '(#\a #\b #\c)
		       (char-set #\D #\E))
    (=> char-set=?)
    (char-set #\a #\b #\c #\D #\E))

;;; --------------------------------------------------------------------

  (check
      (char-set-unfold! null? car cdr '(#\a #\b #\c)
			(char-set #\D #\E))
    (=> char-set=?)
    (char-set #\a #\b #\c #\D #\E))

;;; --------------------------------------------------------------------

  (check
      (char-set-map char-upcase (char-set #\a #\b #\c))
    (=> char-set=?)
    (char-set #\A #\B #\C))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (char-set-for-each add-result (char-set #\a #\b #\c))
       #t)
    => '(#t (#\a #\b #\c)))

  )



(parameterise ((check-test-name 'set-equal))

  (check
      (char-set=?)
    => #t)

  (check
      (char-set=? (char-set #\a))
    => #t)

  (check
      (char-set=? (char-set)
		  (char-set #\a #\b #\c))
    => #f)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (char-set))
    => #f)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (char-set #\a #\b #\c)
		  (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set=? (char-set #\a #\b #\c #\d)
		  (char-set #\a #\b #\c))
    => #f)

  (check
      (char-set=? (char-set #\a #\b #\c)
		  (char-set #\a #\b #\d))
    => #f)

  )



(parameterise ((check-test-name 'set-subset))

  (check
      (char-set<=?)
    => #t)

  (check
      (char-set<=? (char-set #\a))
    => #t)

  (check
      (char-set<=? (char-set)
		   (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set))
    => #f)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set #\a #\b #\c)
		   (char-set #\a #\b #\c))
    => #t)

  (check
      (char-set<=? (char-set #\a #\b #\c #\d)
		   (char-set #\a #\b #\c))
    => #f)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set #\a #\b #\c #\d)
		   (char-set #\a #\b #\c #\d #\e))
    => #t)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set #\a #\b #\c #\d)
		   (char-set)
		   (char-set #\a #\b #\c #\d #\e))
    => #f)

  (check
      (char-set<=? (char-set #\a #\b #\c)
		   (char-set #\a #\b #\d))
    => #f)

  )



;; (parameterise ((check-test-name 'predefined))

;;   (check
;;       (char-set? char-set:lower-case)
;;     => #t)

;;   (check
;;       (char-set? char-set:upper-case)
;;     => #t)

;;   (check
;;       (char-set? char-set:title-case)
;;     => #t)

;;   (check
;;       (char-set? char-set:letter)
;;     => #t)

;;   (check
;;       (char-set? char-set:digit)
;;     => #t)

;;   (check
;;       (char-set? char-set:letter+digit)
;;     => #t)

;;   (check
;;       (char-set? char-set:graphic)
;;     => #t)

;;   (check
;;       (char-set? char-set:printing)
;;     => #t)

;;   (check
;;       (char-set? char-set:whitespace)
;;     => #t)

;;   (check
;;       (char-set? char-set:iso-control)
;;     => #t)

;;   (check
;;       (char-set? char-set:punctuation)
;;     => #t)

;;   (check
;;       (char-set? char-set:symbol)
;;     => #t)

;;   (check
;;       (char-set? char-set:hex-digit)
;;     => #t)

;;   (check
;;       (char-set? char-set:blank)
;;     => #t)

;;   (check
;;       (char-set? char-set:ascii)
;;     => #t)

;;   (check
;;       (char-set? char-set:empty)
;;     => #t)

;;   (check
;;       (char-set? char-set:full)
;;     => #t)


;;   )


;;;; done

(check-report)

;;; end of file
