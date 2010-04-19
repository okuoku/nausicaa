;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (libraries import-specs)
;;;Date: Sat Apr 10, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (libraries import-specs)
  (libraries references)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing import specification library\n")


(parametrise ((check-test-name	'utilities))

  (check
      (apply-import-spec/only '() '(a b c))
    => '())

  (check
      (apply-import-spec/only '((a b)) '())
    => '())

  (check
      (apply-import-spec/only '((a b)) '(b))
    => '((a b)))

  (check
      (apply-import-spec/only '((a b)) '(b c))
    => '((a b)))

  (check
      (apply-import-spec/only '((a ea) (b eb) (c ec))
			      '(eb ec))
    => '((b eb) (c ec)))

;;; --------------------------------------------------------------------

  (check
      (apply-import-spec/except '() '(b))
    => '())

  (check
      (apply-import-spec/except '((a b)) '())
    => '((a b)))

  (check
      (apply-import-spec/except '((a b)) '(b))
    => '())

  (check
      (apply-import-spec/except '((a b)) '(c))
    => '((a b)))

  (check
      (apply-import-spec/except '((a ea) (b eb) (c ec))
				'(eb ec))
    => '((a ea)))

;;; --------------------------------------------------------------------

  (check
      (apply-import-spec/prefix '() 'ciao:)
    => '())

  (check
      (apply-import-spec/prefix '((a b)) 'ciao:)
    => '((a ciao:b)))

  (check
      (apply-import-spec/prefix '((a ea) (b eb) (c ec))
				'ciao:)
    => '((a ciao:ea) (b ciao:eb) (c ciao:ec)))

;;; --------------------------------------------------------------------

  (check
      (apply-import-spec/rename '() '())
    => '())

  (check
      (apply-import-spec/rename '((a b)) '())
    => '((a b)))

  (check
      (apply-import-spec/rename '() '((a b)))
    => '())

  (check
      (apply-import-spec/rename '((a b)) '((b c)))
    => '((a c)))

  (check
      (apply-import-spec/rename '((a ea) (b eb) (c ec))
				'((eb ebb) (ec ecc)))
    => '((a ea) (b ebb) (c ecc)))

  #t)


(parametrise ((check-test-name	'parsing-import-sets))

  (check	;plain library
      (let-fields (((o <import-set>) (make <import-set>
				       '(alpha beta (1)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check	;plain library in LIBRARY
      (let-fields (((o <import-set>) (make <import-set>
				       '(library (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;plain library in LIBRARY
      (let-fields (((o <import-set>) (make <import-set>
				       '(library (for beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (for beta (1))))

  (check	;plain library in LIBRARY
      (let-fields (((o <import-set>) (make <import-set>
				       '(library (rename beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (rename beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty RENAME
      (let-fields (((o <import-set>) (make <import-set>
				       '(rename (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;RENAME with renamings
      (let-fields (((o <import-set>) (make <import-set>
				       '(rename (alpha beta (1))
						(a ae)
						(b be)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty ONLY
      (let-fields (((o <import-set>) (make <import-set>
				       '(only (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;ONLY with identifiers
      (let-fields (((o <import-set>) (make <import-set>
				       '(only (alpha beta (1))
					      a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))


;;; --------------------------------------------------------------------

  (check 	;empty EXCEPT
      (let-fields (((o <import-set>) (make <import-set>
				       '(except (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;EXCEPT with identifiers
      (let-fields (((o <import-set>) (make <import-set>
				       '(except (alpha beta (1))
						a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;PREFIX
      (let-fields (((o <import-set>) (make <import-set>
				       '(prefix (alpha beta (1)) px:))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-fields ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  #t)


(parametrise ((check-test-name	'application-import-set))

;;   (check
;;       (apply-import-spec/only '() '(a b c))
;;     => '())

;;   (check
;;       (apply-import-spec/only '((a b)) '())
;;     => '())

;;   (check
;;       (apply-import-spec/only '((a b)) '(b))
;;     => '((a b)))

;;   (check
;;       (apply-import-spec/only '((a b)) '(b c))
;;     => '((a b)))

;;   (check
;;       (apply-import-spec/only '((a ea) (b eb) (c ec))
;; 			      '(eb ec))
;;     => '((b eb) (c ec)))

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (apply-import-spec/except '() '(b))
;;     => '())

;;   (check
;;       (apply-import-spec/except '((a b)) '())
;;     => '((a b)))

;;   (check
;;       (apply-import-spec/except '((a b)) '(b))
;;     => '())

;;   (check
;;       (apply-import-spec/except '((a b)) '(c))
;;     => '((a b)))

;;   (check
;;       (apply-import-spec/except '((a ea) (b eb) (c ec))
;; 				'(eb ec))
;;     => '((a ea)))

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (apply-import-spec/prefix '() 'ciao:)
;;     => '())

;;   (check
;;       (apply-import-spec/prefix '((a b)) 'ciao:)
;;     => '((a ciao:b)))

;;   (check
;;       (apply-import-spec/prefix '((a ea) (b eb) (c ec))
;; 				'ciao:)
;;     => '((a ciao:ea) (b ciao:eb) (c ciao:ec)))

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (apply-import-spec/rename '() '())
;;     => '())

;;   (check
;;       (apply-import-spec/rename '((a b)) '())
;;     => '((a b)))

;;   (check
;;       (apply-import-spec/rename '() '((a b)))
;;     => '())

;;   (check
;;       (apply-import-spec/rename '((a b)) '((b c)))
;;     => '((a c)))

;;   (check
;;       (apply-import-spec/rename '((a ea) (b eb) (c ec))
;; 				'((eb ebb) (ec ecc)))
;;     => '((a ea) (b ebb) (c ecc)))

  #t)


;;;; done

(check-report)

;;; end of file
