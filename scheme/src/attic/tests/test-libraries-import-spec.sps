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


#!r6rs
(import (nausicaa)
  (nausicaa libraries import-specs)
  (nausicaa libraries references)
  (nausicaa checks))

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
      (let (((o <import-set>) (make* <import-set>
				'(alpha beta (1)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check	;plain library in LIBRARY
      (let (((o <import-set>) (make* <import-set>
				'(library (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;plain library in LIBRARY
      (let (((o <import-set>) (make* <import-set>
				'(library (for beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (for beta (1))))

  (check	;plain library in LIBRARY
      (let (((o <import-set>) (make* <import-set>
				'(library (rename beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (rename beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty RENAME
      (let (((o <import-set>) (make* <import-set>
				'(rename (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;RENAME with renamings
      (let (((o <import-set>) (make* <import-set>
				'(rename (alpha beta (1))
					 (a ae)
					 (b be)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty ONLY
      (let (((o <import-set>) (make* <import-set>
				'(only (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;ONLY with identifiers
      (let (((o <import-set>) (make* <import-set>
				'(only (alpha beta (1))
				       a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))


;;; --------------------------------------------------------------------

  (check 	;empty EXCEPT
      (let (((o <import-set>) (make* <import-set>
				'(except (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;EXCEPT with identifiers
      (let (((o <import-set>) (make* <import-set>
				'(except (alpha beta (1))
					 a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;PREFIX
      (let (((o <import-set>) (make* <import-set>
				'(prefix (alpha beta (1)) px:))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  #t)


(parametrise ((check-test-name	'parsing-import-specs))

  (check	;plain library
      (let (((o <import-spec>) (make* <import-spec>
				 '(alpha beta (1)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check	;plain library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(library (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;plain library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(library (for beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (for beta (1))))

  (check	;plain library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(library (rename beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (rename beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty RENAME
      (let (((o <import-spec>) (make* <import-spec>
				 '(rename (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;RENAME with renamings
      (let (((o <import-spec>) (make* <import-spec>
				 '(rename (alpha beta (1))
					  (a ae)
					  (b be)))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;empty ONLY
      (let (((o <import-spec>) (make* <import-spec>
				 '(only (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;ONLY with identifiers
      (let (((o <import-spec>) (make* <import-spec>
				 '(only (alpha beta (1))
					a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))


;;; --------------------------------------------------------------------

  (check 	;empty EXCEPT
      (let (((o <import-spec>) (make* <import-spec>
				 '(except (alpha beta (1))))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  (check	;EXCEPT with identifiers
      (let (((o <import-spec>) (make* <import-spec>
				 '(except (alpha beta (1))
					  a b c))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check 	;PREFIX
      (let (((o <import-spec>) (make* <import-spec>
				 '(prefix (alpha beta (1)) px:))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check	;FOR on plain library
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (alpha beta (1)) run))))
	(list (is-a? o.library-reference <library-reference>)
	      o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (0) (alpha beta (1))))

  (check	;FOR on plain library
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (alpha beta (1)) run expand))))
	(list (is-a? o.library-reference <library-reference>)
	      o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (0 1) (alpha beta (1))))

  (check	;FOR on plain library
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (alpha beta (1)) (meta -2)))))
	(list (is-a? o.library-reference <library-reference>)
	      o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (-2) (alpha beta (1))))

;;; --------------------------------------------------------------------

  (check	;FOR on library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (library (alpha beta (1))) run))))
	(list o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '((0) (alpha beta (1))))

  (check	;FOR on library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (library (for beta (1))) expand))))
	(list o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '((1) (for beta (1))))

  (check	;FOR on library in LIBRARY
      (let (((o <import-spec>) (make* <import-spec>
				 '(for (library (rename beta (1))) (meta 3)))))
	(list o.import-levels
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '((3) (rename beta (1))))

;;; --------------------------------------------------------------------

  (check
      (let (((o <import-spec>) (make* <import-spec>
				 '(prefix (except (only (rename (alpha beta (1))
								(a ae)
								(b be)
								(c ce))
							b c)
						  b)
					  px:))))
	(list (is-a? o.library-reference <library-reference>)
	      (with-class ((o.library-reference <library-reference>))
		o.library-reference.reference)))
    => '(#t (alpha beta (1))))

  #t)


(parametrise ((check-test-name	'application-import-specs))

  (define (doit spec renamings)
    (let (((o <import-spec>) (make* <import-spec> spec)))
      (o.apply renamings)))

;;; plain library

  (let ((renamings '()))
    (check
	(doit '(alpha beta (1)) renamings)
      => renamings))

  (let ((renamings '((a ae))))
    (check
	(doit '(alpha beta (1)) renamings)
      => renamings))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(alpha beta (1)) renamings)
      => renamings))

;;; --------------------------------------------------------------------
;;; ONLY clause

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(only (alpha beta (1)))
	      renamings)
      => '()))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(only (alpha beta (1)) fe)
	      renamings)
      => '((f fe))))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(only (alpha beta (1))
		     ae ce de ge)
	      renamings)
      => '((a ae) (c ce) (d de) (g ge))))

;;; --------------------------------------------------------------------
;;; EXCEPT clause

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(except (alpha beta (1)))
	      renamings)
      => renamings))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(except (alpha beta (1)) fe)
	      renamings)
      => '((a ae) (b be) (c ce) (d de) (g ge))))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(except (alpha beta (1))
		       ae ce de ge)
	      renamings)
      => '((b be) (f fe))))

;;; --------------------------------------------------------------------
;;; PREFIX clause

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(prefix (alpha beta (1)) px:)
	      renamings)
      => '((a px:ae) (b px:be) (c px:ce) (d px:de) (f px:fe) (g px:ge))))

  (let ((renamings '()))
    (check
	(doit '(prefix (alpha beta (1)) px:)
	      renamings)
      => '()))

;;; --------------------------------------------------------------------
;;; RENAME clause

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(rename (alpha beta (1)))
	      renamings)
      => renamings))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(rename (alpha beta (1))
		       (be bee))
	      renamings)
      => '((a ae) (b bee) (c ce) (d de) (f fe) (g ge))))

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(rename (alpha beta (1))
		       (ae aee) (ce cee) (de dee) (ge gee))
	      renamings)
      => '((a aee) (b be) (c cee) (d dee) (f fe) (g gee))))

;;; --------------------------------------------------------------------
;;; miscellaneous

  (let ((renamings '((a ae) (b be) (c ce) (d de) (f fe) (g ge))))
    (check
	(doit '(prefix (except (only (rename (alpha beta (1))
					     (ae aee)
					     (be bee)
					     (ce cee))
				     bee cee)
			       bee)
		       px:)
	      renamings)
      => '((c px:cee))))

  #t)


;;;; done

(check-report)

;;; end of file
