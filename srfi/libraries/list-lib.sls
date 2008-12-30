;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.



#!r6rs
(library (list-lib)
  ;;These should be all the exports from (srfi lists).
  (export

    ;; constructors
    cons xcons cons*
    list make-list list-tabulate list-copy circular-list iota

    ;; predicats
    proper-list?	circular-list?		dotted-list?
    null?		null-list?
    pair?		not-pair?
    list=

    ;; selectors
    car			cdr			car+cdr
    caar		cadr
    cdar		cddr
    caaar		caadr
    cadar		caddr
    cdaar		cdadr
    cddar		cdddr
    caaaar		caaadr
    caadar		caaddr
    cadaar		cadadr
    caddar		cadddr
    cdaaar		cdaadr
    cdadar		cdaddr
    cddaar		cddadr
    cdddar		cddddr

    list-ref
    first		second			third
    fourth		fifth			sixth
    seventh		eighth			ninth
    tenth

    take		take-right		take!
    drop		drop-right		drop-right!
    split-at		split-at!
    last		last-pair

    ;; misc
    length		length+
    append		append!
    concatenate		concatenate!
    reverse		reverse!
    append-reverse	append-reverse!
    zip
    unzip1		unzip2			unzip3
    unzip4		unzip5
    count

    ;; fold
    fold		fold-right		srfi:fold-right
    pair-fold		pair-fold-right
    reduce		reduce-right
    unfold		unfold-right

    ;; map
    map						srfi:map
    for-each					srfi:for-each
    append-map		append-map!
    map!		map-in-order
    pair-for-each	filter-map

    ;; filtering
    filter		filter!
    partition		partition!
    remove		remove!			srfi:remove

    ;;searching
    find		find-tail
    take-while		take-while!
    drop-while
    span		span!
    break		break!
    any			every
    list-index
    member					srfi:member
    memq		memv

    ;; deletion
    delete		delete!
    delete-duplicates	delete-duplicates!

    ;; alists
    assoc					srfi:assoc
    assq		assv
    alist-cons		alist-copy
    alist-delete	alist-delete!

    ;; sets
    lset<=			lset=
    lset-adjoin
    lset-union			lset-union!
    lset-intersection		lset-intersection!
    lset-difference		lset-difference!
    lset-xor			lset-xor!
    lset-diff+intersection	lset-diff+intersection!

    ;; side effects
    set-car!			set-cdr!)
  (import (rnrs)
    (rename (srfi lists)
	    (assoc		srfi:assoc)
	    (fold-right		srfi:fold-right)
	    (for-each		srfi:for-each)
	    (map		srfi:map)
	    (member		srfi:member)
	    (remove		srfi:remove))
    (rnrs mutable-pairs (6)))

  (define (tree-copy x)
    (let loop ((x x))
      (if (pair? x)
	  (cons (loop (car x))
		(loop (cdr x)))
	x))))

;;; end of file
