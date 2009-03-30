;;;SRFI-1 list-processing library
;;;Reference implementation
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 1998, 1999 by Olin Shivers <shivers@ai.mit.edu>.
;;;Modified by Abdulaziz Ghuloum to port to Ikarus.
;;;Modified by Derick Eddington to port to R6RS.
;;;Modified by Marco Maggi for inclusion in Nausicaa.
;;;
;;;You may do as you please with  this code as long as you do not remove
;;;this copyright notice or hold me liable for its use.  Please send bug
;;;reports to <shivers@ai.mit.edu>.
;;;
;;;This is  a library of list-processing  and pair-processing functions.
;;;I wrote it after carefully  considering the functions provided by the
;;;libraries  found in  R4RS/R5RS Scheme,  MIT Scheme,  Gambit, RScheme,
;;;MzScheme,  slib, Common  Lisp,  Bigloo,  guile, T,  APL  and the  SML
;;;standard basis.  It is a pretty rich toolkit, providing a superset of
;;;the functionality found in any of the various Schemes I considered.
;;;
;;;This   implementation   is   intended   as   a   portable   reference
;;;implementation  for SRFI-1.   See the  porting notes  below  for more
;;;information.
;;;



#!r6rs
(library (lists)
  (export

    ;; constructors
    xcons
    make-list list-tabulate list-copy circular-list iota

    ;; predicats
    proper-list?	circular-list?		dotted-list?
    null-list?		not-pair?		list=?

    ;; selectors
    car+cdr
    first		second			third
    fourth		fifth			sixth
    seventh		eighth			ninth
    tenth

    take		take-right		take!
    drop		drop-right		drop-right!
    split-at		split-at!
    last		last-pair

    ;; misc
    length+
    append!
    concatenate		concatenate!
    reverse!
    append-reverse	append-reverse!
    zip
    unzip1		unzip2			unzip3
    unzip4		unzip5
    count

    ;; fold
    fold		fold-right*
    pair-fold		pair-fold-right
    reduce		reduce-right
    unfold		unfold-right

    ;; map
    map*		for-each*
    append-map		append-map!
    map!		map-in-order
    pair-for-each	filter-map

    ;; filtering
    filter		filter!
    partition		partition!
    remove*		remove*!

    ;;searching
    find-tail
    take-while		take-while!
    drop-while
    span		span!
    break		break!
    any			every
    list-index		member*

    ;; deletion
    delete		delete!
    delete-duplicates	delete-duplicates!

    ;; alists
    assoc*
    assq		assv
    alist-cons		alist-copy
    alist-delete	alist-delete!

    ;; sets
    lset<=?			lset=?
    lset-adjoin
    lset-union			lset-union!
    lset-intersection		lset-intersection!
    lset-difference		lset-difference!
    lset-xor			lset-xor!
    lset-diff+intersection	lset-diff+intersection!)
  (import (scheme)
    (rnrs mutable-pairs (6)))


;;;; constructors

(define (xcons d a)
  (cons a d))

(define make-list
  (case-lambda
   ((len)
    (make-list len #f))
   ((len fill)
    (do ((i 0 (+ 1 i))
	 (l '() (cons fill l)))
	((= i len)
	 l)))))

(define (list-copy ell)
  (let loop ((ell ell))
    (if (pair? ell)
	(cons (car ell) (loop (cdr ell)))
      ell)))

(define (tree-copy x)
  (let loop ((x x))
    (if (pair? x)
	(cons (loop (car x))
	      (loop (cdr x)))
      x)))

(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0)
       ans)))

(define iota
  (case-lambda
   ((count)
    (iota count 0 1))
   ((count start)
    (iota count start 1))
   ((count start step)
    (do ((count count (- count 1))
	 (val (+ start (* (- count 1) step)) (- val step))
	 (ans '() (cons val ans)))
	((<= count 0)
	 ans)))))

(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))


;;;; predicates

(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	    (null? x)))
      (null? x))))

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	    (not (null? x))))
      (not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

(define (not-pair? x)
  (not (pair? x)))

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error 'null-list? "argument out of domain" l))))



;;;; comparison

(define list=?
  (case-lambda
   ((elm=?)
    #t)
   ((elm=? ell)
    #t)
   ((elm=? ell1 ell2)
    (let loop ((ell1 ell1)
	       (ell2 ell2))
      (cond
       ((eq? ell1 ell2)
	#t)
       ((null-list? ell1)
	(or (null-list? ell2)
	    #f))
       ((null-list? ell2)
	#f)
       ((elm=? (car ell1) (car ell2))
	(loop (cdr ell1) (cdr ell2)))
       (else
	#f))))
   ((elm=? . ells)
    (and (list=? elm=? (car ells) (cadr ells))
	 (apply list=? elm=? (cdr ells))))))


;;;; selectors

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

;;; --------------------------------------------------------------------

(define (take ell k)
  (let loop ((ell ell)
	     (k   k))
    (if (zero? k)
	'()
      (cons (car ell)
	    (loop (cdr ell) (- k 1))))))

(define (drop ell k)
  (let loop ((ell ell)
	     (k   k))
    (if (zero? k)
	ell
      (loop (cdr ell) (- k 1)))))

;;; --------------------------------------------------------------------

(define (take-right ell k)
  (let loop ((lag	ell)
	     (lead	(drop ell k)))
    (if (pair? lead)
	(loop (cdr lag) (cdr lead))
      lag)))

(define (drop-right ell k)
  (let loop ((lag	ell)
	     (lead	(drop ell k)))
    (if (pair? lead)
	(cons (car lag)
	      (loop (cdr lag) (cdr lead)))
      '())))

;;; --------------------------------------------------------------------

(define (take! ell k)
  (if (zero? k)
      '()
    (begin
      (set-cdr! (drop ell (- k 1)) '())
      ell)))

(define (drop-right! ell k)
  ;;In this  function, LEAD is actually  K+1 ahead of LAG.  This lets us
  ;;stop LAG one step early, in time to smash its cdr to ().
  (let ((lead (drop ell k)))
    (if (pair? lead)
	(let loop ((lag  ell)
		   (lead (cdr lead))) ; Standard case
	  (if (pair? lead)
	      (loop (cdr lag) (cdr lead))
	    (begin (set-cdr! lag '())
		   ell)))
      '()))) ; Special case dropping everything -- no cons to side-effect.

;;; --------------------------------------------------------------------

(define (split-at x k)
  (let loop ((ell x)
	     (k   k))
    (if (zero? k) (values '() ell)
      (receive (prefix suffix) (loop (cdr ell) (- k 1))
	(values (cons (car ell) prefix) suffix)))))

(define (split-at! x k)
  (if (zero? k) (values '() x)
    (let* ((prev   (drop x (- k 1)))
	   (suffix (cdr prev)))
      (set-cdr! prev '())
      (values x suffix))))

(define (last ell)
  (car (last-pair ell)))

(define (last-pair x)
  (let loop ((x x))
    (if (pair? (cdr x))
	(loop (cdr x))
      x)))


;;;; miscellaneous

(define (length+ x) ; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	    len))
      len)))

;;; --------------------------------------------------------------------

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists)
	   (prev '()))
    (if (not (pair? lists))
	prev
      (let ((first (car lists))
	    (rest (cdr lists)))
	(if (not (pair? first))
	    (lp rest first)
	  ;; Now, do the splicing.
	  (let lp2 ((tail-cons (last-pair first))
		    (rest rest))
	    (if (pair? rest)
		(let ((next (car rest))
		      (rest (cdr rest)))
		  (set-cdr! tail-cons next)
		  (lp2 (if (pair? next) (last-pair next) tail-cons)
		       rest))
	      first)))))))

(define (concatenate  lists)
  (reduce-right append  '() lists))

(define (concatenate! lists)
  (reduce-right append! '() lists))

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head)
	   (tail tail))
    (if (null-list? rev-head)
	tail
      (lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head)
	   (tail tail))
    (if (null-list? rev-head)
	tail
      (let ((next-rev (cdr rev-head)))
	(set-cdr! rev-head tail)
	(lp next-rev rev-head)))))

;;; --------------------------------------------------------------------

(define (zip list1 . more-lists)
  (apply map* list list1 more-lists))

(define (unzip1 lis)
  (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis) ; Use NOT-PAIR? to handle
      (let ((elt (car lis)))		  ; dotted lists.
	(receive (a b) (recur (cdr lis))
	  (values (cons (car  elt) a)
		  (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
      (let ((elt (car lis)))
	(receive (a b c) (recur (cdr lis))
	  (values (cons (car   elt) a)
		  (cons (cadr  elt) b)
		  (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
      (let ((elt (car lis)))
	(receive (a b c d) (recur (cdr lis))
	  (values (cons (car    elt) a)
		  (cons (cadr   elt) b)
		  (cons (caddr  elt) c)
		  (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
      (let ((elt (car lis)))
	(receive (a b c d e) (recur (cdr lis))
	  (values (cons (car     elt) a)
		  (cons (cadr    elt) b)
		  (cons (caddr   elt) c)
		  (cons (cadddr  elt) d)
		  (cons (car (cddddr  elt)) e)))))))

;;; --------------------------------------------------------------------

(define count
  (case-lambda

   ((pred ell)
    (let loop ((ell ell)
	       (i   0))
      (if (null-list? ell) i
	(loop (cdr ell)
	      (if (pred (car ell))
		  (+ i 1)
		i)))))

   ((pred ell . ells)
    (let loop ((ell  ell)
	       (ells ells)
	       (i    0))
      (if (null-list? ell)
	  i
	(receive (as ds)
	    (%cars/cdrs ells)
	  (if (null? as) i
	    (loop (cdr ell)
		  ds
		  (if (apply pred (car ell) as)
		      (+ i 1)
		    i)))))))))

;;; --------------------------------------------------------------------

(define (reverse! lis)
  (let lp ((lis lis) (ans '()))
    (if (null-list? lis) ans
      (let ((tail (cdr lis)))
	(set-cdr! lis ans)
	(lp tail lis)))))



;;;; helpers

;; *** NOTE ***
;;
;;The following helper functions and macros are duplicated and tested in
;;the test file "test-helpers-list.sps".

;;; --------------------------------------------------------------------

;; The following macros  handle queue values: pairs whose  car is a list
;; and  whose  cdr is  the  last  pair in  the  list.   They allow  fast
;; insertion of elements at the end of the list.
;;
;;           -----------
;;   queue  | car | cdr |
;;           -----------
;;             |     |
;;         ----       ----------------
;;        |                           |
;;        v                           v
;;       ---    ---    ---    ---    ---
;; list | | |->| | |->| | |->| | |->| | |->()
;;       ---    ---    ---    ---    ---
;;       |      |      |      |      |
;;       o      o      o      o      o

(define-syntax make-queue
  (syntax-rules ()
    ((_ ?elm)
     (let* ((v		?elm)
	    (pair	(cons v '())))
       (cons pair pair)))))

(define-syntax enqueue!
  (syntax-rules ()
    ((_ ?q ?obj)
     (let ((q ?q)
	   (h (cons ?obj '())))
       (set-cdr! (cdr q) h)
       (set-cdr! q h)
       q))))

;;; --------------------------------------------------------------------

;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return null; else return the list of the cars.
(define (%cars ells)
  (let ((next	(car ells)))
    (if (null? next)
	'()
      (let loop ((cars	(make-queue (car next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (car cars)
	  (let ((next (car ells)))
 	    (if (null? next)
 		'()
	      (loop (enqueue! cars (car next))
		    (cdr ells)))))))))

;;; ELLS must be a list of lists.   If one of the lists in ELLS is null:
;;; return null; else return the list of cars of the lists in ELLS, with
;;; KNIL appended as last element.
(define (%cars+knil ells knil)
  (let ((next (car ells)))
    (if (null? next)
	'()
      (let loop ((cars	(make-queue (car next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (car (enqueue! cars knil))
	  (let ((next (car ells)))
	    (if (null? next)
		'()
	      (loop (enqueue! cars (car next))
		    (cdr ells)))))))))

;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return null; else return the list of the cdrs.
(define (%cdrs ells)
  (let ((next	(car ells)))
    (if (null? next)
	'()
      (let loop ((ells	(cdr ells))
		 (cdrs	(make-queue (cdr next))))
	(if (null? ells)
	    (car cdrs)
	  (let ((next	(car ells)))
	    (if (null? next)
		'()
	      (loop (cdr ells)
		    (enqueue! cdrs (cdr next))))))))))

;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return 2 null  values; else return 2 values: the list  of cars and the
;;list of cdrs.
(define (%cars/cdrs ells)
  (let ((next (car ells)))
    (if (null? next)
	(values '() '())
      (let loop ((cars	(make-queue (car next)))
		 (cdrs	(make-queue (cdr next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (values (car cars)
		    (car cdrs))
	  (let ((next (car ells)))
	    (if (null? next)
		(values '() '())
	      (loop (enqueue! cars (car next))
		    (enqueue! cdrs (cdr next))
		    (cdr ells)))))))))

;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return 2 null values; else return 2 values: the list of cars with KNIL
;;appended, and the list of cdrs.
(define (%cars+knil/cdrs ells knil)
  (let ((next	(car ells)))
    (if (null? next)
	(values '() '())
      (let loop ((cars	(make-queue (car next)))
		 (cdrs	(make-queue (cdr next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (values (car (enqueue! cars knil))
		    (car cdrs))
	  (let ((next	(car ells)))
	    (if (null? next)
		(values '() '())
	      (loop (enqueue! cars (car next))
		    (enqueue! cdrs (cdr next))
		    (cdr ells)))))))))

;;; Like %CARS/CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (if (pair? lists)
	(receive (list other-lists)
	    (car+cdr lists)
	  (receive (a d)
	      (car+cdr list)
	    (receive (cars cdrs)
		(recur other-lists)
	      (values (cons a cars) (cons d cdrs)))))
      (values '() '()))))


;;;; fold/unfold

(define fold
  (case-lambda

   ((kons knil ell)
    (let loop ((ell	ell)
	       (result	knil))
      (if (null-list? ell)
	  result
	(loop (cdr ell) (kons (car ell) result)))))

   ((kons knil ell1 . ells)
    (let loop ((ells	(cons ell1 ells))
	       (result	knil))
      (receive (cars+result cdrs)
	  (%cars+knil/cdrs ells result)
	(if (null? cars+result)
	    result
	  (loop cdrs (apply kons cars+result))))))))

(define fold-right*
  (case-lambda

   ((kons knil ell)
    (let loop ((ell ell))
      (if (null-list? ell)
	  knil
	(kons (car ell) (loop (cdr ell))))))

   ((kons knil ell1 . ells)
    (let loop ((ells (cons ell1 ells)))
      (if (null? ells)
	  knil
	(let ((cdrs (%cdrs ells)))
	  (if (null? cdrs)
	      knil
	    (apply kons (%cars+knil ells (loop cdrs))))))))))

;;; --------------------------------------------------------------------

(define pair-fold
  (case-lambda

   ((f knil ell)
    (let loop ((ell	ell)
	       (return	knil))
      (if (null-list? ell)
	  return
	(let ((tail (cdr ell)))
	  (loop tail (f ell return))))))

   ((f knil ell1 . ells)
    (let loop ((ells	(cons ell1 ells))
	       (return	knil))
      (let ((tails (%cdrs ells)))
	(if (null? tails)
	    return
	  (loop tails (apply f (append! ells (list return))))))))))

(define (pair-fold-right f knil lis1 . ells)
  (if (pair? ells)
      (let loop ((ells (cons lis1 ells))) ; N-ary case
	(let ((cdrs (%cdrs ells)))
	  (if (null? cdrs) knil
	    (apply f (append! ells (list (loop cdrs)))))))
    (let loop ((lis lis1)) ; Fast path
      (if (null-list? lis) knil (f lis (loop (cdr lis)))))))

;;; --------------------------------------------------------------------

(define (reduce f ridentity ell)
  (if (null-list? ell)
      ridentity
    (fold f (car ell) (cdr ell))))

(define (reduce-right f ridentity ell)
  (if (null-list? ell)
      ridentity
    (let loop ((head	(car ell))
	       (ell	(cdr ell)))
      (if (pair? ell)
	  (f head (loop (car ell) (cdr ell)))
	head))))

;;; --------------------------------------------------------------------

(define unfold-right
  (case-lambda

   ((stop? map-to-result seed-step seed)
    (unfold-right stop? map-to-result seed-step seed '()))

   ((stop? map-to-result seed-step seed tail)
    (let loop ((seed seed)
	       (result tail))
      (if (stop? seed)
	  result
	(loop (seed-step seed)
	      (cons (map-to-result seed) result)))))))

(define unfold
  (case-lambda

   ((stop? map-to-result seed-step seed)
    (let loop ((seed seed))
      (if (stop? seed)
	  '()
 	(cons (map-to-result seed)
	      (loop (seed-step seed))))))

   ((stop? map-to-result seed-step seed tail-gen)
    (let loop ((seed seed))
      (if (stop? seed)
	  (tail-gen seed)
	(cons (map-to-result seed)
	      (loop (seed-step seed))))))))



;;;; mappers

(define for-each*
  (case-lambda

   ((f ell)
    (let loop ((ell ell))
      (unless (null-list? ell)
	(f (car ell))
	(loop (cdr ell)))))

   ((f ell . ells)
    (let loop ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs ells)
	(when (pair? cars)
	  (apply f cars)
	  (loop cdrs)))))))

;;; --------------------------------------------------------------------

(define map-in-order
  (case-lambda

   ((f ell)
    (let loop ((ell ell))
      (if (null-list? ell)
	  ell
	(let ((tail	(cdr ell))
	      (x	(f (car ell))))	; Do head first,
	  (cons x (loop tail))))))	; then tail.

   ((f ell . ells)
    (let recur ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs ells)
	(if (pair? cars)
	    (let ((x (apply f cars))) ; Do head first,
	      (cons x (recur cdrs)))  ; then tail.
	  '()))))))

(define map* map-in-order)

;;; --------------------------------------------------------------------

(define (append-map f lis1 . lists)
  (really-append-map append  f lis1 lists))

(define (append-map! f lis1 . lists)
  (really-append-map append! f lis1 lists))

(define really-append-map
  (case-lambda

   ((appender f ell)
    (if (null-list? ell)
	'()
      (let recur ((elt (car ell))
		  (ell (cdr ell)))
	(let ((vals (f elt)))
	  (if (null-list? ell)
	      vals
	    (appender vals (recur (car ell) (cdr ell))))))))

   ((appender f lis1 lists)
    (receive (cars cdrs)
	(%cars/cdrs (cons lis1 lists))
      (if (null? cars) '()
	(let recur ((cars cars)
		    (cdrs cdrs))
	  (let ((vals (apply f cars)))
	    (receive (cars2 cdrs2)
		(%cars/cdrs cdrs)
	      (if (null? cars2)
		  vals
		(appender vals (recur cars2 cdrs2)))))))))))

;;; --------------------------------------------------------------------

(define pair-for-each
  (case-lambda

   ((f ell)
    (let loop ((ell ell))
      (unless (null-list? ell)
	(let ((tail (cdr ell))) ; Grab the cdr now,
	  (f ell)		; in case PROC SET-CDR!s ELL.
	  (loop tail)))))

   ((f ell . ells)
    (let loop ((ells (cons ell ells)))
      (let ((tails (%cdrs ells)))
	(when (pair? tails)
	  (begin (apply f ells)
		 (loop tails))))))))

;;; We stop when ELL runs out, not when any list runs out.
(define map!
  (case-lambda

   ((f ell)
    (pair-for-each (lambda (pair)
		     (set-car! pair (f (car pair))))
		   ell)
    ell)

   ((f ell . ells)
    (let loop ((ell  ell)
	       (ells ells))
      (unless (null-list? ell)
	(receive (heads tails)
	    (%cars+cdrs/no-test ells)
	  (set-car! ell (apply f (car ell) heads))
	  (loop (cdr ell) tails))))
    ell)))

;;; Map F across L, and save up all the non-false results.
(define filter-map
  (case-lambda

   ((f ell)
    (let recur ((ell ell))
      (if (null-list? ell)
	  ell
	(let ((tail (recur (cdr ell))))
	  (cond
	   ((f (car ell))
	    => (lambda (x)
		 (cons x tail)))
	   (else
	    tail))))))

   ((f ell . ells)
    (let recur ((ells (cons ell ells)))
      (receive (cars cdrs)
	  (%cars/cdrs ells)
	(if (pair? cars)
	    (cond
	     ((apply f cars)
	      => (lambda (x)
		   (cons x (recur cdrs))))
	     (else
	      (recur cdrs))) ; Tail call in this arm.
	  '()))))))



;;;; filter, remove, partition

(define (filter! pred lis)
  (let lp ((ans lis))
    (cond
     ((null-list? ans)
      ans)
     ((not (pred (car ans)))
      (lp (cdr ans)))
     (else
      (letrec ((scan-in (lambda (prev lis)
			  (if (pair? lis)
			      (if (pred (car lis))
				  (scan-in lis (cdr lis))
				(scan-out prev (cdr lis))))))
	       (scan-out (lambda (prev lis)
			   (let lp ((lis lis))
			     (if (pair? lis)
				 (if (pred (car lis))
				     (begin (set-cdr! prev lis)
					    (scan-in lis (cdr lis)))
				   (lp (cdr lis)))
			       (set-cdr! prev lis))))))
	(scan-in ans (cdr ans))
	ans)))))

(define (partition! pred lis)
  (if (null-list? lis)
      (values lis lis)
    (letrec ((scan-in (lambda (in-prev out-prev lis)
			(let lp ((in-prev in-prev) (lis lis))
			  (if (pair? lis)
			      (if (pred (car lis))
				  (lp lis (cdr lis))
				(begin (set-cdr! out-prev lis)
				       (scan-out in-prev lis (cdr lis))))
			    (set-cdr! out-prev lis))))) ; Done.

	     (scan-out (lambda (in-prev out-prev lis)
			 (let lp ((out-prev out-prev) (lis lis))
			   (if (pair? lis)
			       (if (pred (car lis))
				   (begin (set-cdr! in-prev lis)
					  (scan-in lis out-prev (cdr lis)))
				 (lp lis (cdr lis)))
			     (set-cdr! in-prev lis)))))) ; Done.
      (if (pred (car lis))
	  (let lp ((prev-l lis) (l (cdr lis)))
	    (cond ((not (pair? l)) (values lis l))
		  ((pred (car l)) (lp l (cdr l)))
		  (else (scan-out prev-l l (cdr l))
			(values lis l)))) ; Done.
	(let lp ((prev-l lis) (l (cdr lis)))
	  (cond ((not (pair? l)) (values l lis))
		((pred (car l))
		 (scan-in l prev-l (cdr l))
		 (values l lis)) ; Done.
		(else (lp l (cdr l)))))))))

(define (remove*  pred l)
  (filter
      (lambda (x)
	(not (pred x)))
    l))

(define (remove*! pred l)
  (filter!
      (lambda (x)
	(not (pred x)))
   l))


;;;; searching

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list))
	     list
	   (lp (cdr list))))))

(define (take-while pred lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	'()
      (let ((x (car lis)))
	(if (pred x)
	    (cons x (recur (cdr lis)))
	  '())))))

(define (drop-while pred lis)
  (let lp ((lis lis))
    (if (null-list? lis)
	'()
      (if (pred (car lis))
	  (lp (cdr lis))
	lis))))

(define (take-while! pred lis)
  (if (or (null-list? lis)
	  (not (pred (car lis))))
      '()
    (begin (let lp ((prev lis)
		    (rest (cdr lis)))
	     (if (pair? rest)
		 (let ((x (car rest)))
		   (if (pred x)
		       (lp rest (cdr rest))
		     (set-cdr! prev '())))))
	   lis)))

(define (span pred lis)
  (let recur ((lis lis))
    (if (null-list? lis)
	(values '() '())
      (let ((x (car lis)))
	(if (pred x)
	    (receive (prefix suffix)
		(recur (cdr lis))
	      (values (cons x prefix) suffix))
	  (values '() lis))))))

(define (span! pred lis)
  (if (or (null-list? lis)
	  (not (pred (car lis))))
      (values '() lis)
    (let ((suffix (let lp ((prev lis)
			   (rest (cdr lis)))
		    (if (null-list? rest) rest
		      (let ((x (car rest)))
			(if (pred x)
			    (lp rest (cdr rest))
			  (begin (set-cdr! prev '())
				 rest)))))))
      (values lis suffix))))

(define (break  pred lis)
  (span  (lambda (x) (not (pred x))) lis))

(define (break! pred lis)
  (span! (lambda (x) (not (pred x))) lis))

(define (any pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars/cdrs (cons lis1 lists))
	(and (pair? heads)
	     (let lp ((heads heads) (tails tails))
	       (receive (next-heads next-tails) (%cars/cdrs tails)
		 (if (pair? next-heads)
		     (or (apply pred heads) (lp next-heads next-tails))
		     (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (and (not (null-list? lis1))
	   (let lp ((head (car lis1)) (tail (cdr lis1)))
	     (if (null-list? tail)
		 (pred head)		; Last PRED app is tail call.
		 (or (pred head) (lp (car tail) (cdr tail))))))))

(define every
  (case-lambda

   ((p ls)
    (or (null-list? ls)
	(let f ((p p) (a (car ls)) (d (cdr ls)))
	  (cond
	   ((pair? d)
	    (and (p a) (f p (car d) (cdr d))))
	   (else (p a))))))

   ((p ls1 ls2)
    (cond
     ((and (pair? ls1) (pair? ls2))
      (let f ((p p) (a1 (car ls1)) (d1 (cdr ls1)) (a2 (car ls2)) (d2 (cdr ls2)))
	(cond
	 ((and (pair? d1) (pair? d2))
	  (and (p a1 a2) (f p (car d1) (cdr d1) (car d2) (cdr d2))))
	 (else (p a1 a2)))))
     (else #t)))

   ((pred lis1 . lists)
    (receive (heads tails) (%cars/cdrs (cons lis1 lists))
      (or (not (pair? heads))
	  (let lp ((heads heads) (tails tails))
	    (receive (next-heads next-tails) (%cars/cdrs tails)
	      (if (pair? next-heads)
		  (and (apply pred heads) (lp next-heads next-tails))
		(apply pred heads)))))))))

(define (list-index pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((lists (cons lis1 lists)) (n 0))
	(receive (heads tails) (%cars/cdrs lists)
	  (and (pair? heads)
	       (if (apply pred heads) n
		 (lp tails (+ n 1))))))

    ;; Fast path
    (let lp ((lis lis1) (n 0))
      (and (not (null-list? lis))
	   (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

(define member*
  (case-lambda
   ((x lis)
    (member x lis))
   ((x lis =)
    (find-tail (lambda (y) (= x y)) lis))))


;;;; deletion

(define delete
  (case-lambda
   ((x lis)
    (delete x lis equal?))
   ((x lis =)
    (filter (lambda (y) (not (= x y))) lis))))

(define delete!
  (case-lambda
   ((x lis)
    (delete! x lis equal?))
   ((x lis =)
    (filter! (lambda (y) (not (= x y))) lis))))

(define delete-duplicates
  (case-lambda
   ((lis)
    (delete-duplicates lis equal?))
   ((lis elt=)
    (let recur ((lis lis))
      (if (null-list? lis) lis
	(let* ((x (car lis))
	       (tail (cdr lis))
	       (new-tail (recur (delete x tail elt=))))
	  (if (eq? tail new-tail) lis (cons x new-tail))))))))

(define delete-duplicates!
  (case-lambda
   ((lis)
    (delete-duplicates! lis equal?))
   ((lis elt=)
    (let recur ((lis lis))
      (if (null-list? lis) lis
	(let* ((x (car lis))
	       (tail (cdr lis))
	       (new-tail (recur (delete! x tail elt=))))
	  (if (eq? tail new-tail) lis (cons x new-tail))))))))


;;;; alists

(define assoc*
  (case-lambda
   ((x lis)
    (assoc x lis))
   ((x lis =)
    (find (lambda (entry) (= x (car entry))) lis))))

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (elt)
	 (cons (car elt) (cdr elt)))
    alist))

(define alist-delete
  (case-lambda
   ((key alist)
    (alist-delete key alist equal?))
   ((key alist =)
    (filter (lambda (elt) (not (= key (car elt)))) alist))))

(define alist-delete!
  (case-lambda
   ((key alist)
    (alist-delete! key alist equal?))
   ((key alist =)
    (filter! (lambda (elt) (not (= key (car elt)))) alist))))


;;; sets

(define (%lset2<= = lis1 lis2)
  (every (lambda (x) (member* x lis2 =)) lis1))

(define (lset<=? = . lists)
  (or (not (pair? lists))
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)
		       (%lset2<= = s1 s2))
		   (lp s2 rest)))))))

(define (lset=? = . lists)
  (or (not (pair? lists))
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2)
		       (and (%lset2<= = s1 s2)
			    (%lset2<= = s2 s1)))
		   (lp s2 rest)))))))


(define (lset-adjoin = lis . elts)
  (fold (lambda (elt ans) (if (member* elt ans =) ans (cons elt ans)))
	lis elts))

(define (lset-union = . lists)
  (reduce (lambda (lis ans)
	    (cond ((null? lis) ans)
		  ((null? ans) lis)
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans)
			   (if (any (lambda (x) (= x elt)) ans)
			       ans
			     (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (lset-union! = . lists)
  (reduce (lambda (lis ans)
	    (cond ((null? lis) ans)
		  ((null? ans) lis)
		  ((eq? lis ans) ans)
		  (else
		   (pair-fold
		    (lambda (pair ans)
		      (let ((elt (car pair)))
			(if (any (lambda (x) (= x elt)) ans)
			    ans
			  (begin (set-cdr! pair ans) pair))))
		    ans lis))))
	  '() lists))

(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?)))
    (cond ((any null-list? lists) '())
	  ((null? lists)          lis1)
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member* x lis =)) lists))
		  lis1)))))

(define (lset-intersection! = lis1 . lists)
  (let ((lists (delete lis1 lists eq?)))
    (cond ((any null-list? lists) '())
	  ((null? lists)          lis1)
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (member* x lis =)) lists))
			 lis1)))))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists)))
    (cond ((null? lists)     lis1)
	  ((memq lis1 lists) '())
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member* x lis =)))
			    lists))
		  lis1)))))

(define (lset-difference! = lis1 . lists)
  (let ((lists (filter pair? lists)))
    (cond ((null? lists)     lis1)
	  ((memq lis1 lists) '())
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (not (member* x lis =)))
			     lists))
			 lis1)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a)
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ans)
				  (if (member* xb a-int-b =) ans (cons xb ans)))
				a-b
				b)))))
	  '() lists))

(define (lset-xor! = . lists)
  (reduce
   (lambda (b a)
     (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
       (cond ((null? a-b)     (lset-difference! = b a))
	     ((null? a-int-b) (append! b a))
	     (else (pair-fold (lambda (b-pair ans)
				(if (member* (car b-pair) a-int-b =) ans
				  (begin (set-cdr! b-pair ans) b-pair)))
			      a-b
			      b)))))
   '() lists))

(define (lset-diff+intersection = lis1 . lists)
  (cond ((every null-list? lists) (values lis1 '()))
	((memq lis1 lists)        (values '() lis1))
	(else (partition (lambda (elt)
			   (not (any (lambda (lis) (member* elt lis =))
				     lists)))
			 lis1))))

(define (lset-diff+intersection! = lis1 . lists)
  (cond ((every null-list? lists) (values lis1 '()))
	((memq lis1 lists)        (values '() lis1))
	(else (partition! (lambda (elt)
			    (not (any (lambda (lis) (member* elt lis =))
				   lists)))
			  lis1))))



;;;; done

)

;;; end of library
