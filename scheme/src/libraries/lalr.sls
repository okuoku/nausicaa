;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: LALR(1) parser generator
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This library  is a LALR(1)  parser generator written  in Scheme.
;;;	It implements an efficient algorithm for computing the lookahead
;;;	sets.  The  algorithm is the  same as used  in GNU Bison  and is
;;;	described in:
;;;
;;;	   F.  DeRemer  and  T.  Pennello.  ``Efficient  Computation  of
;;;	   LALR(1)  Look-Ahead Set''.   TOPLAS, vol.  4, no.  4, october
;;;	   1982.
;;;
;;;	As a consequence, it is not written in a fully functional style.
;;;	In fact,  much of  the code  is a direct  translation from  C to
;;;	Scheme of the Bison sources.
;;;
;;;	The library is  a port to @rnrs{6} Scheme of  Lalr-scm by .  The
;;;	original code is available at:
;;;
;;;			<http://code.google.com/p/lalr-scm/>
;;;
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Port to R6RS and Nausicaa integration by Marco Maggi.
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
(library (lalr)
  (export
    lalr-parser

    :library-spec		:library-imports
    :parser-type		:parser-name
    :output-value		:output-port
    :output-file		:dump-table
    :expect			:terminals
    :rules

    ;; re-exports from (lalr common)
    make-source-location	source-location?
    source-location-line
    source-location-input
    source-location-column
    source-location-offset
    source-location-length

    make-lexical-token		lexical-token?
    lexical-token-value
    lexical-token-category
    lexical-token-source

    lexical-token?/end-of-input)
  (import (rnrs)
    (lalr common)
    (lists)
    (parameters)
    (keywords)
    (pretty-print)
    (rnrs mutable-pairs)
    (rnrs eval))


;;;; Keyword options for the LALR-PARSER function.

(define-keyword :terminals)
(define-keyword :rules)
(define-keyword :expect)

(define-keyword :output-value)
(define-keyword :output-port)
(define-keyword :output-file)

(define-keyword :dump-table)

(define-keyword :library-spec)
(define-keyword :library-imports)
(define-keyword :parser-type)

(define-keyword :parser-name)


;;;; helpers

(define (with-output-to-new-file pathname proc)
  (let ((port #f))
    (dynamic-wind
	(lambda ()
	  (set! port (open-file-output-port pathname
					    (file-options no-fail)
					    (buffer-mode block)
					    (native-transcoder))))
	(lambda () (proc port))
	(lambda ()
	  (close-output-port port)))))

(define-syntax position-in-list
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index (lambda (elm)
		   (equal? ?element elm))
       ?list))))

(define-syntax union-of-sorted-lists-of-numbers
  (syntax-rules ()
    ((_ ell1 ell2)
     (union-of-sorted-lists/uniq ell1 ell2 < >))))

(define-syntax sorted-list-of-numbers-insert
  (syntax-rules ()
    ((_ ?item ?ell)
     (sorted-list-insert ?item ?ell >))))


;;;; bit fields
;;
;;The following functions handle Scheme vectors whose elements are exact
;;integer  numbers.  Each  integer number  is used  as a  "word" holding
;;bits: It  is meant to  hold at least (LALR-BITS-PER-WORD)  bits.  This
;;value is configurable.
;;
;;The whole vector  is a bit field split into words;  let's say the bits
;;per integer are 30, then the first 30 bits are in the word at index 0,
;;the next 30 bits are in the word at index 1, and so on.
;;
;;The library always  use bit fields/vector of fixed  size.  The size is
;;computed with an equivalent of:
;;
;;  (define token-set-size (+ 1 (div nterms (lalr-bits-per-word))))
;;
;;in the body of LALR-PARSER.
;;
;;*NOTE* We do not have to confuse indexes in the vector with indexes of
;;bits.
;;
;;*FIXME*  These bit  fields can  probably be  replaced with  single big
;;integers;  do it  after understanding  how  they are  used.  A  simple
;;vector of  booleans could do, but  it seems we need  the OR operation,
;;which is faster with a single integer.
;;

(define lalr-bits-per-word
  (make-parameter 30))

(define (new-set nelem)
  (make-vector nelem 0))

(define (set-bit bit-field bit-index)
  ;;Interpret V as vector of numbers, which in turn are bit fields.
  ;;
  (let* ((nbits		(lalr-bits-per-word))
	 (word-index	(div  bit-index nbits))
	 (word		(expt 2 (mod bit-index nbits))))
    (vector-set! bit-field word-index
		 (bitwise-ior (vector-ref bit-field word-index) word))))

(define (bit-union v1 v2 vector-size)
  ;;Compute the OR between the bit fields and store the result in V1.
  ;;
  (do ((i 0 (+ 1 i)))
      ((= i vector-size))
    (vector-set! v1 i (bitwise-ior (vector-ref v1 i)
				   (vector-ref v2 i)))))


(define (lalr-parser . options)

  (define (main)
    (let-keywords options #f ((library-spec	:library-spec		#f)
			      (library-imports	:library-imports	'())
			      (parser-type	:parser-type		'lr)
			      (parser-name	:parser-name		#f)

			      (output-value	:output-value		#f)
			      (output-port	:output-port		#f)
			      (output-file	:output-file		#f)

			      (dump-table	:dump-table		#f)

			      (expect		:expect			0)
			      (rules		:rules			#f)
			      (terminals	:terminals		#f))

      (set! expected-conflicts expect)
      (set! driver-name (case parser-type
			  ((glr)	'glr-driver)
			  ((lr)		'lr-driver)
			  (else
			   (assertion-violation 'lalr-parser
			     "expected \"lr\" or \"glr\" as parser type"
			     parser-type))))

      (let* ((gram/actions (gen-tables! terminals rules))
	     (code         `(,driver-name ',action-table
					  ,(build-goto-table)
					  ,(build-reduction-table gram/actions))))

	(when dump-table
	  (with-output-to-new-file dump-table debug:print-states))

	(let* ((imports	(append `((rnrs) (lalr ,driver-name) (lalr common) (sentinel))
				library-imports))
	       (exports	`(,parser-name
			  ;; re-exports from (lalr common)
			  make-source-location		source-location?
			  source-location-line		source-location-input
			  source-location-column	source-location-offset
			  source-location-length
			  make-lexical-token		lexical-token?
			  lexical-token-value		lexical-token-category
			  lexical-token-source		lexical-token?/end-of-input))
	       (code	(cond (library-spec ;generate a library
			       (unless parser-name
				 (assertion-violation 'lalr-parser
				   "parser binding name required when building a library"))
			       `(library ,library-spec
				  (export ,@exports)
				  (import ,@imports)
				  (define (,parser-name) ,code)))

			      (parser-name ;generate a DEFINE form
			       `(define (,parser-name) ,code))

			      (else ;generate a lambda
			       `(lambda () ,code)))))
	  (cond (output-value
		 (eval code (apply environment imports)))
		(output-port
		 (pretty-print code output-port))
		(output-file
		 (with-output-to-new-file output-file
					  (lambda (port)
					    (pretty-print code port)))))))))


;;;; macro pour les structures de donnees

(define (new-core)              (make-vector 4 0))
(define (set-core-number! c n)  (vector-set! c 0 n))
(define (set-core-acc-sym! c s) (vector-set! c 1 s))
(define (set-core-nitems! c n)  (vector-set! c 2 n))
(define (set-core-items! c i)   (vector-set! c 3 i))
(define (core-number c)         (vector-ref c 0))
(define (core-acc-sym c)        (vector-ref c 1))
(define (core-nitems c)         (vector-ref c 2))
(define (core-items c)          (vector-ref c 3))

(define (new-shift)              (make-vector 3 0))
(define (set-shift-number! c x)  (vector-set! c 0 x))
(define (set-shift-nshifts! c x) (vector-set! c 1 x))
(define (set-shift-shifts! c x)  (vector-set! c 2 x))
(define (shift-number s)         (vector-ref s 0))
(define (shift-nshifts s)        (vector-ref s 1))
(define (shift-shifts s)         (vector-ref s 2))

(define (new-red)                (make-vector 3 0))
(define (set-red-number! c x)    (vector-set! c 0 x))
(define (set-red-nreds! c x)     (vector-set! c 1 x))
(define (set-red-rules! c x)     (vector-set! c 2 x))
(define (red-number c)           (vector-ref c 0))
(define (red-nreds c)            (vector-ref c 1))
(define (red-rules c)            (vector-ref c 2))

(define (vector-map f v)
  (let ((n (- (vector-length v) 1)))
    (let loop ((low 0)
	       (high n))
      (if (= low high)
	  (vector-set! v low (f (vector-ref v low) low))
	(let ((middle (div (+ low high) 2)))
	  (loop low middle)
	  (loop (+ middle 1) high))))))


;;;; state variables

;; - Constantes
(define STATE-TABLE-SIZE 1009)

;; - Tableaux
(define rrhs         #f)
(define rlhs         #f)
(define ritem        #f)
(define nullable     #f)
(define derives      #f)
(define fderives     #f)
(define firsts       #f)
(define kernel-base  #f)
(define kernel-end   #f)
(define shift-symbol #f)
(define shift-set    #f)
(define red-set      #f)
(define state-table  (make-vector STATE-TABLE-SIZE '()))
(define acces-symbol #f)
(define reduction-table #f)
(define shift-table  #f)
(define consistent   #f)
(define lookaheads   #f)
(define LA           #f)
(define LAruleno     #f)
(define lookback     #f)
(define goto-map     #f)
(define from-state   #f)
(define to-state     #f)
(define includes     #f)
(define F            #f)
(define action-table #f)

;; - Variables
(define nitems          #f)
(define nrules          #f)
(define nvars           #f)
(define nterms          #f)
(define nsyms           #f)
(define nstates         #f)
(define first-state     #f)
(define last-state      #f)
(define final-state     #f)
(define first-shift     #f)
(define last-shift      #f)
(define first-reduction #f)
(define last-reduction  #f)
(define nshifts         #f)
(define maxrhs          #f)
(define ngotos          #f)
(define token-set-size  #f)

(define driver-name     'lr-driver)


;;;; initialisation functions

(define (gen-tables! tokens gram)
  (rewrite-grammar tokens gram
		   (lambda (terms terms/prec vars gram gram/actions)
		     (set! the-terminals/prec (list->vector terms/prec))
		     (set! the-terminals (list->vector terms))
		     (set! the-nonterminals (list->vector vars))
		     (set! nterms (length terms))
		     (set! nvars  (length vars))
		     (set! nsyms  (+ nterms nvars))
		     (let ((no-of-rules (length gram/actions))
			   (no-of-items (let loop ((l gram/actions) (count 0))
					  (if (null? l)
					      count
					    (loop (cdr l) (+ count (length (caar l))))))))
		       (pack-grammar no-of-rules no-of-items gram)
		       (set-derives)
		       (set-nullable)
		       (generate-states)
		       (lalr)
		       (build-tables)
		       (compact-action-table terms)
		       (action-table-list->alist)
		       gram/actions))))

(define (pack-grammar no-of-rules no-of-items gram)
  (set! nrules (+  no-of-rules 1))
  (set! nitems no-of-items)
  (set! rlhs (make-vector nrules #f))
  (set! rrhs (make-vector nrules #f))
  (set! ritem (make-vector (+ 1 nitems) #f))

  (let loop ((p gram) (item-no 0) (rule-no 1))
    (if (not (null? p))
	(let ((nt (caar p)))
	  (let loop2 ((prods (cdar p)) (it-no2 item-no) (rl-no2 rule-no))
	    (if (null? prods)
		(loop (cdr p) it-no2 rl-no2)
	      (begin
		(vector-set! rlhs rl-no2 nt)
		(vector-set! rrhs rl-no2 it-no2)
		(let loop3 ((rhs (car prods)) (it-no3 it-no2))
		  (if (null? rhs)
		      (begin
			(vector-set! ritem it-no3 (- rl-no2))
			(loop2 (cdr prods) (+ it-no3 1) (+ rl-no2 1)))
		    (begin
		      (vector-set! ritem it-no3 (car rhs))
		      (loop3 (cdr rhs) (+ it-no3 1))))))))))))

(define (set-derives)
  (let ((delts (make-vector (+ nrules 1) 0))
	(dset  (make-vector nvars -1)))
    (let loop ((i 1) (j 0))
      (if (< i nrules)
	  (let ((lhs (vector-ref rlhs i)))
	    (if (>= lhs 0)
		(begin
		  (vector-set! delts j (cons i (vector-ref dset lhs)))
		  (vector-set! dset lhs j)
		  (loop (+ i 1) (+ j 1)))
	      (loop (+ i 1) j)))))
    (set! derives (make-vector nvars 0))
    (let loop ((i 0))
      (if (< i nvars)
	  (let ((q (let loop2 ((j (vector-ref dset i)) (s '()))
		     (if (< j 0)
			 s
		       (let ((x (vector-ref delts j)))
			 (loop2 (cdr x) (cons (car x) s)))))))
	    (vector-set! derives i q)
	    (loop (+ i 1)))))))

(define (set-nullable)
  (set! nullable (make-vector nvars #f))
  (let ((squeue (make-vector nvars #f))
	(rcount (make-vector (+ nrules 1) 0))
	(rsets  (make-vector nvars #f))
	(relts  (make-vector (+ nitems nvars 1) #f)))
    (let loop ((r 0) (s2 0) (p 0))
      (let ((*r (vector-ref ritem r)))
	(if *r
	    (if (< *r 0)
		(let ((symbol (vector-ref rlhs (- *r))))
		  (if (and (>= symbol 0)
			   (not (vector-ref nullable symbol)))
		      (begin
			(vector-set! nullable symbol #t)
			(vector-set! squeue s2 symbol)
			(loop (+ r 1) (+ s2 1) p))))
	      (let loop2 ((r1 r) (any-tokens #f))
		(let* ((symbol (vector-ref ritem r1)))
		  (if (> symbol 0)
		      (loop2 (+ r1 1) (or any-tokens (>= symbol nvars)))
		    (if (not any-tokens)
			(let ((ruleno (- symbol)))
			  (let loop3 ((r2 r) (p2 p))
			    (let ((symbol (vector-ref ritem r2)))
			      (if (> symbol 0)
				  (begin
				    (vector-set! rcount ruleno
						 (+ (vector-ref rcount ruleno) 1))
				    (vector-set! relts p2
						 (cons (vector-ref rsets symbol)
						       ruleno))
				    (vector-set! rsets symbol p2)
				    (loop3 (+ r2 1) (+ p2 1)))
				(loop (+ r2 1) s2 p2)))))
		      (loop (+ r1 1) s2 p))))))
	  (let loop ((s1 0) (s3 s2))
	    (if (< s1 s3)
		(let loop2 ((p (vector-ref rsets (vector-ref squeue s1))) (s4 s3))
		  (if p
		      (let* ((x (vector-ref relts p))
			     (ruleno (cdr x))
			     (y (- (vector-ref rcount ruleno) 1)))
			(vector-set! rcount ruleno y)
			(if (= y 0)
			    (let ((symbol (vector-ref rlhs ruleno)))
			      (if (and (>= symbol 0)
				       (not (vector-ref nullable symbol)))
				  (begin
				    (vector-set! nullable symbol #t)
				    (vector-set! squeue s4 symbol)
				    (loop2 (car x) (+ s4 1)))
				(loop2 (car x) s4)))
			  (loop2 (car x) s4))))
		  (loop (+ s1 1) s4)))))))))

(define (set-firsts)
  (set! firsts (make-vector nvars '()))

  ;; -- initialization
  (let loop ((i 0))
    (if (< i nvars)
	(let loop2 ((sp (vector-ref derives i)))
	  (if (null? sp)
	      (loop (+ i 1))
	    (let ((sym (vector-ref ritem (vector-ref rrhs (car sp)))))
	      (if (< -1 sym nvars)
		  (vector-set! firsts i
			       (sorted-list-of-numbers-insert sym (vector-ref firsts i))))
	      (loop2 (cdr sp)))))))

  ;; -- reflexive and transitive closure
  (let loop ((continue #t))
    (if continue
	(let loop2 ((i 0) (cont #f))
	  (if (>= i nvars)
	      (loop cont)
	    (let* ((x (vector-ref firsts i))
		   (y (let loop3 ((l x) (z x))
			(if (null? l)
			    z
			  (loop3 (cdr l)
				 (union-of-sorted-lists-of-numbers (vector-ref firsts (car l)) z))))))
	      (if (equal? x y)
		  (loop2 (+ i 1) cont)
		(begin
		  (vector-set! firsts i y)
		  (loop2 (+ i 1) #t))))))))

  (let loop ((i 0))
    (if (< i nvars)
	(begin
	  (vector-set! firsts i (sorted-list-of-numbers-insert i (vector-ref firsts i)))
	  (loop (+ i 1))))))

(define (set-fderives)
  (set! fderives (make-vector nvars #f))
  (set-firsts)
  (let loop ((i 0))
    (if (< i nvars)
	(let ((x (let loop2 ((l (vector-ref firsts i)) (fd '()))
		   (if (null? l)
		       fd
		     (loop2 (cdr l)
			    (union-of-sorted-lists-of-numbers (vector-ref derives (car l)) fd))))))
	  (vector-set! fderives i x)
	  (loop (+ i 1))))))

(define (closure core)
  ;; Initialization
  (define ruleset (make-vector nrules #f))

  (let loop ((csp core))
    (if (not (null? csp))
	(let ((sym (vector-ref ritem (car csp))))
	  (if (< -1 sym nvars)
	      (let loop2 ((dsp (vector-ref fderives sym)))
		(if (not (null? dsp))
		    (begin
		      (vector-set! ruleset (car dsp) #t)
		      (loop2 (cdr dsp))))))
	  (loop (cdr csp)))))

  (let loop ((ruleno 1) (csp core) (itemsetv '())) ; ruleno = 0
    (if (< ruleno nrules)
	(if (vector-ref ruleset ruleno)
	    (let ((itemno (vector-ref rrhs ruleno)))
	      (let loop2 ((c csp) (itemsetv2 itemsetv))
		(if (and (pair? c)
			 (< (car c) itemno))
		    (loop2 (cdr c) (cons (car c) itemsetv2))
		  (loop (+ ruleno 1) c (cons itemno itemsetv2)))))
	  (loop (+ ruleno 1) csp itemsetv))
      (let loop2 ((c csp) (itemsetv2 itemsetv))
	(if (pair? c)
	    (loop2 (cdr c) (cons (car c) itemsetv2))
	  (reverse itemsetv2))))))


;;;;

(define (generate-states)
  (set! kernel-base	(make-vector nsyms 0))
  (set! kernel-end	(make-vector nsyms #f))
  (set! red-set		(make-vector (+ nrules 1) 0))
  (set-fderives)
  ;;Initialize states.
  (let ((p (new-core)))
    (set-core-number! p 0)
    (set-core-acc-sym! p #f)
    (set-core-nitems! p 1)
    (set-core-items! p '(0))

    (set! first-state (list p))
    (set! last-state first-state)
    (set! nstates 1))
  (let loop ((this-state first-state))
    (when (pair? this-state)
      (let* ((x  (car this-state))
	     (is (closure (core-items x))))
	(save-reductions x is)
	(new-itemsets is)
	(append-states)
	(if (> nshifts 0)
	    (save-shifts x))
	(loop (cdr this-state))))))

(define (new-itemsets itemset)
  ;; - Initialization
  (set! shift-symbol '())
  (let loop ((i 0))
    (if (< i nsyms)
	(begin
	  (vector-set! kernel-end i '())
	  (loop (+ i 1)))))

  (let loop ((isp itemset))
    (if (pair? isp)
	(let* ((i (car isp))
	       (sym (vector-ref ritem i)))
	  (if (>= sym 0)
	      (begin
		(set! shift-symbol (sorted-list-of-numbers-insert sym shift-symbol))
		(let ((x (vector-ref kernel-end sym)))
		  (if (null? x)
		      (begin
			(vector-set! kernel-base sym (cons (+ i 1) x))
			(vector-set! kernel-end sym (vector-ref kernel-base sym)))
		    (begin
		      (set-cdr! x (list (+ i 1)))
		      (vector-set! kernel-end sym (cdr x)))))))
	  (loop (cdr isp)))))

  (set! nshifts (length shift-symbol)))

(define (get-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (key  (let loop ((isp1 isp) (k 0))
		 (if (null? isp1)
		     (mod k STATE-TABLE-SIZE)
		   (loop (cdr isp1) (+ k (car isp1))))))
	 (sp   (vector-ref state-table key)))
    (if (null? sp)
	(let ((x (new-state sym)))
	  (vector-set! state-table key (list x))
	  (core-number x))
      (let loop ((sp1 sp))
	(if (and (= n (core-nitems (car sp1)))
		 (let loop2 ((i1 isp) (t (core-items (car sp1))))
		   (if (and (pair? i1)
			    (= (car i1)
			       (car t)))
		       (loop2 (cdr i1) (cdr t))
		     (null? i1))))
	    (core-number (car sp1))
	  (if (null? (cdr sp1))
	      (let ((x (new-state sym)))
		(set-cdr! sp1 (list x))
		(core-number x))
	    (loop (cdr sp1))))))))

(define (new-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (p    (new-core)))
    (set-core-number! p nstates)
    (set-core-acc-sym! p sym)
    (if (= sym nvars) (set! final-state nstates))
    (set-core-nitems! p n)
    (set-core-items! p isp)
    (set-cdr! last-state (list p))
    (set! last-state (cdr last-state))
    (set! nstates (+ nstates 1))
    p))


;;;;

(define (append-states)
  (set! shift-set
	(let loop ((l (reverse shift-symbol)))
	  (if (null? l)
	      '()
	    (cons (get-state (car l)) (loop (cdr l)))))))


;;;;

(define (save-shifts core)
  (let ((p (new-shift)))
    (set-shift-number! p (core-number core))
    (set-shift-nshifts! p nshifts)
    (set-shift-shifts! p shift-set)
    (if last-shift
	(begin
	  (set-cdr! last-shift (list p))
	  (set! last-shift (cdr last-shift)))
      (begin
	(set! first-shift (list p))
	(set! last-shift first-shift)))))

(define (save-reductions core itemset)
  (let ((rs (let loop ((l itemset))
	      (if (null? l)
		  '()
		(let ((item (vector-ref ritem (car l))))
		  (if (< item 0)
		      (cons (- item) (loop (cdr l)))
		    (loop (cdr l))))))))
    (if (pair? rs)
	(let ((p (new-red)))
	  (set-red-number! p (core-number core))
	  (set-red-nreds!  p (length rs))
	  (set-red-rules!  p rs)
	  (if last-reduction
	      (begin
		(set-cdr! last-reduction (list p))
		(set! last-reduction (cdr last-reduction)))
	    (begin
	      (set! first-reduction (list p))
	      (set! last-reduction first-reduction)))))))


;;;;

(define (lalr)
  (set! token-set-size (+ 1 (div nterms (lalr-bits-per-word))))
  (set-accessing-symbol)
  (set-shift-table)
  (set-reduction-table)
  (set-max-rhs)
  (initialize-LA)
  (set-goto-map)
  (initialize-F)
  (build-relations)
  (digraph includes)
  (compute-lookaheads))

(define (set-accessing-symbol)
  (set! acces-symbol (make-vector nstates #f))
  (let loop ((l first-state))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! acces-symbol (core-number x) (core-acc-sym x))
	  (loop (cdr l))))))

(define (set-shift-table)
  (set! shift-table (make-vector nstates #f))
  (let loop ((l first-shift))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! shift-table (shift-number x) x)
	  (loop (cdr l))))))

(define (set-reduction-table)
  (set! reduction-table (make-vector nstates #f))
  (let loop ((l first-reduction))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! reduction-table (red-number x) x)
	  (loop (cdr l))))))

(define (set-max-rhs)
  (let loop ((p 0) (curmax 0) (length 0))
    (let ((x (vector-ref ritem p)))
      (if x
	  (if (>= x 0)
	      (loop (+ p 1) curmax (+ length 1))
	    (loop (+ p 1) (max curmax length) 0))
	(set! maxrhs curmax)))))

(define (initialize-LA)
  (define (last l)
    (if (null? (cdr l))
	(car l)
      (last (cdr l))))

  (set! consistent (make-vector nstates #f))
  (set! lookaheads (make-vector (+ nstates 1) #f))

  (let loop ((count 0) (i 0))
    (if (< i nstates)
	(begin
	  (vector-set! lookaheads i count)
	  (let ((rp (vector-ref reduction-table i))
		(sp (vector-ref shift-table i)))
	    (if (and rp
		     (or (> (red-nreds rp) 1)
			 (and sp
			      (not
			       (< (vector-ref acces-symbol
					      (last (shift-shifts sp)))
				  nvars)))))
		(loop (+ count (red-nreds rp)) (+ i 1))
	      (begin
		(vector-set! consistent i #t)
		(loop count (+ i 1))))))

      (begin
	(vector-set! lookaheads nstates count)
	(let ((c (max count 1)))
	  (set! LA (make-vector c #f))
	  (do ((j 0 (+ j 1)))
	      ((= j c))
	    (vector-set! LA j (new-set token-set-size)))
	  (set! LAruleno (make-vector c -1))
	  (set! lookback (make-vector c #f)))
	(let loop ((i 0) (np 0))
	  (if (< i nstates)
	      (if (vector-ref consistent i)
		  (loop (+ i 1) np)
		(let ((rp (vector-ref reduction-table i)))
		  (if rp
		      (let loop2 ((j (red-rules rp)) (np2 np))
			(if (null? j)
			    (loop (+ i 1) np2)
			  (begin
			    (vector-set! LAruleno np2 (car j))
			    (loop2 (cdr j) (+ np2 1)))))
		    (loop (+ i 1) np))))))))))


(define (set-goto-map)
  (set! goto-map (make-vector (+ nvars 1) 0))
  (let ((temp-map (make-vector (+ nvars 1) 0)))
    (let loop ((ng 0) (sp first-shift))
      (if (pair? sp)
	  (let loop2 ((i (reverse (shift-shifts (car sp)))) (ng2 ng))
	    (if (pair? i)
		(let ((symbol (vector-ref acces-symbol (car i))))
		  (if (< symbol nvars)
		      (begin
			(vector-set! goto-map symbol
				     (+ 1 (vector-ref goto-map symbol)))
			(loop2 (cdr i) (+ ng2 1)))
		    (loop2 (cdr i) ng2)))
	      (loop ng2 (cdr sp))))

	(let loop ((k 0) (i 0))
	  (if (< i nvars)
	      (begin
		(vector-set! temp-map i k)
		(loop (+ k (vector-ref goto-map i)) (+ i 1)))

	    (begin
	      (do ((i 0 (+ i 1)))
		  ((>= i nvars))
		(vector-set! goto-map i (vector-ref temp-map i)))

	      (set! ngotos ng)
	      (vector-set! goto-map nvars ngotos)
	      (vector-set! temp-map nvars ngotos)
	      (set! from-state (make-vector ngotos #f))
	      (set! to-state (make-vector ngotos #f))

	      (do ((sp first-shift (cdr sp)))
		  ((null? sp))
		(let* ((x (car sp))
		       (state1 (shift-number x)))
		  (do ((i (shift-shifts x) (cdr i)))
		      ((null? i))
		    (let* ((state2 (car i))
			   (symbol (vector-ref acces-symbol state2)))
		      (if (< symbol nvars)
			  (let ((k (vector-ref temp-map symbol)))
			    (vector-set! temp-map symbol (+ k 1))
			    (vector-set! from-state k state1)
			    (vector-set! to-state k state2))))))))))))))


(define (map-goto state symbol)
  (let loop ((low (vector-ref goto-map symbol))
	     (high (- (vector-ref goto-map (+ symbol 1)) 1)))
    (if (> low high)
	(begin
	  (display (list "Error in map-goto" state symbol)) (newline)
	  0)
      (let* ((middle (div (+ low high) 2))
	     (s (vector-ref from-state middle)))
	(cond
	 ((= s state)
	  middle)
	 ((< s state)
	  (loop (+ middle 1) high))
	 (else
	  (loop low (- middle 1))))))))


(define (initialize-F)
  (set! F (make-vector ngotos #f))
  (do ((i 0 (+ 1 i)))
      ((= i ngotos))
    (vector-set! F i (new-set token-set-size)))

  (let ((reads (make-vector ngotos #f)))

    (let loop ((i 0) (rowp 0))
      (if (< i ngotos)
	  (let* ((rowf (vector-ref F rowp))
		 (stateno (vector-ref to-state i))
		 (sp (vector-ref shift-table stateno)))
	    (if sp
		(let loop2 ((j (shift-shifts sp)) (edges '()))
		  (if (pair? j)
		      (let ((symbol (vector-ref acces-symbol (car j))))
			(if (< symbol nvars)
			    (if (vector-ref nullable symbol)
				(loop2 (cdr j) (cons (map-goto stateno symbol)
						     edges))
			      (loop2 (cdr j) edges))
			  (begin
			    (set-bit rowf (- symbol nvars))
			    (loop2 (cdr j) edges))))
		    (if (pair? edges)
			(vector-set! reads i (reverse edges))))))
	    (loop (+ i 1) (+ rowp 1)))))
    (digraph reads)))

(define (add-lookback-edge stateno ruleno gotono)
  (let ((k (vector-ref lookaheads (+ stateno 1))))
    (let loop ((found #f) (i (vector-ref lookaheads stateno)))
      (if (and (not found) (< i k))
	  (if (= (vector-ref LAruleno i) ruleno)
	      (loop #t i)
	    (loop found (+ i 1)))

	(if (not found)
	    (begin (display "Error in add-lookback-edge : ")
		   (display (list stateno ruleno gotono)) (newline))
	  (vector-set! lookback i
		       (cons gotono (vector-ref lookback i))))))))


(define (transpose r-arg n)
  (let ((new-end (make-vector n #f))
	(new-R  (make-vector n #f)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (let ((x (list 'bidon)))
	(vector-set! new-R i x)
	(vector-set! new-end i x)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (let ((sp (vector-ref r-arg i)))
	(if (pair? sp)
	    (let loop ((sp2 sp))
	      (if (pair? sp2)
		  (let* ((x (car sp2))
			 (y (vector-ref new-end x)))
		    (set-cdr! y (cons i (cdr y)))
		    (vector-set! new-end x (cdr y))
		    (loop (cdr sp2))))))))
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! new-R i (cdr (vector-ref new-R i))))

    new-R))



(define (build-relations)

  (define (get-state stateno symbol)
    (let loop ((j (shift-shifts (vector-ref shift-table stateno)))
	       (stno stateno))
      (if (null? j)
	  stno
	(let ((st2 (car j)))
	  (if (= (vector-ref acces-symbol st2) symbol)
	      st2
	    (loop (cdr j) st2))))))

  (set! includes (make-vector ngotos #f))
  (do ((i 0 (+ i 1)))
      ((= i ngotos))
    (let ((state1 (vector-ref from-state i))
	  (symbol1 (vector-ref acces-symbol (vector-ref to-state i))))
      (let loop ((rulep (vector-ref derives symbol1))
		 (edges '()))
	(if (pair? rulep)
	    (let ((*rulep (car rulep)))
	      (let loop2 ((rp (vector-ref rrhs *rulep))
			  (stateno state1)
			  (states (list state1)))
		(let ((*rp (vector-ref ritem rp)))
		  (if (> *rp 0)
		      (let ((st (get-state stateno *rp)))
			(loop2 (+ rp 1) st (cons st states)))
		    (begin

		      (if (not (vector-ref consistent stateno))
			  (add-lookback-edge stateno *rulep i))

		      (let loop2 ((done #f)
				  (stp (cdr states))
				  (rp2 (- rp 1))
				  (edgp edges))
			(if (not done)
			    (let ((*rp (vector-ref ritem rp2)))
			      (if (< -1 *rp nvars)
				  (loop2 (not (vector-ref nullable *rp))
					 (cdr stp)
					 (- rp2 1)
					 (cons (map-goto (car stp) *rp) edgp))
				(loop2 #t stp rp2 edgp)))

			  (loop (cdr rulep) edgp))))))))
	  (vector-set! includes i edges)))))
  (set! includes (transpose includes ngotos)))



(define (compute-lookaheads)
  (let ((n (vector-ref lookaheads nstates)))
    (let loop ((i 0))
      (if (< i n)
	  (let loop2 ((sp (vector-ref lookback i)))
	    (if (pair? sp)
		(let ((LA-i (vector-ref LA i))
		      (F-j  (vector-ref F (car sp))))
		  (bit-union LA-i F-j token-set-size)
		  (loop2 (cdr sp)))
	      (loop (+ i 1))))))))



(define (digraph relation)
  (define infinity (+ ngotos 2))
  (define INDEX (make-vector (+ ngotos 1) 0))
  (define VERTICES (make-vector (+ ngotos 1) 0))
  (define top 0)
  (define R relation)

  (define (traverse i)
    (set! top (+ 1 top))
    (vector-set! VERTICES top i)
    (let ((height top))
      (vector-set! INDEX i height)
      (let ((rp (vector-ref R i)))
	(if (pair? rp)
	    (let loop ((rp2 rp))
	      (if (pair? rp2)
		  (let ((j (car rp2)))
		    (if (= 0 (vector-ref INDEX j))
			(traverse j))
		    (if (> (vector-ref INDEX i)
			   (vector-ref INDEX j))
			(vector-set! INDEX i (vector-ref INDEX j)))
		    (let ((F-i (vector-ref F i))
			  (F-j (vector-ref F j)))
		      (bit-union F-i F-j token-set-size))
		    (loop (cdr rp2))))))
	(if (= (vector-ref INDEX i) height)
	    (let loop ()
	      (let ((j (vector-ref VERTICES top)))
		(set! top (- top 1))
		(vector-set! INDEX j infinity)
		(if (not (= i j))
		    (begin
		      (bit-union (vector-ref F i)
				 (vector-ref F j)
				 token-set-size)
		      (loop)))))))))

  (let loop ((i 0))
    (if (< i ngotos)
	(begin
	  (if (and (= 0 (vector-ref INDEX i))
		   (pair? (vector-ref R i)))
	      (traverse i))
	  (loop (+ i 1)))))) ; end of DIGRAPH


;;;; operator precedence management

;; a vector of precedence descriptors where each element
;; is of the form (terminal type precedence)
(define the-terminals/prec #f)   ; terminal symbols with precedence
		; the precedence is an integer >= 0
(define (get-symbol-precedence sym)
  (caddr (vector-ref the-terminals/prec sym)))
		; the operator type is either 'none, 'left, 'right, or 'nonassoc
(define (get-symbol-assoc sym)
  (cadr (vector-ref the-terminals/prec sym)))

(define rule-precedences '())
(define (add-rule-precedence! rule sym)
  (set! rule-precedences
	(cons (cons rule sym) rule-precedences)))

(define (get-rule-precedence ruleno)
  (cond
   ((assq ruleno rule-precedences)
    => (lambda (p)
	 (get-symbol-precedence (cdr p))))
   (else
    ;; process the rule symbols from left to right
    (let loop ((i    (vector-ref rrhs ruleno))
	       (prec 0))
      (let ((item (vector-ref ritem i)))
	;; end of rule
	(if (< item 0)
	    prec
	  (let ((i1 (+ i 1)))
	    (if (>= item nvars)
		;; it's a terminal symbol
		(loop i1 (get-symbol-precedence (- item nvars)))
	      (loop i1 prec)))))))))


;;;; build the various tables

(define expected-conflicts	0)
(define the-terminals		#f)	;names of terminal symbols
(define the-nonterminals	#f)	;non-terminals

(define (get-symbol n)
  (if (>= n nvars)
      (vector-ref the-terminals (- n nvars))
    (vector-ref the-nonterminals n)))

(define (build-tables)

  (define (main)
    (set! action-table (make-vector nstates '()))
    (do ((i 0 (+ i 1)))	; i = state
	((= i nstates))
      (let ((red (vector-ref reduction-table i)))
	(if (and red (>= (red-nreds red) 1))
	    (if (and (= (red-nreds red) 1) (vector-ref consistent i))
		(add-action-for-all-terminals i (- (car (red-rules red))))
	      (let ((k (vector-ref lookaheads (+ i 1))))
		(let loop ((j (vector-ref lookaheads i)))
		  (if (< j k)
		      (let ((rule (- (vector-ref LAruleno j)))
			    (lav  (vector-ref LA j)))
			(let loop2 ((token 0) (x (vector-ref lav 0)) (y 1) (z 0))
			  (if (< token nterms)
			      (begin
				(let ((in-la-set? (mod x 2)))
				  (if (= in-la-set? 1)
				      (add-action i token rule)))
				(if (= y (lalr-bits-per-word))
				    (loop2 (+ token 1)
					   (vector-ref lav (+ z 1))
					   1
					   (+ z 1))
				  (loop2 (+ token 1) (div x 2) (+ y 1) z)))))
			(loop (+ j 1)))))))))

      (let ((shiftp (vector-ref shift-table i)))
	(if shiftp
	    (let loop ((k (shift-shifts shiftp)))
	      (if (pair? k)
		  (let* ((state (car k))
			 (symbol (vector-ref acces-symbol state)))
		    (if (>= symbol nvars)
			(add-action i (- symbol nvars) state))
		    (loop (cdr k))))))))

    (add-action final-state 0 'accept)
    (log-conflicts))

  (define (resolve-conflict sym rule)
    (let ((sym-prec   (get-symbol-precedence sym))
	  (sym-assoc  (get-symbol-assoc sym))
	  (rule-prec  (get-rule-precedence rule)))
      (cond
       ((> sym-prec rule-prec)     'shift)
       ((< sym-prec rule-prec)     'reduce)
       ((eq? sym-assoc 'left)      'reduce)
       ((eq? sym-assoc 'right)     'shift)
       (else                       'none))))

  (define conflict-messages '())

  (define (add-conflict-message . l)
    (set! conflict-messages (cons l conflict-messages)))

  (define (log-conflicts)
    (when (and expected-conflicts
	       (> (length conflict-messages) expected-conflicts))
      (for-each
	  (lambda (message)
	    (for-each (lambda (s)
			(display s (current-error-port)))
	      message)
	    (newline (current-error-port)))
	conflict-messages)))

  (define (add-action state symbol new-action)
    ;;Add an action to the action table.
    ;;
    (let* ((state-actions (vector-ref action-table state))
	   (actions       (assv symbol state-actions)))
      (if (pair? actions)
	  (let ((current-action (cadr actions)))
	    (if (not (= new-action current-action))
		;; -- there is a conflict
		(begin
		  (if (and (<= current-action 0) (<= new-action 0))
		      ;; --- reduce/reduce conflict
		      (begin
			(add-conflict-message
			 "%% Reduce/Reduce conflict (reduce " (- new-action) ", reduce " (- current-action)
			 ") on '" (get-symbol (+ symbol nvars)) "' in state " state)
			(if (eq? driver-name 'glr-driver)
			    (set-cdr! (cdr actions) (cons new-action (cddr actions)))
			  (set-car! (cdr actions) (max current-action new-action))))
		    ;; --- shift/reduce conflict
		    ;; can we resolve the conflict using precedences?
		    (case (resolve-conflict symbol (- current-action))
		      ;; -- shift
		      ((shift)   (if (eq? driver-name 'glr-driver)
				     (set-cdr! (cdr actions) (cons new-action (cddr actions)))
				   (set-car! (cdr actions) new-action)))
		      ;; -- reduce
		      ((reduce)  #f) ; well, nothing to do...
		      ;; -- signal a conflict!
		      (else      (add-conflict-message
				  "%% Shift/Reduce conflict (shift " new-action ", reduce " (- current-action)
				  ") on '" (get-symbol (+ symbol nvars)) "' in state " state)
				 (if (eq? driver-name 'glr-driver)
				     (set-cdr! (cdr actions) (cons new-action (cddr actions)))
				   (set-car! (cdr actions) new-action))))))))

	(vector-set! action-table state (cons (list symbol new-action) state-actions)))))

  (define (add-action-for-all-terminals state action)
    (do ((i 1 (+ i 1)))
	((= i nterms))
      (add-action state i action)))

  (main))

(define (compact-action-table terms)
  (define (most-common-action acts)
    (let ((accums '()))
      (let loop ((l acts))
	(if (pair? l)
	    (let* ((x (cadar l))
		   (y (assv x accums)))
	      (if (and (number? x) (< x 0))
		  (if y
		      (set-cdr! y (+ 1 (cdr y)))
		    (set! accums (cons `(,x . 1) accums))))
	      (loop (cdr l)))))

      (let loop ((l accums) (max 0) (sym #f))
	(if (null? l)
	    sym
	  (let ((x (car l)))
	    (if (> (cdr x) max)
		(loop (cdr l) (cdr x) (car x))
	      (loop (cdr l) max sym)))))))

  (define (translate-terms acts)
    (map (lambda (act)
	   (cons (list-ref terms (car act))
		 (cdr act)))
      acts))

  (do ((i 0 (+ i 1)))
      ((= i nstates))
    (let ((acts (vector-ref action-table i)))
      (if (vector? (vector-ref reduction-table i))
	  (let ((act (most-common-action acts)))
	    (vector-set! action-table i
			 (cons `(*default* ,(if act act '*error*))
			       (translate-terms
				(filter (lambda (x)
					  (not (and (= (length x) 2)
						    (eq? (cadr x) act))))
				  acts)))))
	(vector-set! action-table i
		     (cons `(*default* *error*)
			   (translate-terms acts)))))))

(define (action-table-list->alist)
  ;;For some unknown reason the action list is built as a list of lists,
  ;;even though it is used as  alist.  This function is called after the
  ;;construction is finished, to convert the list of lists into a proper
  ;;list.  (Marco Maggi, Wed Aug 5, 2009)
  ;;
  (do ((len (vector-length action-table))
       (i 0 (+ 1 i)))
      ((= i len))
    (vector-set! action-table i
		 (map (lambda (ell)
			(cons (car ell) (cadr ell)))
		   (vector-ref action-table i)))))


;;;;

(define valid-nonterminal?	symbol?)
(define valid-terminal?		symbol?)

(define (rewrite-grammar tokens grammar k)

  (define eoi '*eoi*)

  (define (check-terminal term terms)
    (cond
     ((not (valid-terminal? term))
      (error "invalid terminal: " term))
     ((member term terms)
      (error "duplicate definition of terminal: " term))))

  (define (prec->type prec)
    (cdr (assq prec '((left:     . left)
		      (right:    . right)
		      (nonassoc: . nonassoc)))))

  (cond
   ;; --- a few error conditions
   ((not (list? tokens))
    (error "Invalid token list: " tokens))
   ((not (pair? grammar))
    (error "Grammar definition must have a non-empty list of productions" '()))

   (else
    ;; --- check the terminals
    (let loop1 ((lst            tokens)
		(rev-terms      '())
		(rev-terms/prec '())
		(prec-level     0))
      (if (pair? lst)
	  (let ((term (car lst)))
	    (cond
	     ((pair? term)
	      (if (and (memq (car term) '(left: right: nonassoc:))
		       (not (null? (cdr term))))
		  (let ((prec    (+ prec-level 1))
			(optype  (prec->type (car term))))
		    (let loop-toks ((l             (cdr term))
				    (rev-terms      rev-terms)
				    (rev-terms/prec rev-terms/prec))
		      (if (null? l)
			  (loop1 (cdr lst) rev-terms rev-terms/prec prec)
			(let ((term (car l)))
			  (check-terminal term rev-terms)
			  (loop-toks
			   (cdr l)
			   (cons term rev-terms)
			   (cons (list term optype prec) rev-terms/prec))))))

		(error "invalid operator precedence specification: " term)))

	     (else
	      (check-terminal term rev-terms)
	      (loop1 (cdr lst)
		     (cons term rev-terms)
		     (cons (list term 'none 0) rev-terms/prec)
		     prec-level))))

	;; --- check the grammar rules
	(let loop2 ((lst grammar) (rev-nonterm-defs '()))
	  (if (pair? lst)
	      (let ((def (car lst)))
		(if (not (pair? def))
		    (error "Nonterminal definition must be a non-empty list" '())
		  (let ((nonterm (car def)))
		    (cond ((not (valid-nonterminal? nonterm))
			   (error "Invalid nonterminal:" nonterm))
			  ((or (member nonterm rev-terms)
			       (assoc nonterm rev-nonterm-defs))
			   (error "Nonterminal previously defined:" nonterm))
			  (else
			   (loop2 (cdr lst)
				  (cons def rev-nonterm-defs)))))))
	    (let* ((terms        (cons eoi            (cons 'error          (reverse rev-terms))))
		   (terms/prec   (cons '(eoi none 0)  (cons '(error none 0) (reverse rev-terms/prec))))
		   (nonterm-defs (reverse rev-nonterm-defs))
		   (nonterms     (cons '*start* (map car nonterm-defs))))
	      (if (= (length nonterms) 1)
		  (error "Grammar must contain at least one nonterminal" '())
		(let loop-defs ((defs      (cons `(*start* (,(cadr nonterms) ,eoi) : $1)
						 nonterm-defs))
				(ruleno    0)
				(comp-defs '()))
		  (if (pair? defs)
		      (let* ((nonterm-def  (car defs))
			     (compiled-def (rewrite-nonterm-def
					    nonterm-def
					    ruleno
					    terms nonterms)))
			(loop-defs (cdr defs)
				   (+ ruleno (length compiled-def))
				   (cons compiled-def comp-defs)))

		    (let ((compiled-nonterm-defs (reverse comp-defs)))
		      (k terms
			 terms/prec
			 nonterms
			 (map (lambda (x) (cons (caaar x) (map cdar x)))
			   compiled-nonterm-defs)
			 (apply append compiled-nonterm-defs))))))))))))))


(define (rewrite-nonterm-def nonterm-def ruleno terms nonterms)

  (define No-NT (length nonterms))

  (define (encode x)
    (let ((PosInNT (position-in-list x nonterms)))
      (if PosInNT
	  PosInNT
	(let ((PosInT (position-in-list x terms)))
	  (if PosInT
	      (+ No-NT PosInT)
	    (error "undefined symbol : " x))))))

  (define (process-prec-directive rhs ruleno)
    (let loop ((l rhs))
      (if (null? l)
	  '()
	(let ((first (car l))
	      (rest  (cdr l)))
	  (cond
	   ((or (member first terms) (member first nonterms))
	    (cons first (loop rest)))
	   ((and (pair? first)
		 (eq? (car first) 'prec:))
	    (if (and (pair? (cdr first))
		     (null? (cddr first))
		     (member (cadr first) terms))
		(if (null? rest)
		    (begin
		      (add-rule-precedence! ruleno (position-in-list (cadr first) terms))
		      (loop rest))
		  (error "prec: directive should be at end of rule: " rhs))
	      (error "Invalid prec: directive: " first)))
	   (else
	    (error "Invalid terminal or nonterminal: " first)))))))

  (define (check-error-production rhs)
    (let loop ((rhs rhs))
      (if (pair? rhs)
	  (begin
	    (if (and (eq? (car rhs) 'error)
		     (or (null? (cdr rhs))
			 (not (member (cadr rhs) terms))
			 (not (null? (cddr rhs)))))
		(error "Invalid 'error' production. A single terminal symbol must follow the 'error' token.:" rhs))
	    (loop (cdr rhs))))))


  (if (not (pair? (cdr nonterm-def)))
      (error "At least one production needed for nonterminal:" (car nonterm-def))
    (let ((name (symbol->string (car nonterm-def))))
      (let loop1 ((lst (cdr nonterm-def))
		  (i 1)
		  (rev-productions-and-actions '()))
	(if (not (pair? lst))
	    (reverse rev-productions-and-actions)
	  (let* ((rhs  (process-prec-directive (car lst) (+ ruleno i -1)))
		 (rest (cdr lst))
		 (prod (map encode (cons (car nonterm-def) rhs))))
	    ;; -- check for undefined tokens
	    (for-each (lambda (x)
			(if (not (or (member x terms) (member x nonterms)))
			    (error "Invalid terminal or nonterminal:" x)))
	      rhs)
	    ;; -- check 'error' productions
	    (check-error-production rhs)

	    ;;The  following form detects  the presence  of the  ": ---"
	    ;;trailer in a rule definition, that is it detects if a rule
	    ;;has a client form.
	    (if (and (pair? rest)
		     (eq? (car rest) ':)
		     (pair? (cdr rest)))
		(loop1 (cddr rest)
		       (+ i 1)
		       (cons (cons prod (cadr rest))
			     rev-productions-and-actions))
	      ;;The following form generates
	      (let* ((rhs-length (length rhs))
		     (action
;;;NOTE
;;;(Marco Maggi; Wed Aug  5, 2009)
;;;
;;;The following form  generate a fake client form  whenever the grammar
;;;definition has a right-hand side  rule with no client form.  I cannot
;;;understand  why in  hell the  fake form  for a  rule that  consumes 3
;;;values  from the  stack, in  a non-terminal  called "woppa",  has the
;;;format:
;;;
;;;	(vector 'woppa-3 $1 $2 $3)
;;;
;;;if it consumes 4 values it is:
;;;
;;;	(vector 'woppa-4 $1 $2 $3 $4)
;;;
;;;and so  on.  I  replace it  with the sentinel  value, let's  see what
;;;happens.
;;;
;;;Original code:
;;;
;;; 		      (cons 'vector
;;; 			    (cons (list 'quote (string->symbol
;;; 						(string-append
;;; 						 name
;;; 						 "-"
;;; 						 (number->string i))))
;;; 				  (let loop-j ((j 1))
;;; 				    (if (> j rhs-length)
;;; 					'()
;;; 				      (cons (string->symbol
;;; 					     (string-append
;;; 					      "$"
;;; 					      (number->string j)))
;;; 					    (loop-j (+ j 1)))))))
		      'sentinel))
		(loop1 rest
		       (+ i 1)
		       (cons (cons prod action)
			     rev-productions-and-actions))))))))))


;;; debugging tools: print parser in human-readable format

(define (debug:print-states port)
  (define (%display stuff)
    (display stuff port))
  (define (%newline)
    (newline port))

  (define (print-item item-no)
    (let loop ((i item-no))
      (let ((v (vector-ref ritem i)))
	(if (>= v 0)
	    (loop (+ i 1))
	  (let* ((rlno    (- v))
		 (nt      (vector-ref rlhs rlno)))
	    (%display (vector-ref the-nonterminals nt)) (%display " --> ")
	    (let loop ((i (vector-ref rrhs rlno)))
	      (let ((v (vector-ref ritem i)))
		(if (= i item-no)
		    (%display ". "))
		(if (>= v 0)
		    (begin
		      (%display (get-symbol v))
		      (%display " ")
		      (loop (+ i 1)))
		  (begin
		    (%display "   (rule ")
		    (%display (- v))
		    (%display ")")
		    (%newline))))))))))

  (define (print-action act)
    (cond ((eq? act '*error*)
	   (%display " : Error"))
	  ((eq? act 'accept)
	   (%display " : Accept input"))
	  ((< act 0)
	   (%display " : reduce using rule ")
	   (%display (- act)))
	  (else
	   (%display " : shift and goto state ")
	   (%display act)))
    (%newline))

  (define (print-actions acts)
    (let loop ((l acts))
      (if (null? l)
	  #t
	(let ((sym (caar l))
	      (act (cdar l)))
	  (%display "   ")
	  (cond
	   ((eq? sym 'default)
	    (%display "default action"))
	   (else
	    (if (number? sym)
		(%display (get-symbol (+ sym nvars)))
	      (%display sym))))
	  (print-action act)
	  (loop (cdr l))))))

  (if action-table
      (begin
	(%display "State table") (%newline)
	(%display "-----------") (%newline) (%newline)
	(let loop ((l first-state))
	  (or (null? l)
	      (let* ((core	(car l))
		     (i		(core-number core))
		     (items	(core-items core))
		     (actions	(vector-ref action-table i)))
		(%display "state ")
		(%display i)
		(%newline)
		(%newline)
		(for-each (lambda (x)
			    (%display "   ")
			    (print-item x))
		  items)
		(%newline)
		(print-actions actions)
		(%newline)
		(loop (cdr l))))))
    (begin
      (%display "No generated parser available!")
      (%newline))))



(define build-goto-table
  (lambda ()
    `(vector
      ,@(map
	    (lambda (shifts)
	      (list 'quote
		    (if shifts
			(let loop ((l (shift-shifts shifts)))
			  (if (null? l)
			      '()
			    (let* ((state  (car l))
				   (symbol (vector-ref acces-symbol state)))
			      (if (< symbol nvars)
				  (cons `(,symbol . ,state)
					(loop (cdr l)))
				(loop (cdr l))))))
		      '())))
	  (vector->list shift-table)))))

(define build-reduction-table
  (lambda (gram/actions)
    `(vector
      '()
      ,@(map
	    (lambda (p)
	      (let ((act (cdr p)))
		`(lambda
;;;NOTE
;;;(Marco Maggi; Tue Aug 4, 2009)
;;;
;;;It seems that "___goto-table" and  "yypushback" are never used in the
;;;reduction table closures by the generated code.  I do not see why the
;;;client code  should access the goto  table, so I removed  it from the
;;;formals.  There may be some  reason to pushback the current token, so
;;;I left it.
;;;
;;;Original code:
;;;
;;; 		     ,(if (eq? driver-name 'lr-driver)
;;; 			  '(___stack ___sp ___goto-table ___reduce-pop-and-push yypushback)
;;; 			'(___sp ___goto-table ___reduce-pop-and-push))
;;;
;;;Modified code:
;;;
 		     ,(if (eq? driver-name 'lr-driver)
 			  '(___stack ___sp ___reduce-pop-and-push yypushback yycustom)
 			'(___sp ___reduce-pop-and-push yycustom))

		   ,(let* ((nt (caar p)) (rhs (cdar p)) (n (length rhs)))
		      `(let* (,@(if act
				    (let loop ((i 1) (l rhs))
				      (if (pair? l)
					  (let ((rest (cdr l)))
					    (cons
					     `(,(string->symbol
						 (string-append
						  "$"
						  (number->string
						   (+ (- n i) 1))))
					       ,(if (eq? driver-name 'lr-driver)
						    `(vector-ref ___stack (- ___sp ,(- (* i 2) 1)))
						  `(list-ref ___sp ,(+ (* (- i 1) 2) 1))))
					     (loop (+ i 1) rest)))
					'()))
				  '()))
			 ,(if (= nt 0)
			      '$1
			    `(___reduce-pop-and-push ,n ,nt ,(cdr p)
						     ,@(if (eq? driver-name 'lr-driver)
							   '()
							 '(___sp)))))))))
	  gram/actions))))


;;;; done

(main)))

;;; end of file
