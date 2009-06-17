;;; low level strings library --
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Derived from the SRFI 13 reference implementation.
;;;
;;;Olin Shivers 7/2000
;;;
;;;Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;;Copyright (c) 1998, 1999, 2000 Olin Shivers.  All rights reserved.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.
;;;
;;;Copyright details
;;;=================
;;;
;;;The prefix/suffix and comparison routines in this code had (extremely
;;;distant) origins  in MIT Scheme's  string lib, and  was substantially
;;;reworked by  Olin Shivers (shivers@ai.mit.edu)  9/98. As such,  it is
;;;covered by MIT Scheme's open source copyright. See below for details.
;;;
;;;The KMP string-search code  was influenced by implementations written
;;;by Stephen  Bevan, Brian Dehneyer and Will  Fitzgerald. However, this
;;;version was written from scratch by myself.
;;;
;;;The remainder  of this  code was written  from scratch by  myself for
;;;scsh.  The scsh  copyright is a BSD-style open  source copyright. See
;;;below for details.
;;;
;;;-- Olin Shivers
;;;
;;;MIT Scheme copyright terms
;;;==========================
;;;
;;;This   material  was  developed   by  the   Scheme  project   at  the
;;;Massachusetts  Institute  of  Technology,  Department  of  Electrical
;;;Engineering and Computer Science.  Permission to copy and modify this
;;;software, to redistribute either  the original software or a modified
;;;version, and to use this software for any purpose is granted, subject
;;;to the following restrictions and understandings.
;;;
;;;1. Any copy made of  this software must include this copyright notice
;;;   in full.
;;;
;;;2. Users  of this software  agree to make  their best efforts  (a) to
;;;   return to  the MIT Scheme  project any improvements  or extensions
;;;   that they make, so that  these may be included in future releases;
;;;   and (b) to inform MIT of noteworthy uses of this software.
;;;
;;;3.  All materials  developed  as a  consequence  of the  use of  this
;;;   software shall  duly acknowledge such use, in  accordance with the
;;;   usual standards of acknowledging credit in academic research.
;;;
;;;4. MIT has made no  warrantee or representation that the operation of
;;;   this software will  be error-free, and MIT is  under no obligation
;;;   to  provide  any  services,  by  way of  maintenance,  update,  or
;;;   otherwise.
;;;
;;;5. In  conjunction  with  products  arising  from  the  use  of  this
;;;   material, there shall  be no use of the  name of the Massachusetts
;;;   Institute  of Technology  nor  of any  adaptation  thereof in  any
;;;   advertising,  promotional,  or   sales  literature  without  prior
;;;   written consent from MIT in each case.
;;;
;;;Scsh copyright terms
;;;====================
;;;
;;;All rights reserved.
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1.  Redistributions of source  code must  retain the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary  form must reproduce the above copyright
;;;   notice, this  list of conditions  and the following  disclaimer in
;;;   the  documentation  and/or   other  materials  provided  with  the
;;;   distribution.
;;;
;;;3. The  name of  the authors may  not be  used to endorse  or promote
;;;   products derived from this software without specific prior written
;;;   permission.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;;IMPLIED  WARRANTIES,  INCLUDING,  BUT  NOT LIMITED  TO,  THE  IMPLIED
;;;WARRANTIES OF  MERCHANTABILITY AND  FITNESS FOR A  PARTICULAR PURPOSE
;;;ARE  DISCLAIMED.  IN NO  EVENT SHALL  THE AUTHORS  BE LIABLE  FOR ANY
;;;DIRECT,  INDIRECT, INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL
;;;DAMAGES  (INCLUDING, BUT  NOT LIMITED  TO, PROCUREMENT  OF SUBSTITUTE
;;;GOODS  OR  SERVICES; LOSS  OF  USE,  DATA,  OR PROFITS;  OR  BUSINESS
;;;INTERRUPTION) HOWEVER CAUSED AND  ON ANY THEORY OF LIABILITY, WHETHER
;;;IN  CONTRACT,  STRICT LIABILITY,  OR  TORT  (INCLUDING NEGLIGENCE  OR
;;;OTHERWISE) ARISING IN  ANY WAY OUT OF THE USE  OF THIS SOFTWARE, EVEN
;;;IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;Other copyright terms
;;;=====================
;;;
;;;Copyright (c) 2008 Derick Eddington.  Ported to R6RS.


#!r6rs
(library (strings strings-low)
  (export

    ;; constructors
    string-concatenate  %string-concatenate-reverse  string-tabulate

    ;; predicates
    string-null?  %string-every  %string-any

    ;; comparison
    %string-compare  %string-compare-ci
    %string=  %string<>  %string-ci=  %string-ci<>
    %string<  %string<=  %string-ci<  %string-ci<=
    %string>  %string>=  %string-ci>  %string-ci>=

    ;; mapping
    %string-map        %string-map!
    %string-for-each*  %string-for-each-index

    ;; case hacking
    %string-titlecase*!

    ;; folding and unfolding
    string-fold    string-fold-right
    %string-fold*  %string-fold-right*
    string-unfold  string-unfold-right

    ;; selecting
    (rename (substring %string-copy*)) %string-reverse-copy*
    %string-copy*!  %string-reverse-copy*!
    %string-take    %string-take-right
    %string-drop    %string-drop-right

    ;; padding and trimming
    %string-trim    %string-trim-right  %string-trim-both
    %string-pad     %string-pad-right

    ;; prefix and suffix
    %string-prefix-length  %string-prefix-length-ci
    %string-suffix-length  %string-suffix-length-ci
    %string-prefix?        %string-prefix-ci?
    %string-suffix?        %string-suffix-ci?

    ;; searching
    %string-index     %string-index-right
    %string-skip      %string-skip-right
    %string-contains  %string-contains-ci
    %string-count

    ;; filtering
    %string-delete  %string-filter

    ;; lists
    %string->list*    reverse-list->string
    %string-tokenize  %string-join
    (rename (%string-tokenize %string-tokenise))

    ;; replicating
    %xsubstring  %string-xcopy!

    ;; mutating
    %string-fill*!  string-swap!

    ;; reverse and replace
    %string-reverse  %string-reverse!
    %string-replace

    ;; Knuth-Morris-Pratt search
    %kmp-search %kmp-make-restart-vector %kmp-step %kmp-string-partial-search)
  (import (rnrs)
    (rnrs mutable-strings)
    (only (rnrs r5rs) modulo quotient)
    (char-sets))


;;;; helpers

(define (strings-list-min-length strings)
  (apply min (map string-length strings)))


;;;; constructors

(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
		     (i 0 (+ i (string-length (car strings)))))
		    ((not (pair? strings)) i)))
	 (result (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
	  (let* ((s (car strings))
		 (slen (string-length s)))
	    (%string-copy*! result i s 0 slen)
	    (lp (+ i slen) (cdr strings)))))
    result))

(define (%string-concatenate-reverse string-list final past)
  (let* ((len (let loop ((sum 0) (lis string-list))
		(if (pair? lis)
		    (loop (+ sum (string-length (car lis))) (cdr lis))
		  sum)))
	 (result (make-string (+ past len))))
    (%string-copy*! result len final 0 past)
    (let loop ((i len) (lis string-list))
      (if (pair? lis)
	  (let* ((s   (car lis))
		 (lis (cdr lis))
		 (slen (string-length s))
		 (i (- i slen)))
	    (%string-copy*! result i s 0 slen)
	    (loop i lis))))
    result))

(define (string-tabulate proc len)
  (let ((s (make-string len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0))
      (string-set! s i (proc i)))
    s))


;;;; predicates

(define (string-null? str)
  (zero? (string-length str)))

(define (%string-every criterion str start past)
  (and (< start past)
       (cond ((char? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (char=? criterion (string-ref str i))
			 (loop (+ 1 i))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (char-set-contains? criterion (string-ref str i))
			 (loop (+ 1 i))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i start))  ; final (PRED S[PAST-1]) call
		(let ((c (string-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (and (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%string-every
		"expected char-set, char, or predicate as second parameter"
		criterion)))))

(define (%string-any criterion str start past)
  (and (< start past)
       (cond ((char? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (char=? criterion (string-ref str i))
			 (loop (+ i 1))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (char-set-contains? criterion (string-ref str i))
			 (loop (+ i 1))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i start))  ; final (PRED S[PAST-1]) call
		(let ((c (string-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (or (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%string-any
		"expected char-set, char, or predicate as second parameter"
		criterion)))))


;;;; comparison

(define (%true-string-compare string-prefix-length-proc char-less-proc
			      str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (let ((size1 (- past1 start1))
	(size2 (- past2 start2)))
    (let ((match (string-prefix-length-proc str1 start1 past1 str2 start2 past2)))
      (if (= match size1)
	  ((if (= match size2) proc= proc<) past1)
	((if (= match size2)
	     proc>
	   (if (char-less-proc (string-ref str1 (+ start1 match))
			       (string-ref str2 (+ start2 match)))
	       proc< proc>))
	 (+ match start1))))))

(define (%string-compare str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (%true-string-compare %string-prefix-length char<?
			str1 start1 past1 str2 start2 past2 proc< proc= proc>))

(define (%string-compare-ci str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (%true-string-compare %string-prefix-length-ci char-ci<?
			str1 start1 past1 str2 start2 past2 proc< proc= proc>))

;;; --------------------------------------------------------------------

(define (%true-string= string-compare-proc str1 start1 past1 str2 start2 past2)
  (and (= (- past1 start1) (- past2 start2))       ; Quick filter
       (or (and (eq? str1 str2) (= start1 start2)) ; Fast path
	   (string-compare-proc str1 start1 past1 str2 start2 past2 ; Real test
				(lambda (i) #f) values (lambda (i) #f)))))

(define (%string= str1 start1 past1 str2 start2 past2)
  (%true-string= %string-compare str1 start1 past1 str2 start2 past2))

(define (%string-ci= str1 start1 past1 str2 start2 past2)
  (%true-string= %string-compare-ci str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-string<> string-compare-proc str1 start1 past1 str2 start2 past2)
  (or (not (= (- past1 start1) (- past2 start2)))	     ; Fast path
      (and (not (and (eq? str1 str2) (= start1 start2))) ; Quick filter
	   (string-compare-proc str1 start1 past1 str2 start2 past2	; Real test
				values (lambda (i) #f) values))))

(define (%string<> str1 start1 past1 str2 start2 past2)
  (%true-string<> %string-compare str1 start1 past1 str2 start2 past2))

(define (%string-ci<> str1 start1 past1 str2 start2 past2)
  (%true-string<> %string-compare-ci str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-string< string-prefix-proc char-pred
		       str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (< past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  values (lambda (i) #f) (lambda (i) #f))))

(define (%string< str1 start1 past1 str2 start2 past2)
  (%true-string< %string-prefix-length char<?
		 str1 start1 past1 str2 start2 past2))

(define (%string-ci< str1 start1 past1 str2 start2 past2)
  (%true-string< %string-prefix-length-ci char-ci<?
		 str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-string<= string-prefix-proc char-pred
			str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (<= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  values values (lambda (i) #f))))

(define (%string<= str1 start1 past1 str2 start2 past2)
  (%true-string<= %string-prefix-length char<=?
		  str1 start1 past1 str2 start2 past2))

(define (%string-ci<= str1 start1 past1 str2 start2 past2)
  (%true-string<= %string-prefix-length-ci char-ci<=?
		  str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-string> string-prefix-proc char-pred str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (> past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  (lambda (i) #f) (lambda (i) #f) values)))

(define (%string> str1 start1 past1 str2 start2 past2)
  (%true-string> %string-prefix-length char<?
		 str1 start1 past1 str2 start2 past2))

(define (%string-ci> str1 start1 past1 str2 start2 past2)
  (%true-string> %string-prefix-length-ci char-ci<?
		 str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-string>= string-prefix-proc char-pred str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (>= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-string-compare string-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  (lambda (i) #f) values values)))

(define (%string>= str1 start1 past1 str2 start2 past2)
  (%true-string>= %string-prefix-length char<=?
		 str1 start1 past1 str2 start2 past2))

(define (%string-ci>= str1 start1 past1 str2 start2 past2)
  (%true-string>= %string-prefix-length-ci char-ci<=?
		 str1 start1 past1 str2 start2 past2))


;;;; mapping

(define (%string-map proc str start past)
  (do ((i start (+ 1 i))
       (j 0 (+ 1 j))
       (result (make-string (- past start))))
      ((>= i past)
       result)
    (string-set! result j (proc (string-ref str i)))))

(define (%string-map! proc str start past)
  (do ((i start (+ 1 i)))
      ((>= i past)
       str)
    (string-set! str i (proc (string-ref str i)))))

(define (%string-for-each* proc str start past)
  (let loop ((i start))
    (when (< i past)
      (proc (string-ref str i))
      (loop (+ i 1)))))

(define (%string-for-each-index proc str start past)
  (let loop ((i start))
    (when (< i past)
      (proc i)
      (loop (+ i 1)))))


;;;; case hacking

(define (char-cased? c)
  ;; This works  because CHAR-UPCASE returns #f if  the character has no
  ;; upcase version.
  (char-upper-case? (char-upcase c)))

(define (%string-titlecase*! str start past)
  (let loop ((i start))
    (cond ((%string-index char-cased? str i past)
	   => (lambda (i)
		(string-set! str i (char-titlecase (string-ref str i)))
		(let ((i1 (+ i 1)))
		  (cond ((%string-skip char-cased? str i1 past)
			 => (lambda (j)
			      (%string-map! char-downcase str i1 j)
			      (loop (+ j 1))))
			(else
			 (%string-map! char-downcase str i1 past)))))))))


;;;; folding

(define (string-fold kons knil vec0 . strings)
  (let* ((strings  (cons vec0 strings))
	 (len      (strings-list-min-length strings)))
    (let loop ((i     0)
	       (knil  knil))
      (if (= len i)
	  knil
	(loop (+ 1 i) (apply kons i knil
			     (map (lambda (vec)
				    (string-ref vec i))
			       strings)))))))

(define (string-fold-right kons knil vec0 . strings)
  (let* ((strings  (cons vec0 strings))
	 (len      (strings-list-min-length strings)))
    (let loop ((i     (- len 1))
	       (knil  knil))
      (if (< i 0)
	  knil
	(loop (- i 1) (apply kons i knil
			     (map (lambda (vec)
				    (string-ref vec i))
			       strings)))))))

(define (%string-fold* kons knil str start past)
  (let loop ((v knil)
	     (i start))
    (if (< i past)
	(loop (kons (string-ref str i) v) (+ i 1))
      v)))

(define (%string-fold-right* kons knil str start past)
  (let loop ((v knil)
	     (i (- past 1)))
    (if (>= i start)
	(loop (kons (string-ref str i) v) (- i 1))
      v)))

(define string-unfold
  (case-lambda
   ((p f g seed)
    (string-unfold p f g seed "" (lambda (x) "")))
   ((p f g seed base)
    (string-unfold p f g seed base (lambda (x) "")))
   ((p f g seed base make-final)
    ;;The strategy is  to allocate a series of chunks  into which we stash
    ;;the chars as  we generate them. Chunk size goes up  in powers of two
    ;;beging with 40 and levelling out at 4k, i.e.
    ;;
    ;;	40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
    ;;
    ;;This should  work pretty  well for short  strings, 1-line  (80 char)
    ;;strings, and  longer ones. When  done, we allocate an  answer string
    ;;and copy the chars over from the chunk buffers.
    (let lp ((chunks '())	      ; Previously filled chunks
	     (nchars 0)		      ; Number of chars in CHUNKS
	     (chunk (make-string 40)) ; Current chunk into which we write
	     (chunk-len 40)
	     (i 0) ; Number of chars written into CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed))
	(if (not (p seed))
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (< i chunk-len)
		  (begin (string-set! chunk i c)
			 (lp2 (+ i 1) seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-string chunk-len2)))
		  (string-set! new-chunk 0 c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 1 seed))))

	  ;; We're done. Make the answer string & install the bits.
	  (let* ((final (make-final seed))
		 (flen (string-length final))
		 (base-len (string-length base))
		 (j (+ base-len nchars i))
		 (ans (make-string (+ j flen))))
	    (%string-copy*! ans j final 0 flen) ; Install FINAL.
	    (let ((j (- j i)))
	      (%string-copy*! ans j chunk 0 i) ; Install CHUNK[0,I).
	      (let lp ((j j) (chunks chunks)) ; Install CHUNKS.
		(if (pair? chunks)
		    (let* ((chunk  (car chunks))
			   (chunks (cdr chunks))
			   (chunk-len (string-length chunk))
			   (j (- j chunk-len)))
		      (%string-copy*! ans j chunk 0 chunk-len)
		      (lp j chunks)))))
	    (%string-copy*! ans 0 base 0 base-len) ; Install BASE.
	    ans)))))))

(define string-unfold-right
  (case-lambda
   ((p f g seed)
    (string-unfold-right p f g seed "" (lambda (x) "")))
   ((p f g seed base)
    (string-unfold-right p f g seed base (lambda (x) "")))
   ((p f g seed base make-final)
    (let lp ((chunks '())	      ; Previously filled chunks
	     (nchars 0)		      ; Number of chars in CHUNKS
	     (chunk (make-string 40)) ; Current chunk into which we write
	     (chunk-len 40)
	     (i 40) ; Number of chars available in CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed)) ; Fill up CHUNK from right
	(if (not (p seed))	   ; to left.
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (> i 0)
		  (let ((i (- i 1)))
		    (string-set! chunk i c)
		    (lp2 i seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-string chunk-len2))
		       (i (- chunk-len2 1)))
		  (string-set! new-chunk i c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 i seed))))

	  ;; We're done. Make the answer string & install the bits.
	  (let* ((final (make-final seed))
		 (flen (string-length final))
		 (base-len (string-length base))
		 (chunk-used (- chunk-len i))
		 (j (+ base-len nchars chunk-used))
		 (ans (make-string (+ j flen))))
	    (%string-copy*! ans 0 final 0 flen)	       ; Install FINAL.
	    (%string-copy*! ans flen chunk i chunk-len) ; Install CHUNK[I,).
	    (let lp ((j (+ flen chunk-used))	       ; Install CHUNKS.
		     (chunks chunks))
	      (if (pair? chunks)
		  (let* ((chunk  (car chunks))
			 (chunks (cdr chunks))
			 (chunk-len (string-length chunk)))
		    (%string-copy*! ans j chunk 0 chunk-len)
		    (lp (+ j chunk-len) chunks))
		(%string-copy*! ans j base 0 base-len)))	; Install BASE.
	    ans)))))))


;;;; selecting

(define (%string-reverse-copy* str start past)
  (let ((result (make-string (- past start))))
    (do ((i (- past 1) (- i 1))
	 (j 0 (+ j 1)))
	((< i start)
	 result)
      (string-set! result j (string-ref str i)))))

(define (%string-take nchars str start past)
  (if (<= nchars (- past start))
      (substring str start (+ start nchars))
    (assertion-violation '%string-take
      "requested number of chars greater than length of substring" nchars)))

(define (%string-take-right nchars str start past)
  (if (<= nchars (- past start))
      (substring str (- past nchars) past)
    (assertion-violation '%string-take-right
      "requested number of chars greater than length of substring" nchars)))

(define (%string-drop nchars str start past)
  (if (<= nchars (- past start))
      (substring str nchars past)
    (assertion-violation '%string-take
      "requested number of chars greater than length of substring" nchars)))

(define (%string-drop-right nchars str start past)
  (if (<= nchars (- past start))
      (substring str start (+ start nchars))
    (assertion-violation '%string-take
      "requested number of chars greater than length of substring" nchars)))

(define (%string-trim criterion str start past)
  (cond ((%string-skip criterion str start past)
	 => (lambda (i) (substring str i past)))
	(else "")))

(define (%string-trim-right criterion str start past)
  (cond ((%string-skip-right criterion str start past)
	 => (lambda (i) (substring str start (+ 1 i))))
	(else "")))

(define (%string-trim-both criterion str start past)
  (let ((str (%string-trim-right criterion str start past)))
    (%string-trim criterion str start (string-length str))))

(define (%string-pad requested-len fill-char str start past)
  (let ((len (- past start)))
    (if (<= requested-len len)
	(substring str (- past requested-len) past)
      (let ((result (make-string requested-len fill-char)))
	(%string-copy*! result (- requested-len len) str start past)
	result))))

(define (%string-pad-right requested-len fill-char str start past)
  (let ((len (- past start)))
    (if (<= requested-len len)
	(substring str start (+ start requested-len))
      (let ((result (make-string requested-len fill-char)))
	(%string-copy*! result 0 str start past)
	result))))


;;;; prefix and suffix

(define (%true-string-prefix-length char-cmp? str1 start1 past1 str2 start2 past2)
  ;;Find the length  of the common prefix.  It is  not required that the
  ;;two substrings passed be of equal length.
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (past1 (+ start1 delta)))
    (if (and (eq? str1 str2) (= start1 start2)) ; EQ fast path
	delta
      (let lp ((i start1) (j start2)) ; Regular path
	(if (or (>= i past1)
		(not (char-cmp? (string-ref str1 i)
				(string-ref str2 j))))
	    (- i start1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%string-prefix-length str1 start1 past1 str2 start2 past2)
  (%true-string-prefix-length char=? str1 start1 past1 str2 start2 past2))

(define (%string-prefix-length-ci str1 start1 past1 str2 start2 past2)
  (%true-string-prefix-length char-ci=? str1 start1 past1 str2 start2 past2))

(define (%string-prefix? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%string-prefix-length str1 start1 past1
					str2 start2 past2)))))

(define (%string-prefix-ci? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%string-prefix-length-ci str1 start1 past1
					   str2 start2 past2)))))

;;; --------------------------------------------------------------------

(define (%true-string-suffix-length char-cmp? str1 start1 past1 str2 start2 past2)
  ;;Find the length  of the common suffix.  It is  not required that the
  ;;two substrings passed be of equal length.
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (start1 (- past1 delta)))
    (if (and (eq? str1 str2) (= past1 past2)) ; EQ fast path
	delta
      (let lp ((i (- past1 1)) (j (- past2 1))) ; Regular path
	(if (or (< i start1)
		(not (char-cmp? (string-ref str1 i)
				(string-ref str2 j))))
	    (- (- past1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%string-suffix-length str1 start1 past1 str2 start2 past2)
  (%true-string-suffix-length char=? str1 start1 past1 str2 start2 past2))

(define (%string-suffix-length-ci str1 start1 past1 str2 start2 past2)
  (%true-string-suffix-length char-ci=? str1 start1 past1 str2 start2 past2))

(define (%string-suffix? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%string-suffix-length str1 start1 past1
					str2 start2 past2)))))

(define (%string-suffix-ci? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%string-suffix-length-ci str1 start1 past1
					   str2 start2 past2)))))


;;;; searching

(define (%string-index criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char=? criterion (string-ref str i)) i
		  (loop (+ i 1))))))
	((char-set? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char-set-contains? criterion (string-ref str i)) i
		  (loop (+ i 1))))))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (string-ref str i)) i
		  (loop (+ i 1))))))
	(else (assertion-violation '%string-index
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%string-index-right criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char=? criterion (string-ref str i)) i
		  (loop (- i 1))))))
	((char-set? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char-set-contains? criterion (string-ref str i)) i
		  (loop (- i 1))))))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (string-ref str i)) i
		  (loop (- i 1))))))
	(else (assertion-violation '%string-index-right
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%string-skip criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char=? criterion (string-ref str i))
		    (loop (+ i 1))
		  i))))
	((char-set? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char-set-contains? criterion (string-ref str i))
		    (loop (+ i 1))
		  i))))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (string-ref str i)) (loop (+ i 1))
		  i))))
	(else (assertion-violation '%string-skip
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%string-skip-right criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char=? criterion (string-ref str i))
		    (loop (- i 1))
		  i))))
	((char-set? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char-set-contains? criterion (string-ref str i))
		    (loop (- i 1))
		  i))))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (string-ref str i)) (loop (- i 1))
		  i))))
	(else (assertion-violation '%string-skip-right
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%string-count criterion str start past)
  (cond ((char? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (char=? criterion (string-ref str i))
			   (+ count 1)
			 count)))
	     ((>= i past) count)))
	((char-set? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (char-set-contains? criterion (string-ref str i))
			   (+ count 1)
			 count)))
	     ((>= i past) count)))
	((procedure? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (criterion (string-ref str i)) (+ count 1) count)))
	     ((>= i past) count)))
	(else (assertion-violation '%string-count
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%string-contains text text-start text-past pattern pattern-start pattern-past)
  (%kmp-search char=? text text-start text-past pattern pattern-start pattern-past))

(define (%string-contains-ci text text-start text-past pattern pattern-start pattern-past)
  (%kmp-search char-ci=? text text-start text-past pattern pattern-start pattern-past))


;;;; filtering

(define (%string-delete criterion str start past)
  (if (procedure? criterion)
      (let* ((slen (- past start))
	     (temp (make-string slen))
	     (ans-len (%string-fold* (lambda (c i)
				      (if (criterion c) i
					(begin (string-set! temp i c)
					       (+ i 1))))
				    0 str start past)))
	(if (= ans-len slen) temp (substring temp 0 ans-len)))

    (let* ((cset (cond ((char-set? criterion) criterion)
		       ((char? criterion) (char-set criterion))
		       (else
			(assertion-violation '%string-delete
			  "expected predicate, char or char-set as criterion"
			  criterion))))
	   (len (%string-fold* (lambda (c i) (if (char-set-contains? cset c)
						i
					      (+ i 1)))
			      0 str start past))
	   (ans (make-string len)))
      (%string-fold* (lambda (c i) (if (char-set-contains? cset c)
				     i
				   (begin (string-set! ans i c)
					  (+ i 1))))
		   0 str start past)
      ans)))

(define (%string-filter criterion str start past)
  (if (procedure? criterion)
      (let* ((slen (- past start))
	     (temp (make-string slen))
	     (ans-len (%string-fold* (lambda (c i)
				     (if (criterion c)
					 (begin (string-set! temp i c)
						(+ i 1))
				       i))
				   0 str start past)))
	(if (= ans-len slen) temp (substring temp 0 ans-len)))

    (let* ((cset (cond ((char-set? criterion) criterion)
		       ((char? criterion) (char-set criterion))
		       (else
			(assertion-violation '%string-filter
			  "expected predicate, char or char-set as criterion"
			  criterion))))
	   (len (%string-fold* (lambda (c i) (if (char-set-contains? cset c)
					       (+ i 1)
					     i))
			     0 str start past))
	   (ans (make-string len)))
      (%string-fold* (lambda (c i) (if (char-set-contains? cset c)
				     (begin (string-set! ans i c)
					    (+ i 1))
				   i))
		   0 str start past)
      ans)))


;;;; strings and lists

(define (reverse-list->string clist)
  (let* ((len (length clist))
	 (s (make-string len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
	((not (pair? clist)))
      (string-set! s i (car clist)))
    s))

(define (%string->list* str start past)
  (do ((i (- past 1) (- i 1))
       (result '() (cons (string-ref str i) result)))
      ((< i start) result)))

(define (%string-join strings delim grammar)
  (define (join-with-delim ell final)
    (let loop ((ell ell))
      (if (pair? ell)
	  (cons delim
		(cons (car ell)
		      (loop (cdr ell))))
	final)))
  (cond ((pair? strings)
	 (string-concatenate
	  (case grammar
	    ((infix strict-infix)
	     (cons (car strings)
		   (join-with-delim (cdr strings) '())))
	    ((prefix)
	     (join-with-delim strings '()))
	    ((suffix)
	     (cons (car strings)
		   (join-with-delim (cdr strings) (list delim))))
	    (else
	     (assertion-violation '%string-join
	       "illegal join grammar" grammar)))))

	((not (null? strings))
	 (assertion-violation '%string-join
	   "STRINGS parameter is not a list" strings))

	;; here we know that STRINGS is the empty string
	((eq? grammar 'strict-infix)
	 (assertion-violation '%string-join
	   "empty list cannot be joined with STRICT-INFIX grammar."))

	(else ""))) ; Special-cased for infix grammar.

(define (%string-tokenize token-set str start past)
  (let loop ((i		past)
	     (result	'()))
    (cond ((and (< start i) (%string-index-right token-set str start i))
	   => (lambda (tpast-1)
		(let ((tpast (+ 1 tpast-1)))
		  (cond ((%string-skip-right token-set str start tpast-1)
			 => (lambda (tstart-1)
			      (loop tstart-1
				    (cons (substring str (+ 1 tstart-1) tpast)
					  result))))
			(else (cons (substring str start tpast) result))))))
	  (else result))))


;;;; extended substring

(define (%xsubstring from to str start past)
  (let ((str-len	(- past start))
	(result-len	(- to from)))
    (cond ((zero? result-len) "")
	  ((zero? str-len)
	   (assertion-violation '%xsubstring "cannot replicate empty (sub)string"))
	  ((= 1 str-len)
	   (make-string result-len (string-ref str start)))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from str-len)) (floor (/ to str-len)))
	   (substring str
		      (+ start (modulo from str-len))
		      (+ start (modulo to   str-len))))

	  ;; Selected text requires multiple spans.
	  (else
	   (let ((result (make-string result-len)))
	     (%multispan-repcopy! from to result 0 str start past)
	     result)))))

(define (%string-xcopy! from to
			dst-str dst-start dst-past
			src-str src-start src-past)
  (let* ((tocopy	(- to from))
	 (tend		(+ dst-start tocopy))
	 (str-len	(- src-past src-start)))
    (cond ((zero? tocopy))
	  ((zero? str-len)
	   (assertion-violation '%string-xcopy! "cannot replicate empty (sub)string"))

	  ((= 1 str-len)
	   (%string-fill*! dst-str (string-ref src-str src-start) dst-start dst-past))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from str-len)) (floor (/ to str-len)))
	   (%string-copy*! dst-str dst-start src-str
			  (+ src-start (modulo from str-len))
			  (+ src-start (modulo to   str-len))))

	  (else
	   (%multispan-repcopy! from to dst-str dst-start src-str src-start src-past)))))

(define (%multispan-repcopy! from to dst-str dst-start src-str src-start src-past)
  ;;This  is the  core  copying loop  for  XSUBSTRING and  STRING-XCOPY!
  ;;Internal -- not exported, no careful arg checking.
  (let* ((str-len	(- src-past src-start))
	 (i0		(+ src-start (modulo from str-len)))
	 (total-chars	(- to from)))

    ;; Copy the partial span @ the beginning
    (%string-copy*! dst-str dst-start src-str i0 src-past)

    (let* ((ncopied (- src-past i0))	      ; We've copied this many.
	   (nleft (- total-chars ncopied))    ; # chars left to copy.
	   (nspans (quotient nleft str-len))) ; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ dst-start ncopied) (+ i str-len)) ; Current target index.
	   (nspans nspans (- nspans 1)))	   ; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (%string-copy*! dst-str i src-str src-start (+ src-start (- total-chars (- i dst-start)))))

	(%string-copy*! dst-str i src-str src-start src-past))))) ; Copy a whole span.


;;;; reverse, replace

(define (%string-reverse str start past)
  (let* ((len (- past start))
	 (result (make-string len)))
    (do ((i start (+ i 1))
	 (j (- len 1) (- j 1)))
	((< j 0))
      (string-set! result j (string-ref str i)))
    result))

(define (%string-replace str1 start1 past1 str2 start2 past2)
  (let* ((len1		(string-length str1))
	 (len2		(- past2 start2))
	 (result	(make-string (+ len2 (- len1 (- past1 start1))))))
    (%string-copy*! result 0 str1 0 start1)
    (%string-copy*! result start1 str2 start2 past2)
    (%string-copy*! result (+ start1 len2) str1 past1 len1)
    result))

(define (%string-reverse! str start past)
  (do ((i (- past 1) (- i 1))
       (j start (+ j 1)))
      ((<= i j))
    (let ((ci (string-ref str i)))
      (string-set! str i (string-ref str j))
      (string-set! str j ci))))


;;;; mutating

(define (%string-copy*! dst-str dst-start src-str src-start src-past)
  (when (< (- (string-length dst-str) dst-start)
	   (- src-past src-start))
    (assertion-violation '%string-copy*!
      "not enough room in destination string"))
  (if (> src-start dst-start)
      (do ((i src-start (+ i 1))
	   (j dst-start (+ j 1)))
	  ((>= i src-past))
	(string-set! dst-str j (string-ref src-str i)))
    (do ((i (- src-past 1)                    (- i 1))
	 (j (+ -1 dst-start (- src-past src-start)) (- j 1)))
	((< i src-start))
      (string-set! dst-str j (string-ref src-str i)))))

(define (%string-reverse-copy*! dst-str dst-start src-str src-start src-past)
  (when (< (- (string-length dst-str) dst-start)
	   (- src-past src-start))
    (assertion-violation '%string-reverse-copy*!
      "not enough room in destination string"))
  ;;We  must handle  correctly copying  over  the same  string.  If  the
  ;;source  and  destination strings  are  the  same,  we copy  all  the
  ;;elements  in a  temporary  buffer first;  this  should be  optimised
  ;;someway to reduce to the minimum the size of the buffer.
  (if (eq? src-str dst-str)
      (when (< src-start src-past)
	(let* ((buffer (%string-reverse-copy* src-str src-start src-past)))
	  (%string-copy*! dst-str dst-start buffer 0 (string-length buffer))))
    (do ((i (- src-past 1) (- i 1))
	 (j dst-start (+ j 1)))
	((< i src-start))
      (string-set! dst-str j (string-ref src-str i)))))

(define (%string-fill*! fill-char str start past)
  (do ((i (- past 1) (- i 1)))
      ((< i start))
    (string-set! str i fill-char)))

(define (string-swap! str i j)
  (when (= 0 (string-length str))
    (assertion-violation 'string-swap!
      "attempt to swap elements in an empty string"))
  (when (not (= i j))
    (let ((x (string-ref str i)))
      (string-set! str i (string-ref str j))
      (string-set! str j x))))


;; Knuth-Morris-Pratt string searching. See:
;;
;;  "Fast pattern matching in strings"
;;  SIAM J. Computing 6(2):323-350 1977
;;  D. E. Knuth, J. H. Morris and V. R. Pratt
;;
;; also described in:
;;
;;  "Pattern matching in strings"
;;  Alfred V. Aho
;;  Formal Language Theory - Perspectives and Open Problems
;;  Ronald V. Brook (editor)
;;
;; and of course:
;;
;;  <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>
;;

(define (%kmp-search char= text text-start text-past pattern pattern-start pattern-past)
  (let ((plen (- pattern-past pattern-start))
	(restart-vector (%kmp-make-restart-vector char= pattern pattern-start pattern-past)))
    ;; The search loop. TJ & PJ are redundant state.
    (let loop ((ti text-start) (pi 0)
	       (tj (- text-past text-start)) ; (- tlen ti) -- how many chars left.
	       (pj plen)) ; (- plen pi) -- how many chars left.

      (if (= pi plen)
	  (- ti plen)			     ; Win.
	(and (<= pj tj)			     ; Lose.
	     (if (char= (string-ref text ti) ; Search.
			(string-ref pattern (+ pattern-start pi)))
		 (loop (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.

	       (let ((pi (vector-ref restart-vector pi))) ; Retreat.
		 (if (= pi -1)
		     (loop (+ ti 1) 0  (- tj 1) plen) ; Punt.
		   (loop ti       pi tj       (- plen pi))))))))))

(define (%kmp-make-restart-vector char= pattern pattern-start pattern-past)
  (let* ((rvlen (- pattern-past pattern-start))
	 (restart-vector (make-vector rvlen -1)))
    (when (> rvlen 0)
      (let ((rvlen-1 (- rvlen 1))
	    (c0 (string-ref pattern pattern-start)))
	;;Here's the main loop.  We have  set RV[0] ...  RV[i].  K = I
	;;+ START -- it is the corresponding index into PATTERN.
	(let loop1 ((i 0) (j -1) (k pattern-start))
	  (when (< i rvlen-1)
	    ;; loop2 invariant:
	    ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
	    ;;   or j = -1.
	    (let loop2 ((j j))
	      (cond ((= j -1)
		     (let ((i1 (+ 1 i)))
		       (when (not (char= (string-ref pattern (+ k 1)) c0))
			 (vector-set! restart-vector i1 0))
		       (loop1 i1 0 (+ k 1))))
		    ;; pat[(k-j) .. k] matches pat[start..start+j].
		    ((char= (string-ref pattern k) (string-ref pattern (+ j pattern-start)))
		     (let* ((i1 (+ 1 i))
			    (j1 (+ 1 j)))
		       (vector-set! restart-vector i1 j1)
		       (loop1 i1 j1 (+ k 1))))

		    (else (loop2 (vector-ref restart-vector j)))))))))
    restart-vector))

(define (%kmp-step char= restart-vector next-char-from-text
		   next-index-in-pattern pattern pattern-start)
  (let loop ((i next-index-in-pattern))
    (if (char= next-char-from-text (string-ref pattern (+ i pattern-start)))
	(+ i 1)				       ; Done.
      (let ((i (vector-ref restart-vector i))) ; Back up in PATTERN.
	(if (= i -1)
	    0		;;Can't back up  further, return the first index
			;;in pattern from the start.
	  (loop i))))))	;Keep trying for match.

(define (%kmp-string-partial-search char= restart-vector
				    next-index-in-pattern
				    text text-start text-end
				    pattern pattern-start)
  (let ((patlen (vector-length restart-vector)))
    (let loop ((ti text-start)
	       (pi next-index-in-pattern))
      (cond ((= pi patlen) (- ti)) ; found
	    ((= ti text-end) pi)   ; consumed all text
	    (else
	     (let ((c (string-ref text ti)))
	       (loop (+ ti 1)
		     ;;The  following  loop  is  an inlined  version  of
		     ;;%KMP-STEP.
		     (let loop2 ((pi pi))
		       (if (char= c (string-ref pattern (+ pi pattern-start)))
			   (+ pi 1)
			 (let ((pi (vector-ref restart-vector pi)))
			   (if (= pi -1) 0
			     (loop2 pi))))))))))))


;;;; done

)

;;; end of file
