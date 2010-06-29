;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level bytevector functions
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;;Copyright (c) 2009 Derick Eddington
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
(library (bytevectors u8low)
  (export

    ;; constructors
    bytevector-u8-concatenate  %bytevector-u8-concatenate-reverse  bytevector-u8-tabulate
    subbytevector-u8 bytevector-u8-append

    ;; predicates
    bytevector-u8-null?  %bytevector-u8-every  %bytevector-u8-any

    ;; lexicographic comparison
    %bytevector-u8-compare  %bytevector-u8-compare-ci
    %bytevector-u8=  %bytevector-u8<>  %bytevector-u8-ci=  %bytevector-u8-ci<>
    %bytevector-u8<  %bytevector-u8<=  %bytevector-u8-ci<  %bytevector-u8-ci<=
    %bytevector-u8>  %bytevector-u8>=  %bytevector-u8-ci>  %bytevector-u8-ci>=

    ;; dictionary comparison
    %bytevector-u8-dictionary-compare
    %bytevector-u8-dictionary=?
    %bytevector-u8-dictionary<>?
    %bytevector-u8-dictionary<?
    %bytevector-u8-dictionary<=?
    %bytevector-u8-dictionary>?
    %bytevector-u8-dictionary>=?

    %bytevector-u8-dictionary-compare-ci
    %bytevector-u8-dictionary-ci=?
    %bytevector-u8-dictionary-ci<>?
    %bytevector-u8-dictionary-ci<?
    %bytevector-u8-dictionary-ci<=?
    %bytevector-u8-dictionary-ci>?
    %bytevector-u8-dictionary-ci>=?

    ;; bytevector-u8/numbers lexicographic comparison
    %bytevector-u8/numbers-compare	%bytevector-u8/numbers-compare-ci
    %bytevector-u8/numbers=?		%bytevector-u8/numbers<>?
    %bytevector-u8/numbers-ci=?		%bytevector-u8/numbers-ci<>?
    %bytevector-u8/numbers<?		%bytevector-u8/numbers<=?
    %bytevector-u8/numbers-ci<?		%bytevector-u8/numbers-ci>?
    %bytevector-u8/numbers>?		%bytevector-u8/numbers>=?
    %bytevector-u8/numbers-ci<=?	%bytevector-u8/numbers-ci>=?

    ;; bytevector-u8/numbers dictionary comparison
    %bytevector-u8/numbers-dictionary-compare
    %bytevector-u8/numbers-dictionary=?	%bytevector-u8/numbers-dictionary<>?
    %bytevector-u8/numbers-dictionary<?	%bytevector-u8/numbers-dictionary<=?
    %bytevector-u8/numbers-dictionary>?	%bytevector-u8/numbers-dictionary>=?

    %bytevector-u8/numbers-dictionary-compare-ci
    %bytevector-u8/numbers-dictionary-ci=?	%bytevector-u8/numbers-dictionary-ci<>?
    %bytevector-u8/numbers-dictionary-ci<?	%bytevector-u8/numbers-dictionary-ci>?
    %bytevector-u8/numbers-dictionary-ci<=?	%bytevector-u8/numbers-dictionary-ci>=?

    ;; mapping
    bytevector-u8-map      bytevector-u8-map!
    bytevector-u8-map*     bytevector-u8-map*!     bytevector-u8-for-each*
    %subbytevector-u8-map  %subbytevector-u8-map!  %subbytevector-u8-for-each
    %subbytevector-u8-for-each-index

    ;; case hacking
    %bytevector-u8-titlecase*!

    ;; folding and unfolding
    bytevector-u8-fold-left		bytevector-u8-fold-right
    bytevector-u8-fold-left*		bytevector-u8-fold-right*
    %subbytevector-u8-fold-left	%subbytevector-u8-fold-right
    bytevector-u8-unfold		bytevector-u8-unfold-right

    ;; selecting
    (rename (subbytevector-u8 %bytevector-u8-copy*)) %bytevector-u8-reverse-copy*
    %bytevector-u8-copy*!  %bytevector-u8-reverse-copy*!
    %bytevector-u8-take    %bytevector-u8-take-right
    %bytevector-u8-drop    %bytevector-u8-drop-right

    ;; padding and trimming
    %bytevector-u8-trim    %bytevector-u8-trim-right  %bytevector-u8-trim-both
    %bytevector-u8-pad     %bytevector-u8-pad-right

    ;; prefix and suffix
    %bytevector-u8-prefix-length  %bytevector-u8-prefix-length-ci
    %bytevector-u8-suffix-length  %bytevector-u8-suffix-length-ci
    %bytevector-u8-prefix?        %bytevector-u8-prefix-ci?
    %bytevector-u8-suffix?        %bytevector-u8-suffix-ci?

    ;; searching
    %bytevector-u8-index     %bytevector-u8-index-right
    %bytevector-u8-skip      %bytevector-u8-skip-right
    %bytevector-u8-contains  %bytevector-u8-contains-ci
    %bytevector-u8-count

    ;; filtering
    %bytevector-u8-delete  %bytevector-u8-filter

    ;; lists
    %bytevector->u8-list*   %reverse-bytevector->u8-list
    reverse-u8-list->bytevector
    %bytevector-u8-tokenize  %bytevector-u8-join
    (rename (%bytevector-u8-tokenize %bytevector-u8-tokenise))

    ;; replicating
    %xsubbytevector-u8  %bytevector-u8-xcopy!

    ;; mutating
    %bytevector-u8-fill*!  bytevector-u8-swap!

    ;; reverse and replace
    %bytevector-u8-reverse  %bytevector-u8-reverse!
    %bytevector-u8-replace)
  (import (rnrs)
    (char-sets)
    (knuth-morris-pratt))


;;;; helpers

(define $white-spaces-for-dictionary-comparison
  (map char->integer '(#\space #\tab #\vtab #\linefeed #\return #\page)))

(define (bvs-list-min-length bytevector-u8s)
  (apply min (map bytevector-length bytevector-u8s)))

(define (%total-length-of-bytevectors-list bvs)
  (let loop ((bvs bvs) (len 0))
    (if (null? bvs)
	len
      (loop (cdr bvs) (+ len (bytevector-length (car bvs)))))))

(define (%integer-char-ci=? a b)
  (char-ci=? (integer->char a) (integer->char b)))

(define (%integer-char-ci<? a b)
  (char-ci<? (integer->char a) (integer->char b)))

(define (%integer-char-ci<=? a b)
  (char-ci<=? (integer->char a) (integer->char b)))

(define (%assert-bytevectors-of-same-length who bvs)
  (unless (apply =* (map bytevector-length bvs))
    (assertion-violation who "expected list of bytevectors of the same length")))

(define (=* . args)
  ;;This exists because some implementations (Mosh) do not allow = to be
  ;;called with less than 2 arguments.
  (if (null? args)
      #t
    (let loop ((val  (car args))
	       (args (cdr args)))
      (or (null? args)
	  (let ((new-val (car args)))
	    (and (= val new-val)
		 (loop new-val (cdr args))))))))



;;;; constructors

(define (bytevector-u8-concatenate bvs)
  (let ((result (make-bytevector (%total-length-of-bytevectors-list bvs))))
    (let loop ((i 0) (bvs bvs))
      (if (null? bvs)
	  result
	(let* ((s    (car bvs))
	       (slen (bytevector-length s)))
	  (%bytevector-u8-copy*! result i s 0 slen)
	  (loop (+ i slen) (cdr bvs)))))))

(define (bytevector-u8-append bv0 . bvs)
  (let-values (((port getter) (open-bytevector-output-port)))
    (put-bytevector port bv0)
    (for-each (lambda (bv)
		(put-bytevector port bv))
      bvs)
    (getter)))

(define (%bytevector-u8-concatenate-reverse bvs tail-bv past)
  (let* ((result.len	(%total-length-of-bytevectors-list bvs))
	 (result	(make-bytevector (+ past result.len))))
    (%bytevector-u8-copy*! result result.len tail-bv 0 past)
    (let loop ((i   result.len)
	       (bvs bvs))
      (if (null? bvs)
	  result
	(let* ((s    (car bvs))
	       (bvs  (cdr bvs))
	       (slen (bytevector-length s))
	       (i    (- i slen)))
	  (%bytevector-u8-copy*! result i s 0 slen)
	  (loop i bvs))))))

(define (bytevector-u8-tabulate proc len)
  (let ((s (make-bytevector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0)
	 s)
      (bytevector-u8-set! s i (proc i)))))


;;;; predicates

(define (bytevector-u8-null? bv)
  (zero? (bytevector-length bv)))

(define (%bytevector-u8-every criterion bv start past)
  (and (< start past)
       (cond ((and (integer? criterion) (exact? criterion))
	      (let loop ((i start))
		(or (<= past i)
		    (and (= criterion (bytevector-u8-ref bv i))
			 (loop (+ 1 i))))))

	     ((char? criterion)
	      (let ((criterion (char->integer criterion)))
		(let loop ((i start))
		  (or (<= past i)
		      (and (= criterion (bytevector-u8-ref bv i))
			   (loop (+ 1 i)))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (char-set-contains? criterion (integer->char (bytevector-u8-ref bv i)))
			 (loop (+ 1 i))))))

	     ((procedure? criterion)
	      ;;Slightly funky loop so  that final (PRED S[PAST-1]) call
	      ;;is a tail call.
	      (let loop ((i start))
		(let ((c  (bytevector-u8-ref bv i))
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ;this has to be a tail call
		    (and (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%bytevector-u8-every
		"expected char-set, char, or predicate as second parameter"
		criterion)))))

(define (%bytevector-u8-any criterion bv start past)
  (and (< start past)
       (cond ((and (integer? criterion) (exact? criterion))
	      (let loop ((i start))
		(and (< i past)
		     (or (= criterion (bytevector-u8-ref bv i))
			 (loop (+ i 1))))))

	     ((char? criterion)
	      (let ((criterion (char->integer criterion)))
		(let loop ((i start))
		  (and (< i past)
		       (or (= criterion (bytevector-u8-ref bv i))
			   (loop (+ i 1)))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (char-set-contains? criterion (integer->char (bytevector-u8-ref bv i)))
			 (loop (+ i 1))))))

	     ((procedure? criterion)
	      ;;Slightly funky loop so  that final (PRED S[PAST-1]) call
	      ;;is a tail call.
	      (let loop ((i start))
		(let ((c (bytevector-u8-ref bv i))
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (or (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%bytevector-u8-any
		"expected char-set, char, or predicate as second parameter"
		criterion)))))


;;;; lexicographic comparison

(define (%true-bytevector-u8-compare bytevector-u8-prefix-length-proc less-proc
				     bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (let ((size1 (- past1 start1))
	(size2 (- past2 start2))
	(match (bytevector-u8-prefix-length-proc bv1 start1 past1 bv2 start2 past2)))
    (if (= match size1)
	((if (= match size2) proc= proc<) past1)
      ((if (= match size2)
	   proc>
	 (if (less-proc (bytevector-u8-ref bv1 (+ start1 match))
			(bytevector-u8-ref bv2 (+ start2 match)))
	     proc< proc>))
       (+ match start1)))))

(define (%bytevector-u8-compare bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (%true-bytevector-u8-compare %bytevector-u8-prefix-length <
			       bv1 start1 past1 bv2 start2 past2 proc< proc= proc>))

(define (%bytevector-u8-compare-ci bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (%true-bytevector-u8-compare %bytevector-u8-prefix-length-ci %integer-char-ci<?
			       bv1 start1 past1 bv2 start2 past2 proc< proc= proc>))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8= bytevector-u8-compare-proc bv1 start1 past1 bv2 start2 past2)
  (and (= (- past1 start1) (- past2 start2))	 ; Quick filter
       (or (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
	   (bytevector-u8-compare-proc bv1 start1 past1 bv2 start2 past2 ; Real test
				       (lambda (i) #f) values (lambda (i) #f)))))

(define (%bytevector-u8= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8= %bytevector-u8-compare bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8= %bytevector-u8-compare-ci bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8<> bytevector-u8-compare-proc bv1 start1 past1 bv2 start2 past2)
  (or (not (= (- past1 start1) (- past2 start2)))      ; Fast path
      (and (not (and (eq? bv1 bv2) (= start1 start2))) ; Quick filter
	   (bytevector-u8-compare-proc bv1 start1 past1 bv2 start2 past2 ; Real test
				       values (lambda (i) #f) values))))

(define (%bytevector-u8<> bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8<> %bytevector-u8-compare bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci<> bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8<> %bytevector-u8-compare-ci bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8< bytevector-u8-prefix-proc pred
			      bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (< past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 values (lambda (i) #f) (lambda (i) #f))))

(define (%bytevector-u8< bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8< %bytevector-u8-prefix-length <
			bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci< bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8< %bytevector-u8-prefix-length-ci %integer-char-ci<?
			bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8<= bytevector-u8-prefix-proc pred
			       bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (<= past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 values values (lambda (i) #f))))

(define (%bytevector-u8<= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8<= %bytevector-u8-prefix-length <=
			 bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci<= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8<= %bytevector-u8-prefix-length-ci %integer-char-ci<=?
			 bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8> bytevector-u8-prefix-proc pred bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (> past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 (lambda (i) #f) (lambda (i) #f) values)))

(define (%bytevector-u8> bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8> %bytevector-u8-prefix-length <
			bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci> bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8> %bytevector-u8-prefix-length-ci %integer-char-ci<?
			bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8>= bytevector-u8-prefix-proc pred bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (>= past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 (lambda (i) #f) values values)))

(define (%bytevector-u8>= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8>= %bytevector-u8-prefix-length <=
			 bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-ci>= bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8>= %bytevector-u8-prefix-length-ci %integer-char-ci<=?
			 bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%full-bytevector-u8= a b)
  (%bytevector-u8= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8< a b)
  (%bytevector-u8< a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8<= a b)
  (%bytevector-u8<= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8> a b)
  (%bytevector-u8> a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8>= a b)
  (%bytevector-u8>= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8-ci= a b)
  (%bytevector-u8-ci= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8-ci< a b)
  (%bytevector-u8-ci< a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8-ci<= a b)
  (%bytevector-u8-ci<= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8-ci> a b)
  (%bytevector-u8-ci> a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-bytevector-u8-ci>= a b)
  (%bytevector-u8-ci>= a 0 (bytevector-length a) b 0 (bytevector-length b)))


;;;; dictionary comparison

(define (%true-bytevector-u8-dictionary-compare a b pred=? pred<? $lesser $equal $greater)
  ;;Compare the bytes  from bytevectors A and B  using PRED=? and PRED<?
  ;;and return $LESSER, $EQUAL  or $GREATER.  The comparison skips bytes
  ;;which, interpreted as ASCII, are blanks.
  ;;
  (let ((lena	(bytevector-length a))
	(lenb	(bytevector-length b)))
    (let loop ((i 0) (j 0))
      (cond ((= i lena)
	     (if (= j lenb)
		 $equal
	       $lesser))

	    ((= j lenb)
	     $greater)

	    (else
	     (let ((byte-a (bytevector-u8-ref a i))
		   (byte-b (bytevector-u8-ref b j)))
	       (cond ((memv byte-a $white-spaces-for-dictionary-comparison)
		      (loop (+ 1 i) j))

		     ((memv byte-b $white-spaces-for-dictionary-comparison)
		      (loop i (+ 1 j)))

		     ((pred=? byte-a byte-b)
		      (loop (+ 1 i) (+ 1 j)))

		     ((pred<? byte-a byte-b)
		      $lesser)

		     (else
		      $greater))))))))

;;; --------------------------------------------------------------------

(define (%bytevector-u8-dictionary-compare a b)
  (%true-bytevector-u8-dictionary-compare a b = < -1 0 +1))

(define (%bytevector-u8-dictionary=? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #f #t #f))

(define (%bytevector-u8-dictionary<>? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #t #f #t))

(define (%bytevector-u8-dictionary<? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #t #f #f))

(define (%bytevector-u8-dictionary<=? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #t #t #f))

(define (%bytevector-u8-dictionary>? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #f #f #t))

(define (%bytevector-u8-dictionary>=? a b)
  (%true-bytevector-u8-dictionary-compare a b = < #f #t #t))

;;; --------------------------------------------------------------------

(define (%bytevector-u8-dictionary-compare-ci a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<?))

(define (%bytevector-u8-dictionary-ci=? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #t #f))

(define (%bytevector-u8-dictionary-ci<>? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #f #t))

(define (%bytevector-u8-dictionary-ci<? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #f #f))

(define (%bytevector-u8-dictionary-ci<=? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #t #f))

(define (%bytevector-u8-dictionary-ci>? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #f #t))

(define (%bytevector-u8-dictionary-ci>=? a b)
  (%true-bytevector-u8-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #t #t))


;;;; bytevector-u8/numbers lexicographic comparison

(define $list-of-digits
  (map char->integer '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (%bytevector-u8/numbers->parts input-bv)
  ;;Split a bytevector string into the list of its parts.  Example:
  ;;
  ;;	"foo4bar3zab10"
  ;;
  ;;becomes:
  ;;
  ;;	("foo" ("4" . 4) "bar" ("3" . 3) "zab" ("10" . 10))
  ;;
  (let loop ((chars	(reverse (bytevector->u8-list input-bv)))
	     (str	'())
	     (num	'())
	     (parts	'()))
    (define (%accumulate-bytevector-u8-part)
      (if (null? str)
	  parts
	(cons (u8-list->bytevector str) parts)))
    (define (%accumulate-number-part)
      (if (null? num)
	  parts
	(let ((s (u8-list->bytevector num)))
	  (cons `(,s . ,(string->number (utf8->string s))) parts))))
    (cond ((null? chars)
	   (cond ((not (null? str))
		  (assert (null? num))
		  (%accumulate-bytevector-u8-part))
		 ((not (null? num))
		  (assert (null? str))
		  (%accumulate-number-part))
		 (else parts)))
	  ((memv (car chars) $list-of-digits)
	   (loop (cdr chars)
		 '()
		 (cons (car chars) num)
		 (%accumulate-bytevector-u8-part)))
	  (else
	   (loop (cdr chars)
		 (cons (car chars) str)
		 '()
		 (%accumulate-number-part))))))

(define (%true-bytevector-u8/numbers-compare a b bytevector-u8->parts
					     bytevector=? bytevector<? $lesser $equal $greater)
  (let loop ((a (bytevector-u8->parts a))
             (b (bytevector-u8->parts b)))
    (cond ((null? a)
	   (if (null? b) $equal $lesser))

	  ((null? b) $greater)

	  ((and (bytevector? (car a)) ;both strings
		(bytevector? (car b)))
	   (if (bytevector=? (car a) (car b))
	       (loop (cdr a) (cdr b))
	     (if (bytevector<? (car a) (car b))
		 $lesser
	       $greater)))

	  ((and (pair? (car a))	;both numbers
		(pair? (car b)))
	   (let ((num-a (cdar a))
		 (num-b (cdar b)))
	     (cond ((= num-a num-b)
		    (loop (cdr a) (cdr b)))
		   ((< num-a num-b)
		    $lesser)
		   (else
		    $greater))))

	  ((bytevector? (car a)) ;first string, second number
	   (if (bytevector<? (car a) (caar b))
	       $lesser
	     $greater))

	  (else	;first number, second string
	   (if (bytevector<? (caar a) (car b))
	       $lesser
	     $greater)))))

;;; --------------------------------------------------------------------

(define (%bytevector-u8/numbers-compare a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< -1 0 +1))

(define (%bytevector-u8/numbers=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #f #t #f))

(define (%bytevector-u8/numbers<>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #t #f #t))

(define (%bytevector-u8/numbers<? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #t #f #f))

(define (%bytevector-u8/numbers<=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #t #t #f))

(define (%bytevector-u8/numbers>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #f #f #t))

(define (%bytevector-u8/numbers>=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       bytevector=? %full-bytevector-u8< #f #t #t))

;;; --------------------------------------------------------------------

(define (%bytevector-u8/numbers-compare-ci a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci<))

(define (%bytevector-u8/numbers-ci=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #t #f))

(define (%bytevector-u8/numbers-ci<>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #f #t))

(define (%bytevector-u8/numbers-ci<? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #f #f))

(define (%bytevector-u8/numbers-ci<=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #t #f))

(define (%bytevector-u8/numbers-ci>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #f #t))

(define (%bytevector-u8/numbers-ci>=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #t #t))


;;;; bytevector-u8/numbers dictionary comparison

(define (%bytevector-u8/numbers-dictionary->parts input-string)
  ;;Split a  string into  the list of  its parts, discard  white spaces.
  ;;Example:
  ;;
  ;;	"foo4 bar3   zab10"
  ;;
  ;;becomes:
  ;;
  ;;	("foo" ("4" . 4) "bar" ("3" . 3) "zab" ("10" . 10))
  ;;
  (let loop ((chars	(reverse (bytevector->u8-list input-string)))
	     (str	'())
	     (num	'())
	     (parts	'()))
    (define (%accumulate-bytevector-u8-part)
      (if (null? str)
	  parts
	(cons (u8-list->bytevector str) parts)))
    (define (%accumulate-number-part)
      (if (null? num)
	  parts
	(let ((s (u8-list->bytevector num)))
	  (cons `(,s . ,(string->number (utf8->string s))) parts))))
    (cond ((null? chars)
	   (cond ((not (null? str))
		  (assert (null? num))
		  (%accumulate-bytevector-u8-part))
		 ((not (null? num))
		  (assert (null? str))
		  (%accumulate-number-part))
		 (else parts)))
	  ((memv (car chars) $list-of-digits)
	   (loop (cdr chars)
		 '()
		 (cons (car chars) num)
		 (%accumulate-bytevector-u8-part)))
	  ((memv (car chars) $white-spaces-for-dictionary-comparison)
	   (loop (cdr chars) str num parts))
	  (else
	   (loop (cdr chars)
		 (cons (car chars) str)
		 '()
		 (%accumulate-number-part))))))

;;; --------------------------------------------------------------------

(define (%bytevector-u8/numbers-dictionary-compare a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< -1 0 +1))

(define (%bytevector-u8/numbers-dictionary=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #f #t #f))

(define (%bytevector-u8/numbers-dictionary<>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #t #f #t))

(define (%bytevector-u8/numbers-dictionary<? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #t #f #f))

(define (%bytevector-u8/numbers-dictionary<=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #t #t #f))

(define (%bytevector-u8/numbers-dictionary>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #f #f #t))

(define (%bytevector-u8/numbers-dictionary>=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       bytevector=? %full-bytevector-u8< #f #t #t))

;;; --------------------------------------------------------------------

(define (%bytevector-u8/numbers-dictionary-compare-ci a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci<))

(define (%bytevector-u8/numbers-dictionary-ci=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #t #f))

(define (%bytevector-u8/numbers-dictionary-ci<>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #f #t))

(define (%bytevector-u8/numbers-dictionary-ci<? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #f #f))

(define (%bytevector-u8/numbers-dictionary-ci<=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #t #t #f))

(define (%bytevector-u8/numbers-dictionary-ci>? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #f #t))

(define (%bytevector-u8/numbers-dictionary-ci>=? a b)
  (%true-bytevector-u8/numbers-compare a b %bytevector-u8/numbers-dictionary->parts
				       %full-bytevector-u8-ci= %full-bytevector-u8-ci< #f #t #t))


;;;; mapping

(define (bytevector-u8-map proc bv0 . bvs)
  (let ((bvs (cons bv0 bvs)))
    (%assert-bytevectors-of-same-length 'bytevector-u8-map bvs)
    (let* ((len     (bytevector-length bv0))
	   (result  (make-bytevector len)))
      (do ((i 0 (+ 1 i)))
	  ((= len i)
	   result)
	(bytevector-u8-set! result i
			    (apply proc i (map (lambda (bv) (bytevector-u8-ref bv i))
					    bvs)))))))

(define (bytevector-u8-map! proc bv0 . bvs)
  (let ((bvs (cons bv0 bvs)))
    (%assert-bytevectors-of-same-length 'bytevector-u8-map bvs)
    (let ((len (bytevector-length bv0)))
      (do ((i 0 (+ 1 i)))
	  ((= len i))
	(bytevector-u8-set! bv0 i
			    (apply proc i (map (lambda (bv) (bytevector-u8-ref bv i))
					    bvs)))))))

(define (bytevector-u8-map* proc bv0 . bvs)
  (let* ((bvs (cons bv0 bvs))
	 (len (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i))
	 (result (make-bytevector len)))
	((= len i)
	 result)
      (bytevector-u8-set! result i
			  (apply proc i (map (lambda (bv) (bytevector-u8-ref bv i))
					  bvs))))))

(define (bytevector-u8-map*! proc bv0 . bvs)
  (let* ((bvs  (cons bv0 bvs))
	 (len      (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (bytevector-u8-set! bv0 i
			  (apply proc i (map (lambda (bv) (bytevector-u8-ref bv i))
					  bvs))))))

(define (bytevector-u8-for-each* proc bv0 . bvs)
  (let* ((bvs  (cons bv0 bvs))
	 (len      (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (apply proc i (map (lambda (bv) (bytevector-u8-ref bv i))
		      bvs)))))

;;; --------------------------------------------------------------------

(define (%subbytevector-u8-map proc bv start past)
  (do ((i start (+ 1 i))
       (j 0 (+ 1 j))
       (result (make-bytevector (- past start))))
      ((>= i past)
       result)
    (bytevector-u8-set! result j (proc (bytevector-u8-ref bv i)))))

(define (%subbytevector-u8-map! proc bv start past)
  (do ((i start (+ 1 i)))
      ((>= i past)
       bv)
    (bytevector-u8-set! bv i (proc (bytevector-u8-ref bv i)))))

(define (%subbytevector-u8-for-each proc bv start past)
  (let loop ((i start))
    (when (< i past)
      (proc (bytevector-u8-ref bv i))
      (loop (+ i 1)))))

(define (%subbytevector-u8-for-each-index proc bv start past)
  (let loop ((i start))
    (when (< i past)
      (proc i)
      (loop (+ i 1)))))


;;;; case hacking

(define (byte-cased? c)
  ;;This works  because CHAR-UPCASE returns  #f if the character  has no
  ;;upcase version.
  (char-upper-case? (char-upcase (integer->char c))))

(define (%bytevector-u8-titlecase*! bv start past)
  (let loop ((i start))
    (cond ((%bytevector-u8-index byte-cased? bv i past)
	   => (lambda (i)
		(bytevector-u8-set! bv i
				    (char->integer
				     (char-titlecase
				      (integer->char (bytevector-u8-ref bv i)))))
		(let ((i1 (+ i 1)))
		  (cond ((%bytevector-u8-skip byte-cased? bv i1 past)
			 => (lambda (j)
			      (%subbytevector-u8-map! (lambda (b)
							(char->integer
							 (char-downcase (integer->char b))))
						      bv i1 j)
			      (loop (+ j 1))))
			(else
			 (%subbytevector-u8-map! (lambda (b)
						   (char->integer (char-downcase (integer->char b))))
						 bv i1 past)))))))))


;;;; folding

(define (bytevector-u8-fold-left kons knil vec0 . bvs)
  (let ((bvs (cons vec0 bvs)))
    (%assert-bytevectors-of-same-length 'bytevector-u8-fold-left bvs)
    (let ((len (bytevector-length vec0)))
      (let loop ((i 0) (knil knil))
	(if (= i len)
	    knil
	  (loop (+ 1 i) (apply kons i knil
			       (map (lambda (vec)
				      (bytevector-u8-ref vec i))
				 bvs))))))))

(define (bytevector-u8-fold-right kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs)))
    (%assert-bytevectors-of-same-length 'bytevector-u8-fold-right bvs)
    (let ((len (bvs-list-min-length bvs)))
      (let loop ((i (- len 1)) (knil knil))
	(if (< i 0)
	    knil
	  (loop (- i 1) (apply kons i knil
			       (map (lambda (vec)
				      (bytevector-u8-ref vec i))
				 bvs))))))))

(define (bytevector-u8-fold-left* kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs))
	 (len (bvs-list-min-length bvs)))
    (let loop ((i 0) (knil knil))
      (if (= len i)
	  knil
	(loop (+ 1 i) (apply kons i knil
			     (map (lambda (vec)
				    (bytevector-u8-ref vec i))
			       bvs)))))))

(define (bytevector-u8-fold-right* kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs))
	 (len (bvs-list-min-length bvs)))
    (let loop ((i (- len 1)) (knil knil))
      (if (< i 0)
	  knil
	(loop (- i 1) (apply kons i knil
			     (map (lambda (vec)
				    (bytevector-u8-ref vec i))
			       bvs)))))))

(define (%subbytevector-u8-fold-left kons knil bv start past)
  (let loop ((v knil) (i start))
    (if (< i past)
	(loop (kons (bytevector-u8-ref bv i) v) (+ i 1))
      v)))

(define (%subbytevector-u8-fold-right kons knil bv start past)
  (let loop ((v knil) (i (- past 1)))
    (if (>= i start)
	(loop (kons (bytevector-u8-ref bv i) v) (- i 1))
      v)))

(define bytevector-u8-unfold
  (case-lambda
   ((p f g seed)
    (bytevector-u8-unfold p f g seed '#vu8() (lambda (x) '#vu8())))
   ((p f g seed base)
    (bytevector-u8-unfold p f g seed base (lambda (x) '#vu8())))
   ((p f g seed base make-final)
    ;;The strategy is to allocate a series of chunks into which we stash
    ;;the bytes as we generate them. Chunk size goes up in powers of two
    ;;beginning with 40 and levelling out at 4k, i.e.
    ;;
    ;;	40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
    ;;
    ;;This  should work  pretty well  for short  bytevectors  and longer
    ;;ones.  When  done, we allocate  an answer bytevector and  copy the
    ;;chars over from the chunk buffers.
    (let lp ((chunks '())		  ;previously filled chunks
	     (nchars 0)			  ;number of chars in CHUNKS
	     (chunk (make-bytevector 40)) ;current chunk into which we write
	     (chunk-len 40)
	     (i 0) ;number of chars written into CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed))
	(if (not (p seed))
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (< i chunk-len)
		  (begin (bytevector-u8-set! chunk i c)
			 (lp2 (+ i 1) seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-bytevector chunk-len2)))
		  (bytevector-u8-set! new-chunk 0 c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 1 seed))))

	  ;;Done, make the answer bytevector and install the bits.
	  (let* ((final    (make-final seed))
		 (flen     (bytevector-length final))
		 (base-len (bytevector-length base))
		 (j        (+ base-len nchars i))
		 (ans      (make-bytevector (+ j flen))))
	    (%bytevector-u8-copy*! ans j final 0 flen) ; Install FINAL.
	    (let ((j (- j i)))
	      (%bytevector-u8-copy*! ans j chunk 0 i) ; Install CHUNK[0,I).
	      (let lp ((j j) (chunks chunks))	      ; Install CHUNKS.
		(if (pair? chunks)
		    (let* ((chunk  (car chunks))
			   (chunks (cdr chunks))
			   (chunk-len (bytevector-length chunk))
			   (j (- j chunk-len)))
		      (%bytevector-u8-copy*! ans j chunk 0 chunk-len)
		      (lp j chunks)))))
	    (%bytevector-u8-copy*! ans 0 base 0 base-len) ; Install BASE.
	    ans)))))))

(define bytevector-u8-unfold-right
  (case-lambda
   ((p f g seed)
    (bytevector-u8-unfold-right p f g seed '#vu8() (lambda (x) '#vu8())))
   ((p f g seed base)
    (bytevector-u8-unfold-right p f g seed base (lambda (x) '#vu8())))
   ((p f g seed base make-final)
    (let lp ((chunks '())	      ; Previously filled chunks
	     (nchars 0)		      ; Number of chars in CHUNKS
	     (chunk (make-bytevector 40)) ; Current chunk into which we write
	     (chunk-len 40)
	     (i 40) ; Number of chars available in CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed)) ; Fill up CHUNK from right
	(if (not (p seed))	   ; to left.
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (> i 0)
		  (let ((i (- i 1)))
		    (bytevector-u8-set! chunk i c)
		    (lp2 i seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-bytevector chunk-len2))
		       (i (- chunk-len2 1)))
		  (bytevector-u8-set! new-chunk i c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 i seed))))

	  ;; We're done. Make the answer bytevector-u8 & install the bits.
	  (let* ((final (make-final seed))
		 (flen (bytevector-length final))
		 (base-len (bytevector-length base))
		 (chunk-used (- chunk-len i))
		 (j (+ base-len nchars chunk-used))
		 (ans (make-bytevector (+ j flen))))
	    (%bytevector-u8-copy*! ans 0 final 0 flen)	       ; Install FINAL.
	    (%bytevector-u8-copy*! ans flen chunk i chunk-len) ; Install CHUNK[I,).
	    (let lp ((j (+ flen chunk-used))	       ; Install CHUNKS.
		     (chunks chunks))
	      (if (pair? chunks)
		  (let* ((chunk  (car chunks))
			 (chunks (cdr chunks))
			 (chunk-len (bytevector-length chunk)))
		    (%bytevector-u8-copy*! ans j chunk 0 chunk-len)
		    (lp (+ j chunk-len) chunks))
		(%bytevector-u8-copy*! ans j base 0 base-len)))	; Install BASE.
	    ans)))))))


;;;; selecting

(define (%assert-enough-chars who nchars start past)
  (unless (<= nchars (- past start))
    (assertion-violation who
      "requested number of chars greater than length of bytevector" nchars start past)))

(define (%bytevector-u8-reverse-copy* bv start past)
  (let ((result (make-bytevector (- past start))))
    (do ((i (- past 1) (- i 1))
	 (j 0 (+ j 1)))
	((< i start)
	 result)
      (bytevector-u8-set! result j (bytevector-u8-ref bv i)))))

(define (%bytevector-u8-take nchars bv start past)
  (%assert-enough-chars '%bytevector-u8-take nchars start past)
  (subbytevector-u8 bv start (+ start nchars)))

(define (%bytevector-u8-take-right nchars bv start past)
  (%assert-enough-chars '%bytevector-u8-take-right nchars start past)
  (subbytevector-u8 bv (- past nchars) past))

(define (%bytevector-u8-drop nchars bv start past)
  (%assert-enough-chars '%bytevector-u8-drop nchars start past)
  (subbytevector-u8 bv nchars past))

(define (%bytevector-u8-drop-right nchars bv start past)
  (%assert-enough-chars '%bytevector-u8-drop-right nchars start past)
  (subbytevector-u8 bv start (+ start nchars)))

(define (%bytevector-u8-trim criterion bv start past)
  (cond ((%bytevector-u8-skip criterion bv start past)
	 => (lambda (i) (subbytevector-u8 bv i past)))
	(else '#vu8())))

(define (%bytevector-u8-trim-right criterion bv start past)
  (cond ((%bytevector-u8-skip-right criterion bv start past)
	 => (lambda (i)
	      (subbytevector-u8 bv start (+ 1 i))))
	(else '#vu8())))

(define (%bytevector-u8-trim-both criterion bv start past)
  (let ((bv (%bytevector-u8-trim-right criterion bv start past)))
    (%bytevector-u8-trim criterion bv start (bytevector-length bv))))

(define (%bytevector-u8-pad requested-len fill-char bv start past)
  (let ((len  (- past start))
	(fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (if (<= requested-len len)
	(subbytevector-u8 bv (- past requested-len) past)
      (let ((result (make-bytevector requested-len fill-char)))
	(%bytevector-u8-copy*! result (- requested-len len) bv start past)
	result))))

(define (%bytevector-u8-pad-right requested-len fill-char bv start past)
  (let ((len (- past start))
	(fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (if (<= requested-len len)
	(subbytevector-u8 bv start (+ start requested-len))
      (let ((result (make-bytevector requested-len fill-char)))
	(%bytevector-u8-copy*! result 0 bv start past)
	result))))


;;;; prefix and suffix

(define (%true-bytevector-u8-prefix-length byte-cmp? bv1 start1 past1 bv2 start2 past2)
  ;;Find the length  of the common prefix.  It is  not required that the
  ;;two subbytevector passed be of equal length.
  ;;
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (past1 (+ start1 delta)))
    (if (and (eq? bv1 bv2) (= start1 start2)) ; EQ fast path
	delta
      (let lp ((i start1) (j start2)) ; Regular path
	(if (or (>= i past1)
		(not (byte-cmp? (bytevector-u8-ref bv1 i)
				(bytevector-u8-ref bv2 j))))
	    (- i start1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%bytevector-u8-prefix-length bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8-prefix-length = bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-prefix-length-ci bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8-prefix-length %integer-char-ci=? bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-prefix? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-prefix-length bv1 start1 past1
					       bv2 start2 past2)))))

(define (%bytevector-u8-prefix-ci? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-prefix-length-ci bv1 start1 past1
						  bv2 start2 past2)))))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8-suffix-length byte-cmp? bv1 start1 past1 bv2 start2 past2)
  ;;Find the length  of the common suffix.  It is  not required that the
  ;;two subbytevector-u8s passed be of equal length.
  ;;
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (start1 (- past1 delta)))
    (if (and (eq? bv1 bv2) (= past1 past2)) ; EQ fast path
	delta
      (let lp ((i (- past1 1)) (j (- past2 1))) ; Regular path
	(if (or (< i start1)
		(not (byte-cmp? (bytevector-u8-ref bv1 i)
				(bytevector-u8-ref bv2 j))))
	    (- (- past1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%bytevector-u8-suffix-length bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8-suffix-length = bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-suffix-length-ci bv1 start1 past1 bv2 start2 past2)
  (%true-bytevector-u8-suffix-length %integer-char-ci=? bv1 start1 past1 bv2 start2 past2))

(define (%bytevector-u8-suffix? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-suffix-length bv1 start1 past1
					       bv2 start2 past2)))))

(define (%bytevector-u8-suffix-ci? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-suffix-length-ci bv1 start1 past1
						  bv2 start2 past2)))))


;;;; searching

(define (%bytevector-u8-index criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%bytevector-u8-index (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%bytevector-u8-index (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%bytevector-u8-index (lambda (byte)
				 (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (bytevector-u8-ref bv i))
		    i
		  (loop (+ 1 i))))))
	(else
	 (assertion-violation '%bytevector-u8-index
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%bytevector-u8-index-right criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%bytevector-u8-index-right (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%bytevector-u8-index-right (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%bytevector-u8-index-right (lambda (byte)
				       (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref bv i))
		    i
		  (loop (- i 1))))))
	(else
	 (assertion-violation '%bytevector-u8-index-right
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%bytevector-u8-skip criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%bytevector-u8-skip (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%bytevector-u8-skip (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%bytevector-u8-skip (lambda (byte)
				(char-set-contains? criterion (integer->char byte)))
			      bv start past))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (bytevector-u8-ref bv i))
		    (loop (+ i 1))
		  i))))
	(else
	 (assertion-violation '%bytevector-u8-skip
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%bytevector-u8-skip-right criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%bytevector-u8-skip-right (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%bytevector-u8-skip-right (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%bytevector-u8-skip-right (lambda (byte)
				      (char-set-contains? criterion (integer->char byte)))
				    bv start past))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref bv i))
		    (loop (- i 1))
		  i))))
	(else
	 (assertion-violation '%bytevector-u8-skip-right
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%bytevector-u8-count criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%bytevector-u8-count (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%bytevector-u8-count (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%bytevector-u8-count (lambda (byte)
				 (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (criterion (bytevector-u8-ref bv i))
			   (+ 1 count)
			 count)))
	     ((>= i past)
	      count)))
	(else
	 (assertion-violation '%bytevector-u8-count
	   "expected char-set, char or predicate as criterion" criterion))))

(define (%bytevector-u8-contains bv bv-start bv-past pattern pattern-start pattern-past)
  (%kmp-search = bytevector-u8-ref
	       bv bv-start bv-past
	       pattern pattern-start pattern-past))

(define (%bytevector-u8-contains-ci bv bv-start bv-past pattern pattern-start pattern-past)
  (%kmp-search %integer-char-ci=?
	       bytevector-u8-ref
	       bv bv-start bv-past
	       pattern pattern-start pattern-past))


;;;; filtering

(define (%bytevector-u8-filter criterion bv start past)
  (let-values (((port getter) (open-bytevector-output-port)))
    (cond ((procedure? criterion)
	   (do ((i start (+ 1 i)))
	       ((= i past)
		(getter))
	     (let ((byte (bytevector-u8-ref bv i)))
	       (when (criterion byte)
		 (put-u8 port byte)))))
	  ((and (integer? criterion) (exact? criterion))
	   (%bytevector-u8-filter (lambda (byte)
				    (= byte criterion))
				  bv start past))
	  ((char? criterion)
	   (%bytevector-u8-filter (char->integer criterion) bv start past))
	  ((char-set? criterion)
	   (%bytevector-u8-filter (lambda (byte)
				    (char-set-contains? criterion (integer->char byte)))
				  bv start past))
	  (else
	   (assertion-violation '%bytevector-u8-filter
	     "expected predicate, integer, char or char-set as criterion" criterion)))))

(define (%bytevector-u8-delete criterion bv start past)
  (let-values (((port getter) (open-bytevector-output-port)))
    (cond ((procedure? criterion)
	   (do ((i start (+ 1 i)))
	       ((= i past)
		(getter))
	     (let ((byte (bytevector-u8-ref bv i)))
	       (unless (criterion byte)
		 (put-u8 port byte)))))
	  ((and (integer? criterion) (exact? criterion))
	   (%bytevector-u8-delete (lambda (byte)
				    (= byte criterion))
				  bv start past))
	  ((char? criterion)
	   (%bytevector-u8-delete (char->integer criterion) bv start past))
	  ((char-set? criterion)
	   (%bytevector-u8-delete (lambda (byte)
				    (char-set-contains? criterion (integer->char byte)))
				  bv start past))
	  (else
	   (assertion-violation '%bytevector-u8-delete
	     "expected predicate, integer, char or char-set as criterion" criterion)))))


;;;; bytevectors and lists

(define (reverse-u8-list->bytevector clist)
  (let* ((len (length clist))
	 (s   (make-bytevector len)))
    (do ((i (- len 1) (- i 1))
	 (clist clist (cdr clist)))
	((not (pair? clist))
	 s)
      (bytevector-u8-set! s i (car clist)))))

(define (%reverse-bytevector->u8-list bv start past)
  (let loop ((i start) (result '()))
    (if (= i past)
	result
      (loop (+ 1 i) (cons (bytevector-u8-ref bv i) result)))))

(define (%bytevector->u8-list* bv start past)
  (do ((i (- past 1) (- i 1))
       (result '() (cons (bytevector-u8-ref bv i) result)))
      ((< i start)
       result)))

(define (%bytevector-u8-join bvs delim grammar)
  (define (join-with-delim ell final)
    (let loop ((ell ell))
      (if (pair? ell)
	  (cons delim (cons (car ell) (loop (cdr ell))))
	final)))
  (cond ((pair? bvs)
	 (bytevector-u8-concatenate
	  (case grammar
	    ((infix strict-infix)
	     (cons (car bvs)
		   (join-with-delim (cdr bvs) '())))
	    ((prefix)
	     (join-with-delim bvs '()))
	    ((suffix)
	     (cons (car bvs)
		   (join-with-delim (cdr bvs) (list delim))))
	    (else
	     (assertion-violation '%bytevector-u8-join
	       "illegal join grammar" grammar)))))

	((not (null? bvs))
	 (assertion-violation '%bytevector-u8-join
	   "BVS parameter is not a list" bvs))

	;; here we know that BVS is the empty list
	((eq? grammar 'strict-infix)
	 (assertion-violation '%bytevector-u8-join
	   "empty list cannot be joined with STRICT-INFIX grammar."))

	(else '#vu8()))) ; Special-cased for infix grammar.

(define (%bytevector-u8-tokenize token-set bv start past)
  (let loop ((i past) (result '()))
    (cond ((and (< start i)
		(%bytevector-u8-index-right token-set bv start i))
	   => (lambda (tpast-1)
		(let ((tpast (+ 1 tpast-1)))
		  (cond ((%bytevector-u8-skip-right token-set bv start tpast-1)
			 => (lambda (tstart-1)
			      (loop tstart-1
				    (cons (subbytevector-u8 bv (+ 1 tstart-1) tpast)
					  result))))
			(else
			 (cons (subbytevector-u8 bv start tpast) result))))))
	  (else result))))


;;;; subvector functions

(define (subbytevector-u8 src start past)
  (let ((src-len	(bytevector-length src))
	(dst-len	(- past start)))
    (cond ((zero? dst-len) '#vu8())
	  ((zero? src-len)
	   (assertion-violation 'subbytevector-u8 "cannot replicate empty (sub)bytevector"))
	  (else
	   (let ((dst (make-bytevector dst-len)))
	     (do ((i 0 (+ 1 i))
		  (j start (+ 1 j)))
		 ((= j past)
		  dst)
	       (bytevector-u8-set! dst i (bytevector-u8-ref src j))))))))

(define (%xsubbytevector-u8 from to bv start past)
  (let ((bv-len	(- past start))
	(result-len	(- to from)))
    (cond ((zero? result-len) '#vu8())
	  ((zero? bv-len)
	   (assertion-violation '%xsubbytevector-u8 "cannot replicate empty (sub)bytevector-u8"))
	  ((= 1 bv-len)
	   (make-bytevector result-len (bytevector-u8-ref bv start)))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from bv-len)) (floor (/ to bv-len)))
	   (subbytevector-u8 bv
		      (+ start (mod from bv-len))
		      (+ start (mod to   bv-len))))

	  ;; Selected text requires multiple spans.
	  (else
	   (let ((result (make-bytevector result-len)))
	     (%multispan-repcopy! from to result 0 bv start past)
	     result)))))

(define (%bytevector-u8-xcopy! from to
			dst-bv dst-start dst-past
			src-bv src-start src-past)
  (let* ((tocopy	(- to from))
	 (tend		(+ dst-start tocopy))
	 (bv-len	(- src-past src-start)))
    (cond ((zero? tocopy))
	  ((zero? bv-len)
	   (assertion-violation '%bytevector-u8-xcopy! "cannot replicate empty (sub)bytevector-u8"))

	  ((= 1 bv-len)
	   (%bytevector-u8-fill*! dst-bv (bytevector-u8-ref src-bv src-start) dst-start dst-past))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from bv-len)) (floor (/ to bv-len)))
	   (%bytevector-u8-copy*! dst-bv dst-start src-bv
			  (+ src-start (mod from bv-len))
			  (+ src-start (mod to   bv-len))))

	  (else
	   (%multispan-repcopy! from to dst-bv dst-start src-bv src-start src-past)))))

(define (%multispan-repcopy! from to dst-bv dst-start src-bv src-start src-past)
  ;;This  is the  core  copying loop  for  XSUBBYTEVECTOR-U8 and  BYTEVECTOR-U8-XCOPY!
  ;;Internal -- not exported, no careful arg checking.
  (let* ((bv-len	(- src-past src-start))
	 (i0		(+ src-start (mod from bv-len)))
	 (total-chars	(- to from)))

    ;; Copy the partial span @ the beginning
    (%bytevector-u8-copy*! dst-bv dst-start src-bv i0 src-past)

    (let* ((ncopied (- src-past i0))	   ; We've copied this many.
	   (nleft (- total-chars ncopied)) ; # chars left to copy.
	   (nspans (div nleft bv-len)))   ; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ dst-start ncopied) (+ i bv-len)) ; Current target index.
	   (nspans nspans (- nspans 1)))	   ; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (%bytevector-u8-copy*! dst-bv i src-bv src-start
				  (+ src-start (- total-chars (- i dst-start)))))

	(%bytevector-u8-copy*! dst-bv i src-bv src-start src-past))))) ; Copy a whole span.


;;;; reverse, replace

(define (%bytevector-u8-reverse bv start past)
  (let* ((len (- past start))
	 (result (make-bytevector len)))
    (do ((i start (+ i 1))
	 (j (- len 1) (- j 1)))
	((< j 0)
	 result)
      (bytevector-u8-set! result j (bytevector-u8-ref bv i)))))

(define (%bytevector-u8-replace bv1 start1 past1 bv2 start2 past2)
  (let* ((len1		(bytevector-length bv1))
	 (len2		(- past2 start2))
	 (result	(make-bytevector (+ len2 (- len1 (- past1 start1))))))
    (%bytevector-u8-copy*! result 0 bv1 0 start1)
    (%bytevector-u8-copy*! result start1 bv2 start2 past2)
    (%bytevector-u8-copy*! result (+ start1 len2) bv1 past1 len1)
    result))

(define (%bytevector-u8-reverse! bv start past)
  (do ((i (- past 1) (- i 1))
       (j start (+ j 1)))
      ((<= i j))
    (let ((ci (bytevector-u8-ref bv i)))
      (bytevector-u8-set! bv i (bytevector-u8-ref bv j))
      (bytevector-u8-set! bv j ci))))


;;;; mutating

(define (%bytevector-u8-copy*! dst-bv dst-start src-bv src-start src-past)
  (when (< (- (bytevector-length dst-bv) dst-start)
	   (- src-past src-start))
    (assertion-violation '%bytevector-u8-copy*!
      "not enough room in destination bytevector"))
  (if (> src-start dst-start)
      (do ((i src-start (+ i 1))
	   (j dst-start (+ j 1)))
	  ((>= i src-past))
	(bytevector-u8-set! dst-bv j (bytevector-u8-ref src-bv i)))
    (do ((i (- src-past 1)                    (- i 1))
	 (j (+ -1 dst-start (- src-past src-start)) (- j 1)))
	((< i src-start))
      (bytevector-u8-set! dst-bv j (bytevector-u8-ref src-bv i)))))

(define (%bytevector-u8-reverse-copy*! dst-bv dst-start src-bv src-start src-past)
  (when (< (- (bytevector-length dst-bv) dst-start)
	   (- src-past src-start))
    (assertion-violation '%bytevector-u8-reverse-copy*!
      "not enough room in destination bytevector-u8"))
  ;;We must handle  correctly copying over the same  bytevector.  If the
  ;;source and  destination bytevectors  are the same,  we copy  all the
  ;;elements  in a  temporary  buffer first;  this  should be  optimised
  ;;someway to reduce to the minimum the size of the buffer.
  (if (eq? src-bv dst-bv)
      (when (< src-start src-past)
	(let* ((buffer (%bytevector-u8-reverse-copy* src-bv src-start src-past)))
	  (%bytevector-u8-copy*! dst-bv dst-start buffer 0 (bytevector-length buffer))))
    (do ((i (- src-past 1) (- i 1))
	 (j dst-start (+ j 1)))
	((< i src-start))
      (bytevector-u8-set! dst-bv j (bytevector-u8-ref src-bv i)))))

(define (%bytevector-u8-fill*! fill-char bv start past)
  (let ((fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (do ((i (- past 1) (- i 1)))
	((< i start))
      (bytevector-u8-set! bv i fill-char))))

(define (bytevector-u8-swap! bv i j)
  (when (zero? (bytevector-length bv))
    (assertion-violation 'bytevector-u8-swap!
      "attempt to swap elements in an empty bytevector"))
  (when (not (= i j))
    (let ((x (bytevector-u8-ref bv i)))
      (bytevector-u8-set! bv i (bytevector-u8-ref bv j))
      (bytevector-u8-set! bv j x))))


;;;; done

)

;;; end of file
