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

    ;; predicates
    bytevector-u8-null?  %bytevector-u8-every  %bytevector-u8-any

    ;; mapping
    bytevector-u8-map      bytevector-u8-map!
    bytevector-u8-map*     bytevector-u8-map*!     bytevector-u8-for-each*
    %subbytevector-u8-map  %subbytevector-u8-map!  %subbytevector-u8-for-each  %subbytevector-u8-for-each-index

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
    %bytevector-u8->list*   %reverse-bytevector-u8->list
    reverse-list->bytevector-u8
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

(define (bytevector-u8s-list-min-length bytevector-u8s)
  (apply min (map bytevector-length bytevector-u8s)))


;;;; constructors

(define (bytevector-u8-concatenate bytevector-u8s)
  (let* ((total (do ((bytevector-u8s bytevector-u8s (cdr bytevector-u8s))
		     (i 0 (+ i (bytevector-length (car bytevector-u8s)))))
		    ((not (pair? bytevector-u8s)) i)))
	 (result (make-bytevector total)))
    (let lp ((i 0) (bytevector-u8s bytevector-u8s))
      (if (pair? bytevector-u8s)
	  (let* ((s (car bytevector-u8s))
		 (slen (bytevector-length s)))
	    (%bytevector-u8-copy*! result i s 0 slen)
	    (lp (+ i slen) (cdr bytevector-u8s)))))
    result))

(define (%bytevector-u8-concatenate-reverse bytevector-u8-list final past)
  (let* ((len (let loop ((sum 0) (lis bytevector-u8-list))
		(if (pair? lis)
		    (loop (+ sum (bytevector-length (car lis))) (cdr lis))
		  sum)))
	 (result (make-bytevector (+ past len))))
    (%bytevector-u8-copy*! result len final 0 past)
    (let loop ((i len) (lis bytevector-u8-list))
      (if (pair? lis)
	  (let* ((s   (car lis))
		 (lis (cdr lis))
		 (slen (bytevector-length s))
		 (i (- i slen)))
	    (%bytevector-u8-copy*! result i s 0 slen)
	    (loop i lis))))
    result))

(define (bytevector-u8-tabulate proc len)
  (let ((s (make-bytevector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0))
      (bytevector-u8-set! s i (proc i)))
    s))


;;;; predicates

(define (bytevector-u8-null? str)
  (zero? (bytevector-length str)))

(define (%bytevector-u8-every criterion str start past)
  (and (< start past)
       (cond ((char? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (= criterion (bytevector-u8-ref str i))
			 (loop (+ 1 i))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (char-set-contains? criterion (bytevector-u8-ref str i))
			 (loop (+ 1 i))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i start))  ; final (PRED S[PAST-1]) call
		(let ((c (bytevector-u8-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (and (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%bytevector-u8-every
		"expected char-set, char, or predicate as second parameter"
		criterion)))))

(define (%bytevector-u8-any criterion str start past)
  (and (< start past)
       (cond ((char? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (= criterion (bytevector-u8-ref str i))
			 (loop (+ i 1))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (char-set-contains? criterion (bytevector-u8-ref str i))
			 (loop (+ i 1))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i start))  ; final (PRED S[PAST-1]) call
		(let ((c (bytevector-u8-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (or (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%bytevector-u8-any
		"expected char-set, char, or predicate as second parameter"
		criterion)))))


;;;; lexicographic comparison

(define (%true-bytevector-u8-compare bytevector-u8-prefix-length-proc char-less-proc
			      str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (let ((size1 (- past1 start1))
	(size2 (- past2 start2)))
    (let ((match (bytevector-u8-prefix-length-proc str1 start1 past1 str2 start2 past2)))
      (if (= match size1)
	  ((if (= match size2) proc= proc<) past1)
	((if (= match size2)
	     proc>
	   (if (char-less-proc (bytevector-u8-ref str1 (+ start1 match))
			       (bytevector-u8-ref str2 (+ start2 match)))
	       proc< proc>))
	 (+ match start1))))))

(define (%bytevector-u8-compare str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (%true-bytevector-u8-compare %bytevector-u8-prefix-length <
			str1 start1 past1 str2 start2 past2 proc< proc= proc>))

(define (%bytevector-u8-compare-ci str1 start1 past1 str2 start2 past2 proc< proc= proc>)
  (%true-bytevector-u8-compare %bytevector-u8-prefix-length-ci char-ci<?
			str1 start1 past1 str2 start2 past2 proc< proc= proc>))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8= bytevector-u8-compare-proc str1 start1 past1 str2 start2 past2)
  (and (= (- past1 start1) (- past2 start2))       ; Quick filter
       (or (and (eq? str1 str2) (= start1 start2)) ; Fast path
	   (bytevector-u8-compare-proc str1 start1 past1 str2 start2 past2 ; Real test
				(lambda (i) #f) values (lambda (i) #f)))))

(define (%bytevector-u8= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8= %bytevector-u8-compare str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8= %bytevector-u8-compare-ci str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8<> bytevector-u8-compare-proc str1 start1 past1 str2 start2 past2)
  (or (not (= (- past1 start1) (- past2 start2)))	     ; Fast path
      (and (not (and (eq? str1 str2) (= start1 start2))) ; Quick filter
	   (bytevector-u8-compare-proc str1 start1 past1 str2 start2 past2	; Real test
				values (lambda (i) #f) values))))

(define (%bytevector-u8<> str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8<> %bytevector-u8-compare str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci<> str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8<> %bytevector-u8-compare-ci str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8< bytevector-u8-prefix-proc char-pred
		       str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (< past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  values (lambda (i) #f) (lambda (i) #f))))

(define (%bytevector-u8< str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8< %bytevector-u8-prefix-length <
		 str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci< str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8< %bytevector-u8-prefix-length-ci char-ci<?
		 str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8<= bytevector-u8-prefix-proc char-pred
			str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (<= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  values values (lambda (i) #f))))

(define (%bytevector-u8<= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8<= %bytevector-u8-prefix-length <=
		  str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci<= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8<= %bytevector-u8-prefix-length-ci char-ci<=?
		  str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8> bytevector-u8-prefix-proc char-pred str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (> past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  (lambda (i) #f) (lambda (i) #f) values)))

(define (%bytevector-u8> str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8> %bytevector-u8-prefix-length <
		 str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci> str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8> %bytevector-u8-prefix-length-ci char-ci<?
		 str1 start1 past1 str2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8>= bytevector-u8-prefix-proc char-pred str1 start1 past1 str2 start2 past2)
  (if (and (eq? str1 str2) (= start1 start2)) ; Fast path
      (>= past1 past2)
    ;;Notice that CHAR-PRED is always the less-than one.
    (%true-bytevector-u8-compare bytevector-u8-prefix-proc char-pred ; Real test
			  str1 start1 past1 str2 start2 past2
			  (lambda (i) #f) values values)))

(define (%bytevector-u8>= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8>= %bytevector-u8-prefix-length <=
		 str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-ci>= str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8>= %bytevector-u8-prefix-length-ci char-ci<=?
		 str1 start1 past1 str2 start2 past2))


;;;; mapping

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

(define (bytevector-u8-map proc str0 . bytevector-u8s)
  (let ((bytevector-u8s (cons str0 bytevector-u8s)))
    (if (apply =* (map bytevector-length bytevector-u8s))
	(let* ((len     (bytevector-length str0))
	       (result  (make-bytevector len)))
	  (do ((i 0 (+ 1 i)))
	      ((= len i)
	       result)
	    (bytevector-u8-set! result i
			 (apply proc i (map (lambda (str) (bytevector-u8-ref str i))
					 bytevector-u8s)))))
      (assertion-violation 'bytevector-u8-map
	"expected bytevector-u8s of the same length"))))

(define (bytevector-u8-map! proc str0 . bytevector-u8s)
  (let ((bytevector-u8s (cons str0 bytevector-u8s)))
    (if (apply =* (map bytevector-length bytevector-u8s))
	(let ((len (bytevector-length str0)))
	  (do ((i 0 (+ 1 i)))
	      ((= len i))
	    (bytevector-u8-set! str0 i
			 (apply proc i (map (lambda (str) (bytevector-u8-ref str i))
					 bytevector-u8s)))))
      (assertion-violation 'bytevector-u8-map!
	"expected bytevector-u8s of the same length"))))

(define (bytevector-u8-map* proc str0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons str0 bytevector-u8s))
	 (len      (bytevector-u8s-list-min-length bytevector-u8s)))
    (do ((i 0 (+ 1 i))
	 (result (make-bytevector len)))
	((= len i)
	 result)
      (bytevector-u8-set! result i
		   (apply proc i (map (lambda (str) (bytevector-u8-ref str i))
				   bytevector-u8s))))))

(define (bytevector-u8-map*! proc str0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons str0 bytevector-u8s))
	 (len      (bytevector-u8s-list-min-length bytevector-u8s)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (bytevector-u8-set! str0 i
		   (apply proc i (map (lambda (str) (bytevector-u8-ref str i))
				   bytevector-u8s))))))

(define (bytevector-u8-for-each* proc str0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons str0 bytevector-u8s))
	 (len      (bytevector-u8s-list-min-length bytevector-u8s)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (apply proc i (map (lambda (str) (bytevector-u8-ref str i))
		      bytevector-u8s)))))

;;; --------------------------------------------------------------------

(define (%subbytevector-u8-map proc str start past)
  (do ((i start (+ 1 i))
       (j 0 (+ 1 j))
       (result (make-bytevector (- past start))))
      ((>= i past)
       result)
    (bytevector-u8-set! result j (proc (bytevector-u8-ref str i)))))

(define (%subbytevector-u8-map! proc str start past)
  (do ((i start (+ 1 i)))
      ((>= i past)
       str)
    (bytevector-u8-set! str i (proc (bytevector-u8-ref str i)))))

(define (%subbytevector-u8-for-each proc str start past)
  (let loop ((i start))
    (when (< i past)
      (proc (bytevector-u8-ref str i))
      (loop (+ i 1)))))

(define (%subbytevector-u8-for-each-index proc str start past)
  (let loop ((i start))
    (when (< i past)
      (proc i)
      (loop (+ i 1)))))


;;;; case hacking

(define (char-cased? c)
  ;; This works  because CHAR-UPCASE returns #f if  the character has no
  ;; upcase version.
  (char-upper-case? (char-upcase c)))

(define (%bytevector-u8-titlecase*! str start past)
  (let loop ((i start))
    (cond ((%bytevector-u8-index char-cased? str i past)
	   => (lambda (i)
		(bytevector-u8-set! str i (char-titlecase (bytevector-u8-ref str i)))
		(let ((i1 (+ i 1)))
		  (cond ((%bytevector-u8-skip char-cased? str i1 past)
			 => (lambda (j)
			      (%subbytevector-u8-map! char-downcase str i1 j)
			      (loop (+ j 1))))
			(else
			 (%subbytevector-u8-map! char-downcase str i1 past)))))))))


;;;; folding

(define (bytevector-u8-fold-left kons knil vec0 . bytevector-u8s)
  (let ((bytevector-u8s (cons vec0 bytevector-u8s)))
    (if (apply =* (map bytevector-length bytevector-u8s))
	(let ((len (bytevector-length vec0)))
	  (let loop ((i     0)
		     (knil  knil))
	    (if (= len i)
		knil
	      (loop (+ 1 i) (apply kons i knil
				   (map (lambda (vec)
					  (bytevector-u8-ref vec i))
				     bytevector-u8s))))))
      (assertion-violation 'bytevector-u8-fold-left
	"expected bytevector-u8s of the same length"))))

(define (bytevector-u8-fold-right kons knil vec0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons vec0 bytevector-u8s)))
    (if (apply =* (map bytevector-length bytevector-u8s))
	(let ((len (bytevector-u8s-list-min-length bytevector-u8s)))
	  (let loop ((i     (- len 1))
		     (knil  knil))
	    (if (< i 0)
		knil
	      (loop (- i 1) (apply kons i knil
				   (map (lambda (vec)
					  (bytevector-u8-ref vec i))
				     bytevector-u8s))))))
      (assertion-violation 'bytevector-u8-fold-right
	"expected bytevector-u8s of the same length"))))

(define (bytevector-u8-fold-left* kons knil vec0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons vec0 bytevector-u8s))
	 (len      (bytevector-u8s-list-min-length bytevector-u8s)))
    (let loop ((i     0)
	       (knil  knil))
      (if (= len i)
	  knil
	(loop (+ 1 i) (apply kons i knil
			     (map (lambda (vec)
				    (bytevector-u8-ref vec i))
			       bytevector-u8s)))))))

(define (bytevector-u8-fold-right* kons knil vec0 . bytevector-u8s)
  (let* ((bytevector-u8s  (cons vec0 bytevector-u8s))
	 (len      (bytevector-u8s-list-min-length bytevector-u8s)))
    (let loop ((i     (- len 1))
	       (knil  knil))
      (if (< i 0)
	  knil
	(loop (- i 1) (apply kons i knil
			     (map (lambda (vec)
				    (bytevector-u8-ref vec i))
			       bytevector-u8s)))))))

(define (%subbytevector-u8-fold-left kons knil str start past)
  (let loop ((v knil)
	     (i start))
    (if (< i past)
	(loop (kons (bytevector-u8-ref str i) v) (+ i 1))
      v)))

(define (%subbytevector-u8-fold-right kons knil str start past)
  (let loop ((v knil)
	     (i (- past 1)))
    (if (>= i start)
	(loop (kons (bytevector-u8-ref str i) v) (- i 1))
      v)))

(define bytevector-u8-unfold
  (case-lambda
   ((p f g seed)
    (bytevector-u8-unfold p f g seed '#vu8() (lambda (x) '#vu8())))
   ((p f g seed base)
    (bytevector-u8-unfold p f g seed base (lambda (x) '#vu8())))
   ((p f g seed base make-final)
    ;;The strategy is  to allocate a series of chunks  into which we stash
    ;;the chars as  we generate them. Chunk size goes up  in powers of two
    ;;beging with 40 and levelling out at 4k, i.e.
    ;;
    ;;	40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
    ;;
    ;;This should  work pretty  well for short  bytevector-u8s, 1-line  (80 char)
    ;;bytevector-u8s, and  longer ones. When  done, we allocate an  answer bytevector-u8
    ;;and copy the chars over from the chunk buffers.
    (let lp ((chunks '())	      ; Previously filled chunks
	     (nchars 0)		      ; Number of chars in CHUNKS
	     (chunk (make-bytevector 40)) ; Current chunk into which we write
	     (chunk-len 40)
	     (i 0) ; Number of chars written into CHUNK
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

	  ;; We're done. Make the answer bytevector-u8 & install the bits.
	  (let* ((final (make-final seed))
		 (flen (bytevector-length final))
		 (base-len (bytevector-length base))
		 (j (+ base-len nchars i))
		 (ans (make-bytevector (+ j flen))))
	    (%bytevector-u8-copy*! ans j final 0 flen) ; Install FINAL.
	    (let ((j (- j i)))
	      (%bytevector-u8-copy*! ans j chunk 0 i) ; Install CHUNK[0,I).
	      (let lp ((j j) (chunks chunks)) ; Install CHUNKS.
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

(define (%bytevector-u8-reverse-copy* str start past)
  (let ((result (make-bytevector (- past start))))
    (do ((i (- past 1) (- i 1))
	 (j 0 (+ j 1)))
	((< i start)
	 result)
      (bytevector-u8-set! result j (bytevector-u8-ref str i)))))

(define (%bytevector-u8-take nchars str start past)
  (if (<= nchars (- past start))
      (subbytevector-u8 str start (+ start nchars))
    (assertion-violation '%bytevector-u8-take
      "requested number of chars greater than length of subbytevector-u8" nchars)))

(define (%bytevector-u8-take-right nchars str start past)
  (if (<= nchars (- past start))
      (subbytevector-u8 str (- past nchars) past)
    (assertion-violation '%bytevector-u8-take-right
      "requested number of chars greater than length of subbytevector-u8" nchars)))

(define (%bytevector-u8-drop nchars str start past)
  (if (<= nchars (- past start))
      (subbytevector-u8 str nchars past)
    (assertion-violation '%bytevector-u8-take
      "requested number of chars greater than length of subbytevector-u8" nchars)))

(define (%bytevector-u8-drop-right nchars str start past)
  (if (<= nchars (- past start))
      (subbytevector-u8 str start (+ start nchars))
    (assertion-violation '%bytevector-u8-take
      "requested number of chars greater than length of subbytevector-u8" nchars)))

(define (%bytevector-u8-trim criterion str start past)
  (cond ((%bytevector-u8-skip criterion str start past)
	 => (lambda (i) (subbytevector-u8 str i past)))
	(else '#vu8())))

(define (%bytevector-u8-trim-right criterion str start past)
  (cond ((%bytevector-u8-skip-right criterion str start past)
	 => (lambda (i) (subbytevector-u8 str start (+ 1 i))))
	(else '#vu8())))

(define (%bytevector-u8-trim-both criterion str start past)
  (let ((str (%bytevector-u8-trim-right criterion str start past)))
    (%bytevector-u8-trim criterion str start (bytevector-length str))))

(define (%bytevector-u8-pad requested-len fill-char str start past)
  (let ((len (- past start)))
    (if (<= requested-len len)
	(subbytevector-u8 str (- past requested-len) past)
      (let ((result (make-bytevector requested-len fill-char)))
	(%bytevector-u8-copy*! result (- requested-len len) str start past)
	result))))

(define (%bytevector-u8-pad-right requested-len fill-char str start past)
  (let ((len (- past start)))
    (if (<= requested-len len)
	(subbytevector-u8 str start (+ start requested-len))
      (let ((result (make-bytevector requested-len fill-char)))
	(%bytevector-u8-copy*! result 0 str start past)
	result))))


;;;; prefix and suffix

(define (%true-bytevector-u8-prefix-length char-cmp? str1 start1 past1 str2 start2 past2)
  ;;Find the length  of the common prefix.  It is  not required that the
  ;;two subbytevector-u8s passed be of equal length.
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (past1 (+ start1 delta)))
    (if (and (eq? str1 str2) (= start1 start2)) ; EQ fast path
	delta
      (let lp ((i start1) (j start2)) ; Regular path
	(if (or (>= i past1)
		(not (char-cmp? (bytevector-u8-ref str1 i)
				(bytevector-u8-ref str2 j))))
	    (- i start1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%bytevector-u8-prefix-length str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8-prefix-length = str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-prefix-length-ci str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8-prefix-length char-ci=? str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-prefix? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-prefix-length str1 start1 past1
					str2 start2 past2)))))

(define (%bytevector-u8-prefix-ci? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-prefix-length-ci str1 start1 past1
					   str2 start2 past2)))))

;;; --------------------------------------------------------------------

(define (%true-bytevector-u8-suffix-length char-cmp? str1 start1 past1 str2 start2 past2)
  ;;Find the length  of the common suffix.  It is  not required that the
  ;;two subbytevector-u8s passed be of equal length.
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (start1 (- past1 delta)))
    (if (and (eq? str1 str2) (= past1 past2)) ; EQ fast path
	delta
      (let lp ((i (- past1 1)) (j (- past2 1))) ; Regular path
	(if (or (< i start1)
		(not (char-cmp? (bytevector-u8-ref str1 i)
				(bytevector-u8-ref str2 j))))
	    (- (- past1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%bytevector-u8-suffix-length str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8-suffix-length = str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-suffix-length-ci str1 start1 past1 str2 start2 past2)
  (%true-bytevector-u8-suffix-length char-ci=? str1 start1 past1 str2 start2 past2))

(define (%bytevector-u8-suffix? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-suffix-length str1 start1 past1
					str2 start2 past2)))))

(define (%bytevector-u8-suffix-ci? str1 start1 past1 str2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%bytevector-u8-suffix-length-ci str1 start1 past1
					   str2 start2 past2)))))


;;;; searching

(define (%bytevector-u8-index criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (= criterion (bytevector-u8-ref str i)) i
		  (loop (+ i 1))))))
	((char-set? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char-set-contains? criterion (bytevector-u8-ref str i)) i
		  (loop (+ i 1))))))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (bytevector-u8-ref str i)) i
		  (loop (+ i 1))))))
	(else (assertion-violation '%bytevector-u8-index
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%bytevector-u8-index-right criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (= criterion (bytevector-u8-ref str i)) i
		  (loop (- i 1))))))
	((char-set? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char-set-contains? criterion (bytevector-u8-ref str i)) i
		  (loop (- i 1))))))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref str i)) i
		  (loop (- i 1))))))
	(else (assertion-violation '%bytevector-u8-index-right
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%bytevector-u8-skip criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (= criterion (bytevector-u8-ref str i))
		    (loop (+ i 1))
		  i))))
	((char-set? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (char-set-contains? criterion (bytevector-u8-ref str i))
		    (loop (+ i 1))
		  i))))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (bytevector-u8-ref str i)) (loop (+ i 1))
		  i))))
	(else (assertion-violation '%bytevector-u8-skip
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%bytevector-u8-skip-right criterion str start past)
  (cond ((char? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (= criterion (bytevector-u8-ref str i))
		    (loop (- i 1))
		  i))))
	((char-set? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (char-set-contains? criterion (bytevector-u8-ref str i))
		    (loop (- i 1))
		  i))))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (bytevector-u8-ref str i)) (loop (- i 1))
		  i))))
	(else (assertion-violation '%bytevector-u8-skip-right
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%bytevector-u8-count criterion str start past)
  (cond ((char? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (= criterion (bytevector-u8-ref str i))
			   (+ count 1)
			 count)))
	     ((>= i past) count)))
	((char-set? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (char-set-contains? criterion (bytevector-u8-ref str i))
			   (+ count 1)
			 count)))
	     ((>= i past) count)))
	((procedure? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (criterion (bytevector-u8-ref str i)) (+ count 1) count)))
	     ((>= i past) count)))
	(else (assertion-violation '%bytevector-u8-count
		"expected char-set, char or predicate as criterion"
		criterion))))

(define (%bytevector-u8-contains text text-start text-past pattern pattern-start pattern-past)
  (%kmp-search = bytevector-u8-ref
	       text text-start text-past
	       pattern pattern-start pattern-past))

(define (%bytevector-u8-contains-ci text text-start text-past pattern pattern-start pattern-past)
  (%kmp-search char-ci=? bytevector-u8-ref
	       text text-start text-past
	       pattern pattern-start pattern-past))


;;;; filtering

(define (%bytevector-u8-delete criterion str start past)
  (if (procedure? criterion)
      (let* ((slen (- past start))
	     (temp (make-bytevector slen))
	     (ans-len (%subbytevector-u8-fold-left (lambda (c i)
					      (if (criterion c) i
						(begin (bytevector-u8-set! temp i c)
						       (+ i 1))))
					    0 str start past)))
	(if (= ans-len slen) temp (subbytevector-u8 temp 0 ans-len)))

    (let* ((cset (cond ((char-set? criterion) criterion)
		       ((char? criterion) (char-set criterion))
		       (else
			(assertion-violation '%bytevector-u8-delete
			  "expected predicate, char or char-set as criterion"
			  criterion))))
	   (len (%subbytevector-u8-fold-left (lambda (c i) (if (char-set-contains? cset c)
							i
						      (+ i 1)))
				      0 str start past))
	   (ans (make-bytevector len)))
      (%subbytevector-u8-fold-left (lambda (c i) (if (char-set-contains? cset c)
					      i
					    (begin (bytevector-u8-set! ans i c)
						   (+ i 1))))
			    0 str start past)
      ans)))

(define (%bytevector-u8-filter criterion str start past)
  (if (procedure? criterion)
      (let* ((slen (- past start))
	     (temp (make-bytevector slen))
	     (ans-len (%subbytevector-u8-fold-left (lambda (c i)
					      (if (criterion c)
						  (begin (bytevector-u8-set! temp i c)
							 (+ i 1))
						i))
					    0 str start past)))
	(if (= ans-len slen) temp (subbytevector-u8 temp 0 ans-len)))

    (let* ((cset (cond ((char-set? criterion) criterion)
		       ((char? criterion) (char-set criterion))
		       (else
			(assertion-violation '%bytevector-u8-filter
			  "expected predicate, char or char-set as criterion"
			  criterion))))
	   (len (%subbytevector-u8-fold-left (lambda (c i) (if (char-set-contains? cset c)
							(+ i 1)
						      i))
				      0 str start past))
	   (ans (make-bytevector len)))
      (%subbytevector-u8-fold-left (lambda (c i) (if (char-set-contains? cset c)
					      (begin (bytevector-u8-set! ans i c)
						     (+ i 1))
					    i))
			    0 str start past)
      ans)))


;;;; bytevector-u8s and lists

(define (reverse-list->bytevector-u8 clist)
  (let* ((len (length clist))
	 (s (make-bytevector len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
	((not (pair? clist)))
      (bytevector-u8-set! s i (car clist)))
    s))

(define (%reverse-bytevector-u8->list str start past)
  (let loop ((i       start)
	     (result  '()))
    (if (= i past)
	result
      (loop (+ 1 i) (cons (bytevector-u8-ref str i) result)))))

(define (%bytevector-u8->list* str start past)
  (do ((i (- past 1) (- i 1))
       (result '() (cons (bytevector-u8-ref str i) result)))
      ((< i start) result)))

(define (%bytevector-u8-join bytevector-u8s delim grammar)
  (define (join-with-delim ell final)
    (let loop ((ell ell))
      (if (pair? ell)
	  (cons delim
		(cons (car ell)
		      (loop (cdr ell))))
	final)))
  (cond ((pair? bytevector-u8s)
	 (bytevector-u8-concatenate
	  (case grammar
	    ((infix strict-infix)
	     (cons (car bytevector-u8s)
		   (join-with-delim (cdr bytevector-u8s) '())))
	    ((prefix)
	     (join-with-delim bytevector-u8s '()))
	    ((suffix)
	     (cons (car bytevector-u8s)
		   (join-with-delim (cdr bytevector-u8s) (list delim))))
	    (else
	     (assertion-violation '%bytevector-u8-join
	       "illegal join grammar" grammar)))))

	((not (null? bytevector-u8s))
	 (assertion-violation '%bytevector-u8-join
	   "BYTEVECTOR-U8S parameter is not a list" bytevector-u8s))

	;; here we know that BYTEVECTOR-U8S is the empty list
	((eq? grammar 'strict-infix)
	 (assertion-violation '%bytevector-u8-join
	   "empty list cannot be joined with STRICT-INFIX grammar."))

	(else '#vu8()))) ; Special-cased for infix grammar.

(define (%bytevector-u8-tokenize token-set str start past)
  (let loop ((i		past)
	     (result	'()))
    (cond ((and (< start i) (%bytevector-u8-index-right token-set str start i))
	   => (lambda (tpast-1)
		(let ((tpast (+ 1 tpast-1)))
		  (cond ((%bytevector-u8-skip-right token-set str start tpast-1)
			 => (lambda (tstart-1)
			      (loop tstart-1
				    (cons (subbytevector-u8 str (+ 1 tstart-1) tpast)
					  result))))
			(else (cons (subbytevector-u8 str start tpast) result))))))
	  (else result))))


;;;; extended subbytevector-u8

(define (subbytevector-u8 src start past)
  (let ((src-len	(bytevector-length src))
	(dst-len	(- past start)))
    (cond ((zero? dst-len) '#vu8())
	  ((zero? src-len)
	   (assertion-violation 'subbytevector-u8 "cannot replicate empty (sub)bytevector-u8"))
	  (else
	   (let ((dst (make-bytevector dst-len)))
	     (do ((i 0 (+ 1 i))
		  (j start (+ 1 j)))
		 ((= j past)
		  dst)
	       (bytevector-u8-set! dst i (bytevector-u8-ref src j))))))))

(define (%xsubbytevector-u8 from to str start past)
  (let ((str-len	(- past start))
	(result-len	(- to from)))
    (cond ((zero? result-len) '#vu8())
	  ((zero? str-len)
	   (assertion-violation '%xsubbytevector-u8 "cannot replicate empty (sub)bytevector-u8"))
	  ((= 1 str-len)
	   (make-bytevector result-len (bytevector-u8-ref str start)))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from str-len)) (floor (/ to str-len)))
	   (subbytevector-u8 str
		      (+ start (mod from str-len))
		      (+ start (mod to   str-len))))

	  ;; Selected text requires multiple spans.
	  (else
	   (let ((result (make-bytevector result-len)))
	     (%multispan-repcopy! from to result 0 str start past)
	     result)))))

(define (%bytevector-u8-xcopy! from to
			dst-str dst-start dst-past
			src-str src-start src-past)
  (let* ((tocopy	(- to from))
	 (tend		(+ dst-start tocopy))
	 (str-len	(- src-past src-start)))
    (cond ((zero? tocopy))
	  ((zero? str-len)
	   (assertion-violation '%bytevector-u8-xcopy! "cannot replicate empty (sub)bytevector-u8"))

	  ((= 1 str-len)
	   (%bytevector-u8-fill*! dst-str (bytevector-u8-ref src-str src-start) dst-start dst-past))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from str-len)) (floor (/ to str-len)))
	   (%bytevector-u8-copy*! dst-str dst-start src-str
			  (+ src-start (mod from str-len))
			  (+ src-start (mod to   str-len))))

	  (else
	   (%multispan-repcopy! from to dst-str dst-start src-str src-start src-past)))))

(define (%multispan-repcopy! from to dst-str dst-start src-str src-start src-past)
  ;;This  is the  core  copying loop  for  XSUBBYTEVECTOR-U8 and  BYTEVECTOR-U8-XCOPY!
  ;;Internal -- not exported, no careful arg checking.
  (let* ((str-len	(- src-past src-start))
	 (i0		(+ src-start (mod from str-len)))
	 (total-chars	(- to from)))

    ;; Copy the partial span @ the beginning
    (%bytevector-u8-copy*! dst-str dst-start src-str i0 src-past)

    (let* ((ncopied (- src-past i0))	   ; We've copied this many.
	   (nleft (- total-chars ncopied)) ; # chars left to copy.
	   (nspans (div nleft str-len)))   ; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ dst-start ncopied) (+ i str-len)) ; Current target index.
	   (nspans nspans (- nspans 1)))	   ; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (%bytevector-u8-copy*! dst-str i src-str src-start (+ src-start (- total-chars (- i dst-start)))))

	(%bytevector-u8-copy*! dst-str i src-str src-start src-past))))) ; Copy a whole span.


;;;; reverse, replace

(define (%bytevector-u8-reverse str start past)
  (let* ((len (- past start))
	 (result (make-bytevector len)))
    (do ((i start (+ i 1))
	 (j (- len 1) (- j 1)))
	((< j 0))
      (bytevector-u8-set! result j (bytevector-u8-ref str i)))
    result))

(define (%bytevector-u8-replace str1 start1 past1 str2 start2 past2)
  (let* ((len1		(bytevector-length str1))
	 (len2		(- past2 start2))
	 (result	(make-bytevector (+ len2 (- len1 (- past1 start1))))))
    (%bytevector-u8-copy*! result 0 str1 0 start1)
    (%bytevector-u8-copy*! result start1 str2 start2 past2)
    (%bytevector-u8-copy*! result (+ start1 len2) str1 past1 len1)
    result))

(define (%bytevector-u8-reverse! str start past)
  (do ((i (- past 1) (- i 1))
       (j start (+ j 1)))
      ((<= i j))
    (let ((ci (bytevector-u8-ref str i)))
      (bytevector-u8-set! str i (bytevector-u8-ref str j))
      (bytevector-u8-set! str j ci))))


;;;; mutating

(define (%bytevector-u8-copy*! dst-str dst-start src-str src-start src-past)
  (when (< (- (bytevector-length dst-str) dst-start)
	   (- src-past src-start))
    (assertion-violation '%bytevector-u8-copy*!
      "not enough room in destination bytevector-u8"))
  (if (> src-start dst-start)
      (do ((i src-start (+ i 1))
	   (j dst-start (+ j 1)))
	  ((>= i src-past))
	(bytevector-u8-set! dst-str j (bytevector-u8-ref src-str i)))
    (do ((i (- src-past 1)                    (- i 1))
	 (j (+ -1 dst-start (- src-past src-start)) (- j 1)))
	((< i src-start))
      (bytevector-u8-set! dst-str j (bytevector-u8-ref src-str i)))))

(define (%bytevector-u8-reverse-copy*! dst-str dst-start src-str src-start src-past)
  (when (< (- (bytevector-length dst-str) dst-start)
	   (- src-past src-start))
    (assertion-violation '%bytevector-u8-reverse-copy*!
      "not enough room in destination bytevector-u8"))
  ;;We  must handle  correctly copying  over  the same  bytevector-u8.  If  the
  ;;source  and  destination bytevector-u8s  are  the  same,  we copy  all  the
  ;;elements  in a  temporary  buffer first;  this  should be  optimised
  ;;someway to reduce to the minimum the size of the buffer.
  (if (eq? src-str dst-str)
      (when (< src-start src-past)
	(let* ((buffer (%bytevector-u8-reverse-copy* src-str src-start src-past)))
	  (%bytevector-u8-copy*! dst-str dst-start buffer 0 (bytevector-length buffer))))
    (do ((i (- src-past 1) (- i 1))
	 (j dst-start (+ j 1)))
	((< i src-start))
      (bytevector-u8-set! dst-str j (bytevector-u8-ref src-str i)))))

(define (%bytevector-u8-fill*! fill-char str start past)
  (do ((i (- past 1) (- i 1)))
      ((< i start))
    (bytevector-u8-set! str i fill-char)))

(define (bytevector-u8-swap! str i j)
  (when (zero? (bytevector-length str))
    (assertion-violation 'bytevector-u8-swap!
      "attempt to swap elements in an empty bytevector-u8"))
  (when (not (= i j))
    (let ((x (bytevector-u8-ref str i)))
      (bytevector-u8-set! str i (bytevector-u8-ref str j))
      (bytevector-u8-set! str j x))))


;;;; done

)

;;; end of file
