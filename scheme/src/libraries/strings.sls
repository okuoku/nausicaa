;;; strings library --
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
(library (strings)
  (export

    ;; predicates
    string-every string-any string-null?

    ;; constructors

    ;; mapping
    string-map string-map!
    string-for-each string-for-each-index

    ;; folding
    string-fold       string-unfold
    string-fold-right string-unfold-right
    string-tabulate

    ;; comparison
    string-compare string-compare-ci
    string=    string<    string>    string<=    string>=    string<>
    string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>

    ;; case
    string-downcase  string-upcase  string-titlecase
    string-downcase! string-upcase! string-titlecase!

    ;; selection
    string-copy* string-copy!
    string-take string-take-right
    string-drop string-drop-right

    ;; padding and trimming
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-filter string-delete
    string-index string-index-right
    string-skip  string-skip-right

    ;; prefix and suffix
    string-prefix-length string-prefix-length-ci
    string-suffix-length string-suffix-length-ci
    string-prefix? string-prefix-ci?
    string-suffix? string-suffix-ci?

    ;; search
    string-count
    string-contains string-contains-ci

    ;; reverse and append
    string-reverse string-reverse! reverse-list->string
    string-concatenate string-concatenate/shared string-concatenate-reverse

    ;; replicating
    xsubstring string-xcopy!

    ;; string and lists
    string-join string->list*

    string-tokenize
    string-replace
		; R5RS extended:
    string-fill!
		; R5RS re-exports:
    string? make-string string-length string-ref string-set!
    string string-append list->string
		; Low-level routines:
    ;; 	  make-kmp-restart-vector string-kmp-partial-search kmp-step
    ;; 	  string-parse-start+end
    ;; 	  string-parse-final-start+end
    ;; 	  let-string-start+end
    ;; 	  check-substring-spec
    ;; 	  substring-spec-ok?
    )
  (import (except (nausicaa)
		  string-copy string-for-each string->list
		  string-upcase string-downcase string-titlecase string-hash)
    (except (rnrs mutable-strings) string-fill!)
    (rnrs r5rs)
    (char-sets))



(define-syntax unpack
  (syntax-rules ()
    ((_ (?str))
     (let ((str ?str))
       (values ?str 0 (string-length ?str))))

    ((_ (?str ?beg))
     (let ((str ?str))
       (values ?str ?beg (string-length ?str))))

    ((_ (?str ?beg ?past))
     (values ?str ?beg ?past))

    ((?F ?str)
     (?F (?str)))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))


;;;; predicates

(define-syntax string-every
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-every ?proc str beg past)))))

(define-syntax string-any
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-any ?proc str beg past)))))


;;;; mapping

(define-syntax string-map
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map ?proc str beg past)))))

(define-syntax string-map!
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map! ?proc str beg past)))))

(define-syntax string-for-each*
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-for-each* ?proc str beg past)))))

(define-syntax string-for-each-index
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-for-each-index ?proc str beg past)))))


;;;; folding

(define-syntax string-fold
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-fold ?kons ?knil str beg past)))))

(define-syntax string-fold-right
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-fold-right ?kons ?knil str beg past)))))


;;;; prefix and suffix

(define-syntax string-prefix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-prefix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-length-ci str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-length-ci str1 beg1 past1 str2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax string-prefix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-prefix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-ci? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-ci? str1 beg1 past1 str2 beg2 past2)))))


;;;; comparison

(define-syntax string-compare
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-compare str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

(define-syntax string-compare-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-compare-ci str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))


;;;; case hacking

(define-syntax string-upcase
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map char-upcase str beg past)))))

(define-syntax string-upcase!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map! char-upcase str beg past)))))

(define-syntax string-downcase
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map char-downcase str beg past)))))

(define-syntax string-downcase!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-map! char-downcase str beg past)))))

(define-syntax string-titlecase
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (let ((ans (substring str beg past)))
	 (%string-titlecase! ans 0 (- past beg))
	 ans)))))

(define-syntax string-titlecase!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-titlecase! str beg past)))))


;;;; selecting

(define-syntax string-take
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-take nchars str beg past)))))

(define-syntax string-take-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-take-right nchars str beg past)))))

(define-syntax string-drop
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-drop nchars str beg past)))))

(define-syntax string-drop-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-drop-right nchars str beg past)))))

(define-syntax string-trim
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim criterion str beg past)))))

(define-syntax string-trim-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim-right criterion str beg past)))))

(define-syntax string-trim-both
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim-both criterion str beg past)))))

(define-syntax string-pad
  (syntax-rules ()
    ((_ ?S ?len)
     (string-pad ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-pad ?len ?char str beg past)))))

(define-syntax string-pad-right
  (syntax-rules ()
    ((_ ?S ?len)
     (string-pad-right ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-pad-right ?len ?char str beg past)))))

(define-syntax string-copy!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-copy! str1 beg1 str2 beg2 past2)))))


;;;; filtering

(define-syntax string-delete
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-delete criterion str beg past)))))

(define-syntax string-filter
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-filter criterion str beg past)))))


;;;; searching

(define-syntax string-index
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-index criterion str beg past)))))

(define-syntax string-index-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-index-right criterion str beg past)))))

(define-syntax string-skip
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-skip criterion str beg past)))))

(define-syntax string-skip-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-skip-right criterion str beg past)))))

(define-syntax string-count
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-count criterion str beg past)))))

(define-syntax string-contains
  (syntax-rules ()
    ((_ ?S1 ?S2>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-contains str1 start1 end1 str2 start2 end2)))))

(define-syntax string-contains-ci
  (syntax-rules ()
    ((_ ?S1 ?S2>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-contains-ci str1 start1 end1 str2 start2 end2)))))


;;;; filling

(define-syntax string-fill*!
  (syntax-rules ()
    ((_ ?S ?fill-char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-fill*! ?fill-char str beg past)))))


;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (string-null? s)
;;; (string-reverse  s [start end])
;;; (string-reverse! s [start end])
;;; (reverse-list->string clist)
;;; (string->list s [start end])

(define (string-reverse s . maybe-start+end)
  (let-string-start+end (start end) string-reverse s maybe-start+end
    (let* ((len (- end start))
	   (ans (make-string len)))
      (do ((i start (+ i 1))
	   (j (- len 1) (- j 1)))
	  ((< j 0))
	(string-set! ans j (string-ref s i)))
      ans)))

(define (string-reverse! s . maybe-start+end)
  (let-string-start+end (start end) string-reverse! s maybe-start+end
    (do ((i (- end 1) (- i 1))
	 (j start (+ j 1)))
	((<= i j))
      (let ((ci (string-ref s i)))
	(string-set! s i (string-ref s j))
	(string-set! s j ci)))))


(define (reverse-list->string clist)
  (let* ((len (length clist))
	 (s (make-string len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
	((not (pair? clist)))
      (string-set! s i (car clist)))
    s))


;(define (string->list s . maybe-start+end)
;  (apply string-fold-right cons '() s maybe-start+end))

(define (string->list s . maybe-start+end)
  (let-string-start+end (start end) string->list s maybe-start+end
    (do ((i (- end 1) (- i 1))
	 (ans '() (cons (string-ref s i) ans)))
	((< i start) ans))))

;;; Defined by R5RS, so commented out here.
;(define (list->string lis) (string-unfold null? car cdr lis))


;;; string-concatenate        string-list -> string
;;; string-concatenate/shared string-list -> string
;;; string-append/shared s ... -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRING-APPEND/SHARED has license to return a string that shares storage
;;; with any of its arguments. In particular, if there is only one non-empty
;;; string amongst its parameters, it is permitted to return that string as
;;; its result. STRING-APPEND, by contrast, always allocates new storage.
;;;
;;; STRING-CONCATENATE & STRING-CONCATENATE/SHARED are passed a list of
;;; strings, which they concatenate into a result string. STRING-CONCATENATE
;;; always allocates a fresh string; STRING-CONCATENATE/SHARED may (or may
;;; not) return a result that shares storage with any of its arguments. In
;;; particular, if it is applied to a singleton list, it is permitted to
;;; return the car of that list as its value.

(define (string-append/shared . strings) (string-concatenate/shared strings))

(define (string-concatenate/shared strings)
  (let lp ((strings strings) (nchars 0) (first #f))
    (cond ((pair? strings)			; Scan the args, add up total
	   (let* ((string  (car strings))	; length, remember 1st
		  (tail (cdr strings))		; non-empty string.
		  (slen (string-length string)))
	     (if (zero? slen)
		 (lp tail nchars first)
		 (lp tail (+ nchars slen) (or first strings)))))

	  ((zero? nchars) "")

	  ;; Just one non-empty string! Return it.
	  ((= nchars (string-length (car first))) (car first))

	  (else (let ((ans (make-string nchars)))
		  (let lp ((strings first) (i 0))
		    (if (pair? strings)
			(let* ((s (car strings))
			       (slen (string-length s)))
			  (%string-copy! ans i s 0 slen)
			  (lp (cdr strings) (+ i slen)))))
		  ans)))))


; Alas, Scheme 48's APPLY blows up if you have many, many arguments.
;(define (string-concatenate strings) (apply string-append strings))

;;; Here it is written out. I avoid using REDUCE to add up string lengths
;;; to avoid non-R5RS dependencies.
(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
		     (i 0 (+ i (string-length (car strings)))))
		    ((not (pair? strings)) i)))
	 (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
	  (let* ((s (car strings))
		 (slen (string-length s)))
	    (%string-copy! ans i s 0 slen)
	    (lp (+ i slen) (cdr strings)))))
    ans))


;;; Defined by R5RS, so commented out here.
;(define (string-append . strings) (string-concatenate strings))

;;; string-concatenate-reverse        string-list [final-string end] -> string
;;; string-concatenate-reverse/shared string-list [final-string end] -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return
;;;   (string-concatenate
;;;     (reverse
;;;       (cons (substring final-string 0 end) string-list)))

(define (string-concatenate-reverse string-list . maybe-final+end)
  (let-optionals* maybe-final+end ((final "" (string? final))
				   (end (string-length final)
					(and (integer? end)
					     (exact? end)
					     (<= 0 end (string-length final)))))
    (let ((len (let lp ((sum 0) (lis string-list))
		 (if (pair? lis)
		     (lp (+ sum (string-length (car lis))) (cdr lis))
		     sum))))

      (%finish-string-concatenate-reverse len string-list final end))))

(define (string-concatenate-reverse/shared string-list . maybe-final+end)
  (let-optionals* maybe-final+end ((final "" (string? final))
				   (end (string-length final)
					(and (integer? end)
					     (exact? end)
					     (<= 0 end (string-length final)))))
    ;; Add up the lengths of all the strings in STRING-LIST; also get a
    ;; pointer NZLIST into STRING-LIST showing where the first non-zero-length
    ;; string starts.
    (let lp ((len 0) (nzlist #f) (lis string-list))
      (if (pair? lis)
	  (let ((slen (string-length (car lis))))
	    (lp (+ len slen)
		(if (or nzlist (zero? slen)) nzlist lis)
		(cdr lis)))

	  (cond ((zero? len) (substring/shared final 0 end))

		;; LEN > 0, so NZLIST is non-empty.

		((and (zero? end) (= len (string-length (car nzlist))))
		 (car nzlist))

		(else (%finish-string-concatenate-reverse len nzlist final end)))))))

(define (%finish-string-concatenate-reverse len string-list final end)
  (let ((ans (make-string (+ end len))))
    (%string-copy! ans len final 0 end)
    (let lp ((i len) (lis string-list))
      (if (pair? lis)
	  (let* ((s   (car lis))
		 (lis (cdr lis))
		 (slen (string-length s))
		 (i (- i slen)))
	    (%string-copy! ans i s 0 slen)
	    (lp i lis))))
    ans))




;;; string-replace s1 s2 start1 end1 [start2 end2] -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replace S1[START1,END1) with S2[START2,END2).

(define (string-replace s1 s2 start1 end1 . maybe-start+end)
  (check-substring-spec string-replace s1 start1 end1)
  (let-string-start+end (start2 end2) string-replace s2 maybe-start+end
    (let* ((slen1 (string-length s1))
	   (sublen2 (- end2 start2))
	   (alen (+ (- slen1 (- end1 start1)) sublen2))
	   (ans (make-string alen)))
      (%string-copy! ans 0 s1 0 start1)
      (%string-copy! ans start1 s2 start2 end2)
      (%string-copy! ans (+ start1 sublen2) s1 end1 slen1)
      ans)))


;;; string-tokenize s [token-set start end] -> list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Break S up into a list of token strings, where a token is a maximal
;;; non-empty contiguous sequence of chars belonging to TOKEN-SET.
;;; (string-tokenize "hello, world") => ("hello," "world")

(define (string-tokenize s . token-chars+start+end)
  (let-optionals* token-chars+start+end
                  ((token-chars char-set:graphic (char-set? token-chars)) rest)
    (let-string-start+end (start end) string-tokenize s rest
      (let lp ((i end) (ans '()))
	(cond ((and (< start i) (string-index-right s token-chars start i)) =>
	       (lambda (tend-1)
		 (let ((tend (+ 1 tend-1)))
		   (cond ((string-skip-right s token-chars start tend-1) =>
			  (lambda (tstart-1)
			    (lp tstart-1
				(cons (substring s (+ 1 tstart-1) tend)
				      ans))))
			 (else (cons (substring s start tend) ans))))))
	      (else ans))))))


;;; xsubstring s from [to start end] -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; S is a string; START and END are optional arguments that demarcate
;;; a substring of S, defaulting to 0 and the length of S (e.g., the whole
;;; string). Replicate this substring up and down index space, in both the
;;  positive and negative directions. For example, if S = "abcdefg", START=3,
;;; and END=6, then we have the conceptual bidirectionally-infinite string
;;;     ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f ...
;;;     ... -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 ...
;;; XSUBSTRING returns the substring of this string beginning at index FROM,
;;; and ending at TO (which defaults to FROM+(END-START)).
;;;
;;; You can use XSUBSTRING in many ways:
;;; - To rotate a string left:  (xsubstring "abcdef" 2)  => "cdefab"
;;; - To rotate a string right: (xsubstring "abcdef" -2) => "efabcd"
;;; - To replicate a string:    (xsubstring "abc" 0 7) => "abcabca"
;;;
;;; Note that
;;;   - The FROM/TO indices give a half-open range -- the characters from
;;;     index FROM up to, but not including index TO.
;;;   - The FROM/TO indices are not in terms of the index space for string S.
;;;     They are in terms of the replicated index space of the substring
;;;     defined by S, START, and END.
;;;
;;; It is an error if START=END -- although this is allowed by special
;;; dispensation when FROM=TO.

(define (xsubstring s from . maybe-to+start+end)
  (check-arg (lambda (val) (and (integer? val) (exact? val)))
	     from xsubstring)
  (receive (to start end)
           (if (pair? maybe-to+start+end)
	       (let-string-start+end (start end) xsubstring s (cdr maybe-to+start+end)
		 (let ((to (car maybe-to+start+end)))
		   (check-arg (lambda (val) (and (integer? val)
						 (exact? val)
						 (<= from val)))
			      to xsubstring)
		   (values to start end)))
	       (let ((slen (string-length (check-arg string? s xsubstring))))
		 (values (+ from slen) 0 slen)))
    (let ((slen   (- end start))
	  (anslen (- to  from)))
      (cond ((zero? anslen) "")
	    ((zero? slen) (error "Cannot replicate empty (sub)string"
				  xsubstring s from to start end))

	    ((= 1 slen)		; Fast path for 1-char replication.
	     (make-string anslen (string-ref s start)))

	    ;; Selected text falls entirely within one span.
	    ((= (floor (/ from slen)) (floor (/ to slen)))
	     (substring s (+ start (modulo from slen))
			  (+ start (modulo to   slen))))

	    ;; Selected text requires multiple spans.
	    (else (let ((ans (make-string anslen)))
		    (%multispan-repcopy! ans 0 s from to start end)
		    ans))))))


;;; string-xcopy! target tstart s sfrom [sto start end] -> unspecific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exactly the same as xsubstring, but the extracted text is written
;;; into the string TARGET starting at index TSTART.
;;; This operation is not defined if (EQ? TARGET S) -- you cannot copy
;;; a string on top of itself.

(define-syntax string-copy*
  (syntax-rules ()
    ((_ ?str-spec)
     (unpack-1 substring ?str-spec))))

(define (string-xcopy! target tstart s sfrom . maybe-sto+start+end)
  (check-arg (lambda (val) (and (integer? val) (exact? val)))
	     sfrom string-xcopy!)
  (receive (sto start end)
           (if (pair? maybe-sto+start+end)
	       (let-string-start+end (start end) string-xcopy! s (cdr maybe-sto+start+end)
		 (let ((sto (car maybe-sto+start+end)))
		   (check-arg (lambda (val) (and (integer? val) (exact? val)))
			      sto string-xcopy!)
		   (values sto start end)))
	       (let ((slen (string-length s)))
		 (values (+ sfrom slen) 0 slen)))

    (let* ((tocopy (- sto sfrom))
	   (tend (+ tstart tocopy))
	   (slen (- end start)))
      (check-substring-spec string-xcopy! target tstart tend)
      (cond ((zero? tocopy))
	    ((zero? slen) (error "Cannot replicate empty (sub)string"
				 string-xcopy!
				 target tstart s sfrom sto start end))

	    ((= 1 slen)			; Fast path for 1-char replication.
	     (string-fill! target (string-ref s start) tstart tend))

	    ;; Selected text falls entirely within one span.
	    ((= (floor (/ sfrom slen)) (floor (/ sto slen)))
	     (%string-copy! target tstart s
			    (+ start (modulo sfrom slen))
			    (+ start (modulo sto   slen))))

	    ;; Multi-span copy.
	    (else (%multispan-repcopy! target tstart s sfrom sto start end))))))

;;; This is the core copying loop for XSUBSTRING and STRING-XCOPY!
;;; Internal -- not exported, no careful arg checking.
(define (%multispan-repcopy! target tstart s sfrom sto start end)
  (let* ((slen (- end start))
	 (i0 (+ start (modulo sfrom slen)))
	 (total-chars (- sto sfrom)))

    ;; Copy the partial span @ the beginning
    (%string-copy! target tstart s i0 end)

    (let* ((ncopied (- end i0))			; We've copied this many.
	   (nleft (- total-chars ncopied))	; # chars left to copy.
	   (nspans (quotient nleft slen)))	; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ tstart ncopied) (+ i slen))	; Current target index.
	   (nspans nspans (- nspans 1)))	; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (%string-copy! target i s start (+ start (- total-chars (- i tstart)))))

	(%string-copy! target i s start end))))); Copy a whole span.



;;; (string-join string-list [delimiter grammar]) => string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paste strings together using the delimiter string.
;;;
;;; (join-strings '("foo" "bar" "baz") ":") => "foo:bar:baz"
;;;
;;; DELIMITER defaults to a single space " "
;;; GRAMMAR is one of the symbols {prefix, infix, strict-infix, suffix}
;;; and defaults to 'infix.
;;;
;;; I could rewrite this more efficiently -- precompute the length of the
;;; answer string, then allocate & fill it in iteratively. Using
;;; STRING-CONCATENATE is less efficient.

(define string-join
  (case-lambda
   ((strings)
    (string-join strings " " 'infix))
   ((strings delim)
    (string-join strings delim 'infix))
   ((strings delim grammar)
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
	       (assertion-violation 'string-join
		 "illegal join grammar" grammar)))))

	  ((not (null? strings))
	   (error "STRINGS parameter not list." strings string-join))

	  ;; STRINGS is ()

	  ((eq? grammar 'strict-infix)
	   (error "Empty list cannot be joined with STRICT-INFIX grammar."
	     string-join))

	  (else ""))))) ; Special-cased for infix grammar.



;;;; done

)

;;; end of file
