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
(library (strings-low)
  (export

    ;; predicates
    string-null?
    %string-every %string-any

    ;; constructors

    ;; mapping
    %string-map %string-map!
    %string-for-each* %string-for-each-index

    ;; folding and unfolding
    %string-fold %string-fold-right
    string-unfold string-unfold-right
    string-tabulate

    ;; prefix and suffix
    %string-prefix-length %string-suffix-length
    %string-prefix-length-ci %string-suffix-length-ci
    %string-prefix? %string-suffix?
    %string-prefix-ci? %string-suffix-ci?

    )
  (import (rnrs)
    (rnrs mutable-strings)
    (char-sets))


;;;; predicates

(define-syntax string-null?
  (syntax-rules ()
    ((_ ?str)
     (zero? (string-length ?str)))))

(define (%string-every criterion str beg end)
  (and (< beg end)
       (cond ((char? criterion)
	      (let loop ((i beg))
		(or (<= end i)
		    (and (char=? criterion (string-ref str i))
			 (loop (+ 1 i))))))

	     ((char-set? criterion)
	      (let loop ((i beg))
		(or (<= end i)
		    (and (char-set-contains? criterion (string-ref str i))
			 (loop (+ 1 i))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i beg))    ; final (PRED S[END-1]) call
		(let ((c (string-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 end)
		      (criterion c) ; Tail call.
		    (and (criterion c) (loop i1))))))

	     (else
	      (assertion-violation 'string-every
		"expected char-set, char, or predicate as second parameter"
		criterion)))))

(define (%string-any criterion str beg end)
  (and (< beg end)
       (cond ((char? criterion)
	      (let loop ((i beg))
		(and (< i end)
		     (or (char=? criterion (string-ref str i))
			 (loop (+ i 1))))))

	     ((char-set? criterion)
	      (let loop ((i beg))
		(and (< i end)
		     (or (char-set-contains? criterion (string-ref str i))
			 (loop (+ i 1))))))

	     ((procedure? criterion) ; Slightly funky loop so that
	      (let loop ((i beg))    ; final (PRED S[END-1]) call
		(let ((c (string-ref str i)) ; is a tail call.
		      (i1 (+ i 1)))
		  (if (= i1 end)
		      (criterion c) ; Tail call.
		    (or (criterion c) (loop i1))))))

	     (else
	      (assertion-violation 'string-any
		"expected char-set, char, or predicate as second parameter"
		criterion)))))


;;;; mapping

(define (%string-map proc str beg end)
  (do ((i 0 (+ 1 i))
       (j 0 (+ 1 j))
       (result (make-string (- end beg))))
      ((>= i end)
       result)
    (string-set! result j (proc (string-ref str i)))))

(define (%string-map! proc str beg end)
  (do ((i 0 (+ 1 i))
       (j 0 (+ 1 j)))
      ((>= i end)
       str)
    (string-set! str j (proc (string-ref str i)))))

(define (%string-for-each* proc str beg end)
  (let loop ((i 0))
    (when (< i end)
      (proc (string-ref str i))
      (loop (+ i 1)))))

(define (%string-for-each-index proc str beg end)
  (let loop ((i 0))
    (when (< i end)
      (proc i)
      (loop (+ i 1)))))


;;;; folding

(define (%string-fold kons knil str beg end)
  (let loop ((v knil)
	     (i beg))
    (if (< i end)
	(loop (kons (string-ref str i) v) (+ i 1))
      v)))

(define (%string-fold-right kons knil str beg end)
  (let loop ((v knil)
	     (i (- end 1)))
    (if (>= i beg)
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
    ;;starting with 40 and levelling out at 4k, i.e.
    ;;
    ;;	40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
    ;;
    ;;This should  work pretty  well for short  strings, 1-line  (80 char)
    ;;strings, and  longer ones. When  done, we allocate an  answer string
    ;;and copy the chars over from the chunk buffers.
    (let lp ((chunks '())		; Previously filled chunks
	     (nchars 0)			; Number of chars in CHUNKS
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
	    (%string-copy! ans j final 0 flen) ; Install FINAL.
	    (let ((j (- j i)))
	      (%string-copy! ans j chunk 0 i) ; Install CHUNK[0,I).
	      (let lp ((j j) (chunks chunks)) ; Install CHUNKS.
		(if (pair? chunks)
		    (let* ((chunk  (car chunks))
			   (chunks (cdr chunks))
			   (chunk-len (string-length chunk))
			   (j (- j chunk-len)))
		      (%string-copy! ans j chunk 0 chunk-len)
		      (lp j chunks)))))
	    (%string-copy! ans 0 base 0 base-len) ; Install BASE.
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
	    (%string-copy! ans 0 final 0 flen) ; Install FINAL.
	    (%string-copy! ans flen chunk i chunk-len) ; Install CHUNK[I,).
	    (let lp ((j (+ flen chunk-used))	       ; Install CHUNKS.
		     (chunks chunks))
	      (if (pair? chunks)
		  (let* ((chunk  (car chunks))
			 (chunks (cdr chunks))
			 (chunk-len (string-length chunk)))
		    (%string-copy! ans j chunk 0 chunk-len)
		    (lp (+ j chunk-len) chunks))
		(%string-copy! ans j base 0 base-len)))	; Install BASE.
	    ans)))))))

(define (string-tabulate proc len)
  (let ((s (make-string len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0))
      (string-set! s i (proc i)))
    s))



;;;; prefix and suffix

(define (%string-prefix-length str1 beg1 end1 str2 beg2 end2)
  ;;Find the length  of the common prefix.  It is  not required that the
  ;;two substrings passed be of equal length.
  (let* ((delta (min (- end1 beg1) (- end2 beg2)))
	 (end1 (+ beg1 delta)))
    (if (and (eq? str1 str2) (= beg1 beg2)) ; EQ fast path
	delta
      (let lp ((i beg1) (j beg2)) ; Regular path
	(if (or (>= i end1)
		(not (char=? (string-ref str1 i)
			     (string-ref str2 j))))
	    (- i beg1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%string-suffix-length str1 beg1 end1 str2 beg2 end2)
  ;;Find the length  of the common suffix.  It is  not required that the
  ;;two substrings passed be of equal length.
  (let* ((delta (min (- end1 beg1) (- end2 beg2)))
	 (beg1 (- end1 delta)))
    (if (and (eq? str1 str2) (= end1 end2))	; EQ fast path
	delta
      (let lp ((i (- end1 1)) (j (- end2 1))) ; Regular path
	(if (or (< i beg1)
		(not (char=? (string-ref str1 i)
			     (string-ref str2 j))))
	    (- (- end1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2)
  ;;Find the length of the  common prefix, case insensitive version.  It
  ;;is not required that the two substrings passed be of equal length.
  (let* ((delta (min (- end1 beg1) (- end2 beg2)))
	 (end1 (+ beg1 delta)))
    (if (and (eq? str1 str2) (= beg1 beg2)) ; EQ fast path
	delta
      (let lp ((i beg1) (j beg2)) ; Regular path
	(if (or (>= i end1)
		(not (char-ci=? (string-ref str1 i)
				(string-ref str2 j))))
	    (- i beg1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2)
  ;;Find the length of the  common suffix, case insensitive version.  It
  ;;is not required that the two substrings passed be of equal length.
  (let* ((delta (min (- end1 beg1) (- end2 beg2)))
	 (beg1 (- end1 delta)))
    (if (and (eq? str1 str2) (= end1 end2))	; EQ fast path
	delta
      (let lp ((i (- end1 1)) (j (- end2 1))) ; Regular path
	(if (or (< i beg1)
		(not (char-ci=? (string-ref str1 i)
				(string-ref str2 j))))
	    (- (- end1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%string-prefix? str1 beg1 end1 str2 beg2 end2)
  (let ((len1 (- end1 beg1)))
    (and (<= len1 (- end2 beg2)) ; Quick check
	 (= len1 (%string-prefix-length str1 beg1 end1
					str2 beg2 end2)))))

(define (%string-suffix? str1 beg1 end1 str2 beg2 end2)
  (let ((len1 (- end1 beg1)))
    (and (<= len1 (- end2 beg2)) ; Quick check
	 (= len1 (%string-suffix-length str1 beg1 end1
					str2 beg2 end2)))))

(define (%string-prefix-ci? str1 beg1 end1 str2 beg2 end2)
  (let ((len1 (- end1 beg1)))
    (and (<= len1 (- end2 beg2)) ; Quick check
	 (= len1 (%string-prefix-length-ci str1 beg1 end1
					   str2 beg2 end2)))))

(define (%string-suffix-ci? str1 beg1 end1 str2 beg2 end2)
  (let ((len1 (- end1 beg1)))
    (and (<= len1 (- end2 beg2)) ; Quick check
	 (= len1 (%string-suffix-length-ci str1 beg1 end1
					   str2 beg2 end2)))))


;;;; selecting

(define (%string-copy! dst-str dst-beg src-str src-beg src-end)
  (if (> src-beg dst-beg)
      (do ((i src-beg (+ i 1))
	   (j dst-beg (+ j 1)))
	  ((>= i src-end))
	(string-set! dst-str j (string-ref src-str i)))
    (do ((i (- src-end 1)                    (- i 1))
	 (j (+ -1 dst-beg (- src-end src-beg)) (- j 1)))
	((< i src-beg))
      (string-set! dst-str j (string-ref src-str i)))))



;;;; done

)

;;; end of file
