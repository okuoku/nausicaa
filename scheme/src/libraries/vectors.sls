;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Derived from the SRFI 13 (strings list) reference implementation.
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



#!r6rs
(library (vectors)
  (export

    ;; predicates
    vector-null? vector-every vector-any

    ;; comparison
    vector-compare vector-compare-ci
    vector=    vector<    vector>    vector<=    vector>=    vector<>
    vector-ci= vector-ci< vector-ci> vector-ci<= vector-ci>= vector-ci<>

    ;; mapping
    vector-map vector-map!
    vector-for-each* vector-for-each-index

    ;; case
    vector-downcase* vector-upcase* vector-titlecase*
    vector-downcase*! vector-upcase*! vector-titlecase*!

    ;; folding
    vector-fold       vector-unfold
    vector-fold-right vector-unfold-right
    vector-tabulate

    ;; selecting
    subvector* vector-copy*!
    vector-take vector-take-right
    vector-drop vector-drop-right
    vector-trim vector-trim-right vector-trim-both
    vector-pad vector-pad-right

    ;; prefix and suffix
    vector-prefix-length vector-prefix-length-ci
    vector-suffix-length vector-suffix-length-ci
    vector-prefix? vector-prefix-ci?
    vector-suffix? vector-suffix-ci?

    ;; searching
    vector-index vector-index-right
    vector-skip  vector-skip-right
    vector-count
    vector-contains vector-contains-ci

    ;; filtering
    vector-filter vector-delete

    ;; vectors and lists
    vector->list* reverse-list->vector
    vector-join
    vector-tokenize (rename (vector-tokenize vector-tokenise))

    ;; replicating
    xsubvector vector-xcopy!

    ;; concatenate, reverse, fill, replace
    vector-concatenate vector-concatenate-reverse
    vector-reverse vector-reverse!
    vector-fill*! vector-replace)
  (import (rnrs)
    (vectors low))



(define-syntax unpack
  (syntax-rules (view start past)

    ((_ (view ?vec))
     (let ((vec ?vec))
       (values vec 0 (vector-length vec))))

    ((_ (view ?vec (start ?start)))
     (let ((vec ?vec))
       (values vec ?start (vector-length vec))))

    ((_ (view ?vec (past ?past)))
     (values ?vec 0 ?past))

    ((_ (view ?vec (start ?start) (past ?past)))
     (values ?vec ?start ?past))

    ((_ (?vec))
     (let ((vec ?vec))
       (values ?vec 0 (vector-length ?vec))))

    ((_ (?vec ?start))
     (let ((vec ?vec))
       (values ?vec ?start (vector-length ?vec))))

    ((_ (?vec ?start ?past))
     (values ?vec ?start ?past))

    ((?F ?vec)
     (unpack (?vec)))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))


;;;; predicates

(define-syntax vector-every
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-every ?proc str beg past)))))

(define-syntax vector-any
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-any ?proc str beg past)))))


;;;; comparison

(define-syntax vector-compare
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-compare str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

(define-syntax vector-compare-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-compare-ci str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

;;; --------------------------------------------------------------------

(define-syntax vector=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector= str1 start1 end1 str2 start2 end2)))))

(define-syntax vector<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector<> str1 start1 end1 str2 start2 end2)))))

(define-syntax vector<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector< str1 start1 end1 str2 start2 end2)))))

(define-syntax vector>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector> str1 start1 end1 str2 start2 end2)))))

(define-syntax vector<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector<= str1 start1 end1 str2 start2 end2)))))

(define-syntax vector>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector>= str1 start1 end1 str2 start2 end2)))))

;;; --------------------------------------------------------------------

(define-syntax vector-ci=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci= str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-ci<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci<> str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-ci<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci< str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-ci>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci> str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-ci<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci<= str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-ci>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-ci>= str1 start1 end1 str2 start2 end2)))))


;;;; mapping

(define-syntax vector-map
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map ?proc str beg past)))))

(define-syntax vector-map!
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map! ?proc str beg past)))))

(define-syntax vector-for-each*
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-for-each* ?proc str beg past)))))

(define-syntax vector-for-each-index
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-for-each-index ?proc str beg past)))))


;;;; case hacking

(define-syntax vector-upcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map char-upcase str beg past)))))

(define-syntax vector-upcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map! char-upcase str beg past)))))

(define-syntax vector-downcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map char-downcase str beg past)))))

(define-syntax vector-downcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-map! char-downcase str beg past)))))

(define-syntax vector-titlecase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (let ((ans (subvector str beg past)))
	 (%vector-titlecase*! ans 0 (- past beg))
	 ans)))))

(define-syntax vector-titlecase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-titlecase*! str beg past)))))


;;;; folding

(define-syntax vector-fold
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-fold ?kons ?knil str beg past)))))

(define-syntax vector-fold-right
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-fold-right ?kons ?knil str beg past)))))


;;;; selecting

(define-syntax subvector*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (subvector str beg past)))))

(define-syntax vector-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax vector-take
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-take nchars str beg past)))))

(define-syntax vector-take-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-take-right nchars str beg past)))))

(define-syntax vector-drop
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-drop nchars str beg past)))))

(define-syntax vector-drop-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-drop-right nchars str beg past)))))

(define-syntax vector-trim
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-trim criterion str beg past)))))

(define-syntax vector-trim-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-trim-right criterion str beg past)))))

(define-syntax vector-trim-both
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-trim-both criterion str beg past)))))

(define-syntax vector-pad
  (syntax-rules ()
    ((_ ?S ?len)
     (vector-pad ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-pad ?len ?char str beg past)))))

(define-syntax vector-pad-right
  (syntax-rules ()
    ((_ ?S ?len)
     (vector-pad-right ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-pad-right ?len ?char str beg past)))))


;;;; prefix and suffix

(define-syntax vector-prefix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-prefix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-suffix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-suffix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-prefix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-prefix-length-ci str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-suffix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-suffix-length-ci str1 beg1 past1 str2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax vector-prefix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-prefix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-suffix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-suffix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-prefix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-prefix-ci? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax vector-suffix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%vector-suffix-ci? str1 beg1 past1 str2 beg2 past2)))))


;;;; searching

(define-syntax vector-index
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-index criterion str beg past)))))

(define-syntax vector-index-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-index-right criterion str beg past)))))

(define-syntax vector-skip
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-skip criterion str beg past)))))

(define-syntax vector-skip-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-skip-right criterion str beg past)))))

(define-syntax vector-count
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-count criterion str beg past)))))

(define-syntax vector-contains
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-contains str1 start1 end1 str2 start2 end2)))))

(define-syntax vector-contains-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%vector-contains-ci str1 start1 end1 str2 start2 end2)))))


;;;; filtering

(define-syntax vector-delete
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-delete criterion str beg past)))))

(define-syntax vector-filter
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-filter criterion str beg past)))))


;;;; vectors and lists

(define-syntax vector->list*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector->list* str beg past)))))

(define-syntax vector-tokenize
  (syntax-rules ()
    ((_ ?S ?token-set)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-tokenize ?token-set str beg past)))))

(define vector-join
  (case-lambda
   ((vectors)
    (%vector-join vectors " " 'infix))
   ((vectors delim)
    (%vector-join vectors delim 'infix))
   ((vectors delim grammar)
    (%vector-join vectors delim grammar))))


;;; replicating

(define-syntax xsubvector
  (syntax-rules ()
    ((_ ?S ?from ?to)
     (let-values (((str beg past) (unpack ?S)))
       (%xsubvector ?from ?to str beg past)))))

(define-syntax vector-xcopy!
  (syntax-rules ()
    ((_ ?T ?S ?from ?to)
     (let-values (((str1 beg1 past1) (unpack ?T))
		  ((str2 beg2 past2) (unpack ?S)))
       (%vector-xcopy! ?from ?to str1 beg1 past1 str2 beg2 past2)))))


;;; concatenate, reverse, replace, fill

(define vector-concatenate-reverse
  (case-lambda

   ((vector-list)
    (%vector-concatenate-reverse vector-list "" 0))

   ((vector-list final)
    (%vector-concatenate-reverse vector-list final (vector-length final)))

   ((vector-list final past)
    (%vector-concatenate-reverse vector-list final past))))

(define-syntax vector-replace
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 past1) (unpack ?S1))
		  ((str2 start2 past2) (unpack ?S2)))
       (%vector-replace str1 start1 past1 str2 start2 past2)))))

(define-syntax vector-reverse
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-reverse str beg past)))))

(define-syntax vector-reverse!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-reverse! str beg past)))))

(define-syntax vector-fill*!
  (syntax-rules ()
    ((_ ?S ?fill-char)
     (let-values (((str beg past) (unpack ?S)))
       (%vector-fill*! ?fill-char str beg past)))))


;;;; done

)

;;; end of file
