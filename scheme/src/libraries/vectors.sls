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
    vector= vector<>

    ;; mapping
    vector-map* vector-map*!
    vector-for-each*

    ;; folding
    vector-fold   vector-fold-right
    vector-fold*  vector-fold-right*
    vector-unfold vector-unfold-right
    vector-tabulate

    ;; selecting
    subvector* vector-copy vector-copy*!
    vector-take vector-take-right
    vector-drop vector-drop-right
    vector-trim vector-trim-right vector-trim-both
    vector-pad vector-pad-right

    ;; prefix and suffix
    vector-prefix-length vector-suffix-length
    vector-prefix? vector-suffix?

    ;; searching
    vector-index vector-index-right
    vector-skip vector-skip-right
    vector-count vector-contains
    vector-binary-search

    ;; filtering
    vector-filter vector-delete

    ;; vectors and lists
    vector->list* reverse-list->vector

    ;; replicating
    xsubvector vector-xcopy!

    ;; concatenate, reverse, fill, replace
    vector-append
    vector-concatenate vector-concatenate-reverse
    vector-reverse vector-reverse!
    vector-fill*! vector-replace)
  (import (rnrs)
    (vectors low))



(define-syntax unpack
  (syntax-rules (quote view start past)

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

    ((_ ((quote ?vec)))
     (let ((vec (quote ?vec)))
       (values vec 0 (vector-length vec))))

    ((_ (quote ?vec))
     (let ((vec (quote ?vec)))
       (values vec 0 (vector-length vec))))

    ((_ (?vec))
     (let ((vec ?vec))
       (values vec 0 (vector-length vec))))

    ((_ (?vec ?start))
     (let ((vec ?vec))
       (values vec ?start (vector-length vec))))

    ((_ (?vec ?start ?past))
     (values ?vec ?start ?past))

    ((_ ?vec)
     (let ((vec ?vec))
       (values vec 0 (vector-length vec))))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))


;;;; predicates

(define-syntax vector-null?
  (syntax-rules ()
    ((_ ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-null? vec beg past)))))

(define-syntax vector-every
  (syntax-rules ()
    ((_ ?proc ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-every ?proc vec beg past)))))

(define-syntax vector-any
  (syntax-rules ()
    ((_ ?proc ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-any ?proc vec beg past)))))


;;;; comparison

(define-syntax vector=
  (syntax-rules ()
    ((_ ?proc ?V1 ?V2)
     (let-values (((vec1 start1 end1) (unpack ?V1))
		  ((vec2 start2 end2) (unpack ?V2)))
       (%vector= ?proc vec1 start1 end1 vec2 start2 end2)))))

(define-syntax vector<>
  (syntax-rules ()
    ((_ ?proc ?V1 ?V2)
     (let-values (((vec1 start1 end1) (unpack ?V1))
		  ((vec2 start2 end2) (unpack ?V2)))
       (%vector<> ?proc vec1 start1 end1 vec2 start2 end2)))))


;;;; folding

(define-syntax vector-fold*
  (syntax-rules ()
    ((?F ?kons ?knil ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-fold* ?kons ?knil vec beg past)))))

(define-syntax vector-fold-right*
  (syntax-rules ()
    ((?F ?kons ?knil ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-fold-right* ?kons ?knil vec beg past)))))


;;;; selecting

(define-syntax subvector*
  (syntax-rules ()
    ((_ ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%subvector vec beg past)))))

(define-syntax vector-copy
  (syntax-rules ()
    ((_ ?V)
     (vector-copy ?V #f))
    ((_ ?V ?fill)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-copy ?fill vec beg past)))))

(define-syntax vector-copy*!
  (syntax-rules ()
    ((_ ?V1 ?V2)
     (let-values (((vec1 beg1 past1) (unpack ?V1))
		  ((vec2 beg2 past2) (unpack ?V2)))
       (%vector-copy*! vec1 beg1 vec2 beg2 past2)))))

(define-syntax vector-take
  (syntax-rules ()
    ((_ ?V nchars)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-take nchars vec beg past)))))

(define-syntax vector-take-right
  (syntax-rules ()
    ((_ ?V nchars)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-take-right nchars vec beg past)))))

(define-syntax vector-drop
  (syntax-rules ()
    ((_ ?V nchars)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-drop nchars vec beg past)))))

(define-syntax vector-drop-right
  (syntax-rules ()
    ((_ ?V nchars)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-drop-right nchars vec beg past)))))

(define-syntax vector-trim
  (syntax-rules ()
    ((_ ?V criterion)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-trim criterion vec beg past)))))

(define-syntax vector-trim-right
  (syntax-rules ()
    ((_ ?V criterion)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-trim-right criterion vec beg past)))))

(define-syntax vector-trim-both
  (syntax-rules ()
    ((_ ?V criterion)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-trim-both criterion vec beg past)))))

(define-syntax vector-pad
  (syntax-rules ()
    ((_ ?V ?len)
     (vector-pad ?V ?len #\space))
    ((_ ?V ?len ?char)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-pad ?len ?char vec beg past)))))

(define-syntax vector-pad-right
  (syntax-rules ()
    ((_ ?V ?len)
     (vector-pad-right ?V ?len #\space))
    ((_ ?V ?len ?char)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-pad-right ?len ?char vec beg past)))))


;;;; prefix and suffix

(define-syntax vector-prefix-length
  (syntax-rules ()
    ((_ ?V1 ?V2 ?pred)
     (let-values (((vec1 beg1 past1) (unpack ?V1))
		  ((vec2 beg2 past2) (unpack ?V2)))
       (%vector-prefix-length ?pred vec1 beg1 past1 vec2 beg2 past2)))))

(define-syntax vector-suffix-length
  (syntax-rules ()
    ((_ ?V1 ?V2 ?pred)
     (let-values (((vec1 beg1 past1) (unpack ?V1))
		  ((vec2 beg2 past2) (unpack ?V2)))
       (%vector-suffix-length ?pred vec1 beg1 past1 vec2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax vector-prefix?
  (syntax-rules ()
    ((_ ?V1 ?V2 ?pred)
     (let-values (((vec1 beg1 past1) (unpack ?V1))
		  ((vec2 beg2 past2) (unpack ?V2)))
       (%vector-prefix? ?pred vec1 beg1 past1 vec2 beg2 past2)))))

(define-syntax vector-suffix?
  (syntax-rules ()
    ((_ ?V1 ?V2 ?pred)
     (let-values (((vec1 beg1 past1) (unpack ?V1))
		  ((vec2 beg2 past2) (unpack ?V2)))
       (%vector-suffix? ?pred vec1 beg1 past1 vec2 beg2 past2)))))


;;;; searching

(define-syntax vector-index
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-index ?proc vec beg past)))))

(define-syntax vector-index-right
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-index-right ?proc vec beg past)))))

(define-syntax vector-skip
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-skip ?proc vec beg past)))))

(define-syntax vector-skip-right
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-skip-right ?proc vec beg past)))))

(define-syntax vector-count
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-count ?proc vec beg past)))))

(define-syntax vector-contains
  (syntax-rules ()
    ((_ ?V1 ?V2 ?pred)
     (let-values (((vec1 start1 end1) (unpack ?V1))
		  ((vec2 start2 end2) (unpack ?V2)))
       (%vector-contains ?pred vec1 start1 end1 vec2 start2 end2)))))


;;;; filtering

(define-syntax vector-delete
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-delete ?proc vec beg past)))))

(define-syntax vector-filter
  (syntax-rules ()
    ((_ ?V ?proc)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-filter ?proc vec beg past)))))


;;;; vectors and lists

(define-syntax vector->list*
  (syntax-rules ()
    ((_ ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector->list* vec beg past)))))


;;; replicating

(define-syntax xsubvector
  (syntax-rules ()
    ((_ ?V ?from ?to)
     (let-values (((vec beg past) (unpack ?V)))
       (%xsubvector ?from ?to vec beg past)))))

(define-syntax vector-xcopy!
  (syntax-rules ()
    ((_ ?T ?V ?from ?to)
     (let-values (((vec1 beg1 past1) (unpack ?T))
		  ((vec2 beg2 past2) (unpack ?V)))
       (%vector-xcopy! ?from ?to vec1 beg1 past1 vec2 beg2 past2)))))


;;; concatenate, reverse, replace, fill

(define vector-concatenate-reverse
  (case-lambda

   ((vector-list)
    (%vector-concatenate-reverse vector-list '#() 0))

   ((vector-list final)
    (%vector-concatenate-reverse vector-list final (vector-length final)))

   ((vector-list final past)
    (%vector-concatenate-reverse vector-list final past))))

(define-syntax vector-replace
  (syntax-rules ()
    ((_ ?V1 ?V2)
     (let-values (((vec1 start1 past1) (unpack ?V1))
		  ((vec2 start2 past2) (unpack ?V2)))
       (%vector-replace vec1 start1 past1 vec2 start2 past2)))))

(define-syntax vector-reverse
  (syntax-rules ()
    ((_ ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-reverse vec beg past)))))

(define-syntax vector-reverse!
  (syntax-rules ()
    ((_ ?V)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-reverse! vec beg past)))))

(define-syntax vector-fill*!
  (syntax-rules ()
    ((_ ?V ?fill-char)
     (let-values (((vec beg past) (unpack ?V)))
       (%vector-fill*! ?fill-char vec beg past)))))


;;;; done

)

;;; end of file
