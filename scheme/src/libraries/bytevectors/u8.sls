;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: bytevectors functions
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (bytevectors u8)
  (export

    ;; auxiliary syntaxes
    view start past

    ;; constructors
    bytevector-u8-concatenate  bytevector-u8-concatenate-reverse  bytevector-u8-tabulate
    subbytevector-u8 bytevector-u8-append

    ;; predicates
    bytevector-u8-null?  bytevector-u8-every  bytevector-u8-any

    ;; comparison
    bytevector-u8-compare  bytevector-u8-compare-ci
    bytevector-u8=  bytevector-u8<>  bytevector-u8-ci=  bytevector-u8-ci<>
    bytevector-u8<  bytevector-u8<=  bytevector-u8-ci<  bytevector-u8-ci<=
    bytevector-u8>  bytevector-u8>=  bytevector-u8-ci>  bytevector-u8-ci>=

    ;; mapping
    bytevector-u8-map     bytevector-u8-map!
    bytevector-u8-map*    bytevector-u8-map*!    bytevector-u8-for-each*
    subbytevector-u8-map  subbytevector-u8-map!  subbytevector-u8-for-each  subbytevector-u8-for-each-index

    ;; case
    bytevector-u8-downcase*   bytevector-u8-upcase*   bytevector-u8-titlecase*
    bytevector-u8-downcase*!  bytevector-u8-upcase*!  bytevector-u8-titlecase*!

    ;; folding
    bytevector-u8-fold-left		bytevector-u8-fold-right
    bytevector-u8-fold-left*		bytevector-u8-fold-right*
    subbytevector-u8-fold-left		subbytevector-u8-fold-right
    bytevector-u8-unfold		bytevector-u8-unfold-right

    ;; selecting
    subbytevector-u8*
    (rename (subbytevector-u8* bytevector-u8-copy*)) bytevector-u8-reverse-copy*
    bytevector-u8-copy*!  bytevector-u8-reverse-copy*!
    bytevector-u8-take    bytevector-u8-take-right
    bytevector-u8-drop    bytevector-u8-drop-right

    ;; padding and trimming
    bytevector-u8-trim  bytevector-u8-trim-right  bytevector-u8-trim-both
    bytevector-u8-pad   bytevector-u8-pad-right

    ;; prefix and suffix
    bytevector-u8-prefix-length  bytevector-u8-prefix-length-ci
    bytevector-u8-suffix-length  bytevector-u8-suffix-length-ci
    bytevector-u8-prefix?        bytevector-u8-prefix-ci?
    bytevector-u8-suffix?        bytevector-u8-suffix-ci?

    ;; searching
    bytevector-u8-index     bytevector-u8-index-right
    bytevector-u8-skip      bytevector-u8-skip-right
    bytevector-u8-contains  bytevector-u8-contains-ci
    bytevector-u8-count

    ;; filtering
    bytevector-u8-filter bytevector-u8-delete

    ;; lists
    bytevector->u8-list*  reverse-u8-list->bytevector
    bytevector-u8-join    bytevector-u8-tokenize
    (rename (bytevector-u8-tokenize bytevector-u8-tokenise))

    ;; replicating
    xsubbytevector-u8  bytevector-u8-xcopy!

    ;; mutating
    bytevector-u8-fill*! bytevector-u8-swap!

    ;; reverse and replace
    bytevector-u8-reverse  bytevector-u8-reverse!
    bytevector-u8-replace

    (rename (unpack %bytevector-u8-unpack)))
  (import (rnrs)
    (asciis)
    (bytevectors u8low)
    (auxiliary-syntaxes))


(define-syntax unpack
  (syntax-rules (view start past)

    ((_ (view ?str))
     (let ((str ?str))
       (values str 0 (bytevector-length str))))

    ((_ (view ?str (start ?start)))
     (let ((str ?str))
       (values str ?start (bytevector-length str))))

    ((_ (view ?str (past ?past)))
     (values ?str 0 ?past))

    ((_ (view ?str (start ?start) (past ?past)))
     (values ?str ?start ?past))

    ((?F ?str)
     (let ((str ?str))
       (values str 0 (bytevector-length str))))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))


;;;; predicates

(define-syntax bytevector-u8-every
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-every ?proc str beg past)))))

(define-syntax bytevector-u8-any
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-any ?proc str beg past)))))


;;;; comparison

(define-syntax bytevector-u8-compare
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-compare str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

(define-syntax bytevector-u8-compare-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-compare-ci str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

;;; --------------------------------------------------------------------

(define-syntax bytevector-u8=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8= str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8<> str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8< str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8> str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8<= str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8>= str1 start1 end1 str2 start2 end2)))))

;;; --------------------------------------------------------------------

(define-syntax bytevector-u8-ci=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci= str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-ci<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci<> str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-ci<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci< str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-ci>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci> str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-ci<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci<= str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-ci>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-ci>= str1 start1 end1 str2 start2 end2)))))


;;;; mapping

(define-syntax subbytevector-u8-map
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map ?proc str beg past)))))

(define-syntax subbytevector-u8-map!
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map! ?proc str beg past)))))

(define-syntax subbytevector-u8-for-each
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-for-each ?proc str beg past)))))

(define-syntax subbytevector-u8-for-each-index
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-for-each-index ?proc str beg past)))))


;;;; case hacking

(define-syntax bytevector-u8-upcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map ascii-upcase str beg past)))))

(define-syntax bytevector-u8-upcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map! ascii-upcase str beg past)))))

(define-syntax bytevector-u8-downcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map ascii-downcase str beg past)))))

(define-syntax bytevector-u8-downcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-map! ascii-downcase str beg past)))))

(define-syntax bytevector-u8-titlecase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (let ((ans (subbytevector-u8 str beg past)))
	 (%bytevector-u8-titlecase*! ans 0 (- past beg))
	 ans)))))

(define-syntax bytevector-u8-titlecase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-titlecase*! str beg past)))))


;;;; folding

(define-syntax subbytevector-u8-fold-left
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-fold-left ?kons ?knil str beg past)))))

(define-syntax subbytevector-u8-fold-right
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subbytevector-u8-fold-right ?kons ?knil str beg past)))))


;;;; selecting

(define-syntax subbytevector-u8*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (subbytevector-u8 str beg past)))))

(define-syntax bytevector-u8-reverse-copy*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-reverse-copy* str beg past)))))

(define-syntax bytevector-u8-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax bytevector-u8-reverse-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-reverse-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax bytevector-u8-take
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-take nchars str beg past)))))

(define-syntax bytevector-u8-take-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-take-right nchars str beg past)))))

(define-syntax bytevector-u8-drop
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-drop nchars str beg past)))))

(define-syntax bytevector-u8-drop-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-drop-right nchars str beg past)))))

(define-syntax bytevector-u8-trim
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-trim criterion str beg past)))))

(define-syntax bytevector-u8-trim-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-trim-right criterion str beg past)))))

(define-syntax bytevector-u8-trim-both
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-trim-both criterion str beg past)))))

(define $int-space (char->integer #\space))

(define-syntax bytevector-u8-pad
  (syntax-rules ()
    ((_ ?S ?len)
     (bytevector-u8-pad ?S ?len $int-space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-pad ?len ?char str beg past)))))

(define-syntax bytevector-u8-pad-right
  (syntax-rules ()
    ((_ ?S ?len)
     (bytevector-u8-pad-right ?S ?len $int-space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-pad-right ?len ?char str beg past)))))


;;;; prefix and suffix

(define-syntax bytevector-u8-prefix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-prefix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-suffix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-suffix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-prefix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-prefix-length-ci str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-suffix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-suffix-length-ci str1 beg1 past1 str2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax bytevector-u8-prefix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-prefix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-suffix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-suffix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-prefix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-prefix-ci? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax bytevector-u8-suffix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%bytevector-u8-suffix-ci? str1 beg1 past1 str2 beg2 past2)))))


;;;; searching

(define-syntax bytevector-u8-index
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-index criterion str beg past)))))

(define-syntax bytevector-u8-index-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-index-right criterion str beg past)))))

(define-syntax bytevector-u8-skip
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-skip criterion str beg past)))))

(define-syntax bytevector-u8-skip-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-skip-right criterion str beg past)))))

(define-syntax bytevector-u8-count
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-count criterion str beg past)))))

(define-syntax bytevector-u8-contains
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-contains str1 start1 end1 str2 start2 end2)))))

(define-syntax bytevector-u8-contains-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%bytevector-u8-contains-ci str1 start1 end1 str2 start2 end2)))))


;;;; filtering

(define-syntax bytevector-u8-delete
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-delete criterion str beg past)))))

(define-syntax bytevector-u8-filter
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-filter criterion str beg past)))))


;;;; bytevector-u8s and lists

(define-syntax bytevector->u8-list*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector->u8-list* str beg past)))))

(define-syntax reverse-bytevector->u8-list
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%reverse-bytevector->u8-list str beg past)))))

(define-syntax bytevector-u8-tokenize
  (syntax-rules ()
    ((_ ?S ?token-set)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-tokenize ?token-set str beg past)))))

(define bytevector-u8-join
  (case-lambda
   ((bytevector-u8s)
    (%bytevector-u8-join bytevector-u8s " " 'infix))
   ((bytevector-u8s delim)
    (%bytevector-u8-join bytevector-u8s delim 'infix))
   ((bytevector-u8s delim grammar)
    (%bytevector-u8-join bytevector-u8s delim grammar))))


;;; replicating

(define-syntax xsubbytevector-u8
  (syntax-rules ()
    ((_ ?S ?from ?to)
     (let-values (((str beg past) (unpack ?S)))
       (%xsubbytevector-u8 ?from ?to str beg past)))))

(define-syntax bytevector-u8-xcopy!
  (syntax-rules ()
    ((_ ?T ?S ?from ?to)
     (let-values (((str1 beg1 past1) (unpack ?T))
		  ((str2 beg2 past2) (unpack ?S)))
       (%bytevector-u8-xcopy! ?from ?to str1 beg1 past1 str2 beg2 past2)))))


;;; concatenate, reverse, replace, fill

(define bytevector-u8-concatenate-reverse
  (case-lambda

   ((bytevector-u8-list)
    (%bytevector-u8-concatenate-reverse bytevector-u8-list '#vu8() 0))

   ((bytevector-u8-list final)
    (%bytevector-u8-concatenate-reverse bytevector-u8-list final (bytevector-length final)))

   ((bytevector-u8-list final past)
    (%bytevector-u8-concatenate-reverse bytevector-u8-list final past))))

(define-syntax bytevector-u8-replace
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 past1) (unpack ?S1))
		  ((str2 start2 past2) (unpack ?S2)))
       (%bytevector-u8-replace str1 start1 past1 str2 start2 past2)))))

(define-syntax bytevector-u8-reverse
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-reverse str beg past)))))

(define-syntax bytevector-u8-reverse!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-reverse! str beg past)))))

(define-syntax bytevector-u8-fill*!
  (syntax-rules ()
    ((_ ?S ?fill-char)
     (let-values (((str beg past) (unpack ?S)))
       (%bytevector-u8-fill*! ?fill-char str beg past)))))


;;;; done

)

;;; end of file
