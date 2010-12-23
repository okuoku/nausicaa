;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: label interface to vector functions
;;;Date: Thu Dec 23, 2010
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


#!r6rs
(library (vectors xvectors)
  (export <xvector>)
  (import (nausicaa)
    (vectors))


(define-label <xvector>
  (inherit <vector>)
  (custom-maker vector)
  (predicate vector?)

  ;; constructors
  (method-syntax concatenate
    (syntax-rules ()
      ((_ o ?list-of-vectors)
       (vector-concatenate (cons o ?list-of-vectors)))))

  (method-syntax concatenate-reverse
    (syntax-rules ()
      ((_ o ?list-of-vectors)
       (vector-concatenate-reverse (cons o ?list-of-vectors)))
      ((_ o ?list-of-vectors ?final-vector)
       (vector-concatenate-reverse (cons o ?list-of-vectors) ?final-vector))
      ((_ o ?list-of-vectors ?final-vector ?nvalues)
       (vector-concatenate-reverse (cons o ?list-of-vectors) ?final-vector ?nvalues))
      ))

  (method-syntax append
    (syntax-rules ()
      ((_ o . ?args)
       (vector-append o . ?args))))


  ;; predicates
  (method-syntax null?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-null? o . ?args))))

  (method-syntax subvector-every
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-every o . ?args))))

  (method-syntax every
    (syntax-rules ()
      ((_ o . ?args)
       (vector-every o . ?args))))

  (method-syntax subvector-any
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-any o . ?args))))

  (method-syntax any
    (syntax-rules ()
      ((_ o . ?args)
       (vector-any o . ?args))))

  ;; comparison
  (method-syntax compare
    (syntax-rules ()
      ((_ o . ?args)
       (vector-compare o . ?args))))

  (method-syntax =
    (syntax-rules ()
      ((_ o . ?args)
       (vector= o . ?args))))

  (method-syntax <>
    (syntax-rules ()
      ((_ o . ?args)
       (vector<> o . ?args))))

  (method-syntax <
    (syntax-rules ()
      ((_ o . ?args)
       (vector< o . ?args))))

  (method-syntax <=
    (syntax-rules ()
      ((_ o . ?args)
       (vector<= o . ?args))))

  (method-syntax >
    (syntax-rules ()
      ((_ o . ?args)
       (vector> o . ?args))))

  (method-syntax >=
    (syntax-rules ()
      ((_ o . ?args)
       (vector>= o . ?args))))


  ;; mapping
  (method-syntax map/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map/with-index o . ?args))))

  (method-syntax map!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map! o . ?args))))

  (method-syntax map!/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map!/with-index o . ?args))))

  (method-syntax map*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map* o . ?args))))

  (method-syntax map*/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map*/with-index o . ?args))))

  (method-syntax map*!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map*! o . ?args))))

  (method-syntax map*!/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map*!/with-index o . ?args))))

  (method-syntax for-each*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-for-each* o . ?args))))

  (method-syntax for-each*/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-for-each*/with-index o . ?args))))

  (method-syntax subvector-map
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-map o . ?args))))

  (method-syntax subvector-map/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-map/with-index o . ?args))))

  (method-syntax subvector-map!
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-map! o . ?args))))

  (method-syntax subvector-map!/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-map!/with-index o . ?args))))

  (method-syntax subvector-for-each
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-for-each o . ?args))))

  (method-syntax subvector-for-each/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-for-each/with-index o . ?args))))

  (method-syntax subvector-for-each-index
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-for-each-index o . ?args))))


  (method-syntax map/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map/stx o . ?args))))

  (method-syntax map*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map*/stx o . ?args))))

  (method-syntax map!/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map!/stx o . ?args))))

  (method-syntax map*!/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-map*!/stx o . ?args))))

  (method-syntax for-each/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-for-each/stx o . ?args))))

  (method-syntax for-each*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-for-each*/stx o . ?args))))


  ;; folding
  (method-syntax fold-left
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left o . ?args))))

  (method-syntax fold-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right o . ?args))))

  (method-syntax fold-left*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left* o . ?args))))

  (method-syntax fold-right*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right* o . ?args))))

  (method-syntax fold-left/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left/stx o . ?args))))

  (method-syntax fold-right/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right/stx o . ?args))))

  (method-syntax fold-left*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left*/stx o . ?args))))

  (method-syntax fold-right*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right*/stx o . ?args))))

  (method-syntax fold-left/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left/with-index o . ?args))))

  (method-syntax fold-right/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right/with-index o . ?args))))

  (method-syntax fold-left*/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left*/with-index o . ?args))))

  (method-syntax fold-right*/with-index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-right*/with-index o . ?args))))

  (method-syntax subvector-fold-left
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-fold-left o . ?args))))

  (method-syntax subvector-fold-right
    (syntax-rules ()
      ((_ o . ?args)
       (subvector-fold-right o . ?args))))

  (method-syntax unfold
    (syntax-rules ()
      ((_ o . ?args)
       (vector-unfold o . ?args))))

  (method-syntax unfold-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-unfold-right o . ?args))))

  (method-syntax and-fold-left
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-left o . ?args))))

  (method-syntax and-fold-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-right o . ?args))))

  (method-syntax and-fold-left*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-left* o . ?args))))

  (method-syntax and-fold-right*
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-right* o . ?args))))

  (method-syntax and-fold-left/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-left/stx o . ?args))))

  (method-syntax and-fold-right/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-right/stx o . ?args))))

  (method-syntax and-fold-left*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-left*/stx o . ?args))))

  (method-syntax and-fold-right*/stx
    (syntax-rules ()
      ((_ o . ?args)
       (vector-and-fold-right*/stx o . ?args))))

  (method-syntax fold-left/pred
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fold-left/pred o . ?args))))

  ;; selecting
  (method-syntax subvector
    (syntax-rules ()
      ((_ o . ?args)
       (subvector o . ?args))))

  (method-syntax subvector*
    (syntax-rules ()
      ((_ o . ?args)
       (subvector* o . ?args))))

  (method-syntax copy
    (syntax-rules ()
      ((_ o . ?args)
       (vector-copy o . ?args))))

  (method-syntax reverse-copy
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse-copy o . ?args))))

  (method-syntax copy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-copy! o . ?args))))

  (method-syntax reverse-copy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse-copy! o . ?args))))

  (method-syntax take
    (syntax-rules ()
      ((_ o . ?args)
       (vector-take o . ?args))))

  (method-syntax take-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-take-right o . ?args))))

  (method-syntax drop
    (syntax-rules ()
      ((_ o . ?args)
       (vector-drop o . ?args))))

  (method-syntax drop-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-drop-right o . ?args))))


  ;; padding and trimming
  (method-syntax trim
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim o . ?args))))

  (method-syntax trim-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim-right o . ?args))))

  (method-syntax trim-both
    (syntax-rules ()
      ((_ o . ?args)
       (vector-trim-both o . ?args))))

  (method-syntax pad
    (syntax-rules ()
      ((_ o . ?args)
       (vector-pad o . ?args))))

  (method-syntax pad-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-pad-right o . ?args))))


  ;; prefix and suffix
  (method-syntax prefix-length
    (syntax-rules ()
      ((_ o . ?args)
       (vector-prefix-length o . ?args))))

  (method-syntax suffix-length
    (syntax-rules ()
      ((_ o . ?args)
       (vector-suffix-length o . ?args))))

  (method-syntax prefix?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-prefix? o . ?args))))

  (method-syntax suffix?
    (syntax-rules ()
      ((_ o . ?args)
       (vector-suffix? o . ?args))))


  ;; searching
  (method-syntax index
    (syntax-rules ()
      ((_ o . ?args)
       (vector-index o . ?args))))

  (method-syntax index-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-index-right o . ?args))))

  (method-syntax skip
    (syntax-rules ()
      ((_ o . ?args)
       (vector-skip o . ?args))))

  (method-syntax skip-right
    (syntax-rules ()
      ((_ o . ?args)
       (vector-skip-right o . ?args))))

  (method-syntax count
    (syntax-rules ()
      ((_ o . ?args)
       (vector-count o . ?args))))

  (method-syntax contains
    (syntax-rules ()
      ((_ o . ?args)
       (vector-contains o . ?args))))

  (method-syntax binary-search
    (syntax-rules ()
      ((_ o . ?args)
       (vector-binary-search o . ?args))))


  ;; filtering
  (method-syntax filter
    (syntax-rules ()
      ((_ o . ?args)
       (vector-filter o . ?args))))

  (method-syntax delete
    (syntax-rules ()
      ((_ o . ?args)
       (vector-delete o . ?args))))


  ;; lists
  (method-syntax >list*
    (syntax-rules ()
      ((_ o . ?args)
       (vector->list* o . ?args))))

  (method-syntax reverse-vector->list
    (syntax-rules ()
      ((_ o . ?args)
       (reverse-vector->list o . ?args))))

  ;; replicating
  (method-syntax xsubvector
    (syntax-rules ()
      ((_ o . ?args)
       (xsubvector o . ?args))))

  (method-syntax xcopy!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-xcopy! o . ?args))))


  ;; mutating
  (method-syntax fill*!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-fill*! o . ?args))))

  (method-syntax swap!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-swap! o . ?args))))


  ;; reverse and replace
  (method-syntax reverse
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse o . ?args))))

  (method-syntax reverse!
    (syntax-rules ()
      ((_ o . ?args)
       (vector-reverse! o . ?args))))

  (method-syntax replace
    (syntax-rules ()
      ((_ o . ?args)
       (vector-replace o . ?args))))

  )


;;;; done

)

;;; end of file
