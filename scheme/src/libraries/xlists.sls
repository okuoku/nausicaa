;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: extended class interface to list functions
;;;Date: Sat Jun 12, 2010
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


(library (xlists)
  (export <xlist>)
  (import (nausicaa)
    (lists))


(define-virtual-class <xlist>
  (inherit <list>)
  (nongenerative nausicaa:xlists:<xlist>)

  ;; constructors
  (method-syntax list-copy
    (syntax-rules ()
      ((_ o)
       (list-copy o))))

  (method-syntax tree-copy
    (syntax-rules ()
      ((_ o)
       (tree-copy o))))

  ;; circular lists
  (method-syntax list->circular-list!
    (syntax-rules ()
      ((_ o)
       (list->circular-list! o))))

  (method-syntax circular-list->list!
    (syntax-rules ()
      ((_ o)
       (circular-list->list! o))))

  (method-syntax circular-list-copy
    (syntax-rules ()
      ((_ o)
       (circular-list-copy o))))

  (method-syntax circular-list-length
    (syntax-rules ()
      ((_ o)
       (circular-list-length o))))

;;;  circular-list=

  ;; predicates
  (method-syntax circular-list?
    (syntax-rules ()
      ((_ o)
       (circular-list? o))))

  (method-syntax circular-list?/or-null
    (syntax-rules ()
      ((_ o)
       (circular-list?/or-null o))))

  (method-syntax dotted-list?
    (syntax-rules ()
      ((_ o)
       (dotted-list? o))))

  (method-syntax dotted-list?/or-null
    (syntax-rules ()
      ((_ o)
       (dotted-list?/or-null o))))


  ;; selectors
  (method-syntax car+cdr
    (syntax-rules ()
      ((_ o)
       (car+cdr o))))

  (method-syntax first
    (syntax-rules ()
      ((_ o)
       (first o))))

  (method-syntax second
    (syntax-rules ()
      ((_ o)
       (second o))))

  (method-syntax third
    (syntax-rules ()
      ((_ o)
       (third o))))

  (method-syntax fourth
    (syntax-rules ()
      ((_ o)
       (fourth o))))

  (method-syntax fifth
    (syntax-rules ()
      ((_ o)
       (fifth o))))

  (method-syntax sixth
    (syntax-rules ()
      ((_ o)
       (sixth o))))

  (method-syntax seventh
    (syntax-rules ()
      ((_ o)
       (seventh o))))

  (method-syntax eighth
    (syntax-rules ()
      ((_ o)
       (eighth o))))

  (method-syntax ninth
    (syntax-rules ()
      ((_ o)
       (ninth o))))

  (method-syntax tenth
    (syntax-rules ()
      ((_ o)
       (tenth o))))

  (method-syntax take-left
    (syntax-rules ()
      ((_ o i)
       (take-left o i))))

  (method-syntax take-right
    (syntax-rules ()
      ((_ o i)
       (take-right o i))))

  (method-syntax take-left!
    (syntax-rules ()
      ((_ o i)
       (set! o (take-left! o i)))))

  (method-syntax drop-left
    (syntax-rules ()
      ((_ o i)
       (drop-left o i))))

  (method-syntax drop-right
    (syntax-rules ()
      ((_ o i)
       (drop-right o i))))

  (method-syntax drop-right!
    (syntax-rules ()
      ((_ o i)
       (set! o (drop-right! o i)))))

  (method-syntax split-at
    (syntax-rules ()
      ((_ o i)
       (split-at o i))))

  (method-syntax split-at!
    (syntax-rules ()
      ((_ o i)
       (set! o (split-at! o i)))))

  (method-syntax last
    (syntax-rules ()
      ((_ o)
       (last o))))

  (method-syntax last-pair
    (syntax-rules ()
      ((_ o)
       (last-pair o))))

  ;; misc
  (method-syntax length+
    (syntax-rules ()
      ((_ o)
       (length+ o))))

  (method-syntax append!
    (syntax-rules ()
      ((_ o . ?rest)
       (set! o (append! o . ?rest)))))

;;; --------------------------------------------------------------------

  (method-syntax concatenate
    (syntax-rules ()
      ((_ o)
       (concatenate o))))

  (method-syntax concatenate!
    (syntax-rules ()
      ((_ o)
       (concatenate! o))))

  (method-syntax reverse!
    (syntax-rules ()
      ((_ o)
       (reverse! o))))

  (method-syntax append-reverse
    (syntax-rules ()
      ((_ o)
       (append-reverse o))))

  (method-syntax append-reverse!
    (syntax-rules ()
      ((_ o)
       (append-reverse! o))))

  (method-syntax zip
    (syntax-rules ()
      ((_ o)
       (zip o))))

  (method-syntax zip*
    (syntax-rules ()
      ((_ o)
       (zip* o))))

  (method-syntax unzip1
    (syntax-rules ()
      ((_ o)
       (unzip1 o))))

  (method-syntax unzip2
    (syntax-rules ()
      ((_ o)
       (unzip2 o))))

  (method-syntax unzip3
    (syntax-rules ()
      ((_ o)
       (unzip3 o))))

  (method-syntax unzip4
    (syntax-rules ()
      ((_ o)
       (unzip4 o))))

  (method-syntax unzip5
    (syntax-rules ()
      ((_ o)
       (unzip5 o))))

  (method-syntax count
    (syntax-rules ()
      ((_ o)
       (count o))))

  ;; fold
  (method-syntax and-fold-left*
    (syntax-rules ()
      ((_ o)
       (and-fold-left* o))))

  (method-syntax and-fold-right*
    (syntax-rules ()
      ((_ o)
       (and-fold-right* o))))

  (method-syntax fold-left/pred
    (syntax-rules ()
      ((_ o)
       (fold-left/pred o))))

  (method-syntax pair-fold
    (syntax-rules ()
      ((_ o)
       (pair-fold o))))

  (method-syntax pair-fold*
    (syntax-rules ()
      ((_ o)
       (pair-fold* o))))

  (method-syntax reduce
    (syntax-rules ()
      ((_ o)
       (reduce o))))

  (method-syntax reduce*
    (syntax-rules ()
      ((_ o)
       (reduce* o))))

  ;; map
  (method-syntax map*
    (syntax-rules ()
      ((_ o)
       (map* o))))

  (method-syntax map-in-order*
    (syntax-rules ()
      ((_ o)
       (map-in-order* o))))

  (method-syntax map!
    (syntax-rules ()
      ((_ o)
       (map! o))))

  (method-syntax map*!
    (syntax-rules ()
      ((_ o)
       (map*! o))))

  (method-syntax for-each*
    (syntax-rules ()
      ((_ o)
       (for-each* o))))

  (method-syntax append-map
    (syntax-rules ()
      ((_ o)
       (append-map o))))

  (method-syntax append-map!
    (syntax-rules ()
      ((_ o)
       (append-map! o))))

  (method-syntax pair-for-each
    (syntax-rules ()
      ((_ o)
       (pair-for-each o))))

  (method-syntax pair-for-each*
    (syntax-rules ()
      ((_ o)
       (pair-for-each* o))))

  (method-syntax filter-map
    (syntax-rules ()
      ((_ o)
       (filter-map o))))

  (method-syntax filter-map*
    (syntax-rules ()
      ((_ o)
       (filter-map* o))))

  ;; filtering
  (method-syntax filter!
    (syntax-rules ()
      ((_ o)
       (filter! o))))

  (method-syntax partition!
    (syntax-rules ()
      ((_ o)
       (partition! o))))

  (method-syntax remove*
    (syntax-rules ()
      ((_ o)
       (remove* o))))

  (method-syntax remove*!
    (syntax-rules ()
      ((_ o)
       (remove*! o))))

  ;;searching
  (method-syntax find-tail
    (syntax-rules ()
      ((_ o)
       (find-tail o))))

  (method-syntax take-while
    (syntax-rules ()
      ((_ o)
       (take-while o))))

  (method-syntax take-while!
    (syntax-rules ()
      ((_ o)
       (take-while! o))))

  (method-syntax drop-while
    (syntax-rules ()
      ((_ o)
       (drop-while o))))

  (method-syntax span
    (syntax-rules ()
      ((_ o)
       (span o))))

  (method-syntax span!
    (syntax-rules ()
      ((_ o)
       (span! o))))

  (method-syntax break
    (syntax-rules ()
      ((_ o)
       (break o))))

  (method-syntax break!
    (syntax-rules ()
      ((_ o)
       (break! o))))

  (method-syntax any
    (syntax-rules ()
      ((_ o)
       (any o))))

  (method-syntax any*
    (syntax-rules ()
      ((_ o)
       (any* o))))

  (method-syntax every
    (syntax-rules ()
      ((_ o)
       (every o))))

  (method-syntax every*
    (syntax-rules ()
      ((_ o)
       (every* o))))

  (method-syntax list-index
    (syntax-rules ()
      ((_ o)
       (list-index o))))

  (method-syntax list-index*
    (syntax-rules ()
      ((_ o)
       (list-index* o))))

  (method-syntax member*
    (syntax-rules ()
      ((_ o)
       (member* o))))

  (method-syntax position
    (syntax-rules ()
      ((_ o)
       (position o))))

  ;; deletion
  (method-syntax delete
    (syntax-rules ()
      ((_ o)
       (delete o))))

  (method-syntax delete!
    (syntax-rules ()
      ((_ o)
       (delete! o))))

  (method-syntax delete-duplicates
    (syntax-rules ()
      ((_ o)
       (delete-duplicates o))))

  (method-syntax delete-duplicates!
    (syntax-rules ()
      ((_ o)
       (delete-duplicates! o))))

  ;; sorted lists
  (method-syntax sorted-list-insert
    (syntax-rules ()
      ((_ o)
       (sorted-list-insert o))))

  (method-syntax sorted-list-insert/uniq
    (syntax-rules ()
      ((_ o)
       (sorted-list-insert/uniq o))))

  (method-syntax union-of-sorted-lists
    (syntax-rules ()
      ((_ o)
       (union-of-sorted-lists o))))

  (method-syntax union-of-sorted-lists/uniq
    (syntax-rules ()
      ((_ o)
       (union-of-sorted-lists/uniq o))))


  ;; alists
  (method-syntax assoc*
    (syntax-rules ()
      ((_ o)
       (assoc* o))))

  (method-syntax alist-cons
    (syntax-rules ()
      ((_ o)
       (alist-cons o))))

  (method-syntax alist-copy
    (syntax-rules ()
      ((_ o)
       (alist-copy o))))

  (method-syntax alist-delete
    (syntax-rules ()
      ((_ o)
       (alist-delete o))))

  (method-syntax alist-delete!
    (syntax-rules ()
      ((_ o)
       (alist-delete! o))))

  )


;;;; done

)

;;; end of file
