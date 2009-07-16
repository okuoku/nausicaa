; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; Le vecteur d'etats contient la table de transition du nfa.
; Chaque entree contient les arcs partant de l'etat correspondant.
; Les arcs sont stockes dans une liste.
; Chaque arc est une paire (class . destination).
; Les caracteres d'une classe sont enumeres par ranges.
; Les ranges sont donnes dans une liste,
;   chaque element etant une paire (debut . fin).
; Le symbole eps peut remplacer une classe.
; L'acceptation est decrite par une paire (acc-if-eol . acc-if-no-eol).

; Quelques variables globales
(define r2n-counter 0)
(define r2n-v-arcs '#(#f))
(define r2n-v-acc '#(#f))
(define r2n-v-len 1)

; Initialisation des variables globales
(define r2n-init
  (lambda ()
    (set! r2n-counter 0)
    (set! r2n-v-arcs (vector '()))
    (set! r2n-v-acc (vector #f))
    (set! r2n-v-len 1)))

; Agrandissement des vecteurs
(define r2n-extend-v
  (lambda ()
    (let* ((new-len (* 2 r2n-v-len))
	   (new-v-arcs (make-vector new-len '()))
	   (new-v-acc (make-vector new-len #f)))
      (let loop ((i 0))
	(if (< i r2n-v-len)
	    (begin
	      (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	      (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
	      (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len new-len))))

; Finalisation des vecteurs
(define r2n-finalize-v
  (lambda ()
    (let* ((new-v-arcs (make-vector r2n-counter))
	   (new-v-acc (make-vector r2n-counter)))
      (let loop ((i 0))
	(if (< i r2n-counter)
	    (begin
	      (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
	      (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
	      (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len r2n-counter))))

; Creation d'etat
(define r2n-get-state
  (lambda (acc)
    (if (= r2n-counter r2n-v-len)
	(r2n-extend-v))
    (let ((state r2n-counter))
      (set! r2n-counter (+ r2n-counter 1))
      (vector-set! r2n-v-acc state (or acc (cons #f #f)))
      state)))

; Ajout d'un arc
(define r2n-add-arc
  (lambda (start chars end)
    (vector-set! r2n-v-arcs
		 start
		 (cons (cons chars end) (vector-ref r2n-v-arcs start)))))

; Construction de l'automate a partir des regexp
(define r2n-build-epsilon
  (lambda (re start end)
    (r2n-add-arc start 'eps end)))

(define r2n-build-or
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re))
	  (re2 (get-re-attr2 re)))
      (r2n-build-re re1 start end)
      (r2n-build-re re2 start end))))

(define r2n-build-conc
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (re2 (get-re-attr2 re))
	   (inter (r2n-get-state #f)))
      (r2n-build-re re1 start inter)
      (r2n-build-re re2 inter end))))

(define r2n-build-star
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter1 'eps inter2)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter2 inter1))))

(define r2n-build-plus
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
	   (inter1 (r2n-get-state #f))
	   (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter2 'eps inter1)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter1 inter2))))

(define r2n-build-question
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re)))
      (r2n-add-arc start 'eps end)
      (r2n-build-re re1 start end))))

(define r2n-build-class
  (lambda (re start end)
    (let ((class (get-re-attr1 re)))
      (r2n-add-arc start class end))))

(define r2n-build-char
  (lambda (re start end)
    (let* ((c (get-re-attr1 re))
	   (class (list (cons c c))))
      (r2n-add-arc start class end))))

(define r2n-build-re
  (let ((sub-function-v (vector r2n-build-epsilon
				r2n-build-or
				r2n-build-conc
				r2n-build-star
				r2n-build-plus
				r2n-build-question
				r2n-build-class
				r2n-build-char)))
    (lambda (re start end)
      (let* ((re-type (get-re-type re))
	     (sub-f (vector-ref sub-function-v re-type)))
	(sub-f re start end)))))

; Construction de l'automate relatif a une regle
(define r2n-build-rule
  (lambda (rule ruleno nl-start no-nl-start)
    (let* ((re (get-rule-regexp rule))
	   (bol? (get-rule-bol? rule))
	   (eol? (get-rule-eol? rule))
	   (rule-start (r2n-get-state #f))
	   (rule-end (r2n-get-state (if eol?
					(cons ruleno #f)
					(cons ruleno ruleno)))))
      (r2n-build-re re rule-start rule-end)
      (r2n-add-arc nl-start 'eps rule-start)
      (if (not bol?)
	  (r2n-add-arc no-nl-start 'eps rule-start)))))

; Construction de l'automate complet
(define re2nfa
  (lambda (rules)
    (let ((nb-of-rules (vector-length rules)))
      (r2n-init)
      (let* ((nl-start (r2n-get-state #f))
	     (no-nl-start (r2n-get-state #f)))
	(let loop ((i 0))
	  (if (< i nb-of-rules)
	      (begin
		(r2n-build-rule (vector-ref rules i)
				i
				nl-start
				no-nl-start)
		(loop (+ i 1)))))
	(r2n-finalize-v)
	(let ((v-arcs r2n-v-arcs)
	      (v-acc r2n-v-acc))
	  (r2n-init)
	  (list nl-start no-nl-start v-arcs v-acc))))))
