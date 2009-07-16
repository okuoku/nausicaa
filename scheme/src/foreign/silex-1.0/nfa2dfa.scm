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

; Recoupement de deux arcs
(define n2d-2arcs
  (lambda (arc1 arc2)
    (let* ((class1 (car arc1))
	   (ss1 (cdr arc1))
	   (class2 (car arc2))
	   (ss2 (cdr arc2))
	   (result (class-sep class1 class2))
	   (classl (vector-ref result 0))
	   (classc (vector-ref result 1))
	   (classr (vector-ref result 2))
	   (ssl ss1)
	   (ssc (ss-union ss1 ss2))
	   (ssr ss2))
      (vector (if (or (null? classl) (null? ssl)) #f (cons classl ssl))
	      (if (or (null? classc) (null? ssc)) #f (cons classc ssc))
	      (if (or (null? classr) (null? ssr)) #f (cons classr ssr))))))

; Insertion d'un arc dans une liste d'arcs a classes distinctes
(define n2d-insert-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
	(list new-arc)
	(let* ((arc (car arcs))
	       (others (cdr arcs))
	       (result (n2d-2arcs new-arc arc))
	       (arcl (vector-ref result 0))
	       (arcc (vector-ref result 1))
	       (arcr (vector-ref result 2))
	       (list-arcc (if arcc (list arcc) '()))
	       (list-arcr (if arcr (list arcr) '())))
	  (if arcl
	      (append list-arcc list-arcr (n2d-insert-arc arcl others))
	      (append list-arcc list-arcr others))))))

; Regroupement des arcs qui aboutissent au meme sous-ensemble d'etats
(define n2d-factorize-arcs
  (lambda (arcs)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (arc-ss (cdr arc))
	       (others-no-fact (cdr arcs))
	       (others (n2d-factorize-arcs others-no-fact)))
	  (let loop ((o others))
	    (if (null? o)
		(list arc)
		(let* ((o1 (car o))
		       (o1-ss (cdr o1)))
		  (if (equal? o1-ss arc-ss)
		      (let* ((arc-class (car arc))
			     (o1-class (car o1))
			     (new-class (class-union arc-class o1-class))
			     (new-arc (cons new-class arc-ss)))
			(cons new-arc (cdr o)))
		      (cons o1 (loop (cdr o)))))))))))

; Transformer une liste d'arcs quelconques en des arcs a classes distinctes
(define n2d-distinguish-arcs
  (lambda (arcs)
    (let loop ((arcs arcs) (n-arcs '()))
      (if (null? arcs)
	  n-arcs
	  (loop (cdr arcs) (n2d-insert-arc (car arcs) n-arcs))))))

; Transformer une liste d'arcs quelconques en des arcs a classes et a
; destinations distinctes
(define n2d-normalize-arcs
  (lambda (arcs)
    (n2d-factorize-arcs (n2d-distinguish-arcs arcs))))

; Factoriser des arcs a destination unique (~deterministes)
(define n2d-factorize-darcs
  (lambda (arcs)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (arc-end (cdr arc))
	       (other-arcs (cdr arcs))
	       (farcs (n2d-factorize-darcs other-arcs)))
	  (let loop ((farcs farcs))
	    (if (null? farcs)
		(list arc)
		(let* ((farc (car farcs))
		       (farc-end (cdr farc)))
		  (if (= farc-end arc-end)
		      (let* ((arc-class (car arc))
			     (farc-class (car farc))
			     (new-class (class-union farc-class arc-class))
			     (new-arc (cons new-class arc-end)))
			(cons new-arc (cdr farcs)))
		      (cons farc (loop (cdr farcs)))))))))))

; Normaliser un vecteur de listes d'arcs
(define n2d-normalize-arcs-v
  (lambda (arcs-v)
    (let* ((nbnodes (vector-length arcs-v))
	   (new-v (make-vector nbnodes)))
      (let loop ((n 0))
	(if (= n nbnodes)
	    new-v
	    (begin
	      (vector-set! new-v n (n2d-normalize-arcs (vector-ref arcs-v n)))
	      (loop (+ n 1))))))))

; Inserer un arc dans une liste d'arcs a classes distinctes en separant
; les arcs contenant une partie de la classe du nouvel arc des autres arcs
; Retourne: (oui . non)
(define n2d-ins-sep-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
	(cons (list new-arc) '())
	(let* ((arc (car arcs))
	       (others (cdr arcs))
	       (result (n2d-2arcs new-arc arc))
	       (arcl (vector-ref result 0))
	       (arcc (vector-ref result 1))
	       (arcr (vector-ref result 2))
	       (l-arcc (if arcc (list arcc) '()))
	       (l-arcr (if arcr (list arcr) '()))
	       (result (if arcl
			   (n2d-ins-sep-arc arcl others)
			   (cons '() others)))
	       (oui-arcs (car result))
	       (non-arcs (cdr result)))
	  (cons (append l-arcc oui-arcs) (append l-arcr non-arcs))))))

; Combiner deux listes d'arcs a classes distinctes
; Ne tente pas de combiner les arcs qui ont nec. des classes disjointes
; Conjecture: les arcs crees ont leurs classes disjointes
; Note: envisager de rajouter un "n2d-factorize-arcs" !!!!!!!!!!!!
(define n2d-combine-arcs
  (lambda (arcs1 arcs2)
    (let loop ((arcs1 arcs1) (arcs2 arcs2) (dist-arcs2 '()))
      (if (null? arcs1)
	  (append arcs2 dist-arcs2)
	  (let* ((arc (car arcs1))
		 (result (n2d-ins-sep-arc arc arcs2))
		 (oui-arcs (car result))
		 (non-arcs (cdr result)))
	    (loop (cdr arcs1) non-arcs (append oui-arcs dist-arcs2)))))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation naive
; ; des arcs d'un ensemble d'etats.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
; 	  (n2d-add-state ss)
; 	  (let* ((dentry (vector-ref n2d-state-dict n))
; 		 (dentry-ss (get-dentry-ss dentry)))
; 	    (if (equal? dentry-ss ss)
; 		n
; 		(loop (+ n 1))))))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcss (map (lambda (s) (vector-ref arcs-v s)) ss))
; 		   (arcs (apply append arcss))
; 		   (dist-arcs (n2d-distinguish-arcs arcs))
; 		   (darcs (n2d-translate-arcs dist-arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
; 	  (n2d-add-state ss)
; 	  (let* ((dentry (vector-ref n2d-state-dict n))
; 		 (dentry-ss (get-dentry-ss dentry)))
; 	    (if (equal? dentry-ss ss)
; 		n
; 		(loop (+ n 1))))))))
; 
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
; 	'()
; 	(let* ((arcs (car arcs-l))
; 	       (other-arcs-l (cdr arcs-l))
; 	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
; 	  (n2d-combine-arcs arcs other-arcs)))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start))
; 	   (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
; 		   (arcs (n2d-combine-arcs-l arcs-l))
; 		   (darcs (n2d-translate-arcs arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Arbre de recherche.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; (define n2d-state-tree '#(#f ()))
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Fonctions de gestion de l'arbre de recherche
; (define make-snode (lambda () (vector #f '())))
; 
; (define get-snode-dstate   (lambda (snode) (vector-ref snode 0)))
; (define get-snode-children (lambda (snode) (vector-ref snode 1)))
; 
; (define set-snode-dstate
;   (lambda (snode dstate)   (vector-set! snode 0 dstate)))
; (define set-snode-children
;   (lambda (snode children) (vector-set! snode 1 children)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)
;     (set! n2d-state-tree (make-snode))))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
; 	   (v (make-vector new-len #f)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (begin
; 	      (set! n2d-state-dict v)
; 	      (set! n2d-state-len new-len))
; 	    (begin
; 	      (vector-set! v n (vector-ref n2d-state-dict n))
; 	      (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
; 	   (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
; 	  (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((s-l ss) (snode n2d-state-tree))
;       (if (null? s-l)
; 	  (or (get-snode-dstate snode)
; 	      (let ((s (n2d-add-state ss)))
; 		(set-snode-dstate snode s)
; 		s))
; 	  (let* ((next-s (car s-l))
; 		 (alist (get-snode-children snode))
; 		 (ass (or (assv next-s alist)
; 			  (let ((ass (cons next-s (make-snode))))
; 			    (set-snode-children snode (cons ass alist))
; 			    ass))))
; 	    (loop (cdr s-l) (cdr ass)))))))
; 
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
; 	'()
; 	(let* ((arcs (car arcs-l))
; 	       (other-arcs-l (cdr arcs-l))
; 	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
; 	  (n2d-combine-arcs arcs other-arcs)))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
; 	   (ss (cdr arc))
; 	   (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
; 		   (cond ((not rule1)
; 			  rule2)
; 			 ((not rule2)
; 			  rule1)
; 			 (else
; 			  (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
; 	    (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
; 	(cons #f #f)
; 	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
; 	   (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
; 	(if (= n n2d-state-count)
; 	    (cons arcs-v acc-v)
; 	    (begin
; 	      (vector-set! arcs-v n (get-dentry-darcs
; 				     (vector-ref n2d-state-dict n)))
; 	      (vector-set! acc-v n (get-dentry-acc
; 				    (vector-ref n2d-state-dict n)))
; 	      (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
; 	   (no-nl-d (n2d-search-state no-nl-start))
; 	   (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
; 	(if (< n n2d-state-count)
; 	    (let* ((dentry (vector-ref n2d-state-dict n))
; 		   (ss (get-dentry-ss dentry))
; 		   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
; 		   (arcs (n2d-combine-arcs-l arcs-l))
; 		   (darcs (n2d-translate-arcs arcs))
; 		   (fact-darcs (n2d-factorize-darcs darcs))
; 		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
; 		   (acc (n2d-acc-mins accs)))
; 	      (set-dentry-darcs dentry fact-darcs)
; 	      (set-dentry-acc   dentry acc)
; 	      (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
; 	     (new-arcs-v (car result))
; 	     (new-acc-v (cdr result)))
; 	(n2d-init-glob-vars)
; 	(list nl-d no-nl-d new-arcs-v new-acc-v)))))

; 
; Section temporaire: vieille facon de generer le dfa
; Dictionnaire d'etat det.  Table de hashage.  Creation des
; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; classes distinctes.
; 

; Quelques variables globales
(define n2d-state-dict '#(#f))
(define n2d-state-len 1)
(define n2d-state-count 0)
(define n2d-state-hash '#())

; Fonctions de gestion des entrees du dictionnaire
(define make-dentry (lambda (ss) (vector ss #f #f)))

(define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
(define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
(define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))

(define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
(define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))

; Initialisation des variables globales
(define n2d-init-glob-vars
  (lambda (hash-len)
    (set! n2d-state-dict (vector #f))
    (set! n2d-state-len 1)
    (set! n2d-state-count 0)
    (set! n2d-state-hash (make-vector hash-len '()))))

; Extension du dictionnaire
(define n2d-extend-dict
  (lambda ()
    (let* ((new-len (* 2 n2d-state-len))
	   (v (make-vector new-len #f)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (begin
	      (set! n2d-state-dict v)
	      (set! n2d-state-len new-len))
	    (begin
	      (vector-set! v n (vector-ref n2d-state-dict n))
	      (loop (+ n 1))))))))

; Ajout d'un etat
(define n2d-add-state
  (lambda (ss)
    (let* ((s n2d-state-count)
	   (dentry (make-dentry ss)))
      (if (= n2d-state-count n2d-state-len)
	  (n2d-extend-dict))
      (vector-set! n2d-state-dict s dentry)
      (set! n2d-state-count (+ n2d-state-count 1))
      s)))

; Recherche d'un etat
(define n2d-search-state
  (lambda (ss)
    (let* ((hash-no (if (null? ss) 0 (car ss)))
	   (alist (vector-ref n2d-state-hash hash-no))
	   (ass (assoc ss alist)))
      (if ass
	  (cdr ass)
	  (let* ((s (n2d-add-state ss))
		 (new-ass (cons ss s)))
	    (vector-set! n2d-state-hash hash-no (cons new-ass alist))
	    s)))))

; Combiner des listes d'arcs a classes dictinctes
(define n2d-combine-arcs-l
  (lambda (arcs-l)
    (if (null? arcs-l)
	'()
	(let* ((arcs (car arcs-l))
	       (other-arcs-l (cdr arcs-l))
	       (other-arcs (n2d-combine-arcs-l other-arcs-l)))
	  (n2d-combine-arcs arcs other-arcs)))))

; Transformer un arc non-det. en un arc det.
(define n2d-translate-arc
  (lambda (arc)
    (let* ((class (car arc))
	   (ss (cdr arc))
	   (s (n2d-search-state ss)))
      (cons class s))))

; Transformer une liste d'arcs non-det. en ...
(define n2d-translate-arcs
  (lambda (arcs)
    (map n2d-translate-arc arcs)))

; Trouver le minimum de deux acceptants
(define n2d-acc-min2
  (let ((acc-min (lambda (rule1 rule2)
		   (cond ((not rule1)
			  rule2)
			 ((not rule2)
			  rule1)
			 (else
			  (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
	    (acc-min (cdr acc1) (cdr acc2))))))

; Trouver le minimum de plusieurs acceptants
(define n2d-acc-mins
  (lambda (accs)
    (if (null? accs)
	(cons #f #f)
	(n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))

; Fabriquer les vecteurs d'arcs et d'acceptance
(define n2d-extract-vs
  (lambda ()
    (let* ((arcs-v (make-vector n2d-state-count))
	   (acc-v (make-vector n2d-state-count)))
      (let loop ((n 0))
	(if (= n n2d-state-count)
	    (cons arcs-v acc-v)
	    (begin
	      (vector-set! arcs-v n (get-dentry-darcs
				     (vector-ref n2d-state-dict n)))
	      (vector-set! acc-v n (get-dentry-acc
				    (vector-ref n2d-state-dict n)))
	      (loop (+ n 1))))))))

; Effectuer la transformation de l'automate de non-det. a det.
(define nfa2dfa
  (lambda (nl-start no-nl-start arcs-v acc-v)
    (n2d-init-glob-vars (vector-length arcs-v))
    (let* ((nl-d (n2d-search-state nl-start))
	   (no-nl-d (n2d-search-state no-nl-start))
	   (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
      (let loop ((n 0))
	(if (< n n2d-state-count)
	    (let* ((dentry (vector-ref n2d-state-dict n))
		   (ss (get-dentry-ss dentry))
		   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
		   (arcs (n2d-combine-arcs-l arcs-l))
		   (darcs (n2d-translate-arcs arcs))
		   (fact-darcs (n2d-factorize-darcs darcs))
		   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
		   (acc (n2d-acc-mins accs)))
	      (set-dentry-darcs dentry fact-darcs)
	      (set-dentry-acc   dentry acc)
	      (loop (+ n 1)))))
      (let* ((result (n2d-extract-vs))
	     (new-arcs-v (car result))
	     (new-acc-v (cdr result)))
	(n2d-init-glob-vars 0)
	(list nl-d no-nl-d new-arcs-v new-acc-v)))))
