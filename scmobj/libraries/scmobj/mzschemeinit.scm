;utilities for MzScheme
;Dorai Sitaram

(require-library "compat.ss")
(require-library "function.ss")

(define nreverse reverse!)
(define nconc append!)
(define every andmap)
(define some ormap)

;MzScheme's quicksort isn't destructive, but since
;we are not relying on sort! to be destructive in this
;code, we will use this alias.
(define sort! quicksort)

(define delete
  (lambda (x l)
    ;Prunes l of elts that are the same as x.
    (let loop ((l l))
      (cond ((null? l) l)
	    ((eqv? (car l) x) (loop (cdr l)))
	    (else (set-cdr! l (loop (cdr l)))
		  l)))))

(define delete-if
  (lambda (p l)
    ;Prunes l of multiple copies of elts that satisfy
    ;the binary predicate p.
    (let loop ((l l))
      (cond ((null? l) l)
	    ((p (car l)) (loop (cdr l)))
	    (else (set-cdr! l (loop (cdr l)))
		  l)))))

(define position
  (lambda (x l)
    ;Returns the first index of l that's x.
    (let loop ((i 0) (l l))
      (cond ((not (pair? l)) #f)
	    ((eqv? (car l) x) i)
	    (else (loop (+ i 1) (cdr l)))))))

(define mapcan
  (lambda (f l . ll)
    ;Maps f columnwise across the l's, returning
    ;the spliced list of the result.
    (let loop ((l l) (ll ll))
      (if (null? l) '()
	  (append! (apply f (car l) (map car ll))
		   (loop (cdr l) (map cdr ll)))))))

(define delete-duplicates
  (lambda (l)
    ;if l contains multiple copies of any elt,
    ;then all but the last copy are discarded
    (let loop ((l l))
      (if (pair? l)
          (let ((d (loop (cdr l))))
            (if (memv (car l) d)
                (begin (set-car! l (car d))
                       (set-cdr! l (cdr d))))))
      l)))

;mbe

;mbe.scm
;Dorai Sitaram, 1991, 1992, 1993, 1996

(define mbe:ellipsis?
  (lambda (x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

(define mbe:matches-pattern?
  (lambda (p e k)
    (cond ((mbe:ellipsis? p) (if (not (= (length p) 2))
                               (error "bad ellipsis: ~a" p))
           (and (list? e)
             (let ((p0 (car p)))
               (every (lambda (e_i)
                        (mbe:matches-pattern? p0 e_i k))
                 e))))
      ((pair? p) (and (pair? e)
                   (mbe:matches-pattern? (car p) (car e) k)
                   (mbe:matches-pattern? (cdr p) (cdr e) k)))
      ((symbol? p) (if (memq p k) (eq? p e) #t))
      (else (equal? p e)))))

(define mbe:get-ellipsis-nestings
  (lambda (p k)
    (let sub ((p p))
      (cond ((mbe:ellipsis? p) (cons (sub (car p)) (sub (cddr p))))
        ((pair? p) (nconc (sub (car p)) (sub (cdr p))))
        ((symbol? p) (if (memq p k) '() (list p)))
        (else '())))))

(define mbe:ellipsis-sub-envs
  (lambda (nestings r)
    (some (lambda (c)
            (if (mbe:intersect? nestings (car c)) (cdr c) #f))
      r)))

(define mbe:intersect?
  (lambda (v y)
    (if (or (symbol? v) (symbol? y)) (eq? v y)
      (some (lambda (v_i)
              (some (lambda (y_j)
                      (mbe:intersect? v_i y_j))
                y))
        v))))

(define mbe:get-bindings
  (lambda (p e k)
    (cond ((mbe:ellipsis? p) (let ((p0 (car p)))
                               (list (cons (mbe:get-ellipsis-nestings p0 k)
                                       (map (lambda (e_i)
                                              (mbe:get-bindings p0 e_i k))
                                         e)))))
      ((pair? p) (nconc (mbe:get-bindings (car p) (car e) k)
                   (mbe:get-bindings (cdr p) (cdr e) k)))
      ((symbol? p) (if (memq p k) '() (list (cons p e))))
      (else '()))))

(define mbe:expand-pattern
  (lambda (p r k)
    (cond ((mbe:ellipsis? p)
	    (append ;append! should work, unfortunately
	      ;(append! list nonpair) fails in scm
	      (let* ((p0 (car p))
		      (nestings (mbe:get-ellipsis-nestings
				  p0 k))
		      (rr (mbe:ellipsis-sub-envs nestings r)))
		(map (lambda (r_i)
		       (mbe:expand-pattern p0
			 (append r_i r) k))
		  rr))
	      (mbe:expand-pattern (cddr p) r k)))
      ((pair? p) (cons (mbe:expand-pattern (car p) r k)
                   (mbe:expand-pattern (cdr p) r k)))
      ((symbol? p) (if (memq p k) p
                     (let ((x (assq p r)))
                       (if x (cdr x) p))))
      (else p))))

(defmacro define-syntax (macro-name syn-rules)
  (let ((kk (cons macro-name (cadr syn-rules)))
        (cc (cddr syn-rules)))
    `(defmacro ,macro-name aa
       (let ((e (cons ',macro-name aa)) (kk ',kk))
         (cond ,@(map
                   (lambda (c)
                     (let ((in-pat (car c)) (out-pat (cadr c)))
                       `((mbe:matches-pattern? ',in-pat e kk)
                         (let ((r (mbe:get-bindings ',in-pat e kk)))
                           ,(if (and (pair? out-pat)
                                  (eq? (car out-pat) 'with))
                              `(mbe:expand-pattern ',(caddr out-pat)
                                 (nconc
                                   (list
                                     ,@(map (lambda (w)
                                              `(cons ',(car w) ,(cadr w)))
                                         (cadr out-pat)))
                                   r) kk)
                              `(mbe:expand-pattern ',out-pat r kk))))))
                   cc)
           (else (error "~a: no matching clause" ',macro-name)))))))

;(define-syntax extend-syntax
;  (syntax-rules ()
;    ((extend-syntax (macro-name . other-keywords) . cc)
;     (define-syntax macro-name
;       (syntax-rules other-keywords . cc)))))
