;;;!ypsilon
;;;
;;;Explanation at:
;;;
;;;  <http://proteus.freeshell.org/_shunting-yard-e.scm>
;;;
;;;Original code by Eduardo Cavazos:
;;;
;;;  <http://proteus.freeshell.org/_shunting-yard-e.scm>
;;;

(import (rnrs)
  (checks))

(define (shunting-yard expr current-operand operands-stack binary-operators-stack)
  (define (log . args)
    (when #f
      (write args)
      (newline)))

  (define %precedence-table '((sentinel . 0)
			      (=        . 10)
			      (+        . 20)
			      (-        . 20)
			      (*        . 30)
			      (/        . 30)
			      (^        . 40)))
  (define %operator-map '((^ . expt)
			  (neg . -)))
  (define %right-associative-list '(^))

  (define (%map-operator op)
    (or (let ((p (assq op %operator-map)))
	  (and p (cdr p)))
	op))
  (define %operators-table
    (cdr (map car %precedence-table)))
  (define (%precedence pred item-a item-b)
    (pred (cdr (assq item-a %precedence-table))
	  (cdr (assq item-b %precedence-table))))
  (define (precedence=? item-a item-b)
    (or (eq? item-a item-b)
	(%precedence = item-a item-b)))
  (define (precedence>? item-a item-b)
    (and (not (eq? item-a item-b))
	 (%precedence > item-a item-b)))
  (define (right-associative? obj)
    (member obj %right-associative-list))
  (define (binary-operator? obj)
    (member obj %operators-table))
  (define top car)
  (define (close-current-operand)
    (let loop ((old (cdr current-operand))
	       (new (car current-operand)))
      (if (null? old)
	  new
	(loop (cdr old) (list (%map-operator (car old)) new)))))
  (define (close-binary-operator-subexpression)
    #;(log 'close-expr expr
	   'close-curr-op current-operand
	   'close-operands operands-stack
	   'close-operators binary-operators-stack)
    ;;This consumes the top of  the operators stack, and changes the top
    ;;of the operands stack.  Example:
    ;;
    ;; expression:		(+ a b)
    ;;
    ;; before:	operators stack	(+ sentinel)
    ;;		operands  stack	(b a)
    ;;
    ;; after:	operators stack	(sentinel)
    ;;		operands  stack	(+ a b)
    ;;
    (shunting-yard expr
		   '()
		   (cond ((null? current-operand)
			  (cons (list (%map-operator (car  binary-operators-stack))
				      (cadr operands-stack)
				      (car  operands-stack))
				(cddr operands-stack)))
			 ((null? operands-stack)
			  (list (close-current-operand)))
			 (else
			  (cons (list (%map-operator (car binary-operators-stack))
				      (car operands-stack)
				      (close-current-operand))
				(cdr operands-stack))))
		   (cdr binary-operators-stack)))
  (log 'enter
       'expr expr
       'curr-op current-operand
       'operands operands-stack
       'operators binary-operators-stack)
  (if (null? expr)
      (if (eq? (top binary-operators-stack) 'sentinel)
	  (cond ((null? operands-stack)
		 (close-current-operand))
		(else
		 (car operands-stack)))
	(close-binary-operator-subexpression))
    (let ((token (car expr)))
      (cond ((binary-operator? token)
	     (log 'operator token)
	     (if (or (precedence>? token (top binary-operators-stack))
		     (and (right-associative? token)
			  (precedence=? token (top binary-operators-stack))))
		 (shunting-yard (cdr expr)
				'()
				(if (null? current-operand)
				    operands-stack
				  (cons (close-current-operand) operands-stack))
				(cons token binary-operators-stack))
	       (close-binary-operator-subexpression)))
	    ((list? token)
	     (log 'list-operand token)
	     (shunting-yard (cdr expr)
			    (cons (shunting-yard token '() '() '(sentinel))
				  current-operand)
			    operands-stack binary-operators-stack))
	    (else
	     (log 'operand token)
	     (shunting-yard (cdr expr)
			    (cons token current-operand)
			    operands-stack binary-operators-stack))))))

(define-syntax infix
  (syntax-rules ()
    ((_ ?token ...)
      (shunting-yard '(?token ...) '() '() '(sentinel)))))


(check-set-mode! 'report-failed)

(check (infix a + b)			=> '(+ a b))
(check (infix a + b + c)		=> '(+ (+ a b) c))
(check (infix a - b - c)		=> '(- (- a b) c))
(check (infix a - b + c)		=> '(+ (- a b) c))
(check (infix a + b - c)		=> '(- (+ a b) c))
(check (infix a * b * c)		=> '(* (* a b) c))
(check (infix a / b / c)		=> '(/ (/ a b) c))
(check (infix a * b / c)		=> '(/ (* a b) c))
(check (infix a / b * c)		=> '(* (/ a b) c))
(check (infix a + b * c)		=> '(+ a (* b c)))
(check (infix a + b / c)		=> '(+ a (/ b c)))
(check (infix a * b + c)		=> '(+ (* a b) c))
(check (infix a / b + c)		=> '(+ (/ a b) c))
(check (infix a * b - c)		=> '(- (* a b) c))
(check (infix a / b - c)		=> '(- (/ a b) c))
(check (infix a - b * c)		=> '(- a (* b c)))
(check (infix a - b / c)		=> '(- a (/ b c)))
(check (infix a = b * c)		=> '(= a (* b c)))
(check (infix a = b / c)		=> '(= a (/ b c)))
(check (infix (a - b) / (c + d))	=> '(/ (- a b) (+ c d)))
(check (infix 3 + 4 * 5 - 6 ^ 7 + 8)	=> '(+ (- (+ 3 (* 4 5)) (expt 6 7)) 8))
(check (infix 3 + 4 * (5 - 6) ^ 7 + 8)	=> '(+ (+ 3 (* 4 (expt (- 5 6) 7))) 8))
(check (infix a + b - c * d / e = f)	=> '(= (- (+ a b) (/ (* c d) e)) f))

;; ^ is right associative:
(check (infix a ^ b)			=> '(expt a b))
(check (infix a ^ b ^ c)		=> '(expt a (expt b c)))
(check (infix a ^ b ^ c ^ d)		=> '(expt a (expt b (expt c d))))
(check (infix a ^ b ^ c ^ d ^ e)	=> '(expt a (expt b (expt c (expt d e)))))

(check 'this (infix neg 4)		=> '(- 4))
(check 'this (infix neg neg 4)		=> '(- (- 4)))
(check 'this (infix neg (neg 4))	=> '(- (- 4)))
(check 'this (infix 1 + neg 4)		=> '(+ 1 (- 4)))
(check
    (infix 3 + 4 * neg (5 - 6) ^ 7 + 8)
  => '(+ (+ 3 (* 4 (expt (- (- 5 6)) 7))) 8))

(check-report)

;;; end of file
