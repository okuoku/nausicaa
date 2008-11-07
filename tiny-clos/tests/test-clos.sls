#!/usr/bin/env scheme-script

(import (ikarus)
        (clos core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-class direct-supers direct-slots)
  (make <class>
        'direct-supers direct-supers
        'direct-slots  direct-slots))

(define (make-generic)
  (make <generic>))

(define (make-method specializers procedure)
  (make <method>
        'specializers specializers
        'procedure    procedure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-class
  (syntax-rules ()
    ((define-class ?name (?super ...) (?slot-def ...))
     (define ?name
       (make-class (list ?super ...) '(?slot-def ...))))))

(define-syntax define-generic 
  (syntax-rules ()
    ((define-generic ?name)
     (define ?name
       (make-generic)))))

(define-class <point> (<object>) 
  (x y))

(add-method initialize
  (make-method (list <point>)
    (lambda (call-next-method point init-args)
      (call-next-method)
      (slot-set! point 'x (get-arg 'x init-args))
      (slot-set! point 'y (get-arg 'y init-args)))))

(define p1 (make <point> 'x 3 'y 4))

(define-generic distance-to-origin)

(add-method distance-to-origin
  (make-method (list <point>)
    (lambda (call-next-method point)
      (sqrt (+ (expt (slot-ref point 'x) 2)
               (expt (slot-ref point 'y) 2))))))

(display (distance-to-origin p1))
(newline)

(define (fib x)
  (if (= x 0)
      1
      (* x (fib (- x 1)))))

(define-generic gfib)

(add-method gfib
  (make-method (list)
    (lambda (call-next-method x)
      (if (= x 0)
          1
          (* x (gfib (- x 1)))))))

(define (bench f)
  (do ((x 0 (+ x 1)))
      ((> x 10000))
    (f 12)))

(time (bench fib))
(time (bench gfib))

(define-generic test)

(define-class <c1> (<object>)
  ())

(add-method test
  (make-method (list <c1>)
    (lambda (call-next-method c)
      (display "c1")
      (newline))))

(define-class <c2> (<c1>)
  ())

(add-method test
  (make-method (list <c2>)
    (lambda (call-next-method c)
      (call-next-method)
      (display "c2")
      (newline))))

(define-class <c3> (<c2>)
  ())

(add-method test
  (make-method (list <c3>)
    (lambda (call-next-method c)
      (call-next-method)
      (display "c3")
      (newline))))

(test (make <c3>))

      


