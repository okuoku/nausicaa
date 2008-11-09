;; test-clos.sls --
;;

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(import (ikarus)
	(clos core)
	(clos user)
	(srfi lightweight-testing))

(check-set-mode! 'report-failed)

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Interaction with built in stuff.
;; ------------------------------------------------------------

;; (check
;;  (class-of 123)
;;  => <entity-class>)


;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Class definition tests and class object inspection.
;; ------------------------------------------------------------

(define-class <one> ()
  a b c)

(define-class <two> ()
  d e f)

(define-class <three> (<one> <two>)
  g h i)

;; ------------------------------------------------------------

(check
 (class-of <one>)
 => <class>)

;; ------------------------------------------------------------

(check (class-definition-name <one>)
       => '<one>)

(check (class-definition-name (class-of <one>))
       => '<class>)

;; ------------------------------------------------------------

(check (map class-definition-name (class-direct-supers <one>))
       => '(<object>))

(check (map class-definition-name (class-direct-supers <three>))
       => '(<one> <two>))

(check (class-direct-supers <three>)
       => (list <one> <two>))

(check (class-direct-supers <class>)
       => (list <object>))

;; ------------------------------------------------------------

(check
 (class-direct-slots <one>)
 => '((a) (b) (c)))

(check
 (class-direct-slots <three>)
 => '((g) (h) (i)))

;; ------------------------------------------------------------

(check
 (class-slots <one>)
 => '((a) (b) (c)))

(check
 (class-slots <three>)
 => '((g) (h) (i)
      (a) (b) (c)
      (d) (e) (f)))

;; ------------------------------------------------------------

;; (add-method print-object 'before
;; 	    (make <method>
;; 	      'specializers (list <one>)
;; 	      'procedure (lambda (%generic %next-methods object port)
;; 			   (display 23 port))))

;; (print-object (make <one> 'a 1 'b 2 'c 3))

(let* ((o (car (class-direct-supers <class>)))
       (printer (struct-printer o)))
  (display (struct-name o))(newline)
  (display (eq? print-object printer))(newline)
  (printer o (current-output-port) (lambda (v)
 				     #t)))
  
;(display (car (class-direct-supers <one>)))
;(print-object-with-slots <one> (current-output-port))

;; (check
;;  (class-precedence-list <one>)
;;  => (list <object>))

;; (check
;;  (class-precedence-list <three>)
;;  => (list <three> (<one> <two>) <object>))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Class instantiation tests and instance inspection.
;; ------------------------------------------------------------

(check
 (class-of (make <one> 'a 1 'b 2 'c 3))
 => <one>)

(check
 (class-definition-name (class-of (make <one> 'a 1 'b 2 'c 3)))
 => '<one>)

;; ------------------------------------------------------------


;; (check (let ((o (colour 1 2 3)))
;; 	 #t)
;;        => #t)

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Generic function tests.
;; ------------------------------------------------------------

(define-generic my-slots)

(define-method my-slots ((o <one>))
  (list (slot-ref o 'a)
	(slot-ref o 'b)
	(slot-ref o 'c)))

;; ------------------------------------------------------------

;; (check
;;  (let ((o (make <one>
;; 	     'a 1 'b 2 'c 3)))
;;    (slot-set! o 'a 123)
;;    (slot-ref o 'a))
;;  => '(1 2 3))



;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

(check-report)


;;; end of file
