;;;
;;;Part of: Nausicaa-ScmObj
;;;Contents: object system for Scheme
;;;Date: Tue Nov 11, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 1996 Dorai Sitaram
;;;
;;;This is  free software; you can redistribute  it and/or modify
;;;it under the terms of the GNU Lesser General Public License as
;;;published by the Free  Software Foundation; either version 2.1
;;;of the License, or (at your option) any later version.
;;;
;;;This  library is  distributed  in  the hope  that  it will  be
;;;useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;;;warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;;;PURPOSE.  See  the GNU Lesser General Public  License for more
;;;details.
;;;
;;;You  should have  received a  copy of  the GNU  Lesser General
;;;Public License along  with this library; if not,  write to the
;;;Free Software  Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;Boston, MA 02111-1307 USA.
;;;


;;;page
;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(library (scmobj)
    (export define-class make-class make <class>
	    class-of subclass?
	    class-definition-name class-precedence-list
	    slot-ref slot-set! list-of-slots
	    make-generic-procedure
	    define-generic define-method)
    (import ;;(rnrs)
	    (ikarus);;GOTCHA, temporarily to have PRETTY-PRINT
	    (rnrs mutable-pairs (6))
	    (except (srfi lists) delete-duplicates cons))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Helper functions.
;;; ------------------------------------------------------------

(define mapcan
  (lambda (f l . ll)
    ;;Maps f columnwise across the l's, returning
    ;;the spliced list of the result.
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

(define (nconc . list-of-lists)
  (concatenate list-of-lists))

(define (position element the-list)
  (list-index (lambda (elm) (eq? element elm)) the-list))

(define (sort! the-list <)
  (list-sort < the-list))

(define (some pred the-list)
  (let loop ((value (car the-list))
	     (ell   (cdr the-list)))
    (or value (if (null? ell)
		  #f
		(some (car ell) (cdr ell))))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Access to slots and inspection.
;;; ------------------------------------------------------------

(define slot-ref
  (lambda (al f)
    (let ((c (assq f al)))
      (if c (cdr c)
        (assertion-violation 'slot-ref
			     "trying to access nonexistent slot" f)))))

(define slot-set!
  (lambda (al slot-name value)
    (let ((pair (assq slot-name al)))
;;;GOTCHA
;;      (write 'there)
;;      (pretty-print pair)
      (if pair
	  (set-cdr! pair value)
        (assertion-violation 'slot-set!
			     "trying to set nonexistent slot" slot-name)))))

;;; ------------------------------------------------------------

(define (class-definition-name class)
  (slot-ref class ':class-definition-name))

(define (class-precedence-list class)
  (cons class (slot-ref class ':class-precedence-list)))

(define (list-of-slots object)
  (if (eq? object <class>)
      (cons ':class (slot-ref object ':slots))
    (filter (lambda (name)
	      (not (memq name
			 '(:class-definition-name
			   :class-precedence-list :slots))))
	    (slot-ref object ':slots))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class and instance constructors.
;;; ------------------------------------------------------------

(define <class>
  ;;All classes are instances of <class>. 
  '((:class . look-at-the-end-of-the-file-for-the-initialisation-of-this-slot)
    (:class-definition-name . <class>)
    (:class-precedence-list . ())
    (:slots . (:class-definition-name :class-precedence-list :slots))))

(define make
  (lambda (c . svsv)
    (let ((i (cons (cons ':class c)
	       (map (lambda (x)
		      (cons x ':uninitialized))
		 (slot-ref c ':slots)))))
;;;GOTCHA
;;        (write i)
;;        (write svsv)
;;       (exit)
      (let loop ((svsv svsv))
	(if (null? svsv) i
	  (begin
	    (slot-set! i (car svsv) (cadr svsv))
	    (loop (cddr svsv))))))))

(define-syntax make-class
  (syntax-rules ()
    ((make-class () (?slot ...))
     (make-class (<class>) (?slot ...)))
    ((make-class (?superclass ...) (?slot ...))
     (let ((%superclasses (list ?superclass ...)))
       (make <class>
         ':class-precedence-list
	 (delete-duplicates
	  (mapcan
	   (lambda (s)
	     (cons s
		   (append (slot-ref s ':class-precedence-list) '())))
	   %superclasses))
         ':slots
	 (delete-duplicates
	  (nconc '(?slot ...)
		 (mapcan
		  (lambda (s)
		    (append (slot-ref s ':slots) '()))
		  %superclasses))))))))

(define-syntax define-class
  (syntax-rules ()
    ((_ ?name ?superclasses ?slot0 ?slot ...)
     (define ?name
       (let ((c (make-class ?superclasses (?slot0 ?slot ...))))
	 (slot-set! c ':class-definition-name (quote ?name))
	 c)))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class inspection.
;;; ------------------------------------------------------------

(define subclass?
  (lambda (c1 c2)
    (cond ((eq? c1 c2) #t)
          ((eq? c1 #t) #f)
          ((eq? c2 #t) #t)
          ((memq c2 (slot-ref c1 ':class-precedence-list)) #t)
          (else #f))))

(define class-of
  (lambda (o)
    (if (pair? o)
      (let ((a (car o)))
        (if (pair? a)
          (if (eq? (car a) ':class) (cdr a) #t)
          #t))
      #t)))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Generic functions.
;;; ------------------------------------------------------------

(define scmobj:generic-procedure-class
  (make-class ()
	      (:generic-procedure
	       :add-new-primary-method
	       :add-new-before-method
	       :add-new-after-method
	       :add-new-around-method)))

(define scmobj:more-specific-method
  (lambda (m1 m2 cc)
    (let loop ((cc1 (car m1)) (cc2 (car m2)) (cc cc))
      (if (null? cc)
	  (assertion-violation 'scmobj:more-specific-method
			       "unknown error comparing methods")
        (let ((c1 (car cc1)) (c2 (car cc2)))
          (cond ((eq? c1 c2)
                 (loop (cdr cc1) (cdr cc2) (cdr cc)))
		((subclass? c1 c2) #t)
		((subclass? c2 c1) #f)
		(else
		 (let ((c (car cc)))
		   (let ((cpl (if (eq? c #t) '()
				(slot-ref c ':class-precedence-list))))
		     (let ((i1 (position c1 cpl))
			   (i2 (position c2 cpl)))
		       (if (and i1 i2)
			   (< i1 i2)
			 (assertion-violation 'scmobj:more-specific-method
					      "unknown error comparing methods"))))))))))))

(define scmobj:compute-applicable-methods
  (lambda (list-of-classes method-table)
    (let loop ((methods method-table)
               (the-applicable-methods '()))
      (if (null? methods)
        (map cdr
          (sort! the-applicable-methods
            (lambda (m1 m2)
              (scmobj:more-specific-method m1 m2 list-of-classes))))
        (loop (cdr methods)
          (let ((method (car methods)))
            (if (every subclass? list-of-classes (car method))
              (cons method the-applicable-methods)
              the-applicable-methods)))))))

(define-syntax scmobj:create-generic-procedure
  (syntax-rules ()
    ((scmobj:create-generic-procedure x ...)
     (let ((%primary-method-table '())
	   (%before-method-table '())
	   (%after-method-table '())
	   (%around-method-table '()))
       (make
	scmobj:generic-procedure-class
	':generic-procedure
	(lambda (x ... . %rest)
	  (let ((%arg-classes (list (class-of x) ...)))
	    (let ((%applicable-primary-methods
		   (scmobj:compute-applicable-methods %arg-classes
						      %primary-method-table))
		  (%applicable-before-methods
		   (scmobj:compute-applicable-methods %arg-classes
						      %before-method-table))
		  (%applicable-after-methods
		   (reverse
		    (scmobj:compute-applicable-methods %arg-classes
						       %after-method-table)))
		  (%applicable-around-methods
		   (scmobj:compute-applicable-methods %arg-classes
						      %around-method-table))
		  (%primary-method-called #f))
	      (letrec
		  ((%next-method?
		    (lambda ()
		      (if %primary-method-called
			  (not (null? %applicable-primary-methods))
			(or (not (null? %applicable-around-methods))
			    (not (null? %applicable-primary-methods))))))
		   (%call-next-method
		    (lambda ()
		      (cond (%primary-method-called
			     (apply
			      (let ((m (car %applicable-primary-methods)))
				(set! %applicable-primary-methods
				      (cdr %applicable-primary-methods))
				m)
			      %next-method?
			      %call-next-method
			      x ... %rest))
			    ((not (null? %applicable-around-methods))
			     (apply
			      (let ((m (car %applicable-around-methods)))
				(set! %applicable-around-methods
				      (cdr %applicable-around-methods))
				m)
			      %next-method?
			      %call-next-method
			      x ... %rest))
			    ((null? %applicable-primary-methods)
			     (assertion-violation 'scmobj:create-generic-procedure
						  "no method defined for these argument classes"))
			    (else
			     (set! %primary-method-called #t)
			     (for-each
				 (lambda (%before-method)
				   (apply %before-method x ... %rest))
			       %applicable-before-methods)
			     (let ((%res
				    (apply
				     (let
					 ((m (car
					      %applicable-primary-methods)))
				       (set! %applicable-primary-methods
					     (cdr %applicable-primary-methods))
				       m)
				     %next-method?
				     %call-next-method
				     x ... %rest)))
			       (for-each
				   (lambda (%after-method)
				     (apply %after-method x ... %rest))
				 %applicable-after-methods)
			       %res))))))
		(%call-next-method)))))

	':add-new-primary-method
	(lambda (specializer-classes m)
	  (or (some (lambda (c)
		      (and (every eq? specializer-classes (car c))
			   (begin
			     (set-cdr! c m)
			     #t))) %primary-method-table)
	      (set! %primary-method-table
		    (cons (cons specializer-classes m)
			  %primary-method-table)))
	  #t)

	':add-new-before-method
	(lambda (specializer-classes m)
	  (or (some (lambda (c)
		      (and (every eq? specializer-classes (car c))
			   (begin
			     (set-cdr! c m)
			     #t))) %before-method-table)
	      (set! %before-method-table
		    (cons (cons specializer-classes m)
			  %before-method-table)))
	  #t)

	':add-new-after-method
	(lambda (specializer-classes m)
	  (or (some (lambda (c)
		      (and (every eq? specializer-classes (car c))
			   (begin
			     (set-cdr! c m)
			     #t))) %after-method-table)
	      (set! %after-method-table
		    (cons (cons specializer-classes m)
			  %after-method-table)))
	  #t)

	':add-new-around-method
	(lambda (specializer-classes m)
	  (or (some (lambda (c)
		      (and (every eq? specializer-classes (car c))
			   (begin
			     (set-cdr! c m)
			     #t))) %around-method-table)
	      (set! %around-method-table
		    (cons (cons specializer-classes m)
			  %around-method-table)))
	  #t))))))

;;; ------------------------------------------------------------

;;generic procedures, as defined above, are lists.  the
;;following syntactic sugar allows generic procedures to
;;be invoked like regular procedures

(define scmobj:*generic-procedures* '())

;;;GOTCHA, service function for testing
(define (for-test--alist? what ell)
  (printf "*** Validating ~a as alist:\n" what)
;  (pretty-print ell)
  (write ell)(newline)
  (let ((r (list? ell)))
    (printf "  * Is it a list? ~s\n" r)
    (when r
      (let ((r (proper-list? ell)))
	(printf "  * Is it a proper list? ~s\n" r)
	(when r
	  (let ((r (map (lambda (l)
			  (and (pair? l)
			       (dotted-list? l))) ell)))
	    (printf "  * Are the elements pairs and dotted-lists? ~s\n" r))
	  )))))

(define (update-generic-procedures gproc gp)
;;;GOTCHA
;;  (pretty-print scmobj:*generic-procedures*)
  (set! scmobj:*generic-procedures*
	(alist-cons gproc gp scmobj:*generic-procedures*))
  (display "-- The alist of generic procedures is:\n")
  (pretty-print scmobj:*generic-procedures*)
  (for-test--alist? "list of generic procedures" scmobj:*generic-procedures*)
  (for-test--alist? "first method class" (cdar scmobj:*generic-procedures*))
  )

(define-syntax make-generic-procedure
  (syntax-rules ()
    ((make-generic-procedure ?arg ...)
     (let* ((%gp (scmobj:create-generic-procedure ?arg ...))
            (%gproc (slot-ref %gp ':generic-procedure)))
       (update-generic-procedures %gproc %gp)
       ;; Originally it was:
       ;;(set! scmobj:*generic-procedures*
       ;;      (cons (cons %gproc %gp) scmobj:*generic-procedures*))
       %gproc))))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name ?arg ...)
     (define ?name (make-generic-procedure ?arg ...)))))

(define (add-new-before-method gp types func)
  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
	    ':add-new-before-method)
   types func))

(define (add-new-after-method gp types func)
  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
	     ':add-new-after-method)
   types func))

(define (add-new-primary-method gp types func)
;;;GOTCHA - hurt me!
;;;  (pretty-print gp)
;;;  (display "-- The method element in the alist of generic procedures is:\n")
;;;  (pretty-print (assq gp scmobj:*generic-procedures*))
  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
	     ':add-new-primary-method)
   types func)
  )

(define (add-new-around-method gp types func)
  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
	     ':add-new-around-method)
   types func))

(define-syntax define-method
  (syntax-rules (:primary :before :after :around)

    ((_ 1 ?gp ?qlfr ?args . ?body)
     (define-method 2 ?gp ?qlfr ?args
       ()  ;;specialising args
       ()  ;;non-specialising args
       . ?body))

    ((_ 2 ?gp ?qlfr ((?x ?c) . ?args) (?sa ...) () . ?body)
     (define-method 2 ?gp ?qlfr ?args (?sa ... (?x ?c)) ()  . ?body))

    ((_ 2 ?gp ?qlfr (?x . ?args) ?sargs (?nsa ...) . ?body)
     (define-method 2 ?gp ?qlfr ?args ?sargs (?nsa ... ?x)  . ?body))

    ((_ 2 ?gp ?qlfr () ?sargs ?nsargs . ?body)
     (define-method 3 ?gp ?qlfr ?sargs ?nsargs . ?body))

    ((_ 2 ?gp ?qlfr ?r ?sargs (?nsa ...) . ?body)
     (define-method 3 ?gp ?qlfr ?sargs (?nsa ... . ?r) . ?body))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 gp :primary ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
    ;;             ':add-new-primary-method)
    ;;   (list c ...)
    ;;   (lambda (next-method? call-next-method x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?gp :primary ((?x ?c) ...) ?nsargs . ?body)
     (add-new-primary-method ?gp
			     (list ?c ...)
			     (lambda (next-method? call-next-method ?x ... . ?nsargs)
			       . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 gp :before ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
    ;;             ':add-new-before-method)
    ;;   (list c ...)
    ;;   (lambda (x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?gp :before ((?x ?c) ...) ?nsargs . ?body)
     (add-new-before-method ?gp
			    (list ?c ...)
			    (lambda (?x ... . ?nsargs)
			      . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 gp :after ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
    ;;             ':add-new-after-method)
    ;;   (list c ...)
    ;;   (lambda (x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?gp :after ((?x ?c) ...) ?nsargs . ?body)
     (add-new-after-method ?gp
			   (list ?c ...)
			   (lambda (?x ... . ?nsargs)
			     . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 gp :around ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq gp scmobj:*generic-procedures*))
    ;;             ':add-new-around-method)
    ;;   (list c ...)
    ;;   (lambda (next-method? call-next-method x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?gp :around ((?x ?c) ...) ?nsargs . ?body)
     (add-new-around-method
      (list ?c ...)
      (lambda (next-method? call-next-method ?x ... . ?nsargs)
        . ?body)))

    ((_ ?gp :primary ?args . ?body)
     (define-method 1 ?gp :primary ?args . ?body))

    ((_ ?gp :before ?args . ?body)
     (define-method 1 ?gp :before ?args . ?body))

    ((_ ?gp :after ?args . ?body)
     (define-method 1 ?gp :after ?args . ?body))

    ((_ ?gp :around ?args . ?body)
     (define-method 1 ?gp :around ?args . ?body))

    ((_ ?gp ?args . ?body)
     (define-method 1 ?gp :primary ?args . ?body))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

(slot-set! <class> ':class <class>)

) ;; end of library form

;;; end of file
