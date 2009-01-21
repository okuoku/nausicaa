;;;Lightweight testing (reference implementation)
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2005-2006 Sebastian Egner <Sebastian.Egner@philips.com>
;;;Modified by Derick Eddington for R6RS Scheme.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.



#!r6rs
(library (checks)
  (export

    ;; bindings from the SRFI
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?

    ;; result handling
    with-result add-result get-result

    ;; more macros
    catch-exception false-if-exception check-for-true

    ;; selecting tests
    testname

    ;; debugging
    debug debugging debug-print-condition)
  (import (rename (scheme)
		  (display	rnrs:display)
		  (write	rnrs:write)
		  (newline	rnrs:newline))
    (format)
    (loops))


;;; utilities

(define (display thing)
  (rnrs:display thing (current-error-port)))

(define (write thing)
  (rnrs:write thing (current-error-port)))

(define (newline)
  (rnrs:newline (current-error-port)))

(define (check:write thing)
  (write thing (current-error-port)))

(define pretty-print/no-trailing-newline
  (case-lambda
   ((datum output-port)
    (let* ((os	(call-with-string-output-port
		    (lambda (sop) (pretty-print datum sop))))
	   (len	(string-length os))
	   (os	(if (and (positive? len)
			 (char=? #\newline
				 (string-ref os (- len 1))))
		    (substring os 0 (- len 1))
		  os)))
      (display os output-port)))
   ((datum)
    (pretty-print/no-trailing-newline datum (current-output-port)))))

;;Return true if S1 is the prefix in S2.
(define (string-prefix? s1 s2)
  (or (eq? s1 s2)
      (let ((len1 (string-length s1)))
	(and (<= len1 (string-length s2))
	     (string=? s1 (substring s2 0 len1))))))



;;; mode handling

(define check:mode
  (make-parameter 'report
    (lambda (v)
      (case v
	((off)           0)
	((summary)       1)
	((report-failed) 10)
	((report)        100)
	(else (error 'check:mode
		"unrecognized mode" v))))))

(define (check-set-mode! mode)
  (check:mode mode))


;;; state handling

(define check:correct 0)
(define check:failed '())

(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed '()))

(define (check:add-correct!)
  (set! check:correct (+ check:correct 1)))

(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
	(cons (list expression actual-result expected-result)
	      check:failed)))


;;; reporting

(define (check:report-expression expression)
  (newline)
  (write expression)
  (display " => "))

(define (check:report-actual-result actual-result)
  (write actual-result)
  (display " ; "))

(define (check:report-correct cases)
  (display "correct")
  (if (not (= cases 1))
      (begin (display " (")
	     (display cases)
	     (display " cases checked)")))
  (newline))

(define (check:report-failed expected-result)
  (display "*** failed ***")
  (newline)
  (display " ; expected result: ")
  (write expected-result)
  (newline))

(define (check-report)
  (if (>= (check:mode) 1)
      (begin
	(newline)
	(display "; *** checks *** : ")
	(display check:correct)
	(display " correct, ")
	(display (length check:failed))
	(display " failed.")
	(if (or (null? check:failed) (<= (check:mode) 1))
	    (newline)
	  (let* ((w (car (reverse check:failed)))
		 (expression (car w))
		 (actual-result (cadr w))
		 (expected-result (caddr w)))
	    (display " First failed example:")
	    (newline)
	    (check:report-expression expression)
	    (check:report-actual-result actual-result)
	    (check:report-failed expected-result))))))

(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))


;;; simple checks

(define (check:proc expression thunk equal expected-result)
  (case (check:mode)
    ((0) #f)
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (check:add-failed! expression actual-result expected-result))))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (begin
	   (check:report-expression expression)
	   (check:report-actual-result actual-result)
	   (check:report-failed expected-result)
	   (check:add-failed! expression actual-result expected-result)))))
    ((100)
     (check:report-expression expression)
     (let ((actual-result (thunk)))
       (check:report-actual-result actual-result)
       (if (equal actual-result expected-result)
	   (begin (check:report-correct 1)
		  (check:add-correct!))
	 (begin (check:report-failed expected-result)
		(check:add-failed! expression
				   actual-result
				   expected-result)))))
    (else (error 'check:proc
	    "unrecognized check:mode" (check:mode))))
  (if #f #f))

(define-syntax srfi:check
  (syntax-rules (=>)
    ((check expr => expected)
     (check expr (=> equal?) expected))
    ((check expr (=> equal) expected)
     (if (>= (check:mode) 1)
	 (check:proc 'expr (lambda () expr) equal expected)))))


;;; parametric checks

(define (check:proc-ec w)
  (let ((correct? (car w))
	(expression (cadr w))
	(actual-result (caddr w))
	(expected-result (cadddr w))
	(cases (car (cddddr w))))
    (if correct?
	(begin (if (>= (check:mode) 100)
		   (begin (check:report-expression expression)
			  (check:report-actual-result actual-result)
			  (check:report-correct cases)))
	       (check:add-correct!))
      (begin (if (>= (check:mode) 10)
		 (begin (check:report-expression expression)
			(check:report-actual-result actual-result)
			(check:report-failed expected-result)))
	     (check:add-failed! expression
				actual-result
				expected-result)))))

(define-syntax check-ec:make
  (syntax-rules (=>)
    ((check-ec:make qualifiers expr (=> equal) expected (arg ...))
     (if (>= (check:mode) 1)
	 (check:proc-ec
	  (let ((cases 0))
	    (let ((w (first-ec
		      #f
		      qualifiers
		      (:let equal-pred equal)
		      (:let expected-result expected)
		      (:let actual-result
			    (let ((arg arg) ...) ; (*)
			      expr))
		      (begin (set! cases (+ cases 1)))
		      (if (not (equal-pred actual-result expected-result)))
		      (list (list 'let (list (list 'arg arg) ...) 'expr)
			    actual-result
			    expected-result
			    cases))))
	      (if w
		  (cons #f w)
		(list #t
		      '(check-ec qualifiers
			   expr (=> equal)
			   expected (arg ...))
		      (if #f #f)
		      (if #f #f)
		      cases)))))))))

		; (*) is a compile-time check that (arg ...) is a list
		; of pairwise disjoint bound variables at this point.

(define-syntax check-ec
  (syntax-rules (nested =>)
    ((check-ec expr => expected)
     (check-ec:make (nested) expr (=> equal?) expected ()))
    ((check-ec expr (=> equal) expected)
     (check-ec:make (nested) expr (=> equal) expected ()))
    ((check-ec expr => expected (arg ...))
     (check-ec:make (nested) expr (=> equal?) expected (arg ...)))
    ((check-ec expr (=> equal) expected (arg ...))
     (check-ec:make (nested) expr (=> equal) expected (arg ...)))

    ((check-ec qualifiers expr => expected)
     (check-ec:make qualifiers expr (=> equal?) expected ()))
    ((check-ec qualifiers expr (=> equal) expected)
     (check-ec:make qualifiers expr (=> equal) expected ()))
    ((check-ec qualifiers expr => expected (arg ...))
     (check-ec:make qualifiers expr (=> equal?) expected (arg ...)))
    ((check-ec qualifiers expr (=> equal) expected (arg ...))
     (check-ec:make qualifiers expr (=> equal) expected (arg ...)))

    ((check-ec (nested q1 ...) q etc ...)
     (check-ec (nested q1 ... q) etc ...))
    ((check-ec q1 q2             etc ...)
     (check-ec (nested q1 q2)    etc ...))))


;;;; handling results

(define result
  (make-parameter #f))

(define-syntax with-result
  (syntax-rules ()
    ((_ ?form ... ?last-form)
     (parameterize ((result '()))
       ?form ... (list ?last-form (get-result))))))

(define (add-result value)
  (result (cons value (result))))

(define (get-result)
  (reverse (result)))


;;;; more macros

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else #f))
       ?form0 ?form ...))))

(define-syntax check-for-true
  (syntax-rules ()
    ((_ ?form)
     (check-it (and ?form #t) => #t))))


;;;; selecting tests

(define testname
  (make-parameter #f
    (lambda (value)
      (unless (or (not value) (string? value) (symbol? value))
	(assertion-violation 'testname
	  "expected #f or string as parameter value" value))
      (if (symbol? value)
	  (symbol->string value)
	value))))

(define selected-test (get-environment-variable "CHECK_TEST_NAME"))

(define (check-activation)
  (or (not selected-test)
      (= 0 (string-length selected-test))
      (if (testname)
	  (or (string-prefix? selected-test (testname))
	      (string-prefix? (testname) selected-test))
	#f)))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (srfi:check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (when (check-activation)
       (check ?expr (=> ?equal) ?expected-result)))

    ((_ ?name ?expr => ?expected-result)
     (srfi:check ?name ?expr (=> equal?) ?expected-result))

    ((_ ?name ?expr (=> ?equal) ?expected-result)
     (parameterize ((testname ?name))
       (when (check-activation)
	 (check ?expr (=> ?equal) ?expected-result))))))


;;;; debugging

(define debugging
  (make-parameter #f))

(define (debug thing . args)
  (when (debugging)
    (if (string? thing)
	  (apply format (current-error-port) thing args)
      (write thing (current-error-port)))
    (newline (current-error-port))))

(define (debug-print-condition message exc)
  (debug "~a\nwho: ~s\nmessage: ~s\nirritants: ~s"
	 message
	 (if (who-condition? exc)
	     (condition-who exc)
	   'no-who)
	 (if (message-condition? exc)
	     (condition-message exc)
	   #f)
	 (if (irritants-condition? exc)
	     (condition-irritants exc)
	   #f)))


;;;; done

)

;;; end of file
