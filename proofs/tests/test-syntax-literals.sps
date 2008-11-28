;;; syntax-case-literal-01.sls --
;;;
;;; Demonstrates the use of literals.

(import (rnrs)
  (uriel test)
  (uriel printing)
  (lib-for-syntax-case-literal-01))

(check-set-mode! 'report-failed)



;;;In this  example 'alpha'  has NO lexical  binding in  both the
;;;literals list of SYNTAX-RULES and the input expression.
(check
 (let ()
   (define-syntax doit
     (syntax-rules (alpha)
       ((_ ?arg1 (alpha ?arg2))
	(list ?arg1 ?arg2))))

   (doit 1 (alpha 2)))
 => '(1 2))

;;;In this example  'alpha' has THE SAME lexical  binding in both
;;;the literals list of SYNTAX-RULES and the input expression.
(check
 (let ((alpha 123))
   (define-syntax doit
     (syntax-rules (alpha)
       ((_ ?arg1 (alpha ?arg2))
	(list ?arg1 ?arg2))))

   (doit 1 (alpha 2)))
 => '(1 2))

;;;In this example 'alpha'  has DIFFERENT lexical bindings in the
;;;literals list of SYNTAX-RULES  and the input expression.  This
;;;form will raise a compile time error.
;; (check
;;  (let ()
;;    (define-syntax doit
;;      (syntax-rules (alpha)
;;        ((_ ?arg1 (alpha ?arg2))
;; 	(list ?arg1 ?arg2))))

;;    (let ((alpha 123))
;;      (doit 1 (alpha 2))))
;;  => '(1 2))

;;;In this example 'alpha'  has DIFFERENT lexical bindings in the
;;;literals list of SYNTAX-RULES  and the input expression.  This
;;;form will raise a compile time error.
;; (check
;;  (let ()
;;    (define-syntax doit
;;      (let ((alpha 123))
;;        (syntax-rules (alpha)
;; 	 ((_ ?arg1 (alpha ?arg2))
;; 	  (list ?arg1 ?arg2)))))

;;      (doit 1 (alpha 2)))
;;  => '(1 2))

;;;Quoting 'alpha' changes nothing.  Still a compile time error.
;; (check
;;  (let ()
;;    (define-syntax doit
;;      (syntax-rules (alpha)
;;        ((_ ?arg1 ((quote alpha) ?arg2))
;; 	(list ?arg1 ?arg2))))

;;    (let ((alpha 123))
;;      (doit 1 ((quote alpha) 2))))
;;  => '(1 2))


;;;This works, dunno why.
(check
 (let ((=> #f))
   (cond (#t => 'ok)))
 => 'ok)

;; (check
;;  (let ((beta #f))
;;    (doit-beta 1 (beta 2)))
;;  => '(1 2))


;;;; done

(check-report)

;;; end of file
