; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
; port to R6RS -- 2007 Christian Sloma
; 

(library (clos std-protocols generic-invocation)
  
  (export register-generic-invocation-generics!
          generic-compute-apply-generic
          generic-compute-apply-methods
          generic-compute-methods
          generic-compute-method-more-specific?)
           
  (import (rnrs)
          (clos introspection)
          (srfi lists))

  (define compute-apply-generic #f)
  (define compute-apply-methods #f)
  (define compute-methods #f)      
  (define compute-method-more-specific? #f)

  (define (register-generic-invocation-generics! 
           gf-compute-apply-generic
           gf-compute-apply-methods
           gf-compute-methods
           gf-compute-method-more-specific?)

    (set! compute-apply-generic
          gf-compute-apply-generic)
    (set! compute-apply-methods
          gf-compute-apply-methods)
    (set! compute-methods
          gf-compute-methods)
    (set! compute-method-more-specific?
          gf-compute-method-more-specific?))
  
  (define (generic-invocation-generic? obj)
    (or (eq? obj compute-apply-generic)
        (eq? obj compute-apply-methods)
        (eq? obj compute-methods)
        (eq? obj compute-method-more-specific?)))
  
  (define (generic-compute-apply-generic generic)
    (lambda args
      (if (and (generic-invocation-generic? generic)
               (generic-invocation-generic? (car args)))
          (apply (method-procedure (last (generic-methods generic))) 
                 (cons #f args))
          (let* ((apply-methods (compute-apply-methods generic))
                 (methods       (compute-methods generic)))
            (apply-methods (methods args) args)))))
  
  (define (generic-compute-methods generic)
    (lambda (args)
      (let ((applicable
             (filter (lambda (method)
                       (every applicable?
                              (method-specializers method)
                              args))
                     (generic-methods generic))))
        (list-sort (lambda (m1 m2)
                     ((compute-method-more-specific? generic)
                      m1
                      m2
                      args))
                   applicable))))
  
  (define (generic-compute-method-more-specific? generic)
    (lambda (m1 m2 args)
      (let loop ((specls1 (method-specializers m1))
                 (specls2 (method-specializers m2))
                 (args args))
        (cond ((and (null? specls1) (null? specls2))
               (error
                "Two methods are equally specific."))
              ((or  (null? specls1) (null? specls2))
               (error
                "Two methods have a different number of specializers."))
              ((null? args)
               (error
                "Fewer arguments than specializers."))
              (else
               (let ((c1  (car specls1))
                     (c2  (car specls2))
                     (arg (car args)))
                 (if (eq? c1 c2)
                     (loop (cdr specls1)
                           (cdr specls2)
                           (cdr args))
                     (more-specific? c1 c2 arg))))))))
  
  (define (generic-compute-apply-methods generic)
    (lambda (methods args)
      (letrec ((one-step
                (lambda (tail)
                  (lambda ()
                    (if (null? tail)
                        (error "No applicable methods/next methods.")
                        (apply (method-procedure (car tail))
                               (cons (one-step (cdr tail)) args)))))))
        ((one-step methods)))))
  
  (define applicable?
    (lambda (c arg)
      (memq c (class-precedence-list (class-of arg)))))
  
  (define more-specific?
    (lambda (c1 c2 arg)
      (memq c2 (memq c1 (class-precedence-list (class-of arg))))))
  
  ) ;; library (clos std-protocols generic-invocation)
