;;; Copyright (c) 2007 Christian Sloma (port to R6RS).
;;; Copyright (c) 1992 Xerox Corporation.
;;; All Rights Reserved.
;;;
;;; Use,   reproduction,  and  preparation   of  derivative   works  are
;;; permitted.  Any copy of this software or of any derivative work must
;;; include  the  above  copyright  notice of  Xerox  Corporation,  this
;;; paragraph and the  one after it.  Any distribution  of this software
;;; or derivative  works must comply  with all applicable  United States
;;; export control laws.
;;;
;;; This  software  is  made  available  AS IS,  and  XEROX  CORPORATION
;;; DISCLAIMS  ALL  WARRANTIES, EXPRESS  OR  IMPLIED, INCLUDING  WITHOUT
;;; LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A  PARTICULAR  PURPOSE,  AND  NOTWITHSTANDING  ANY  OTHER  PROVISION
;;; CONTAINED  HEREIN,  ANY LIABILITY  FOR  DAMAGES  RESULTING FROM  THE
;;; SOFTWARE  OR ITS  USE IS  EXPRESSLY DISCLAIMED,  WHETHER  ARISING IN
;;; CONTRACT, TORT  (INCLUDING NEGLIGENCE) OR STRICT  LIABILITY, EVEN IF
;;; XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

(library (clos private compat)

  (export every
          every-2
          append-map
          last
          get-arg
          position
          make-parameter
          pointer-value)

  (import (rnrs)
    (only (srfi lists) every append-map last)
    (only (srfi parameters) make-parameter))

  (define (pointer-value value)
    (string-hash
     (call-with-string-output-port (lambda () (display value)))))

  (define (position obj lst)
    (let loop ((lst lst) (idx 0) (obj obj))
      (cond
       ((null? lst)
	#f)
       ((eq? (car lst) obj)
	idx)
       (else
	(loop (cdr lst) (+ idx 1) obj)))))

  (define (every-2 pred lst1 lst2)
    (let loop ((pred pred) (lst1 lst1) (lst2 lst2))
      (or (null? lst1)
          (null? lst2)
          (and (pred (car lst1) (car lst2))
               (loop pred (cdr lst1) (cdr lst2))))))

  (define (get-arg key lst . def)
    (let ((probe (member key lst)))
      (if (or (not probe)
              (not (pair? (cdr probe))))
          (if (pair? def)
              (car def)
	    (error 'get-arg
	      "mandatory keyword argument ~a missing in ~a" key lst))
	(cadr probe))))

  ) ;; library (clos private compat)

