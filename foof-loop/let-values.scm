;;; -*- Mode: Scheme -*-

;;;; Multiple-Value Binding Macros

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (receive-values consumer producer)
  (call-with-values producer consumer))

(define-syntax receive
  (syntax-rules ()
    ((RECEIVE (variable) producer body0 body1+ ...)
     (LET ((variable producer)) body0 body1+ ...))

    ((RECEIVE bvl producer body0 body1+ ...)
     (CALL-WITH-VALUES (LAMBDA () producer)
       (LAMBDA bvl body0 body1+ ...)))))

(define-syntax let*-values
  (syntax-rules ()
    ((LET*-VALUES () body0 body1+ ...)
     (LET () body0 body1+ ...))

    ((LET*-VALUES ((bvl producer)) body0 body1+ ...)
     (RECEIVE bvl producer body0 body1+ ...))

    ((LET*-VALUES ((bvl0 producer0)
                   (bvl1+ producer1+)
                   ...)
       body0
       body1+
       ...)
     (RECEIVE bvl0 producer0
       (LET*-VALUES ((bvl1+ producer1+) ...)
         body0
         body1+
         ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((LET-VALUES () body0 body1+ ...)
     (LET () body0 body1+ ...))

    ((LET-VALUES ((bvl producer)) body0 body1+ ...)
     (RECEIVE bvl producer body0 body1+ ...))

    ((LET-VALUES ((bvl producer) ...) body0 body1+ ...)
     (LET-VALUES/PROCESS-CLAUSES ((bvl producer) ...)
                                 (LET-VALUES/MAKE-OUTPUT body0 body1+ ...)))))

(define-syntax let-values/process-clauses
  (syntax-rules ()
    ((LET-VALUES/PROCESS-CLAUSES clauses continuation)
     (LET-VALUES/PROCESS-CLAUSES () () clauses continuation))

    ((LET-VALUES/PROCESS-CLAUSES let-bindings
                                 bvls&producers
                                 ()
                                 (continuation . environment))
     (continuation let-bindings bvls&producers . environment))

    ((LET-VALUES/PROCESS-CLAUSES let-bindings
                                 bvls&producers
                                 (((variable) producer) . clauses)
                                 continuation)
     (LET-VALUES/PROCESS-CLAUSES ((variable producer) . let-bindings)
                                 bvls&producers
                                 clauses
                                 continuation))

    ((LET-VALUES/PROCESS-CLAUSES let-bindings
                                 bvls&producers
                                 ((bvl producer) . clauses)
                                 continuation)
     (LET-VALUES/GENERATE-TEMPORARIES
      bvl
      (LET-VALUES/CONTINUATION producer
                               let-bindings
                               bvls&producers
                               clauses
                               continuation)))))

(define-syntax let-values/continuation
  (syntax-rules ()
    ((LET-VALUES/CONTINUATION bvl
                              (let-binding ...)
                              producer
                              let-bindings
                              bvls&producers
                              clauses
                              continuation)
     (LET-VALUES/PROCESS-CLAUSES (let-binding ... . let-bindings)
                                 ((bvl producer) . bvls&producers)
                                 clauses
                                 continuation))))

(define-syntax let-values/generate-temporaries
  (syntax-rules ()
    ((LET-VALUES/GENERATE-TEMPORARIES bvl continuation)
     (LET-VALUES/GENERATE-TEMPORARIES () () bvl continuation))

    ((LET-VALUES/GENERATE-TEMPORARIES (bvl-out ...)
                                      let-bindings
                                      (variable . bvl-in)
                                      continuation)
     (LET-VALUES/GENERATE-TEMPORARIES (bvl-out ... TEMPORARY)
                                      ((variable TEMPORARY) . let-bindings)
                                      bvl-in
                                      continuation))

    ((LET-VALUES/GENERATE-TEMPORARIES bvl
                                      let-bindings
                                      ()
                                      (continuation . environment))
     (continuation bvl let-bindings . environment))

    ((LET-VALUES/GENERATE-TEMPORARIES (bvl ...)
                                      let-bindings
                                      rest-variable
                                      (continuation . environment))
     (continuation (bvl ... . TEMPORARY)
                   ((rest-variable TEMPORARY) . let-bindings)
                   . environment))))

(define-syntax let-values/make-output
  (syntax-rules ()
    ((LET-VALUES/MAKE-OUTPUT let-bindings () . body)
     (LET let-bindings . body))

    ((LET-VALUES/MAKE-OUTPUT let-bindings
                             ((bvl producer) . bvls&producers)
                             . body)
     (RECEIVE bvl producer
       (LET-VALUES/MAKE-OUTPUT let-bindings bvls&producers . body)))))
