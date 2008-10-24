;;; -*- Mode: Scheme -*-

;;;; Extensible Looping Macros, version 8

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This is a variation on Alex Shinn's looping macros described in
;;; message-id <1157562097.001179.11470@i42g2000cwa.googlegroups.com>.
;;; It has diverged substantially from the original macros, and is now
;;; documented at <http://mumble.net/~campbell/scheme/foof-loop.txt>.
;;;
;;; This file depends on syn-param.scm, also by Taylor R. Campbell, and
;;; SRFI 11 (LET-VALUES).  Ideally, the implementation of LET-VALUES
;;; should gracefully handle single-value clauses to elide superfluous
;;; uses of CALL-WITH-VALUES.

(define-syntax loop
  (syntax-rules ()
    ((LOOP ((loop-clause0 loop-clause1 ...) ...)
       body
       ...)
     (LOOP ANONYMOUS-LOOP ((loop-clause0 loop-clause1 ...) ...)
       body
       ...
       (ANONYMOUS-LOOP)))

    ((LOOP name ((loop-clause0 loop-clause1 ...) ...) body ...)
     (%LOOP START name ((loop-clause0 loop-clause1 ...) ...) (body ...)))))

;;; We must be very careful about where to add laziness annotations.
;;; In particular, we don't want to wrap only the loop's body, because
;;; if we did that, the outer bindings produced by the iterators would
;;; be evaluate eagerly, which is too soon.  So instead, we wrap the
;;; whole thing in a LAZY, and then wrap every call to the loop as
;;; well.

(define-syntax lazy-loop
  (syntax-rules (=>)
    ((LAZY-LOOP name (iterator ...) => result body0 body1 ...)
     (LAZY (LOOP EAGER-LOOP (iterator ...)
             => result
             (LET-SYNTAX ((name
                           (SYNTAX-RULES ()
                             ((name . arguments)
                              (LAZY (EAGER-LOOP . arguments))))))
               body0 body1 ...))))))

;;; Use this definition of SYNTACTIC-ERROR if your favourite Scheme
;;; doesn't have one already.  Note that this is distinct from a
;;; SYNTAX-ERROR procedure, since it must signal a compile-time error.

(define-syntax syntactic-error (syntax-rules ()))

;;; Utility for reporting syntax errors in LOOP clauses.

(define-syntax loop-clause-error
  (syntax-rules ()
    ((LOOP-CLAUSE-ERROR (macro (variable ...) arguments message))
     (SYNTACTIC-ERROR message (FOR variable ... (macro . arguments))))))

;;;; The Guts of LOOP

(define-syntax %loop
  (syntax-rules (=> FOR WITH LET LET-VALUES WHILE UNTIL
                    START GO PARSE-FOR CONTINUE FINISH SIMPLIFY-BODY)

    ((%LOOP START name loop-clauses body)
     (%LOOP GO name (() () () () () () () ()) loop-clauses body))

    ;; Simple case of a single variable, for clarity.
    ((%LOOP GO name state
            ((FOR variable (looper argument ...))
             . loop-clauses)
            body)
     (looper (variable) (argument ...)
             %LOOP CONTINUE name state loop-clauses body))

    ;; FOR handler with tail patterns.  Unfortunately, tail patterns are non-
    ;; standard...
    ;; 
    ;; ((%LOOP GO name state
    ;;         ((FOR variable0 variable1 ... (looper argument ...))
    ;;          . loop-clauses)
    ;;         body)
    ;;  (looper (variable0 variable1 ...)
    ;;          (argument ...)
    ;;          %LOOP CONTINUE name state loop-clauses body))

;;;;; FOR Clauses: Dealing with Iterators

    ((%LOOP GO name state
            ((FOR variable0 variable1 variable2 ...) . loop-clauses)
            body)
     (%LOOP PARSE-FOR (variable0 variable1 variable2 ...)
            ()
            (FOR variable0 variable1 variable2 ...)   ;Copy for error message.
            name state loop-clauses body))

    ((%LOOP PARSE-FOR ((looper argument ...))
            variables
            original-clause name state loop-clauses body)
     (looper variables (argument ...)
             %LOOP CONTINUE name state loop-clauses body))

    ((%LOOP PARSE-FOR (next-variable more0 more1 ...)
            (variable ...)
            original-clause name state loop-clauses body)
     (%LOOP PARSE-FOR (more0 more1 ...)
            (variable ... next-variable)
            original-clause name state loop-clauses body))

    ((%LOOP PARSE-FOR (non-list)
            variables
            original-clause name state loop-clauses body)
     (SYNTACTIC-ERROR "Malformed FOR clause in LOOP:" original-clause))

    ((%LOOP ((outer-bvl outer-producer) ...)
            ((loop-variable loop-initializer loop-stepper) ...)
            ((entry-bvl entry-producer) ...)
            (termination-condition ...)
            ((body-bvl body-producer) ...)
            ((final-bvl final-producer) ...)
            CONTINUE
            name
            ((loop-variables ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            loop-clauses
            body)
     (%LOOP GO name
            (;; Preserve the order of loop variables, so that the user
             ;; can put hers first and still use positional arguments.
             (loop-variables ...
                             (loop-variable loop-initializer loop-stepper) ...)
             user-bindings
             user-termination-conditions
             ((outer-bvl outer-producer) ... . outer-bindings)
             ((entry-bvl entry-producer) ... . entry-bindings)
             (termination-condition ... . termination-conditions)
             ((body-bvl body-producer) ... . body-bindings)
             ((final-bvl final-producer) ... . final-bindings))
            loop-clauses
            body))

;;;;; User-Directed Clauses

    ((%LOOP GO name state
            ((WITH variable initializer) . loop-clauses)
            body)
     (%LOOP GO name state
            ((WITH variable initializer variable) . loop-clauses)
            body))

    ((%LOOP GO name
            ((loop-variable ...) . more-state)
            ((WITH variable initializer stepper) . loop-clauses)
            body)
     (%LOOP GO name
            ;; Preserve ordering of the user's loop variables.
            ((loop-variable ... (variable initializer stepper))
             . more-state)
            loop-clauses
            body))

    ((%LOOP GO name state ((LET variable expression) . loop-clauses) body)
     (%LOOP GO name state ((LET-VALUES (variable) expression) . loop-clauses)
            body))

    ((%LOOP GO name (loop-variables (user-binding ...) . more-state)
            ((LET-VALUES user-bvl user-producer) . loop-clauses)
            body)
     (%LOOP GO name (loop-variables
                     ;; Preserve order of the user's termination conditions.
                     (user-binding ... (user-bvl user-producer))
                     . more-state)
            loop-clauses
            body))

    ((%LOOP GO name state ((WHILE condition) . loop-clauses) body)
     (%LOOP GO name state ((UNTIL (NOT condition)) . loop-clauses) body))

    ((%LOOP GO name (loop-variables
                     user-bindings
                     (user-termination-condition ...)
                     . more-state)
            ((UNTIL user-termination-condition*) . loop-clauses)
            body)
     (%LOOP GO name
            (loop-variables
             user-bindings
             (user-termination-condition ... user-termination-condition*)
             . more-state)
            loop-clauses
            body))

    ;; Compatibility forms.  These clauses *must* come after all
    ;; others, because there is no keyword, so these would shadow any
    ;; clauses with keywords.

    ((%LOOP GO name state ((variable initializer) . loop-clauses) body)
     (%LOOP GO name state ((WITH variable initializer) . loop-clauses) body))

    ((%LOOP GO name state ((variable initializer stepper) . loop-clauses) body)
     (%LOOP GO name state ((WITH variable initializer stepper) . loop-clauses)
            body))

    ((%LOOP GO name state (clause . loop-clauses) body)
     (SYNTACTIC-ERROR "Malformed LOOP clause:" clause))

;;;;; Finishing -- Generating Output

    ((%LOOP GO name state () (=> result-form . body))
     (%LOOP FINISH name state result-form body))

    ((%LOOP GO name state () body)
     (%LOOP FINISH name state (IF #F #F) body))

    ((%LOOP FINISH name
            (((loop-variable loop-initializer loop-stepper) ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            result-form
            body)
     (LET-VALUES outer-bindings
       (DEFINE (LOOP-PROCEDURE loop-variable ...)
         (LET-VALUES entry-bindings
           (%LOOP SIMPLIFY-BODY
                  termination-conditions
                  (LET-VALUES final-bindings
                    (WITH-EXTENDED-PARAMETER-OPERATORS
                        ((name
                          (LOOP-PROCEDURE (loop-variable . loop-stepper)
                                          ...)))
                      result-form))
                  body-bindings
                  user-bindings
                  user-termination-conditions
                  (WITH-EXTENDED-PARAMETER-OPERATORS
                      ((name
                        (LOOP-PROCEDURE (loop-variable . loop-stepper)
                                        ...)))
                    . body))))
       (LOOP-PROCEDURE loop-initializer ...)))

;;;;;; Simplifying the Body

    ;; No iterator- or user-introduced termination conditions at all.
    ;; No test or closure needed.
    ((%LOOP SIMPLIFY-BODY
            ()
            final-form
            body-bindings
            user-bindings
            ()
            body-form)
     (LET-VALUES body-bindings
       (LET-VALUES user-bindings
         body-form)))

    ;; Iterator-introduced termination conditions only.  One test and
    ;; no closure needed.
    ((%LOOP SIMPLIFY-BODY
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            ()                          ;No user termination conditions
            body-form)
     (IF (OR termination-condition ...)
         final-form
         (LET-VALUES body-bindings
           (LET-VALUES user-bindings
             body-form))))

    ;; The closure is needed here because the body bindings shouldn't
    ;; be visible in the final form.
    ((%LOOP SIMPLIFY-BODY
            ()
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (LET ((FINISH (LAMBDA () final-form)))
       (LET-VALUES body-bindings
         (LET-VALUES user-bindings
           (IF (OR user-termination-condition ...)
               (FINISH)
               body-form)))))

    ((%LOOP SIMPLIFY-BODY
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (LET ((FINISH (LAMBDA () final-form)))
       (IF (OR termination-condition ...)
           (FINISH)
           (LET-VALUES body-bindings
             (LET-VALUES user-bindings
               (IF (OR user-termination-condition ...)
                   (FINISH)
                   body-form))))))))

;;;; Accumulators

;;; Accumulators have the following syntax:
;;;
;;;   (FOR <result> (ACCUMULATING <generator>))
;;;   (FOR <result> (ACCUMULATING <generator> (IF <condition>)))
;;;   (FOR <result> (ACCUMULATING <generator> => <mapper>))    ;COND-style
;;;   (FOR <result> (ACCUMULATING <generator> <tester>         ;SRFI-61-style
;;;                               => <mapper>))
;;;
;;; In addition, some of them support initial values, which are
;;; specified with an optional first argument of (INITIAL <initial
;;; value>).  For example, to accumulate a list starting with some tail
;;; <tail>, write
;;;
;;;   (FOR <result-list> (LISTING (INITIAL <tail>) <element>)).

(define-syntax listing
  (syntax-rules (INITIAL)
    ((LISTING variables ((INITIAL tail-expression) . arguments) next . rest)
     (%ACCUMULATING variables arguments (((TAIL) tail-expression))
                    ('() CONS (LAMBDA (RESULT)
                                (APPEND-REVERSE RESULT TAIL)))
                    (LISTING variables
                             ((INITIAL tail-expression) . arguments)
                             "Malformed LISTING clause in LOOP:")
                    next . rest))

    ((LISTING variables arguments next . rest)
     (%ACCUMULATING variables arguments ()
                    ('() CONS REVERSE)
                    (LISTING variables arguments
                             "Malformed LISTING clause in LOOP:")
                    next . rest))))

(define-syntax listing-reverse
  (syntax-rules (INITIAL)
    ((LISTING-REVERSE variables ((INITIAL tail-expression) . arguments)
                      next . rest)
     (%ACCUMULATING variables arguments (((TAIL) tail-expression))
                    (TAIL CONS)
                    (LISTING-REVERSE
                     variables ((INITIAL tail-expression) . arguments)
                     "Malformed LISTING-REVERSE clause in LOOP:")
                    next . rest))

    ((LISTING-REVERSE variables arguments next . rest)
     (%ACCUMULATING variables arguments ()
                    ('() CONS)
                    (LISTING-REVERSE
                     variables arguments
                     "Malformed LISTING-REVERSE clause in LOOP:")
                    next . rest))))

;;; This is non-reentrant but produces precisely one garbage cons cell.

(define-syntax listing!
  (syntax-rules ()
    ((LISTING! variables arguments next . rest)
     (%LISTING! variables arguments (CONS #F '())
                (LISTING! variables arguments
                          "Malformed LISTING! clause in LOOP:")
                next . rest))))

(define-syntax listing-into!
  (syntax-rules ()
    ((LISTING-INTO! variables (first-expression . arguments) next . rest)
     (%LISTING! variables arguments first-expression
                (LISTING-INTO! variables
                               (first-expression . arguments)
                               "Malformed LISTING-INTO! clause in LOOP:")
                next . rest))))

(define-syntax %listing!
  (syntax-rules (INITIAL)
    ((%LISTING! variables ((INITIAL tail-expression) . arguments)
                first-expression
                error-context
                next . rest)
     (%ACCUMULATING variables arguments
                    (((FIRST TAIL)
                      (LET ((FIRST first-expression)
                            (TAIL tail-expression))
                        (SET-CDR! FIRST TAIL)
                        (VALUES FIRST TAIL))))
                    (FIRST (LAMBDA (DATUM PREVIOUS-CELL)
                             (LET ((NEXT-CELL (CONS DATUM TAIL)))
                               (SET-CDR! PREVIOUS-CELL NEXT-CELL)
                               NEXT-CELL))
                           (LAMBDA (CELL) CELL (CDR FIRST)))
                    error-context
                    next . rest))

    ((%LISTING! variables arguments first-expression error-context next . rest)
     (%LISTING! variables ((INITIAL '()) . arguments)
                first-expression
                error-context
                next . rest))))

;;;;; List Appending Accumulators

(define-syntax appending
  (syntax-rules (INITIAL)
    ((APPENDING variables ((INITIAL tail-expression) . arguments)
                next . rest)
     (%ACCUMULATING variables arguments (((TAIL) tail-expression))
                    ('() APPEND-REVERSE (LAMBDA (RESULT)
                                          (APPEND-REVERSE RESULT TAIL)))
                    (APPENDING variables
                               ((INITIAL tail-expression) . arguments)
                               "Malformed APPENDING clause in LOOP:")
                    next . rest))

    ((APPENDING variables arguments next . rest)
     (%ACCUMULATING variables arguments ()
                    ('() APPEND-REVERSE REVERSE)
                    (APPENDING variables arguments
                               "Malformed APPENDING clause in LOOP:")
                    next . rest))))

(define-syntax appending-reverse
  (syntax-rules (INITIAL)
    ((APPENDING-REVERSE variables ((INITIAL tail-expression) . arguments)
                        next . rest)
     (%ACCUMULATING variables arguments (((TAIL) tail-expression))
                    (TAIL APPEND-REVERSE)
                    (APPENDING-REVERSE
                     variables ((INITIAL tail-expression) . arguments)
                     "Malformed APPENDING-REVERSE clause in LOOP:")
                    next . rest))

    ((APPENDING-REVERSE variables arguments next . rest)
     (%ACCUMULATING variables arguments ()
                    ('() APPEND-REVERSE)
                    (APPENDING-REVERSE
                     variables arguments
                     "Malformed APPENDING-REVERSE clause in LOOP:")
                    next . rest))))

;; (define (append-reverse list tail)
;;   (loop ((FOR elt (IN-LIST list))
;;          (FOR result (LISTING-REVERSE (INITIAL tail) elt)))
;;     => result))

(define (append-reverse list tail)
  (if (pair? list)
      (append-reverse (cdr list) (cons (car list) tail))
      tail))

;;;;; Numerical Accumulators

(define-syntax summing
  (syntax-rules (INITIAL)
    ((SUMMING variables ((INITIAL initial-expression) . arguments) next . rest)
     (%ACCUMULATING variables arguments () (initial-expression +)
                    (SUMMING variables
                             ((INITIAL initial-expression) . arguments)
                             "Malformed SUMMING clause in LOOP:")
                    next . rest))

    ((SUMMING variables arguments next . rest)
     (%ACCUMULATING variables arguments () (0 +)
                    (SUMMING variables arguments
                             "Malformed SUMMING clause in LOOP:")
                    next . rest))))

(define-syntax multiplying
  (syntax-rules (INITIAL)
    ((MULTIPLYING variables ((INITIAL initial-expression) . arguments)
                  next . rest)
     (%ACCUMULATING variables arguments () (initial-expression *)
                    (MULTIPLYING variables
                                 ((INITIAL initial-expression) . arguments)
                                 "Malformed MULTIPLYING clause in LOOP:")
                    next . rest))

    ((MULTIPLYING variables arguments next . rest)
     (%ACCUMULATING variables arguments () (1 *)
                    (MULTIPLYING variables arguments
                                 "Malformed MULTIPLYING clause in LOOP:")
                    next . rest))))

(define-syntax maximizing
  (syntax-rules ()
    ((MAXIMIZING variables arguments next . rest)
     (%EXTREMIZING variables arguments MAX
                   (MAXIMIZING variables arguments
                               "Malformed MAXIMIZING clause in LOOP:")
                   next . rest))))

(define-syntax minimizing
  (syntax-rules ()
    ((MINIMIZING variables arguments next . rest)
     (%EXTREMIZING variables arguments MIN
                   (MINIMIZING variables arguments
                               "Malformed MINIMIZING clause in LOOP:")
                   next . rest))))

(define-syntax %extremizing
  (syntax-rules (INITIAL)
    ((%EXTREMIZING variables ((INITIAL initial-expression) . arguments)
                   chooser
                   error-context next . rest)
     (%ACCUMULATING variables arguments (((INITIAL-VALUE) initial-expression))
                    (INITIAL-VALUE chooser)
                    error-context next . rest))

    ((%EXTREMIZING variables arguments chooser error-context next . rest)
     (%ACCUMULATING variables arguments ()
                    (#F (LAMBDA (DATUM EXTREME)
                          (IF (AND DATUM EXTREME)
                              (chooser DATUM EXTREME)
                              (OR DATUM EXTREME))))
                    error-context next . rest))))

(define-syntax %accumulating
  (syntax-rules ()

    ;; There is a finalization step, so the result variable cannot be
    ;; the accumulator variable, and we must apply the finalizer at the
    ;; end.
    ((%ACCUMULATING (result-variable) arguments outer-bindings
                    (initializer combiner finalizer)
                    error-context
                    next . rest)
     (%%ACCUMULATING arguments (ACCUMULATOR initializer combiner)
                     outer-bindings
                     (((result-variable) (finalizer ACCUMULATOR)))
                     error-context
                     next . rest))

    ;; There is no finalizer step, so the accumulation is incremental,
    ;; and can be exploited; therefore, the result variable and the
    ;; accumulator variable are one and the same.
    ((%ACCUMULATING (accumulator-variable) arguments outer-bindings
                    (initializer combiner)
                    error-context
                    next . rest)
     (%%ACCUMULATING arguments (accumulator-variable initializer combiner)
                     outer-bindings
                     ()
                     error-context
                     next . rest))

    ;; The user supplied more than one variable.  Lose lose.
    ((%ACCUMULATING variables arguments outer-bindings parameters
                    error-context next . rest)
     (LOOP-CLAUSE-ERROR error-context))))

(define-syntax %%%accumulating
  (syntax-rules ()
    ((%%%ACCUMULATING outer-bindings loop-variable final-bindings next . rest)
     (next outer-bindings
           (loop-variable)
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           ()                           ;Body bindings
           final-bindings
           . rest))))

(define-syntax %%accumulating
  (syntax-rules (IF =>)
    ((%%ACCUMULATING (generator)        ;No conditional
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%ACCUMULATING outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (combiner generator accumulator))
                      final-bindings next . rest))

    ((%%ACCUMULATING (generator (IF condition))
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%ACCUMULATING outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (IF condition
                                       (combiner generator accumulator)
                                       accumulator))
                      final-bindings next . rest))

    ((%%ACCUMULATING (generator => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%ACCUMULATING outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (COND (generator
                                          => (LAMBDA (DATUM)
                                               (combiner (mapper DATUM)
                                                         accumulator)))
                                         (ELSE accumulator)))
                      final-bindings next . rest))

    ((%%ACCUMULATING (generator tester => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%ACCUMULATING outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (RECEIVE ARGS generator
                                     (IF (APPLY tester ARGS)
                                         (combiner (APPLY mapper ARGS)
                                                   accumulator)
                                         accumulator)))
                      final-bindings next . rest))

    ((%%ACCUMULATING arguments parameters outer-bindings final-bindings
                     error-context next . rest)
     (LOOP-CLAUSE-ERROR error-context))))

;;;; List Iteration

;;; (FOR <elt> [<pair>] (IN-LIST <list> [<successor>]))
;;;   Step across <list>, letting <pair> be each successive pair in
;;;   <list>, stepping by (<successor> <pair>), or (CDR <pair>) if no
;;;   successor procedure is explicitly provided.  Let <elt> be the car
;;;   of <pair> in the body of the loop.

(define-syntax in-list
  (syntax-rules ()
    ((IN-LIST (element-variable pair-variable)
              (list-expression successor-expression)
              next . rest)
     (next (((LIST) list-expression)                  ;Outer bindings
            ((SUCCESSOR) successor-expression))
           ((pair-variable LIST TAIL))                ;Loop variables
           ()                                         ;Entry bindings
           ((NOT (PAIR? pair-variable)))              ;Termination conditions
           (((element-variable) (CAR pair-variable))  ;Body bindings
            ((TAIL)             (SUCCESSOR pair-variable)))
           ()                                         ;Final bindings
           . rest))

    ((IN-LIST (element-variable pair-variable) (list-expression) next . rest)
     (IN-LIST (element-variable pair-variable) (list-expression CDR)
              next . rest))

    ((IN-LIST (element-variable) (list-expression successor) next . rest)
     (IN-LIST (element-variable PAIR) (list-expression successor) next . rest))

    ((IN-LIST (element-variable) (list-expression) next . rest)
     (IN-LIST (element-variable PAIR) (list-expression CDR) next . rest))

    ((IN-LIST variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (IN-LIST variables arguments
                                 "Malformed IN-LIST clause in LOOP:")))))

;;;;; Parallel List Iteration

(define-syntax in-lists
  (syntax-rules ()
    ((IN-LISTS (elements-variable pairs-variable)
               (lists-expression tail-expression)
               next . rest)
     (next (((LISTS) lists-expression))   ;Outer bindings
           ((pairs-variable LISTS CDRS))  ;Loop variables
           (((LOSE? CARS CDRS)            ;Entry bindings
             (%CARS&CDRS pairs-variable tail-expression '())))
           (LOSE?)                        ;Termination conditions
           (((elements-variable) CARS))   ;Body bindings
           ()                             ;Final bindings
           . rest))

    ((IN-LISTS (elements-variable pairs-variable) (lists) next . rest)
     (IN-LISTS (elements-variable pairs-variable) (lists '()) next . rest))

    ((IN-LISTS (elements-variable) (lists tail) next . rest)
     (IN-LISTS (elements-variable PAIRS) (lists tail) next . rest))

    ((IN-LISTS (elements-variable) (lists) next . rest)
     (IN-LISTS (elements-variable PAIRS) (lists '()) next . rest))

    ((IN-LISTS variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (IN-LISTS variables arguments
                                  "Malformed IN-LISTS clause in LOOP:")))))

(define (%cars&cdrs lists cars-tail cdrs-tail)
  (loop proceed ((for list (in-list lists))
                 (for cars (listing (initial cars-tail) (car list)))
                 (for cdrs (listing (initial cdrs-tail) (cdr list))))
    => (values #f cars cdrs)
    (if (pair? list)
        (proceed)
        (values #t #f #f))))

;;;; Vector and String Iteration

;;; (FOR <elt> [<index>] (IN-VECTOR <vector> [<start> [<end>]]))
;;;
;;; IN-VECTOR-REVERSE, IN-STRING, and IN-STRING-REVERSE all have the
;;; same syntax.
;;;
;;; The reverse iterators run from end to start; the bounds are still
;;; given in the same order as the forward iterators.

(define-syntax in-vector
  (syntax-rules ()
    ((IN-VECTOR variables (vector-expression start/end ...) next . rest)
     (%IN-VECTOR (FORWARD VECTOR-REF VECTOR 0 (VECTOR-LENGTH VECTOR))
                 variables (vector-expression start/end ...)
                 (IN-VECTOR variables (vector-expression start/end ...)
                            "Malformed IN-VECTOR clause in LOOP:")
                 next . rest))))

(define-syntax in-vector-reverse
  (syntax-rules ()
    ((IN-VECTOR-REVERSE variables (vector-expression start/end ...)
                        next . rest)
     (%IN-VECTOR (BACKWARD VECTOR-REF VECTOR (VECTOR-LENGTH VECTOR) 0)
                 variables (vector-expression start/end ...)
                 (IN-VECTOR-REVERSE
                  variables (vector-expression start/end ...)
                  "Malformed IN-VECTOR-REVERSE clause in LOOP:")
                 next . rest))))

(define-syntax in-string
  (syntax-rules ()
    ((IN-STRING variables (vector-expression start/end ...) next . rest)
     (%IN-VECTOR (FORWARD STRING-REF STRING 0 (STRING-LENGTH STRING))
                 variables (vector-expression start/end ...)
                 (IN-STRING variables (vector-expression start/end ...)
                            "Malformed IN-STRING clause in LOOP:")
                 next . rest))))

(define-syntax in-string-reverse
  (syntax-rules ()
    ((IN-STRING-REVERSE variables (string-expression start/end ...)
                        next . rest)
     (%IN-VECTOR (BACKWARD STRING-REF STRING (STRING-LENGTH STRING) 0)
                 variables (string-expression start/end ...)
                 (IN-STRING-REVERSE
                  variables (string-expression start/end ...)
                  "Malformed IN-STRING-REVERSE clause in LOOP:")
                 next . rest))))

;;;;; Random-Access Sequence Generalization

(define-syntax %in-vector
  (syntax-rules (FORWARD BACKWARD)
    ((%IN-VECTOR (FORWARD vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (next (((vector-variable START END);Outer bindings
             (LET ((vector-variable vector-expression))
               (VALUES vector-variable start-expression end-expression))))
           ((index-variable START       ;Loop variables
                            (+ index-variable 1)))
           ()                           ;Entry bindings
           ((>= index-variable END))    ;Termination conditions
           (((element-variable)         ;Body bindings
             (vector-ref vector-variable index-variable)))
           ()                           ;Final bindings
           . rest))

    ((%IN-VECTOR (BACKWARD
                  vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (next (((vector-variable START END);Outer bindings
             (LET ((vector-variable vector-expression))
               (VALUES vector-variable start-expression end-expression))))
           ((index-variable START       ;Loop variables
                            index-variable))
           ()                           ;Entry bindings
           ((<= index-variable END))    ;Termination conditions
           (((index-variable)           ;Body bindings
             (- index-variable 1))
            ((element-variable)
             (vector-ref vector-variable (- index-variable 1))))
           ()                           ;Final bindings
           . rest))

    ;; Supply an index variable if absent.
    ((%IN-VECTOR iteration-parameters (element-variable) arguments
                 error-context next . rest)
     (%IN-VECTOR iteration-parameters (element-variable INDEX) arguments
                 error-context next . rest))

    ;; Supply the default start index if necessary.
    ((%IN-VECTOR (direction vector-ref variable default-start default-end)
                 variables (vector-expression)
                 error-context next . rest)
     (%IN-VECTOR (direction vector-ref variable default-start default-end)
                 variables (vector-expression default-start)
                 error-context next . rest))

    ;; Supply the default end index if necessary.
    ((%IN-VECTOR (direction vector-ref variable default-start default-end)
                 variables (vector-expression start-expression)
                 error-context next . rest)
     (%IN-VECTOR (direction vector-ref variable default-start default-end)
                 variables (vector-expression start-expression default-end)
                 error-context next . rest))

    ((%IN-VECTOR iteration-parameters modified-variables modified-arguments
                 error-context next . rest)
     (LOOP-CLAUSE-ERROR error-context))))

;;;; Input

;;; (FOR <item> (IN-PORT <input-port> [<reader> [<eof?>]]))
;;;
;;; IN-FILE has the same syntax, but with a pathname in the place of
;;; the input port.

(define-syntax in-port
  (syntax-rules ()
    ((IN-PORT (datum-variable)
              (port-expression reader-expression eof-predicate)
              next . rest)
     (next (((PORT) port-expression)              ;Outer bindings
            ((READER) reader-expression)
            ((EOF?) eof-predicate))
           ()                                     ;Loop variables
           (((datum-variable) (READER PORT)))     ;Entry bindings
           ((EOF? datum-variable))                ;Termination conditions
           ()                                     ;Body bindings
           ()                                     ;Final bindings
           . rest))

    ;; Supply a reader if absent.
    ((IN-PORT (datum-variable) (port-expression) next . rest)
     (IN-PORT (datum-variable) (port-expression READ-CHAR) next . rest))

    ;; Supply an EOF predicate if absent.
    ((IN-PORT (datum-variable) (port-expression reader-expression) next . rest)
     (IN-PORT (datum-variable) (port-expression reader-expression EOF-OBJECT?)
              next . rest))

    ((IN-PORT variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (IN-PORT variables arguments
                                 "Malformed IN-PORT clause in LOOP:")))))

(define-syntax in-file
  (syntax-rules ()
    ((IN-FILE (datum-variable)
              (pathname-expression reader-expression eof-predicate)
              next . rest)
     (next (((PORT)                               ;Outer bindings
             (OPEN-INPUT-FILE pathname-expression))
            ((READER) reader-expression)
            ((EOF?) eof-predicate))
           ()                                     ;Loop variables
           (((datum-variable) (READER PORT)))     ;Entry bindings
           ((EOF? datum-variable))                ;Termination conditions
           ()                                     ;Body bindings
           ((()                                   ;Final bindings
             (BEGIN (CLOSE-INPUT-PORT PORT)
                    (VALUES))))
           . rest))

    ;; Supply a reader if absent.
    ((IN-FILE (datum-variable) (pathname-expression) next . rest)
     (IN-FILE (datum-variable) (pathname-expression READ-CHAR) next . rest))

    ;; Supply an EOF predicate if absent.
    ((IN-FILE (datum-variable) (pathname-expression reader) next . rest)
     (IN-FILE (datum-variable) (pathname-expression reader EOF-OBJECT?)
              next . rest))

    ((IN-FILE variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (IN-FILE variables arguments
                                 "Malformed IN-FILE clause in LOOP:")))))

;;;; Iterating Up through Numbers

(define-syntax up-from
  (syntax-rules (TO BY)
    ((UP-FROM (variable)
              (start-expression (TO end-expression)
                                (BY step-expression))
              next . rest)
     (next (((START) start-expression)  ;Outer bindings
            ((END) end-expression)
            ((STEP) step-expression))
           ((variable START             ;Loop variables
                      (+ variable STEP)))
           ()                           ;Entry bindings
           ((>= variable END))          ;Termination conditions
           ()                           ;Body bindings
           ()                           ;Final bindings
           . rest))

    ((UP-FROM (variable)
              (start-expression (BY step-expression))
              next . rest)
     (next (((START) start-expression)  ;Outer bindings
            ((STEP) step-expression))
           ((variable START             ;Loop variables
                      (+ variable STEP)))
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           ()                           ;Body bindings
           ()                           ;Final bindings
           . rest))

    ;; Add a default step of 1.
    ((UP-FROM (variable)
              (start-expression (TO end-expression))
              next . rest)
     (UP-FROM (variable)
              (start-expression (TO end-expression) (BY 1))
              next . rest))

    ((UP-FROM (variable)
              (start-expression)
              next . rest)
     (UP-FROM (variable)
              (start-expression (BY 1))
              next . rest))

    ((UP-FROM variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (UP-FROM variables arguments
                                 "Malformed UP-FROM clause in LOOP:")))))

;;;; Iterating Down through Numbers

(define-syntax down-from
  (syntax-rules (TO BY)
    ((DOWN-FROM (variable)
                (start-expression (TO end-expression)
                                  (BY step-expression))
                next . rest)
     (next (((START) start-expression)  ;Outer bindings
            ((END) end-expression)
            ((STEP) step-expression))
           ((variable START variable))  ;Loop variables
           ()                           ;Entry bindings
           ((<= variable END))          ;Termination conditions
           (((variable)                 ;Body bindings
             (- variable STEP)))
           ()                           ;Final bindings
           . rest))

    ((DOWN-FROM (variable)
                (start-expression (BY step-expression))
                next . rest)
     (next (((START) start-expression)  ;Outer bindings
            ((STEP) step-expression))
           ((variable START variable))  ;Loop variables
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           (((variable)                 ;Body bindings
             (- variable STEP)))
           ()                           ;Final bindings
           . rest))

    ;; Add a default step of 1.
    ((DOWN-FROM (variable)
                (start-expression (TO end-expression))
                next . rest)
     (DOWN-FROM (variable)
                (start-expression (TO end-expression)
                                  (BY 1))
                next . rest))

    ((DOWN-FROM (variable)
                (start-expression)
                next . rest)
     (DOWN-FROM (variable)
                (start-expression (BY 1))
                next . rest))

    ((DOWN-FROM variables arguments next . rest)
     (LOOP-CLAUSE-ERROR (DOWN-FROM variables arguments
                                   "Malformed DOWN-FROM clause in LOOP:")))))
