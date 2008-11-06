#!r6rs
(library (srfi random)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  
  (import (except (rnrs) error)
          (rnrs r5rs)
          (srfi parameters)
          (only (srfi time) time-nanosecond current-time)
          (prefix (srfi error-reporting) ER:)
          (srfi private include-resolve)
          )
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi random))"])
      (apply ER:error args)))
  
   (include/resolve ("srfi" "random") "random.sls")
  )
