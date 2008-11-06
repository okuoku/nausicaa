#!r6rs
(library (srfi compare)
  (export  </<=? </<? <=/<=? <=/<? <=? <? =?
           >/>=? >/>? >=/>=? >=/>? >=? >?
           boolean-compare chain<=? chain<? chain=? chain>=? chain>?
           char-compare char-compare-ci
           compare-by< compare-by<= compare-by=/< compare-by=/> compare-by> 
           compare-by>= complex-compare cond-compare
           debug-compare default-compare
           if-not=? if3 if<=? if<? if=? if>=? if>? integer-compare
           kth-largest list-compare list-compare-as-vector
           max-compare min-compare not=? number-compare
           pair-compare pair-compare-car pair-compare-cdr
           pairwise-not=? rational-compare real-compare
           refine-compare select-compare string-compare string-compare-ci 
           symbol-compare vector-compare vector-compare-as-list)
  
  (import (except (rnrs) error)
          (rnrs r5rs)    ; for modulo
          (srfi random)  ; for random-integer
          (srfi parameters)
          (prefix (srfi error-reporting) ER:)
          (srfi private include-resolve))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (srfi compare))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "compare") "compare.sls")
  )
