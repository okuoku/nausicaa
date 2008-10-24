#!r6rs
(library (xitomatl srfi compare)
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
          (xitomatl srfi random)  ; for random-integer
          (xitomatl srfi parameters)
          (prefix (xitomatl srfi error-reporting) ER:)
          (xitomatl srfi private include-resolve))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (xitomatl srfi compare))"])
      (apply ER:error args)))
  
  (include/resolve ("xitomatl" "srfi" "compare") "compare.ss")  
  )
