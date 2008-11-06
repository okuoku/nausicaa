#!r6rs
(library (srfi error-reporting)
  (export error error-who)
  (import 
    (rename (rnrs base) (error rnrs:error))
    (srfi parameters))
  
  (define error-who (make-parameter #f))
  
  (define (error . args)
    (apply rnrs:error (error-who) args))
)
