#!r6rs
(library (xitomatl srfi eager-comprehensions)
  (export
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import
    (except (rnrs) error)
    (rnrs r5rs)
    (xitomatl srfi parameters)
    (prefix (xitomatl srfi error-reporting) ER:)
    (xitomatl srfi private include-resolve))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    "(library (xitomatl srfi eager-comprehensions))"])
      (apply ER:error args)))
  
  (include/resolve ("xitomatl" "srfi" "eager-comprehensions") "ec.scm")  
)
