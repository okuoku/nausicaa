#!r6rs
(library (xitomatl srfi lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?
    ;;; All of (xitomatl srfi eager-comprehensions):
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import 
    (except (rnrs) error)
    (xitomatl srfi lightweight-testing compat)
    (xitomatl srfi parameters)
    (xitomatl srfi private include-resolve)
    (prefix (xitomatl srfi error-reporting) ER:)
    (xitomatl srfi eager-comprehensions))
  
  (define (error . args)
    (parameterize ([ER:error-who
                    "(library (xitomatl srfi lightweight-testing))"])
      (apply ER:error args)))
  
  (include/resolve ("xitomatl" "srfi" "lightweight-testing") "check.scm")
  
  (set! check:write pretty-print/no-trailing-newline)  
)
