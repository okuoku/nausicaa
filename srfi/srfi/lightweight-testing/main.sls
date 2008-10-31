#!r6rs
(library (srfi lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?
    ;;; All of (srfi eager-comprehensions):
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import 
    (except (rnrs) error)
    (srfi lightweight-testing compat)
    (srfi parameters)
    (srfi private include-resolve)
    (prefix (srfi error-reporting) ER:)
    (srfi eager-comprehensions))
  
  (define (error . args)
    (parameterize ([ER:error-who
                    "(library (srfi lightweight-testing))"])
      (apply ER:error args)))
  
  (include/resolve ("srfi" "lightweight-testing") "check.scm")
  
  (set! check:write pretty-print/no-trailing-newline)  
)
