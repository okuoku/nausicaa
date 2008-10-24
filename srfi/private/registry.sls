#!r6rs
(library (xitomatl srfi private registry)
  (export
    available-features)
  (import 
    (rnrs)
    (xitomatl srfi private implementation-features))
  
  (define srfi-features
    (map 
     (lambda (x)
       (list `(xitomatl srfi ,(car x)) 
             (string->symbol (string-append "srfi-" (number->string (cadr x))))))
     ;  xitomatl name     SRFI code number
     '([cond-expand             0]
       [lists                   1]
       [and-let*                2]
       [string-ports            6]
       [receive                 8]
       [records                 9]
       [let-values             11]
       [strings                13]
       [char-set               14]
       [case-lambda            16]
       [time                   19]
       [error-reporting        23]
       [cut                    26]
       [random                 27]
       [rec                    31]
       [args-fold              37]
       [sharing                38]
       [parameters             39]
       [streams                41]
       [eager-comprehensions   42]
       [vectors                43]
       [format                 48]
       [general-cond           61]
       [compare                67]
       [lightweight-testing    78])))
  
  (define available-features
    (apply append
           '(r6rs)
           implementation-features
           srfi-features))
  
)
