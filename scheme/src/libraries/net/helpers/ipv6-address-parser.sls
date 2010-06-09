(library (net helpers ipv6-address-parser)
  (export make-ipv6-address-parser)
  (import
    (rnrs)
    (lalr lr-driver)
    (parser-tools source-location)
    (parser-tools lexical-token)
    (sentinel))
  (define (make-ipv6-address-parser)
    (lr-driver
      '#(((*default* . *error*) (HEXINT . 1))
         ((*default* . *error*) (COLON . 3))
         ((*default* . *error*) (*eoi* . 4))
         ((*default* . *error*) (HEXINT . 5))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (COLON . 6))
         ((*default* . *error*) (HEXINT . 7))
         ((*default* . *error*) (COLON . 8))
         ((*default* . *error*) (HEXINT . 9))
         ((*default* . *error*) (COLON . 10))
         ((*default* . *error*) (HEXINT . 11))
         ((*default* . *error*) (COLON . 12))
         ((*default* . *error*) (HEXINT . 13))
         ((*default* . *error*) (COLON . 14))
         ((*default* . *error*) (HEXINT . 15))
         ((*default* . *error*) (COLON . 16))
         ((*default* . *error*) (HEXINT . 17))
         ((*default* . -2)))
      (vector '((1 . 2)) '() '() '() '() '() '() '() '() '()
        '() '() '() '() '() '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $15 $14 $13 $12 $11 $10 $9 $8 $7
           $6 $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 15 1 #f yy-stack-states
            yy-stack-values))))))
