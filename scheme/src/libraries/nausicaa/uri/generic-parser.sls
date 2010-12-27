(library (nausicaa uri generic-parser)
  (export make-uri-generic-parser)
  (import
    (rnrs)
    (nausicaa lalr lr-driver)
    (nausicaa parser-tools source-location)
    (nausicaa parser-tools lexical-token)
    (nausicaa language sentinel))
  (define (make-uri-generic-parser)
    (lr-driver
      '#(((*default* . *error*) (COMPONENT . 1))
         ((*default* . *error*) (COLON . 3))
         ((*default* . *error*) (*eoi* . 4))
         ((*default* . *error*) (SLASH . 5))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (SLASH . 6))
         ((*default* . *error*) (COMPONENT . 7))
         ((*default* . -7) (SLASH . 8))
         ((*default* . -6) (COMPONENT . 9))
         ((*default* . -10) (SLASH . 11))
         ((*default* . -5) (QUESTION . 13)
           (NUMBER-SIGN . 12))
         ((*default* . -9) (COMPONENT . 9))
         ((*default* . *error*) (COMPONENT . 15))
         ((*default* . *error*) (COMPONENT . 16))
         ((*default* . -8)) ((*default* . -4))
         ((*default* . -3) (NUMBER-SIGN . 17))
         ((*default* . *error*) (COMPONENT . 18))
         ((*default* . -2)))
      (vector '((1 . 2)) '() '() '() '() '() '() '()
        '((2 . 10)) '() '() '((2 . 14)) '() '() '() '() '()
        '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $11 $10 $9 $8 $7 $6 $5 $4 $3 $2
           $1 . yy-stack-values)
          (yy-reduce-pop-and-push 11 1
            (values $1 $5 $7 $8 $10) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $9 $8 $7 $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 9 1
            (values $1 $5 $7 $8 #f) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $9 $8 $7 $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 9 1
            (values $1 $5 $7 #f $9) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $7 $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 7 1
            (values $1 $5 $7 #f #f) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $6 $5 $4 $3 $2 $1
           . yy-stack-values)
          (yy-reduce-pop-and-push 6 1
            (values $1 $5 #f #f #f) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 1
            (values $1 $5 #f #f #f) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 (list $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 (list $1)
            yy-stack-states yy-stack-values))))))
