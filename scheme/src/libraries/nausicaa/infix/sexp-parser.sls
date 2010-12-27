(library (infix sexp-parser)
  (export make-infix-sexp-parser)
  (import
    (rnrs)
    (lalr lr-driver)
    (parser-tools source-location)
    (parser-tools lexical-token)
    (sentinel))
  (define (make-infix-sexp-parser)
    (lr-driver
      '#(((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -18))
         ((*default* . -15) (LPAREN . 10))
         ((*default* . *error*) (*eoi* . 23)
           (QUESTION-ID . 22) (ADD . 21) (SUB . 20)
           (MUL . 19) (DIV . 18) (MOD . 17) (EXPT . 16)
           (LT . 15) (GT . 14) (LE . 13) (GE . 12) (EQ . 11))
         ((*default* . -14) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19)) ((*default* . -13))
         ((*default* . *error*) (QUESTION-ID . 22)
           (RPAREN . 24) (ADD . 21) (SUB . 20) (MUL . 19)
           (DIV . 18) (MOD . 17) (EXPT . 16) (LT . 15)
           (GT . 14) (LE . 13) (GE . 12) (EQ . 11))
         ((*default* . -20) (SUB . 1) (ADD . 2) (LPAREN . 3)
           (NUM . 4) (ID . 5))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -19))
         ((*default* . *error*) (RPAREN . 39))
         ((*default* . -23) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19) (SUB . 40) (ADD . 41)
           (LPAREN . 3) (NUM . 4) (QUESTION-ID . 22)
           (ID . 5)) ((*default* . -12)) ((*default* . -11))
         ((*default* . -10)) ((*default* . -9))
         ((*default* . -8))
         ((*default* . -7) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15))
         ((*default* . -6) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16))
         ((*default* . -4) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17))
         ((*default* . -5) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17))
         ((*default* . -3) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19))
         ((*default* . -2) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19))
         ((*default* . *error*) (QUESTION-ID . 22)
           (COLON-ID . 44) (ADD . 21) (SUB . 20) (MUL . 19)
           (DIV . 18) (MOD . 17) (EXPT . 16) (LT . 15)
           (GT . 14) (LE . 13) (GE . 12) (EQ . 11))
         ((*default* . -16))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -21))
         ((*default* . -23) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19) (SUB . 40) (ADD . 41)
           (LPAREN . 3) (NUM . 4) (QUESTION-ID . 22)
           (ID . 5))
         ((*default* . *error*) (ID . 5) (NUM . 4)
           (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -3) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19))
         ((*default* . -2) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19)) ((*default* . -22))
         ((*default* . -17) (EQ . 11) (GE . 12) (LE . 13)
           (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17)
           (DIV . 18) (MUL . 19) (SUB . 20) (ADD . 21)
           (QUESTION-ID . 22)))
      (vector '((1 . 6)) '((1 . 7)) '((1 . 8)) '((1 . 9))
        '() '() '() '() '() '() '((2 . 25) (1 . 26))
        '((1 . 27)) '((1 . 28)) '((1 . 29)) '((1 . 30))
        '((1 . 31)) '((1 . 32)) '((1 . 33)) '((1 . 34))
        '((1 . 35)) '((1 . 36)) '((1 . 37)) '((1 . 38)) '()
        '() '() '((3 . 42) (1 . 43)) '() '() '() '() '() '()
        '() '() '() '() '() '() '() '((1 . 45)) '((1 . 46))
        '() '((3 . 47) (1 . 43)) '((1 . 48)) '() '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (list $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 1 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 1 (list $2 $1 $3 $5)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 3 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 3 '() yy-stack-states
            yy-stack-values))))))
