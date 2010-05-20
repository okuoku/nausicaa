(library (infix string-parser)
  (export make-infix-string-parser)
  (import (rnrs)
          (lalr lr-driver)
          (parser-tools source-location)
          (parser-tools lexical-token)
          (sentinel))
  (define (make-infix-string-parser)
    (lr-driver
      '#(((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -19))
         ((*default* . -16) (LPAREN . 10))
         ((*default* . *error*)
          (*eoi* . 24)
          (QUESTION-ID . 23)
          (ADD . 22)
          (SUB . 21)
          (MUL . 20)
          (DIV . 19)
          (DIV0 . 18)
          (MOD . 17)
          (EXPT . 16)
          (LT . 15)
          (GT . 14)
          (LE . 13)
          (GE . 12)
          (EQ . 11))
         ((*default* . -15)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20))
         ((*default* . -14))
         ((*default* . *error*)
          (QUESTION-ID . 23)
          (RPAREN . 25)
          (ADD . 22)
          (SUB . 21)
          (MUL . 20)
          (DIV . 19)
          (DIV0 . 18)
          (MOD . 17)
          (EXPT . 16)
          (LT . 15)
          (GT . 14)
          (LE . 13)
          (GE . 12)
          (EQ . 11))
         ((*default* . -21) (SUB . 1) (ADD . 2) (LPAREN . 3) (NUM . 4) (ID . 5))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -20))
         ((*default* . *error*) (RPAREN . 41))
         ((*default* . -24)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20)
          (SUB . 21)
          (ADD . 22)
          (COMMA . 42)
          (QUESTION-ID . 23))
         ((*default* . -13))
         ((*default* . -12))
         ((*default* . -11))
         ((*default* . -10))
         ((*default* . -9))
         ((*default* . -8) (EQ . 11) (GE . 12) (LE . 13) (GT . 14) (LT . 15))
         ((*default* . -7) (EQ . 11) (GE . 12) (LE . 13) (GT . 14) (LT . 15) (EXPT . 16))
         ((*default* . -6) (EQ . 11) (GE . 12) (LE . 13) (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17))
         ((*default* . -4) (EQ . 11) (GE . 12) (LE . 13) (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17))
         ((*default* . -5) (EQ . 11) (GE . 12) (LE . 13) (GT . 14) (LT . 15) (EXPT . 16) (MOD . 17))
         ((*default* . -3)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20))
         ((*default* . -2)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20))
         ((*default* . *error*)
          (QUESTION-ID . 23)
          (COLON-ID . 44)
          (ADD . 22)
          (SUB . 21)
          (MUL . 20)
          (DIV . 19)
          (DIV0 . 18)
          (MOD . 17)
          (EXPT . 16)
          (LT . 15)
          (GT . 14)
          (LE . 13)
          (GE . 12)
          (EQ . 11))
         ((*default* . -17))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -22))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -24)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20)
          (SUB . 21)
          (ADD . 22)
          (COMMA . 42)
          (QUESTION-ID . 23))
         ((*default* . -18)
          (EQ . 11)
          (GE . 12)
          (LE . 13)
          (GT . 14)
          (LT . 15)
          (EXPT . 16)
          (MOD . 17)
          (DIV0 . 18)
          (DIV . 19)
          (MUL . 20)
          (SUB . 21)
          (ADD . 22)
          (QUESTION-ID . 23))
         ((*default* . -23)))
      (vector
        '((1 . 6))
        '((1 . 7))
        '((1 . 8))
        '((1 . 9))
        '()
        '()
        '()
        '()
        '()
        '()
        '((2 . 26) (1 . 27))
        '((1 . 28))
        '((1 . 29))
        '((1 . 30))
        '((1 . 31))
        '((1 . 32))
        '((1 . 33))
        '((1 . 34))
        '((1 . 35))
        '((1 . 36))
        '((1 . 37))
        '((1 . 38))
        '((1 . 39))
        '((1 . 40))
        '()
        '()
        '()
        '((3 . 43))
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '((1 . 45))
        '()
        '((1 . 46))
        '((3 . 47))
        '()
        '())
      (vector
        '()
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 (list $2 $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 $2 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (list $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 4 1 (cons $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 5 1 (list 'if $1 $3 $5) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 1 $2 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 3 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 3 '() yy-stack-states yy-stack-values))))))