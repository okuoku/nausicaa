(library (infix string-parser)
  (export make-infix-string-parser
          make-source-location
          source-location?
          source-location-line
          source-location-input
          source-location-column
          source-location-offset
          source-location-length
          make-lexical-token
          lexical-token?
          lexical-token-value
          lexical-token-category
          lexical-token-source
          lexical-token?/end-of-input)
  (import (rnrs) (lalr lr-driver) (lalr common) (sentinel))
  (define (make-infix-string-parser)
    (lr-driver
      '#(((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -9))
         ((*default* . -7) (LPAREN . 10))
         ((*default* . *error*) (*eoi* . 14) (ADD . 13) (SUB . 12) (OPERATOR . 11))
         ((*default* . -6))
         ((*default* . -5))
         ((*default* . *error*) (RPAREN . 15) (ADD . 13) (SUB . 12) (OPERATOR . 11))
         ((*default* . -11) (SUB . 1) (ADD . 2) (LPAREN . 3) (NUM . 4) (ID . 5))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -10))
         ((*default* . *error*) (RPAREN . 21))
         ((*default* . -14) (OPERATOR . 11) (SUB . 12) (ADD . 13) (COMMA . 22))
         ((*default* . -4))
         ((*default* . -3))
         ((*default* . -2))
         ((*default* . -8))
         ((*default* . *error*) (ID . 5) (NUM . 4) (LPAREN . 3) (ADD . 2) (SUB . 1))
         ((*default* . -12))
         ((*default* . -14) (OPERATOR . 11) (SUB . 12) (ADD . 13) (COMMA . 22))
         ((*default* . -13)))
      (vector
        '((1 . 6) (1 . 6) (1 . 6) (1 . 6))
        '((1 . 7) (1 . 7) (1 . 7) (1 . 7))
        '((1 . 8) (1 . 8) (1 . 8) (1 . 8))
        '((1 . 9) (1 . 9) (1 . 9) (1 . 9))
        '()
        '()
        '()
        '()
        '()
        '()
        '((2 . 16) (1 . 17) (1 . 17) (1 . 17) (1 . 17))
        '((1 . 18) (1 . 18) (1 . 18) (1 . 18))
        '((1 . 19) (1 . 19) (1 . 19) (1 . 19))
        '((1 . 20) (1 . 20) (1 . 20) (1 . 20))
        '()
        '()
        '()
        '((3 . 23))
        '()
        '()
        '()
        '()
        '((1 . 24) (1 . 24) (1 . 24) (1 . 24))
        '()
        '((3 . 25))
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
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 $2 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (- $2) yy-stack-states yy-stack-values))
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