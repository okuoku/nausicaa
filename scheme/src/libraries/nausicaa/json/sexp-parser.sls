(library (nausicaa json sexp-parser)
  (export make-json-sexp-parser)
  (import
    (rnrs)
    (nausicaa lalr lr-driver)
    (nausicaa parser-tools source-location)
    (nausicaa parser-tools lexical-token)
    (nausicaa language sentinel))
  (define (make-json-sexp-parser)
    (lr-driver
      '#(((*default* . *error*) (BEGIN_ARRAY . 2)
           (BEGIN_OBJECT . 1))
         ((*default* . *error*) (END_OBJECT . 7)
           (STRING . 6))
         ((*default* . *error*) (BEGIN_ARRAY . 2)
           (END_ARRAY . 14) (BEGIN_OBJECT . 1) (FALSE . 13)
           (TRUE . 12) (NULL . 11) (NUMBER . 10)
           (STRING . 9)) ((*default* . -3))
         ((*default* . -2))
         ((*default* . *error*) (*eoi* . 18))
         ((*default* . *error*) (NAME_SEPARATOR . 19))
         ((*default* . -4))
         ((*default* . *error*) (END_OBJECT . 21)
           (VALUE_SEPARATOR . 20)) ((*default* . -17))
         ((*default* . -16)) ((*default* . -14))
         ((*default* . -15)) ((*default* . -13))
         ((*default* . -10))
         ((*default* . *error*) (END_ARRAY . 24)
           (VALUE_SEPARATOR . 23)) ((*default* . -19))
         ((*default* . -18))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (BEGIN_ARRAY . 2)
           (BEGIN_OBJECT . 1) (FALSE . 13) (TRUE . 12)
           (NULL . 11) (NUMBER . 10) (STRING . 9))
         ((*default* . *error*) (STRING . 6))
         ((*default* . -5))
         ((*default* . *error*) (END_OBJECT . 28))
         ((*default* . *error*) (BEGIN_ARRAY . 2)
           (BEGIN_OBJECT . 1) (FALSE . 13) (TRUE . 12)
           (NULL . 11) (NUMBER . 10) (STRING . 9))
         ((*default* . -11))
         ((*default* . *error*) (END_ARRAY . 30))
         ((*default* . -7))
         ((*default* . -8) (VALUE_SEPARATOR . 20))
         ((*default* . -6))
         ((*default* . -20) (VALUE_SEPARATOR . 23))
         ((*default* . -12)) ((*default* . -9))
         ((*default* . -21)))
      (vector '((5 . 3) (2 . 4) (1 . 5)) '((3 . 8))
        '((6 . 15) (5 . 16) (2 . 17)) '() '() '() '() '()
        '((4 . 22)) '() '() '() '() '() '() '((7 . 25)) '()
        '() '() '((6 . 26) (5 . 16) (2 . 17)) '((3 . 27))
        '() '() '((6 . 29) (5 . 16) (2 . 17)) '() '() '()
        '((4 . 31)) '() '((7 . 32)) '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2 (list $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 2 (cons $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 3 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4 (list $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 4 (cons $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 '#() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (vector $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 5
            (list->vector (cons $2 $3)) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 7 (list $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 7 (cons $2 $3)
            yy-stack-states yy-stack-values))))))
