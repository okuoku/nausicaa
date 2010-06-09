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
      '#(((*default* . *error*) (COLON . 2) (NUMBER . 1))
         ((*default* . -8) (COLON . 5))
         ((*default* . *error*) (COLON . 8))
         ((*default* . -4))
         ((*default* . *error*) (*eoi* . 9))
         ((*default* . *error*) (COLON . 8) (NUMBER . 10))
         ((*default* . -2)) ((*default* . -3))
         ((*default* . -12) (NUMBER . 13))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -8) (DOT . 16) (COLON . 5))
         ((*default* . *error*) (DOT . 19))
         ((*default* . -5))
         ((*default* . -15) (DOT . 16) (COLON . 20))
         ((*default* . -11)) ((*default* . -9))
         ((*default* . *error*) (NUMBER . 22))
         ((*default* . -7)) ((*default* . -6))
         ((*default* . *error*) (NUMBER . 23))
         ((*default* . *error*) (NUMBER . 25))
         ((*default* . -10)) ((*default* . -17))
         ((*default* . *error*) (DOT . 16))
         ((*default* . -16))
         ((*default* . -15) (DOT . 16) (COLON . 20))
         ((*default* . -14)) ((*default* . -13)))
      (vector '((3 . 3) (1 . 4)) '((3 . 6) (2 . 7)) '() '()
        '() '((7 . 11) (6 . 12)) '() '()
        '((7 . 11) (6 . 14) (4 . 15)) '()
        '((3 . 17) (2 . 18)) '() '() '((5 . 21)) '() '() '()
        '() '() '((7 . 24)) '((7 . 11) (6 . 26)) '() '() '()
        '() '((5 . 27)) '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 2
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 3 (cons #f $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4
            (cons (string->number $1 16) $2) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 4 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 5
            (cons (string->number $2 16) $3) yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 $2 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 5 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 6 (list $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 7
            (+
              (bitwise-arithmetic-shift-left
                (string->number $1 10) 8)
              (string->number $3 10))
            yy-stack-states yy-stack-values))))))
