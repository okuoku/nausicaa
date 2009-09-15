(library (email addresses parser)
  (export make-address-parser)
  (import (rnrs)
          (lalr lr-driver)
          (parser-tools source-location)
          (parser-tools lexical-token)
          (sentinel)
          (email addresses common)
          (strings))
  (define (make-address-parser)
    (lr-driver
      '#(((*default* . -8) (COMMA . 4) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (COLON . -38))
         ((*default* . -35))
         ((*default* . *error*) (ATOM . 15) (AT . 14))
         ((*default* . -38) (DOT . 19) (ATOM . 18) (AT . -33))
         ((*default* . -8) (COMMA . 4) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (COLON . -38))
         ((*default* . -36))
         ((*default* . -34))
         ((*default* . *error*) (COLON . 26) (ANGLE-OPEN . 25))
         ((*default* . *error*) (AT . 27))
         ((*default* . -20))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -4))
         ((*default* . *error*) (*eoi* . 30))
         ((*default* . *error*) (ATOM . 32) (DOMAIN-LITERAL-OPEN . 31))
         ((*default* . -33) (DOT . 19))
         ((*default* . *error*) (ANGLE-CLOSE . 36))
         ((*default* . *error*) (ATOM . 15))
         ((*default* . -38) (ATOM . 18))
         ((*default* . *error*) (ATOM . 38))
         ((*default* . -37))
         ((*default* . -31))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -7))
         ((*default* . *error*) (ATOM . 15) (AT . 14))
         ((*default* . *error*)
          (COMMA . 44)
          (ATOM . 3)
          (QUOTED-TEXT . 1)
          (ANGLE-OPEN . 2)
          (SEMICOLON . 43))
         ((*default* . *error*) (ATOM . 32) (DOMAIN-LITERAL-OPEN . 31))
         ((*default* . -3))
         ((*default* . -2))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 50))
         ((*default* . -29) (DOT . 51))
         ((*default* . -26))
         ((*default* . -25))
         ((*default* . *error*) (COMMA . 54) (COLON . 53))
         ((*default* . -19))
         ((*default* . *error*) (ANGLE-CLOSE . 56))
         ((*default* . -33) (DOT . 19))
         ((*default* . -6))
         ((*default* . -5))
         ((*default* . *error*) (ANGLE-CLOSE . 58))
         ((*default* . *error*) (ATOM . 15))
         ((*default* . -9))
         ((*default* . -15) (COMMA . 44) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2))
         ((*default* . *error*) (ANGLE-OPEN . 25))
         ((*default* . -15) (COMMA . 44))
         ((*default* . -12))
         ((*default* . *error*) (SEMICOLON . 63))
         ((*default* . -24))
         ((*default* . *error*) (DOT . 64))
         ((*default* . *error*) (ATOM . 65))
         ((*default* . -27))
         ((*default* . -23))
         ((*default* . *error*) (AT . 66))
         ((*default* . -21))
         ((*default* . -18))
         ((*default* . -32))
         ((*default* . -17))
         ((*default* . *error*) (ANGLE-CLOSE . 67))
         ((*default* . -15) (COMMA . 44))
         ((*default* . -14))
         ((*default* . -11))
         ((*default* . -10))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 69))
         ((*default* . -29) (DOT . 51))
         ((*default* . *error*) (ATOM . 32) (DOMAIN-LITERAL-OPEN . 31))
         ((*default* . -16))
         ((*default* . -13))
         ((*default* . *error*) (DOT . 72))
         ((*default* . -28))
         ((*default* . *error*) (COMMA . 54) (COLON . 53))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 74))
         ((*default* . -22))
         ((*default* . *error*) (DOT . 75))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 76))
         ((*default* . *error*) (DOMAIN-LITERAL-CLOSE . 77))
         ((*default* . -30)))
      (vector
        '((18 . 5) (17 . 6) (16 . 7) (14 . 8) (9 . 9) (6 . 10) (3 . 11) (2 . 12) (1 . 13))
        '()
        '((14 . 8) (9 . 16) (7 . 17))
        '((18 . 20) (15 . 21))
        '((18 . 5) (17 . 6) (16 . 7) (14 . 8) (9 . 9) (6 . 22) (3 . 23) (2 . 24))
        '()
        '()
        '()
        '()
        '()
        '((2 . 28))
        '((2 . 29))
        '()
        '()
        '((13 . 33) (11 . 34) (10 . 35))
        '((15 . 21))
        '()
        '((14 . 8) (9 . 37))
        '((18 . 20))
        '()
        '()
        '()
        '((2 . 39))
        '((2 . 40))
        '()
        '((14 . 8) (9 . 41) (7 . 42))
        '((18 . 5) (17 . 6) (16 . 45) (14 . 8) (9 . 9) (6 . 46) (5 . 47) (4 . 48))
        '((13 . 33) (11 . 34) (10 . 49))
        '()
        '()
        '()
        '()
        '((12 . 52))
        '()
        '()
        '((8 . 55))
        '()
        '()
        '((15 . 57))
        '()
        '()
        '()
        '((14 . 8) (9 . 59))
        '()
        '((18 . 5) (17 . 6) (16 . 45) (14 . 8) (9 . 9) (6 . 60) (5 . 61))
        '()
        '((5 . 62))
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
        '((5 . 68))
        '()
        '()
        '()
        '()
        '((12 . 70))
        '((13 . 33) (11 . 34) (10 . 71))
        '()
        '()
        '()
        '()
        '((8 . 73))
        '()
        '()
        '()
        '()
        '()
        '())
      (vector
        '()
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (cons $1 $2) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 3 2 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 2 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 $2 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 3 (make-<group> $1 '()) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 3 (make-<group> $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 $2 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 5 '() yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 5 6 (make-<mailbox> $1 $3 $4) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 6 (make-<mailbox> $1 #f $3) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 6 (make-<mailbox> #f $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 6 (make-<mailbox> #f #f $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 (make-<mailbox> #f #f $1) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 7 (make-<route> (cons $2 $3)) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 8 (cons $3 $4) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 8 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 9 (make-<addr-spec> $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push
            2
            11
            (make-<domain> #f (cons $1 $2))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 12 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 12 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $9
                  $8
                  $7
                  $6
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push
            9
            13
            (make-<domain> #t (list $2 $4 $6 $8))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push
            2
            14
            (make-<local-part> (cons $1 $2))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 15 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 15 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 16 (string-join $1 " ") yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 16 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 17 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 18 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 18 '() yy-stack-states yy-stack-values))))))