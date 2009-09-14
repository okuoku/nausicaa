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
         ((*default* . *error*) (DOMAIN . 31))
         ((*default* . -33) (DOT . 19))
         ((*default* . *error*) (ANGLE-CLOSE . 32))
         ((*default* . *error*) (ATOM . 15))
         ((*default* . -38) (ATOM . 18))
         ((*default* . *error*) (ATOM . 34))
         ((*default* . -37))
         ((*default* . -31))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -8) (COMMA . 4))
         ((*default* . -7))
         ((*default* . *error*) (ATOM . 15) (AT . 14))
         ((*default* . *error*)
          (COMMA . 40)
          (ATOM . 3)
          (QUOTED-TEXT . 1)
          (ANGLE-OPEN . 2)
          (SEMICOLON . 39))
         ((*default* . *error*) (ATOM . 46) (DOMAIN-LITERAL-OPEN . 45))
         ((*default* . -3))
         ((*default* . -2))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . *error*) (COMMA . 51) (COLON . 50))
         ((*default* . -19))
         ((*default* . *error*) (ANGLE-CLOSE . 53))
         ((*default* . -33) (DOT . 19))
         ((*default* . -6))
         ((*default* . -5))
         ((*default* . *error*) (ANGLE-CLOSE . 55))
         ((*default* . *error*) (COLON . 56))
         ((*default* . -9))
         ((*default* . -15) (COMMA . 40) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2))
         ((*default* . *error*) (ANGLE-OPEN . 25))
         ((*default* . -15) (COMMA . 40))
         ((*default* . -12))
         ((*default* . *error*) (SEMICOLON . 60))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 61))
         ((*default* . -29) (DOT . 62))
         ((*default* . -26))
         ((*default* . -25))
         ((*default* . -24))
         ((*default* . -23))
         ((*default* . *error*) (AT . 64))
         ((*default* . -21))
         ((*default* . -18))
         ((*default* . -32))
         ((*default* . -17))
         ((*default* . *error*) (ATOM . 15))
         ((*default* . -15) (COMMA . 40))
         ((*default* . -14))
         ((*default* . -11))
         ((*default* . -10))
         ((*default* . *error*) (DOT . 67))
         ((*default* . *error*) (ATOM . 68))
         ((*default* . -27))
         ((*default* . *error*) (DOMAIN . 69))
         ((*default* . *error*) (ANGLE-CLOSE . 70))
         ((*default* . -13))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 71))
         ((*default* . -29) (DOT . 62))
         ((*default* . *error*) (COMMA . 51) (COLON . 50))
         ((*default* . -16))
         ((*default* . *error*) (DOT . 74))
         ((*default* . -28))
         ((*default* . -22))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 75))
         ((*default* . *error*) (DOT . 76))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 77))
         ((*default* . *error*) (DOMAIN-LITERAL-CLOSE . 78))
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
        '()
        '((15 . 21))
        '()
        '((14 . 8) (9 . 33))
        '((18 . 20))
        '()
        '()
        '()
        '((2 . 35))
        '((2 . 36))
        '()
        '((14 . 8) (9 . 37) (7 . 38))
        '((18 . 5) (17 . 6) (16 . 41) (14 . 8) (9 . 9) (6 . 42) (5 . 43) (4 . 44))
        '((13 . 47) (11 . 48) (10 . 49))
        '()
        '()
        '()
        '((8 . 52))
        '()
        '()
        '((15 . 54))
        '()
        '()
        '()
        '()
        '()
        '((18 . 5) (17 . 6) (16 . 41) (14 . 8) (9 . 9) (6 . 57) (5 . 58))
        '()
        '((5 . 59))
        '()
        '()
        '()
        '((12 . 63))
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '()
        '((14 . 8) (9 . 65))
        '((5 . 66))
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
        '((12 . 72))
        '((8 . 73))
        '()
        '()
        '()
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
          (yy-reduce-pop-and-push 3 3 (make-group $1 '()) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 3 (make-group $1 $3) yy-stack-states yy-stack-values))
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
                  $6
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 6 6 (make-mailbox $1 $3 $5) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 6 (make-mailbox $1 #f $3) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 4 6 (make-mailbox #f $2 $4) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 6 (make-mailbox #f #f $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6 (make-mailbox #f #f $1) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 7 (make-route (cons $2 $3)) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 3 9 (make-addr-spec $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push
            2
            11
            (make-domain #f (cons $1 $2))
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
            (make-domain #t (list $2 $4 $6 $8))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push
            2
            14
            (make-local-part (cons $1 $2))
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