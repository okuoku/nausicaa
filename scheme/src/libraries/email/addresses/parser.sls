(library (email addresses parser)
  (export make-address-parser
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
          lexical-token?/end-of-input
          lexical-token?/lexer-error
          lexical-token?/special)
  (import (rnrs) (lalr lr-driver) (lalr common) (sentinel) (email addresses common) (strings))
  (define (make-address-parser)
    (lr-driver
      '#(((*default* . -34) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3))
         ((*default* . -31))
         ((*default* . *error*) (AT . 13) (ATOM . 12))
         ((*default* . -34) (DOT . 17) (ATOM . 16) (AT . -29))
         ((*default* . -32))
         ((*default* . -30))
         ((*default* . *error*) (COLON . 21) (ANGLE-OPEN . 20))
         ((*default* . *error*) (AT . 22))
         ((*default* . -12))
         ((*default* . -6) (COMMA . 23))
         ((*default* . -6) (COMMA . 23))
         ((*default* . *error*) (*eoi* . 26))
         ((*default* . -29) (DOT . 17))
         ((*default* . *error*) (DOMAIN . 27))
         ((*default* . *error*) (ANGLE-CLOSE . 28))
         ((*default* . *error*) (COLON . 29))
         ((*default* . -34) (ATOM . 16))
         ((*default* . *error*) (ATOM . 30))
         ((*default* . -33))
         ((*default* . -27))
         ((*default* . *error*) (AT . 13) (ATOM . 12))
         ((*default* . -34) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3) (SEMICOLON . 33))
         ((*default* . *error*) (ATOM . 38) (DOMAIN-LITERAL-OPEN . 37))
         ((*default* . -34) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3))
         ((*default* . -3))
         ((*default* . -2))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -19) (COMMA . 44))
         ((*default* . -13))
         ((*default* . *error*) (ATOM . 12))
         ((*default* . -29) (DOT . 17))
         ((*default* . *error*) (ANGLE-CLOSE . 48))
         ((*default* . *error*) (COLON . 49))
         ((*default* . -7))
         ((*default* . *error*) (ANGLE-OPEN . 20))
         ((*default* . -11) (COMMA . 50))
         ((*default* . *error*) (SEMICOLON . 52))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 53))
         ((*default* . -25) (DOT . 54))
         ((*default* . -22))
         ((*default* . -21))
         ((*default* . -20))
         ((*default* . -6) (COMMA . 23))
         ((*default* . -6) (COMMA . 23))
         ((*default* . *error*) (AT . 58))
         ((*default* . -17))
         ((*default* . *error*) (ANGLE-CLOSE . 59))
         ((*default* . -28))
         ((*default* . -15))
         ((*default* . *error*) (ATOM . 12))
         ((*default* . -34) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3))
         ((*default* . -9))
         ((*default* . -8))
         ((*default* . *error*) (DOT . 62))
         ((*default* . *error*) (ATOM . 63))
         ((*default* . -23))
         ((*default* . -5))
         ((*default* . -4))
         ((*default* . *error*) (DOMAIN . 64))
         ((*default* . -14))
         ((*default* . *error*) (ANGLE-CLOSE . 65))
         ((*default* . -11) (COMMA . 50))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 67))
         ((*default* . -25) (DOT . 54))
         ((*default* . -19) (COMMA . 44))
         ((*default* . -16))
         ((*default* . -10))
         ((*default* . *error*) (DOT . 70))
         ((*default* . -24))
         ((*default* . -18))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 71))
         ((*default* . *error*) (DOT . 72))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 73))
         ((*default* . *error*) (DOMAIN-LITERAL-CLOSE . 74))
         ((*default* . -26)))
      (vector
        '((18 . 4) (17 . 5) (16 . 6) (14 . 7) (9 . 8) (6 . 9) (3 . 10) (1 . 11))
        '()
        '((14 . 7) (9 . 14) (7 . 15))
        '((18 . 18) (15 . 19))
        '()
        '()
        '()
        '()
        '()
        '((2 . 24))
        '((2 . 25))
        '()
        '((15 . 19))
        '()
        '()
        '()
        '((18 . 18))
        '()
        '()
        '()
        '((14 . 7) (9 . 31) (7 . 32))
        '((18 . 4) (17 . 5) (16 . 34) (14 . 7) (9 . 8) (6 . 35) (4 . 36))
        '((13 . 39) (11 . 40) (10 . 41))
        '((18 . 4) (17 . 5) (16 . 6) (14 . 7) (9 . 8) (6 . 42) (3 . 43))
        '()
        '()
        '()
        '((8 . 45))
        '()
        '((14 . 7) (9 . 46))
        '((15 . 47))
        '()
        '()
        '()
        '()
        '((5 . 51))
        '()
        '()
        '((12 . 55))
        '()
        '()
        '()
        '((2 . 56))
        '((2 . 57))
        '()
        '()
        '()
        '()
        '()
        '((14 . 7) (9 . 60))
        '((18 . 4) (17 . 5) (16 . 34) (14 . 7) (9 . 8) (6 . 61))
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
        '((5 . 66))
        '()
        '((12 . 68))
        '((8 . 69))
        '()
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
          (yy-reduce-pop-and-push 4 3 (make-group $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 5 '() yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 3 6 (make-mailbox #f #f $2) yy-stack-states yy-stack-values))
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
          (yy-reduce-pop-and-push 5 6 (make-mailbox #f $2 $4) yy-stack-states yy-stack-values))
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
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 8 '() yy-stack-states yy-stack-values))
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