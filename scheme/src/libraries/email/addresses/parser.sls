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
      '#(((*default* . -36) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3))
         ((*default* . -33))
         ((*default* . *error*) (AT . 13) (ATOM . 12))
         ((*default* . -36) (DOT . 17) (ATOM . 16) (AT . -31))
         ((*default* . -34))
         ((*default* . -32))
         ((*default* . *error*) (COLON . 21) (ANGLE-OPEN . 20))
         ((*default* . *error*) (AT . 22))
         ((*default* . -18))
         ((*default* . -7) (COMMA . 23))
         ((*default* . -7) (COMMA . 23))
         ((*default* . *error*) (*eoi* . 26))
         ((*default* . -31) (DOT . 17))
         ((*default* . *error*) (DOMAIN . 27))
         ((*default* . *error*) (ANGLE-CLOSE . 28))
         ((*default* . *error*) (COLON . 29))
         ((*default* . -36) (ATOM . 16))
         ((*default* . *error*) (ATOM . 30))
         ((*default* . -35))
         ((*default* . -29))
         ((*default* . *error*) (AT . 13) (ATOM . 12))
         ((*default* . -36) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (ATOM . 3) (SEMICOLON . 33))
         ((*default* . *error*) (ATOM . 38) (DOMAIN-LITERAL-OPEN . 37))
         ((*default* . -7) (COMMA . 23) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2) (COLON . -36))
         ((*default* . -3))
         ((*default* . -2))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -21) (COMMA . 45))
         ((*default* . -17))
         ((*default* . *error*) (ATOM . 12))
         ((*default* . -31) (DOT . 17))
         ((*default* . *error*) (ANGLE-CLOSE . 49))
         ((*default* . *error*) (COLON . 50))
         ((*default* . -8))
         ((*default* . *error*) (ANGLE-OPEN . 20))
         ((*default* . -13) (COMMA . 51))
         ((*default* . *error*) (SEMICOLON . 53))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 54))
         ((*default* . -27) (DOT . 55))
         ((*default* . -24))
         ((*default* . -23))
         ((*default* . -22))
         ((*default* . -7) (COMMA . 23))
         ((*default* . -7) (COMMA . 23))
         ((*default* . -6))
         ((*default* . *error*) (AT . 59))
         ((*default* . -19))
         ((*default* . *error*) (ANGLE-CLOSE . 60))
         ((*default* . -30))
         ((*default* . -15))
         ((*default* . *error*) (ATOM . 12))
         ((*default* . -13) (COMMA . 51) (ATOM . 3) (QUOTED-TEXT . 1) (ANGLE-OPEN . 2))
         ((*default* . -10))
         ((*default* . -9))
         ((*default* . *error*) (DOT . 64))
         ((*default* . *error*) (ATOM . 65))
         ((*default* . -25))
         ((*default* . -5))
         ((*default* . -4))
         ((*default* . *error*) (DOMAIN . 66))
         ((*default* . -16))
         ((*default* . *error*) (ANGLE-CLOSE . 67))
         ((*default* . -13) (COMMA . 51))
         ((*default* . -12))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 69))
         ((*default* . -27) (DOT . 55))
         ((*default* . -21) (COMMA . 45))
         ((*default* . -14))
         ((*default* . -11))
         ((*default* . *error*) (DOT . 72))
         ((*default* . -26))
         ((*default* . -20))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 73))
         ((*default* . *error*) (DOT . 74))
         ((*default* . *error*) (DOMAIN-LITERAL-INTEGER . 75))
         ((*default* . *error*) (DOMAIN-LITERAL-CLOSE . 76))
         ((*default* . -28)))
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
        '((18 . 4) (17 . 5) (16 . 6) (14 . 7) (9 . 8) (6 . 42) (3 . 43) (2 . 44))
        '()
        '()
        '()
        '((8 . 46))
        '()
        '((14 . 7) (9 . 47))
        '((15 . 48))
        '()
        '()
        '()
        '()
        '((5 . 52))
        '()
        '()
        '((12 . 56))
        '()
        '()
        '()
        '((2 . 57))
        '((2 . 58))
        '()
        '()
        '()
        '()
        '()
        '()
        '((14 . 7) (9 . 61))
        '((18 . 4) (17 . 5) (16 . 34) (14 . 7) (9 . 8) (6 . 62) (5 . 63))
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
        '((12 . 70))
        '((8 . 71))
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