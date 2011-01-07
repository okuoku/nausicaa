(library (nausicaa r6rs parser-table)
  (export make-r6rs-parser)
  (import
    (rnrs)
    (nausicaa lalr lr-driver)
    (nausicaa parser-tools source-location)
    (nausicaa parser-tools lexical-token)
    (nausicaa language sentinel)
    (nausicaa r6rs datum-processing))
  (define (make-r6rs-parser)
    (lr-driver
      '#(((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1)) ((*default* . -15))
         ((*default* . -14)) ((*default* . -13))
         ((*default* . -12)) ((*default* . -11))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1)) ((*default* . -8))
         ((*default* . -7)) ((*default* . -10))
         ((*default* . -9)) ((*default* . -5))
         ((*default* . -6)) ((*default* . -4))
         ((*default* . -3)) ((*default* . -2))
         ((*default* . *error*) (*eoi* . 24))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 25)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 28)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (CBRACKET . 31) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 35)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (DOT . 34) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1)) ((*default* . -1) (*eoi* . accept))
         ((*default* . -27)) ((*default* . -26))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 25)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1)) ((*default* . -24))
         ((*default* . -23))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 28)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1)) ((*default* . -21))
         ((*default* . -18))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (CBRACKET . 31) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1))
         ((*default* . *error*) (OPAREN . 9) (OBRACKET . 8)
           (SHARPPAREN . 7) (SHARPVU8PAREN . 6)
           (IDENTIFIER . 5) (BOOLEAN . 4) (NUMBER . 3)
           (CHARACTER . 2) (STRING . 1)) ((*default* . -19))
         ((*default* . -17))
         ((*default* . *error*) (OPAREN . 9) (CPAREN . 35)
           (OBRACKET . 8) (SHARPPAREN . 7)
           (SHARPVU8PAREN . 6) (IDENTIFIER . 5)
           (BOOLEAN . 4) (NUMBER . 3) (CHARACTER . 2)
           (STRING . 1)) ((*default* . -28))
         ((*default* . -25)) ((*default* . -22))
         ((*default* . *error*) (CPAREN . 43))
         ((*default* . -20)) ((*default* . -16)))
      (vector
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 19))
        '() '() '() '() '()
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 20))
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 21))
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 22))
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 23))
        '() '() '() '() '() '() '() '() '() '()
        '((14 . 26) (13 . 10) (11 . 11) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 27))
        '((13 . 10) (12 . 29) (11 . 11) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 30))
        '((13 . 10) (11 . 11) (10 . 32) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 33))
        '((13 . 10) (11 . 11) (9 . 36) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 37))
        '() '() '()
        '((14 . 38) (13 . 10) (11 . 11) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 27))
        '() '()
        '((13 . 10) (12 . 39) (11 . 11) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 30))
        '() '()
        '((13 . 10) (11 . 11) (10 . 40) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 33))
        '((13 . 10) (11 . 11) (8 . 12) (7 . 13) (6 . 14)
           (5 . 15) (4 . 16) (3 . 17) (2 . 18) (1 . 41))
        '() '()
        '((13 . 10) (11 . 11) (9 . 42) (8 . 12) (7 . 13)
           (6 . 14) (5 . 15) (4 . 16) (3 . 17) (2 . 18)
           (1 . 37))
        '() '() '() '() '() '())
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
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2
            ((identifier-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3
            ((boolean-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4
            ((number-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5
            ((character-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6
            ((string-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 7
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 8
            ((list-datum-maker) yypushback yycustom
              (cons $2 $3))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 8
            ((list-datum-maker) yypushback yycustom
              (cons $2 $3))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 9 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 9 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 10 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 11
            ((vector-datum-maker) yypushback yycustom
              (cons $2 $3))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 12 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 12 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 13
            ((bytevector-datum-maker) yypushback yycustom
              (cons $2 $3))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 14 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 14 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 15
            ((quoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 16
            ((quasiquoted-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 17
            ((unquoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 18
            ((unquoted-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 19
            ((syntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 20
            ((quasisyntax-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 21
            ((unsyntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 22
            ((unsyntax-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 23 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24 #f yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 25 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26 #f yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26 #f yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((sharp-bang-r6rs-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 26
            ((sharp-bang-datum-maker) yypushback yycustom $3)
            yy-stack-states yy-stack-values))))))
