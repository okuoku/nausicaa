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
      '#(((*default* . *error*) (*eoi* . 25) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -61))
         ((*default* . -60)) ((*default* . -59))
         ((*default* . -58)) ((*default* . -26))
         ((*default* . -25)) ((*default* . -24))
         ((*default* . -23)) ((*default* . -22))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -54))
         ((*default* . -53))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 58)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 61)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (CBRACKET . 64) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 67)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -2))
         ((*default* . -55))
         ((*default* . -51) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -21)) ((*default* . -20))
         ((*default* . -19)) ((*default* . -18))
         ((*default* . -17)) ((*default* . -16))
         ((*default* . -15)) ((*default* . -14))
         ((*default* . -13)) ((*default* . -10))
         ((*default* . -9)) ((*default* . -12))
         ((*default* . -11)) ((*default* . -7))
         ((*default* . -8)) ((*default* . -6))
         ((*default* . -5)) ((*default* . -4))
         ((*default* . *error*) (*eoi* . 25) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (*eoi* . 73))
         ((*default* . -21) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (STRING . 5) (CHARACTER . 6)
           (NUMBER . 7) (BOOLEAN . 8) (IDENTIFIER . 9)
           (SHARPSEMICOLON . 10) (LINEENDING . 11)
           (WHITESPACE . 12) (SHARPCOMMAAT . 13)
           (SHARPCOMMA . 14) (SHARPBACKTICK . 15)
           (SHARPTICK . 16) (COMMAAT . 17) (COMMA . 18)
           (BACKTICK . 19) (TICK . 20) (SHARPVU8PAREN . 21)
           (SHARPPAREN . 22) (OBRACKET . 23) (OPAREN . 24))
         ((*default* . -63)) ((*default* . -50))
         ((*default* . -49)) ((*default* . -48))
         ((*default* . -47)) ((*default* . -46))
         ((*default* . -45)) ((*default* . -44))
         ((*default* . -43)) ((*default* . -41))
         ((*default* . -40))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 58)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -38))
         ((*default* . -37))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 61)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -34))
         ((*default* . -30))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (CBRACKET . 64) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 77)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -31))
         ((*default* . -29))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 67)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 80)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -52))
         ((*default* . -56) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -3))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -62)) ((*default* . -42))
         ((*default* . -39))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -36))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (CBRACKET . 64) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 85)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -33))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 67)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 87)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -57))
         ((*default* . *error*) (CBRACKET . 88))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (CPAREN . 90))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -28))
         ((*default* . *error*) (CBRACKET . 92))
         ((*default* . -27))
         ((*default* . *error*) (CPAREN . 93))
         ((*default* . -35)) ((*default* . -32)))
      (vector
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 46) (1 . 47))
        '() '() '() '() '() '() '() '() '()
        '((27 . 26) (25 . 27) (24 . 48) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 49))
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 50))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 51))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 52))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 53))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 54))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 55))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 56))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 57))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (15 . 59) (14 . 37) (12 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 60))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (13 . 62) (12 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 63))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (11 . 65) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 66))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (10 . 68) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 69))
        '() '() '((27 . 26) (26 . 70) (25 . 71)) '() '() '()
        '() '() '() '() '() '() '() '() '() '() '() '() '()
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 46) (1 . 72))
        '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 74))
        '() '() '() '() '() '() '() '() '() '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (15 . 75) (14 . 37) (12 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 60))
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (13 . 76) (12 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 63))
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (11 . 78) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 79))
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (10 . 81) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 82))
        '() '((27 . 26) (26 . 83) (25 . 71)) '() '() '() '()
        '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 84))
        '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (11 . 78) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 79))
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 86))
        '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (10 . 81) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 82))
        '() '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 89))
        '()
        '((27 . 26) (25 . 27) (24 . 28) (23 . 29) (22 . 30)
           (21 . 31) (20 . 32) (19 . 33) (18 . 34) (17 . 35)
           (16 . 36) (14 . 37) (12 . 38) (9 . 39) (8 . 40)
           (7 . 41) (6 . 42) (5 . 43) (4 . 44) (3 . 45)
           (2 . 91))
        '() '() '() '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3
            ((identifier-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4
            ((boolean-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5
            ((number-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6
            ((character-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 7
            ((string-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 8
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 8
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 9
            ((list-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 9
            ((list-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 10 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 10 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 11 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 11 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 11 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 12
            ((vector-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 13 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 13 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 14
            ((bytevector-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 15 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 15 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 16
            ((quoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 17
            ((quasiquoted-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 18
            ((unquoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 19
            ((unquoted-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 20
            ((syntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 21
            ((quasisyntax-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 22
            ((unsyntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 23
            ((unsyntax-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24
            ((interlexeme-space-datum-maker) yypushback
              yycustom (list $1))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 24
            ((interlexeme-space-datum-maker) yypushback
              yycustom (cons $1 $2))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 25
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 25
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 25 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26 (list $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 26 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 27
            ((line-comment-datum-maker) yypushback yycustom
              $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 27
            ((nested-comment-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 27
            ((sharp-bang-r6rs-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 27
            ((sharp-bang-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 27
            ((sharp-semicolon-datum-maker) yypushback
              yycustom $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 27
            ((sharp-semicolon-datum-maker) yypushback
              yycustom #f $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 28 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 29 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 29 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 30 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 31 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 31 (cons $1 $2)
            yy-stack-states yy-stack-values))))))
