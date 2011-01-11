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
      '#(((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -62))
         ((*default* . -61)) ((*default* . -60))
         ((*default* . -59)) ((*default* . -27))
         ((*default* . -26)) ((*default* . -25))
         ((*default* . -24)) ((*default* . -23))
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
           (SHARPBANG . 1)) ((*default* . -55))
         ((*default* . -54))
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
           (SHARPBANG . 1)) ((*default* . -56))
         ((*default* . -52) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -22)) ((*default* . -21))
         ((*default* . -20)) ((*default* . -19))
         ((*default* . -18)) ((*default* . -17))
         ((*default* . -16)) ((*default* . -15))
         ((*default* . -14)) ((*default* . -11))
         ((*default* . -10)) ((*default* . -13))
         ((*default* . -12)) ((*default* . -8))
         ((*default* . -9)) ((*default* . -7))
         ((*default* . -6)) ((*default* . -5))
         ((*default* . -3) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (STRING . 5) (CHARACTER . 6)
           (NUMBER . 7) (BOOLEAN . 8) (IDENTIFIER . 9)
           (SHARPSEMICOLON . 10) (LINEENDING . 11)
           (WHITESPACE . 12) (SHARPCOMMAAT . 13)
           (SHARPCOMMA . 14) (SHARPBACKTICK . 15)
           (SHARPTICK . 16) (COMMAAT . 17) (COMMA . 18)
           (BACKTICK . 19) (TICK . 20) (SHARPVU8PAREN . 21)
           (SHARPPAREN . 22) (OBRACKET . 23) (OPAREN . 24))
         ((*default* . -2))
         ((*default* . *error*) (*eoi* . 73))
         ((*default* . -22) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (STRING . 5) (CHARACTER . 6)
           (NUMBER . 7) (BOOLEAN . 8) (IDENTIFIER . 9)
           (SHARPSEMICOLON . 10) (LINEENDING . 11)
           (WHITESPACE . 12) (SHARPCOMMAAT . 13)
           (SHARPCOMMA . 14) (SHARPBACKTICK . 15)
           (SHARPTICK . 16) (COMMAAT . 17) (COMMA . 18)
           (BACKTICK . 19) (TICK . 20) (SHARPVU8PAREN . 21)
           (SHARPPAREN . 22) (OBRACKET . 23) (OPAREN . 24))
         ((*default* . -64)) ((*default* . -51))
         ((*default* . -50)) ((*default* . -49))
         ((*default* . -48)) ((*default* . -47))
         ((*default* . -46)) ((*default* . -45))
         ((*default* . -44)) ((*default* . -42))
         ((*default* . -41))
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
           (SHARPBANG . 1)) ((*default* . -39))
         ((*default* . -38))
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
           (SHARPBANG . 1)) ((*default* . -35))
         ((*default* . -31))
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
           (SHARPBANG . 1)) ((*default* . -32))
         ((*default* . -30))
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
           (SHARPBANG . 1)) ((*default* . -53))
         ((*default* . -57) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -4))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -63)) ((*default* . -43))
         ((*default* . -40))
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
           (SHARPBANG . 1)) ((*default* . -37))
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
           (SHARPBANG . 1)) ((*default* . -34))
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
           (SHARPBANG . 1)) ((*default* . -58))
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
           (SHARPBANG . 1)) ((*default* . -29))
         ((*default* . *error*) (CBRACKET . 92))
         ((*default* . -28))
         ((*default* . *error*) (CPAREN . 93))
         ((*default* . -36)) ((*default* . -33)))
      (vector
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 46) (1 . 47))
        '() '() '() '() '() '() '() '() '()
        '((28 . 25) (26 . 26) (25 . 48) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 49))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 50))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 51))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 52))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 53))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 54))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 55))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 56))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 57))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (16 . 59) (15 . 36) (13 . 37) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 60))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (14 . 62) (13 . 37) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 63))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (12 . 65) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 66))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (11 . 68) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 69))
        '() '((28 . 25) (27 . 70) (26 . 71)) '() '() '() '()
        '() '() '() '() '() '() '() '() '() '() '() '() '()
        '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 45) (2 . 72))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 74))
        '() '() '() '() '() '() '() '() '() '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (16 . 75) (15 . 36) (13 . 37) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 60))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (14 . 76) (13 . 37) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 63))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (12 . 78) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 79))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (11 . 81) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 82))
        '() '((28 . 25) (27 . 83) (26 . 71)) '() '() '() '()
        '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 84))
        '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (12 . 78) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 79))
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 86))
        '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (11 . 81) (10 . 38)
           (9 . 39) (8 . 40) (7 . 41) (6 . 42) (5 . 43)
           (4 . 44) (3 . 82))
        '() '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 89))
        '()
        '((28 . 25) (26 . 26) (25 . 27) (24 . 28) (23 . 29)
           (22 . 30) (21 . 31) (20 . 32) (19 . 33) (18 . 34)
           (17 . 35) (15 . 36) (13 . 37) (10 . 38) (9 . 39)
           (8 . 40) (7 . 41) (6 . 42) (5 . 43) (4 . 44)
           (3 . 91))
        '() '() '() '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1
            ((list-of-datums-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 (list $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4
            ((identifier-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5
            ((boolean-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 6
            ((number-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 7
            ((character-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 8
            ((string-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 9
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 9
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 10
            ((list-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 10
            ((list-datum-maker) yypushback yycustom $2)
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
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 12 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 4 12 (cons $1 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 12 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 13
            ((vector-datum-maker) yypushback yycustom $2)
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
            ((bytevector-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 16 '() yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 16 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 17
            ((quoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 18
            ((quasiquoted-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 19
            ((unquoted-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 20
            ((unquoted-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 21
            ((syntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 22
            ((quasisyntax-datum-maker) yypushback yycustom
              $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 23
            ((unsyntax-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 24
            ((unsyntax-splicing-datum-maker) yypushback
              yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 25
            ((interlexeme-space-datum-maker) yypushback
              yycustom (list $1))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 25
            ((interlexeme-space-datum-maker) yypushback
              yycustom (cons $1 $2))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 27 (list $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 27 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 28
            ((line-comment-datum-maker) yypushback yycustom
              $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 28
            ((nested-comment-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 28
            ((sharp-bang-r6rs-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 28
            ((sharp-bang-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 28
            ((sharp-semicolon-datum-maker) yypushback
              yycustom $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 28
            ((sharp-semicolon-datum-maker) yypushback
              yycustom #f $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 29 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 30 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 30 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 31 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 32 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 32 (cons $1 $2)
            yy-stack-states yy-stack-values))))))
