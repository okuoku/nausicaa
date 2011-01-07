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
           (SHARPBANG . 1))
         ((*default* . *error*) (IDENTIFIER . 9))
         ((*default* . -56)) ((*default* . -55))
         ((*default* . -54)) ((*default* . -24))
         ((*default* . -23)) ((*default* . -22))
         ((*default* . -21)) ((*default* . -20))
         ((*default* . *error*) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (LINECOMMENT . 4) (NESTEDCOMMENT . 3)
           (SHARPBANGR6RS . 2) (SHARPBANG . 1))
         ((*default* . -50)) ((*default* . -49))
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
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 56)
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
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 59)
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
           (OBRACKET . 23) (CBRACKET . 62) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 65)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -51))
         ((*default* . -47) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -19)) ((*default* . -18))
         ((*default* . -17)) ((*default* . -16))
         ((*default* . -15)) ((*default* . -14))
         ((*default* . -13)) ((*default* . -12))
         ((*default* . -11)) ((*default* . -8))
         ((*default* . -7)) ((*default* . -10))
         ((*default* . -9)) ((*default* . -5))
         ((*default* . -6)) ((*default* . -4))
         ((*default* . -3)) ((*default* . -2))
         ((*default* . *error*) (*eoi* . 70))
         ((*default* . -57))
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
           (SHARPBANG . 1)) ((*default* . -46))
         ((*default* . -45)) ((*default* . -44))
         ((*default* . -43)) ((*default* . -42))
         ((*default* . -41)) ((*default* . -40))
         ((*default* . -39)) ((*default* . -37))
         ((*default* . -36))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 56)
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
         ((*default* . -33))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 59)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -31))
         ((*default* . -28))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (CBRACKET . 62) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 74)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -29))
         ((*default* . -27))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 65)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (DOT . 77)
           (SHARPTICK . 16) (SHARPBACKTICK . 15)
           (SHARPCOMMA . 14) (SHARPCOMMAAT . 13)
           (WHITESPACE . 12) (LINEENDING . 11)
           (SHARPSEMICOLON . 10) (IDENTIFIER . 9)
           (BOOLEAN . 8) (NUMBER . 7) (CHARACTER . 6)
           (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -48))
         ((*default* . -52) (SHARPBANG . 1)
           (SHARPBANGR6RS . 2) (NESTEDCOMMENT . 3)
           (LINECOMMENT . 4) (SHARPSEMICOLON . 10)
           (LINEENDING . 11) (WHITESPACE . 12))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -58)) ((*default* . -38))
         ((*default* . -35))
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
           (SHARPBANG . 1)) ((*default* . -32))
         ((*default* . *error*) (OPAREN . 24)
           (OBRACKET . 23) (CBRACKET . 62) (SHARPPAREN . 22)
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
           (SHARPBANG . 1)) ((*default* . -30))
         ((*default* . *error*) (OPAREN . 24) (CPAREN . 65)
           (OBRACKET . 23) (SHARPPAREN . 22)
           (SHARPVU8PAREN . 21) (TICK . 20) (BACKTICK . 19)
           (COMMA . 18) (COMMAAT . 17) (SHARPTICK . 16)
           (SHARPBACKTICK . 15) (SHARPCOMMA . 14)
           (SHARPCOMMAAT . 13) (WHITESPACE . 12)
           (LINEENDING . 11) (SHARPSEMICOLON . 10)
           (IDENTIFIER . 9) (BOOLEAN . 8) (NUMBER . 7)
           (CHARACTER . 6) (STRING . 5) (LINECOMMENT . 4)
           (NESTEDCOMMENT . 3) (SHARPBANGR6RS . 2)
           (SHARPBANG . 1)) ((*default* . -53))
         ((*default* . *error*) (CBRACKET . 83))
         ((*default* . *error*) (CPAREN . 84))
         ((*default* . -26)) ((*default* . -25)))
      (vector
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 45))
        '((2 . 46)) '() '() '() '() '() '() '() '()
        '((26 . 25) (24 . 26) (23 . 47)) '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 48))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 49))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 50))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 51))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 52))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 53))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 54))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 55))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (14 . 57) (13 . 36) (11 . 37) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 58))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (12 . 60) (11 . 37) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 61))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (10 . 63) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 64))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (9 . 66) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 67))
        '() '((26 . 25) (25 . 68) (24 . 69)) '() '() '() '()
        '() '() '() '() '() '() '() '() '() '() '() '() '()
        '() '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 71))
        '() '() '() '() '() '() '() '() '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (14 . 72) (13 . 36) (11 . 37) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 58))
        '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (12 . 73) (11 . 37) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 61))
        '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (10 . 75) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 76))
        '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (9 . 78) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 79))
        '() '((26 . 25) (25 . 80) (24 . 69)) '() '() '() '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 81))
        '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (10 . 75) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 76))
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (8 . 38) (7 . 39)
           (6 . 40) (5 . 41) (4 . 42) (3 . 43) (2 . 44)
           (1 . 82))
        '()
        '((26 . 25) (24 . 26) (23 . 27) (22 . 28) (21 . 29)
           (20 . 30) (19 . 31) (18 . 32) (17 . 33) (16 . 34)
           (15 . 35) (13 . 36) (11 . 37) (9 . 78) (8 . 38)
           (7 . 39) (6 . 40) (5 . 41) (4 . 42) (3 . 43)
           (2 . 44) (1 . 79))
        '() '() '() '() '())
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
           yy-stack-states $5 $4 $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 5 7
            ((pair-datum-maker) yypushback yycustom $2 $4)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 8
            ((list-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 8
            ((list-datum-maker) yypushback yycustom $2)
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
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 11
            ((vector-datum-maker) yypushback yycustom $2)
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
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 13
            ((bytevector-datum-maker) yypushback yycustom $2)
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
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 23
            ((interlexeme-datum-maker) yypushback yycustom
              (list $1))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 23
            ((interlexeme-datum-maker) yypushback yycustom
              (cons $1 $2))
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24
            ((whitespace-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 24 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 25 (list $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 25 (cons $1 $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((line-comment-datum-maker) yypushback yycustom
              $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((nested-comment-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 26
            ((sharp-bang-r6rs-datum-maker) yypushback
              yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 26
            ((sharp-bang-datum-maker) yypushback yycustom $2)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 26
            ((sharp-semicolon-datum-maker) yypushback
              yycustom $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 27 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 28 sentinel
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 28 (cons $1 $2)
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
            yy-stack-states yy-stack-values))))))
