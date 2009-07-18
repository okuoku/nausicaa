(library (calc-tree)
  (export
    calc-lexer-table/tree)
  (import (rnrs) (silex multilex))

;
; Table generated from the file calc.l by SILex 1.0
;

(define calc-lexer-table/tree
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
       		(begin #f)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
         	(assertion-violation #f
                  "invalid lexer token" yytext)
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	;; skip blanks, tabs and newlines
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number (string-append "+" yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
     		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
     		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
          	(case (string-ref yytext 0)
		  ((#\+) +)
		  ((#\-) -)
		  ((#\*) *)
		  ((#\/) /)
		  ((#\%) mod)
		  ((#\^) expt)
		  ((#\\) div)
		  ((#\=) =)
		  ((#\<) <)
		  ((#\>) >))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
             	(cond
                  ((string=? yytext "<=") <=)
                  ((string=? yytext ">=") >=))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
        	(string->symbol yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
       		(begin cons)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	(begin #\()
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	(begin #\))
        )))
   'decision-trees
   0
   0
   '#((58 (38 (32 (11 (10 err 16) (= 13 16 err)) (35 (33 16 (34 4 err)) (36
    15 (37 4 8)))) (43 (40 (39 4 err) (41 2 (42 1 8))) (46 (44 11 (45 3
    12)) (47 13 (48 8 14))))) (94 (63 (60 (59 4 err) (61 7 (62 5 6))) (91
    (= 64 err 4) (= 92 8 err))) (106 (96 (95 8 4) (97 err (105 4 9))) (123
    (= 110 10 4) (= 126 4 err))))) err err err (48 (37 (34 (33 err 4) (36
    err 4)) (39 (38 err 4) (= 46 4 err))) (96 (60 (59 4 err) (91 4 (95 err
    4))) (123 (97 err 4) (= 126 4 err)))) (48 (37 (34 (33 err 4) (36 err
    4)) (39 (38 err 4) (= 46 4 err))) (96 (60 (59 4 err) (91 4 (95 err 4)))
    (123 (97 err 4) (= 126 4 err)))) (59 (38 (34 (33 err 4) (= 36 4 err))
    (46 (39 4 err) (= 47 err 4))) (95 (61 (60 err 4) (62 17 (91 4 err)))
    (123 (= 96 err 4) (= 126 4 err)))) (59 (38 (34 (33 err 4) (= 36 4 err))
    (46 (39 4 err) (= 47 err 4))) (95 (61 (60 err 4) (62 17 (91 4 err)))
    (123 (= 96 err 4) (= 126 4 err)))) err (59 (38 (34 (33 err 4) (= 36 4
    err)) (46 (39 4 err) (= 47 err 4))) (97 (91 (60 err 4) (= 95 4 err))
    (123 (= 110 18 4) (= 126 4 err)))) (59 (38 (34 (33 err 4) (= 36 4 err))
    (46 (39 4 err) (= 47 err 4))) (97 (91 (60 err 4) (= 95 4 err)) (123 (98
    19 4) (= 126 4 err)))) (106 (105 err 20) (= 110 21 err)) (106 (105 err
    22) (= 110 23 err)) (48 err (58 24 err)) (69 (47 (46 err 26) (48 err
    (58 14 err))) (102 (70 25 (101 err 25)) (= 105 27 err))) (89 (79 (= 66
    30 err) (80 29 (88 err 28))) (111 (= 98 30 err) (120 (112 29 err) (121
    28 err)))) (13 (= 10 16 err) (32 (14 16 err) (33 16 err))) (48 (37 (34
    (33 err 4) (36 err 4)) (39 (38 err 4) (= 46 4 err))) (96 (60 (59 4 err)
    (91 4 (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (59 (38 (34 (33 err
    4) (= 36 4 err)) (46 (39 4 err) (= 47 err 4))) (97 (91 (60 err 4) (= 95
    4 err)) (123 (= 102 31 4) (= 126 4 err)))) (59 (38 (34 (33 err 4) (= 36
    4 err)) (46 (39 4 err) (= 47 err 4))) (97 (91 (60 err 4) (= 95 4 err))
    (123 (= 110 32 4) (= 126 4 err)))) (= 110 33 err) (= 97 34 err) (= 110
    35 err) (= 97 36 err) (70 (58 (48 err 24) (69 err 37)) (102 (101 err
    37) (= 105 27 err))) (45 (= 43 39 err) (48 (46 39 err) (58 38 err)))
    (58 (48 err 24) (= 105 27 err)) err (65 (48 err (58 40 err)) (97 (71 40
    err) (103 40 err))) (48 err (56 41 err)) (48 err (50 42 err)) (48 (37
    (34 (33 err 4) (36 err 4)) (39 (38 err 4) (= 46 43 err))) (96 (60 (59 4
    err) (91 4 (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (48 (37 (34
    (33 err 4) (36 err 4)) (39 (38 err 4) (= 46 44 err))) (96 (60 (59 4
    err) (91 4 (95 err 4))) (123 (97 err 4) (= 126 4 err)))) (= 102 45 err)
    (= 110 46 err) (= 102 47 err) (= 110 48 err) (45 (= 43 50 err) (48 (46
    50 err) (58 49 err))) (58 (48 err 38) (= 105 27 err)) (48 err (58 38
    err)) (71 (58 (48 err 40) (65 err 40)) (103 (97 err 40) (= 105 27
    err))) (56 (48 err 41) (= 105 27 err)) (50 (48 err 42) (= 105 27 err))
    (49 (38 (34 (33 err 4) (= 36 4 err)) (46 (39 4 err) (47 4 (48 err
    51)))) (96 (60 (59 4 err) (91 4 (95 err 4))) (123 (97 err 4) (= 126 4
    err)))) (49 (38 (34 (33 err 4) (= 36 4 err)) (46 (39 4 err) (47 4 (48
    err 52)))) (96 (60 (59 4 err) (91 4 (95 err 4))) (123 (97 err 4) (= 126
    4 err)))) (= 46 53 err) (= 46 54 err) (= 46 55 err) (= 46 56 err) (58
    (48 err 49) (= 105 27 err)) (48 err (58 49 err)) (48 (37 (34 (33 err 4)
    (36 err 4)) (39 (38 err 4) (= 46 4 err))) (96 (60 (59 4 err) (91 4 (95
    err 4))) (123 (97 err 4) (= 126 4 err)))) (48 (37 (34 (33 err 4) (36
    err 4)) (39 (38 err 4) (= 46 4 err))) (96 (60 (59 4 err) (91 4 (95 err
    4))) (123 (97 err 4) (= 126 4 err)))) (= 48 57 err) (= 48 58 err) (= 48
    57 err) (= 48 58 err) err err)
   '#((#f . #f) (10 . 10) (9 . 9) (8 . 8) (7 . 7) (5 . 5) (5 . 5) (5 . 5)
    (5 . 5) (7 . 7) (7 . 7) (5 . 5) (5 . 5) (#f . #f) (2 . 2) (#f . #f) (0
    . 0) (6 . 6) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2
    . 2) (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (7 . 7) (7
    . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (#f .
    #f) (2 . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f . #f) (#f
    . #f) (#f . #f) (4 . 4) (3 . 3))))

) ; end of library

