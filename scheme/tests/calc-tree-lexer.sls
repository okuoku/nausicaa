(library (calc-tree-lexer)
  (export
    calc-lexer-table/tree)
  (import (rnrs) (silex lexer))

;
; Table generated from the file calc.l by SILex 1.0
;

(define calc-lexer-table/tree
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
       		(eof-object)
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
   '#((48 (37 (14 (11 (9 err 15) (13 err 15)) (33 (32 err 15) (= 35 14
    err))) (43 (40 (38 7 err) (41 2 (42 1 7))) (45 (44 10 3) (46 11 (47 12
    7))))) (93 (62 (60 (58 13 err) (61 6 7)) (65 (63 5 err) (91 4 (92 err
    7)))) (105 (95 (94 err 7) (= 96 err 4)) (110 (106 8 4) (111 9 (123 4
    err)))))) err err err (64 (47 (46 err 4) (48 err (58 4 err))) (96 (91 4
    (95 err 4)) (97 err (123 4 err)))) (= 61 16 err) (= 61 16 err) err (91
    (48 (= 46 4 err) (58 4 (64 err 4))) (97 (= 95 4 err) (111 (110 4 17)
    (123 4 err)))) (91 (48 (= 46 4 err) (58 4 (64 err 4))) (97 (= 95 4 err)
    (98 18 (123 4 err)))) (106 (105 err 19) (= 110 20 err)) (106 (105 err
    21) (= 110 22 err)) (48 err (58 23 err)) (69 (47 (46 err 25) (48 err
    (58 13 err))) (102 (70 24 (101 err 24)) (= 105 26 err))) (89 (79 (= 66
    29 err) (80 28 (88 err 27))) (111 (= 98 29 err) (120 (112 28 err) (121
    27 err)))) (13 (9 err (11 15 err)) (32 (14 15 err) (33 15 err))) err
    (91 (48 (= 46 4 err) (58 4 (64 err 4))) (97 (= 95 4 err) (103 (102 4
    30) (123 4 err)))) (91 (48 (= 46 4 err) (58 4 (64 err 4))) (97 (= 95 4
    err) (111 (110 4 31) (123 4 err)))) (= 110 32 err) (= 97 33 err) (= 110
    34 err) (= 97 35 err) (70 (58 (48 err 23) (69 err 36)) (102 (101 err
    36) (= 105 26 err))) (45 (= 43 38 err) (48 (46 38 err) (58 37 err)))
    (58 (48 err 23) (= 105 26 err)) err (65 (48 err (58 39 err)) (97 (71 39
    err) (103 39 err))) (48 err (56 40 err)) (48 err (50 41 err)) (64 (47
    (46 err 42) (48 err (58 4 err))) (96 (91 4 (95 err 4)) (97 err (123 4
    err)))) (64 (47 (46 err 43) (48 err (58 4 err))) (96 (91 4 (95 err 4))
    (97 err (123 4 err)))) (= 102 44 err) (= 110 45 err) (= 102 46 err) (=
    110 47 err) (45 (= 43 49 err) (48 (46 49 err) (58 48 err))) (58 (48 err
    37) (= 105 26 err)) (48 err (58 37 err)) (71 (58 (48 err 39) (65 err
    39)) (103 (97 err 39) (= 105 26 err))) (56 (48 err 40) (= 105 26 err))
    (50 (48 err 41) (= 105 26 err)) (64 (48 (= 46 4 err) (49 50 (58 4
    err))) (96 (91 4 (95 err 4)) (97 err (123 4 err)))) (64 (48 (= 46 4
    err) (49 51 (58 4 err))) (96 (91 4 (95 err 4)) (97 err (123 4 err))))
    (= 46 52 err) (= 46 53 err) (= 46 54 err) (= 46 55 err) (58 (48 err 48)
    (= 105 26 err)) (48 err (58 48 err)) (64 (47 (46 err 4) (48 err (58 4
    err))) (96 (91 4 (95 err 4)) (97 err (123 4 err)))) (64 (47 (46 err 4)
    (48 err (58 4 err))) (96 (91 4 (95 err 4)) (97 err (123 4 err)))) (= 48
    56 err) (= 48 57 err) (= 48 56 err) (= 48 57 err) err err)
   '#((#f . #f) (10 . 10) (9 . 9) (8 . 8) (7 . 7) (5 . 5) (5 . 5) (5 . 5)
    (7 . 7) (7 . 7) (5 . 5) (5 . 5) (#f . #f) (2 . 2) (#f . #f) (0 . 0) (6
    . 6) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2)
    (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (7 . 7) (7 . 7)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (2
    . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f
    . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (4 . 4) (3 . 3))))

) ; end of library

