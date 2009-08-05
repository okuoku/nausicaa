(library (calc-parser-lexer)
  (export
    calc-parser-lexer-table)
  (import (rnrs) (silex lexer)(lalr common))

;
; Table generated from the file #f by SILex 1.0
;

(define calc-parser-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(make-lexical-token
		 '*eoi*
		 (make-source-location #f yyline yycolumn yyoffset 0)
		 (eof-object))
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         	(assertion-violation #f "invalid lexer token")
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
        	;; skip spaced and tabs
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-lexical-token 'NUM
				    (make-source-location #f
							  yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->number (string-append "+" yytext)))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-lexical-token 'NUM
				    (make-source-location #f
							  yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->number yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     		(make-lexical-token 'NUM
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    +nan.0)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     		(make-lexical-token 'NUM
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    +inf.0)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(let ((position (make-source-location #f yyline yycolumn yyoffset
						      (string-length yytext))))
		  (case (string-ref yytext 0)
		    ((#\+)	(make-lexical-token '+ position '+))
		    ((#\-)	(make-lexical-token '- position '-))
		    ((#\*)	(make-lexical-token '* position '*))
		    ((#\/)	(make-lexical-token '/ position '/))
		    ((#\%)	(make-lexical-token 'FUN position mod))
		    ((#\^)	(make-lexical-token 'FUN position expt))
		    ((#\\)	(make-lexical-token 'FUN position div))
		    ((#\<)	(make-lexical-token 'FUN position <))
		    ((#\>)	(make-lexical-token 'FUN position >))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
             	(let ((position (make-source-location #f
						      yyline yycolumn yyoffset
						      (string-length yytext))))
		  (case yytext
		   (("==") (make-lexical-token 'FUN position =))
		   (("<=") (make-lexical-token 'FUN position <=))
		   ((">=") (make-lexical-token 'FUN position >=))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-lexical-token 'ID
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->symbol yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-lexical-token
		 'ASSIGN
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'ASSIGN)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       		(make-lexical-token
		 'COMMA
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'COMMA)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         	(make-lexical-token
		 'NEWLINE
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'NEWLINE)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-lexical-token
		 'LPAREN
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'LPAREN)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-lexical-token
		 'RPAREN
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'RPAREN)
        )))
   'decision-trees
   0
   0
   '#((47 (36 (13 (10 (9 err 17) (11 3 err)) (32 (14 3 err) (33 17 (35 err
    16)))) (42 (38 (37 err 9) (40 err (41 2 1))) (44 (43 9 12) (45 4 (46 13
    14))))) (93 (62 (58 (48 9 15) (60 err (61 8 6))) (65 (63 7 err) (91 5
    (92 err 9)))) (105 (95 (94 err 9) (= 96 err 5)) (110 (106 10 5) (111 11
    (123 5 err)))))) err err (11 (10 err 3) (= 13 3 err)) err (64 (47 (46
    err 5) (48 err (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5
    err)))) (= 61 18 err) (= 61 18 err) (= 61 18 err) err (91 (48 (= 46 5
    err) (58 5 (64 err 5))) (97 (= 95 5 err) (111 (110 5 19) (123 5 err))))
    (91 (48 (= 46 5 err) (58 5 (64 err 5))) (97 (= 95 5 err) (98 20 (123 5
    err)))) (106 (105 err 21) (= 110 22 err)) (106 (105 err 23) (= 110 24
    err)) (48 err (58 25 err)) (69 (47 (46 err 27) (48 err (58 15 err)))
    (102 (70 26 (101 err 26)) (= 105 28 err))) (89 (79 (= 66 31 err) (80 30
    (88 err 29))) (111 (= 98 31 err) (120 (112 30 err) (121 29 err)))) (10
    (9 err 17) (= 32 17 err)) err (91 (48 (= 46 5 err) (58 5 (64 err 5)))
    (97 (= 95 5 err) (103 (102 5 32) (123 5 err)))) (91 (48 (= 46 5 err)
    (58 5 (64 err 5))) (97 (= 95 5 err) (111 (110 5 33) (123 5 err)))) (=
    110 34 err) (= 97 35 err) (= 110 36 err) (= 97 37 err) (70 (58 (48 err
    25) (69 err 38)) (102 (101 err 38) (= 105 28 err))) (45 (= 43 40 err)
    (48 (46 40 err) (58 39 err))) (58 (48 err 25) (= 105 28 err)) err (65
    (48 err (58 41 err)) (97 (71 41 err) (103 41 err))) (48 err (56 42
    err)) (48 err (50 43 err)) (64 (47 (46 err 44) (48 err (58 5 err))) (96
    (91 5 (95 err 5)) (97 err (123 5 err)))) (64 (47 (46 err 45) (48 err
    (58 5 err))) (96 (91 5 (95 err 5)) (97 err (123 5 err)))) (= 102 46
    err) (= 110 47 err) (= 102 48 err) (= 110 49 err) (45 (= 43 51 err) (48
    (46 51 err) (58 50 err))) (58 (48 err 39) (= 105 28 err)) (48 err (58
    39 err)) (71 (58 (48 err 41) (65 err 41)) (103 (97 err 41) (= 105 28
    err))) (56 (48 err 42) (= 105 28 err)) (50 (48 err 43) (= 105 28 err))
    (64 (48 (= 46 5 err) (49 52 (58 5 err))) (96 (91 5 (95 err 5)) (97 err
    (123 5 err)))) (64 (48 (= 46 5 err) (49 53 (58 5 err))) (96 (91 5 (95
    err 5)) (97 err (123 5 err)))) (= 46 54 err) (= 46 55 err) (= 46 56
    err) (= 46 57 err) (58 (48 err 50) (= 105 28 err)) (48 err (58 50 err))
    (64 (47 (46 err 5) (48 err (58 5 err))) (96 (91 5 (95 err 5)) (97 err
    (123 5 err)))) (64 (47 (46 err 5) (48 err (58 5 err))) (96 (91 5 (95
    err 5)) (97 err (123 5 err)))) (= 48 58 err) (= 48 59 err) (= 48 58
    err) (= 48 59 err) err err)
   '#((#f . #f) (12 . 12) (11 . 11) (10 . 10) (9 . 9) (7 . 7) (8 . 8) (5 .
    5) (5 . 5) (5 . 5) (7 . 7) (7 . 7) (5 . 5) (5 . 5) (#f . #f) (2 . 2)
    (#f . #f) (0 . 0) (6 . 6) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (2 . 2) (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f .
    #f) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (2 . 2) (#f . #f) (2 . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f
    . #f) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (4 . 4) (3 . 3))))

) ; end of library

