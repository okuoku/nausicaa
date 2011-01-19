(library (nausicaa json rfc-lexer)
  (export
    json-rfc-lexer-table)
  (import (rnrs)
(nausicaa silex lexer)
(nausicaa silex default-error-handler)
(nausicaa parser-tools lexical-token)
(nausicaa parser-tools source-location)
)

;
; Table generated from the file rfc-lexer.l by SILex 1.0
;

(define json-rfc-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(silex-default-error-handler)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
             		(make-<lexical-token> 'BEGIN_ARRAY
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\[ 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              		(make-<lexical-token> 'BEGIN_OBJECT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\{ 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
           		(make-<lexical-token> 'END_ARRAY
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\] 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
            		(make-<lexical-token> 'END_OBJECT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\} 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                	(make-<lexical-token> 'NAME_SEPARATOR
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                 	(make-<lexical-token> 'VALUE_SEPARATOR
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\, 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
           		(make-<lexical-token> 'FALSE
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #f 5)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
          		(make-<lexical-token> 'TRUE
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #t 4)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
          		(make-<lexical-token> 'NULL
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 '() 4)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number yytext) (string-length yytext))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                   	(make-<lexical-token> 'QUOTED-TEXT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\" 1)
        )))
   'decision-trees
   0
   0
   '#((59 (34 (13 (9 err (11 12 err)) (32 (14 12 err) (33 12 err))) (46 (44
    (35 1 err) (45 7 err)) (48 (47 2 err) (58 3 8)))) (110 (93 (= 91 13
    err) (102 (94 10 err) (103 6 err))) (123 (116 (111 4 err) (117 5 err))
    (125 (124 11 err) (126 9 err))))) err (48 err (58 14 err)) (58 (47 (46
    err 16) (48 err 3)) (70 (69 err 15) (= 101 15 err))) (= 117 17 err) (=
    114 18 err) (= 97 19 err) (13 (9 err (11 7 err)) (32 (14 7 err) (33 7
    err))) (13 (9 err (11 8 err)) (32 (14 8 err) (33 8 err))) (13 (9 err
    (11 9 err)) (32 (14 9 err) (33 9 err))) (13 (9 err (11 10 err)) (32 (14
    10 err) (33 10 err))) (13 (9 err (11 11 err)) (32 (14 11 err) (33 11
    err))) (58 (14 (11 (9 err 12) (13 err 12)) (33 (32 err 12) (= 44 7
    err))) (94 (91 (59 8 err) (92 13 (93 err 10))) (124 (123 err 11) (= 125
    9 err)))) (13 (9 err (11 13 err)) (32 (14 13 err) (33 13 err))) (69 (48
    err (58 14 err)) (101 (70 20 err) (102 20 err))) (45 (= 43 22 err) (48
    (46 22 err) (58 21 err))) (48 err (58 14 err)) (= 108 23 err) (= 117 24
    err) (= 108 25 err) (45 (= 43 27 err) (48 (46 27 err) (58 26 err))) (48
    err (58 21 err)) (48 err (58 21 err)) (= 108 28 err) (= 101 29 err) (=
    115 30 err) (48 err (58 26 err)) (48 err (58 26 err)) err err (= 101 31
    err) err)
   '#((#f . #f) (10 . 10) (#f . #f) (9 . 9) (#f . #f) (#f . #f) (#f . #f)
    (5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (#f . #f) (0 . 0) (9 . 9) (#f .
    #f) (9 . 9) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (9 . 9) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (9 . 9) (#f . #f) (8 . 8) (7 . 7) (#f .
    #f) (6 . 6))))

) ; end of library

