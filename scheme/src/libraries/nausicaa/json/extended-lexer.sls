#!r6rs
(library (nausicaa json extended-lexer)
  (export
    json-extended-lexer-table)
  (import (rnrs)
(nausicaa silex input-system)
(nausicaa silex default-error-handler)
(nausicaa parser-tools lexical-token)
(nausicaa parser-tools source-location)
)

;
; Table generated from the file extended-lexer.l by SILex 1.0
;

(define json-extended-lexer-table
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
			 -inf.0 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 +inf.0 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 +nan.0 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number yytext) (string-length yytext))

;; {binint}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number yytext) (string-length yytext))

;; {octint}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number yytext) (string-length yytext))

;; {hexint}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number yytext) (string-length yytext))
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
   '#((73 (35 (14 (11 (9 err 16) (13 err 16)) (33 (32 err 16) (34 err 1)))
    (46 (44 (36 4 err) (45 11 7)) (48 (47 2 err) (58 3 (59 12 err))))) (103
    (91 (78 (74 6 err) (79 5 err)) (93 (92 17 err) (94 14 (102 err 10))))
    (117 (111 (110 err 8) (116 err 9)) (124 (123 err 15) (= 125 13 err)))))
    err (48 err (58 18 err)) (58 (47 (46 err 20) (48 err 3)) (70 (69 err
    19) (= 101 19 err))) (89 (79 (= 66 23 err) (80 22 (88 err 21))) (111 (=
    98 23 err) (120 (112 22 err) (121 21 err)))) (= 97 24 err) (= 110 25
    err) (= 73 26 err) (= 117 27 err) (= 114 28 err) (= 97 29 err) (13 (9
    err (11 11 err)) (32 (14 11 err) (33 11 err))) (13 (9 err (11 12 err))
    (32 (14 12 err) (33 12 err))) (13 (9 err (11 13 err)) (32 (14 13 err)
    (33 13 err))) (13 (9 err (11 14 err)) (32 (14 14 err) (33 14 err))) (13
    (9 err (11 15 err)) (32 (14 15 err) (33 15 err))) (58 (14 (11 (9 err
    16) (13 err 16)) (33 (32 err 16) (= 44 11 err))) (94 (91 (59 12 err)
    (92 17 (93 err 14))) (124 (123 err 15) (= 125 13 err)))) (13 (9 err (11
    17 err)) (32 (14 17 err) (33 17 err))) (69 (48 err (58 18 err)) (101
    (70 30 err) (102 30 err))) (45 (= 43 32 err) (48 (46 32 err) (58 31
    err))) (48 err (58 18 err)) (65 (48 err (58 33 err)) (97 (71 33 err)
    (103 33 err))) (48 err (56 34 err)) (48 err (50 35 err)) (= 78 36 err)
    (= 102 37 err) (= 110 38 err) (= 108 39 err) (= 117 40 err) (= 108 41
    err) (45 (= 43 43 err) (48 (46 43 err) (58 42 err))) (48 err (58 31
    err)) (48 err (58 31 err)) (65 (48 err (58 33 err)) (97 (71 33 err)
    (103 33 err))) (48 err (56 34 err)) (48 err (50 35 err)) err (= 105 44
    err) (= 102 45 err) (= 108 46 err) (= 101 47 err) (= 115 48 err) (48
    err (58 42 err)) (48 err (58 42 err)) (= 110 49 err) (= 105 50 err) err
    err (= 101 51 err) (= 105 52 err) (= 110 53 err) err (= 116 54 err) (=
    105 55 err) (= 121 56 err) (= 116 57 err) err (= 121 58 err) err)
   '#((#f . #f) (13 . 13) (#f . #f) (12 . 12) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (5 . 5) (4 . 4) (3 . 3) (2 . 2)
    (1 . 1) (#f . #f) (0 . 0) (12 . 12) (#f . #f) (12 . 12) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (12 . 12) (#f . #f) (12 . 12) (12 . 12) (12 . 12) (11 .
    11) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (12 . 12) (#f .
    #f) (#f . #f) (#f . #f) (8 . 8) (7 . 7) (#f . #f) (#f . #f) (#f . #f)
    (6 . 6) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (10 . 10) (#f . #f) (9
    . 9))))

) ; end of library

