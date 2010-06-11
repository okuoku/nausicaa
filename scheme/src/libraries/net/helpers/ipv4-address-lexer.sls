(library (net helpers ipv4-address-lexer)
  (export
    ipv4-address-lexer-table)
  (import (rnrs) (silex lexer)(silex default-error-handler)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file ipv4-address-lexer.l by SILex 1.0
;

(define ipv4-address-lexer-table
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
     			(make-<lexical-token> 'DOT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\. 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number yytext) (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number (substring yytext 2 (string-length yytext)) 16)
			 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number (substring yytext 1 (string-length yytext)) 8)
			 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		(make-<lexical-token> 'PREFIX-LENGTH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number (substring yytext 1 (string-length yytext)))
			 (string-length yytext))
        )))
   'decision-trees
   0
   0
   '#((49 (47 (46 err 6) (48 1 2)) (51 (50 4 3) (58 5 err))) (50 (48 err
    (49 10 9)) (52 (51 8 7) (58 10 err))) (56 (48 err 11) (= 120 12 err))
    (53 (48 err 14) (54 13 (58 15 err))) (48 err (58 16 err)) (48 err (58
    15 err)) err (48 err (51 10 err)) (48 err (58 10 err)) (48 err (58 10
    err)) err (48 err (56 17 err)) (65 (48 err (58 18 err)) (97 (71 18 err)
    (103 18 err))) (48 err (54 15 err)) (48 err (58 15 err)) err (48 err
    (58 15 err)) (48 err (56 19 err)) (65 (48 err (58 20 err)) (97 (71 20
    err) (103 20 err))) err err)
   '#((#f . #f) (#f . #f) (#f . #f) (1 . 1) (1 . 1) (1 . 1) (0 . 0) (4 . 4)
    (4 . 4) (4 . 4) (4 . 4) (3 . 3) (#f . #f) (1 . 1) (1 . 1) (1 . 1) (1 .
    1) (3 . 3) (2 . 2) (3 . 3) (2 . 2))))

) ; end of library

