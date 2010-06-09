(library (net helpers ipv6-address-lexer)
  (export
    ipv6-address-lexer-table)
  (import (rnrs) (silex lexer)(silex default-error-handler)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file ipv6-address-lexer.l by SILex 1.0
;

(define ipv6-address-lexer-table
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
       			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)
        ))
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
			 yytext (string-length yytext))
        )))
   'decision-trees
   0
   0
   '#((58 (48 (= 46 5 err) (50 2 (51 1 3))) (71 (59 6 (65 err 4)) (97 err
    (103 4 err)))) (58 (53 (48 err 9) (54 8 7)) (71 (65 err 7) (97 err (103
    7 err)))) (65 (48 err (58 10 err)) (97 (71 7 err) (103 7 err))) (65 (48
    err (58 7 err)) (97 (71 7 err) (103 7 err))) (65 (48 err (58 7 err))
    (97 (71 7 err) (103 7 err))) err err (65 (48 err (58 11 err)) (97 (71
    11 err) (103 11 err))) (65 (48 err (58 11 err)) (97 (71 11 err) (103 11
    err))) (65 (48 err (58 11 err)) (97 (71 11 err) (103 11 err))) (65 (48
    err (58 11 err)) (97 (71 11 err) (103 11 err))) (65 (48 err (58 12
    err)) (97 (71 12 err) (103 12 err))) err)
   '#((#f . #f) (2 . 2) (2 . 2) (2 . 2) (2 . 2) (1 . 1) (0 . 0) (2 . 2) (2
    . 2) (2 . 2) (2 . 2) (2 . 2) (2 . 2))))

) ; end of library

