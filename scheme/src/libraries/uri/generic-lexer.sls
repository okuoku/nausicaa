(library (uri generic-lexer)
  (export
    uri-generic-lexer-table)
  (import (rnrs) (silex lexer)(silex default-error-handler)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file generic-lexer.l by SILex 1.0
;

(define uri-generic-lexer-table
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
        		(make-<lexical-token> 'QUESTION
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\? 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
     			(make-<lexical-token> 'SHARP
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\# 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
     			(make-<lexical-token> 'SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\/ 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
            		(make-<lexical-token> 'DOUBLE_SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 "//" 2)

;; {NUMBER}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number yytext) (string-length yytext))
        )))
   'decision-trees
   0
   0
   '#((81 (68 (67 err 4) (69 1 err)) (83 (82 3 err) (84 2 err))) (= 79 5
    err) (73 (72 err 7) (= 76 6 err)) (= 85 8 err) (= 79 9 err) (= 85 10
    err) (= 65 11 err) (= 65 12 err) (= 69 13 err) (= 76 14 err) (= 66 15
    err) (= 83 16 err) (= 82 17 err) (= 83 18 err) (= 79 19 err) (= 76 20
    err) (= 72 21 err) (= 80 22 err) (= 84 23 err) (= 78 24 err) (= 69 25
    err) err err (= 73 26 err) err (= 95 27 err) (= 79 28 err) (= 83 29
    err) (= 78 30 err) (= 76 31 err) err (= 65 32 err) (= 83 33 err) (= 72
    34 err) err)
   '#((#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (3 . 3) (2 . 2) (#f . #f) (0 . 0) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (4 . 4))))

) ; end of library

