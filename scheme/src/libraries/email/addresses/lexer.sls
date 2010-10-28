(library (email addresses lexer)
  (export
    address-table)
  (import (rnrs) (silex lexer)(email addresses common)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file lexer.l by SILex 1.0
;

(define address-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-<lexical-token>
			 '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object)
			 0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(make-<lexical-token>
			 '*lexer-error*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			  (string-length yytext))

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
     			;ignore it
        (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
    			;ignore it

        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      			(make-<lexical-token>
			 'ATOM
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           		(let ((ch (string-ref yytext 0)))
			  (make-<lexical-token>
			   (case ch
			     ((#\@) 'AT)
			     ((#\.) 'DOT)
			     ((#\,) 'COMMA)
			     ((#\:) 'COLON)
			     ((#\;) 'SEMICOLON)
			     ((#\<) 'ANGLE-OPEN)
			     ((#\>) 'ANGLE-CLOSE))
			   (make-<source-location> #f yyline yycolumn yyoffset)
			   ch
			   1))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		(make-<lexical-token>
			 'COMMENT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   	(make-<lexical-token>
			 'QUOTED-TEXT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                      	(make-<lexical-token>
			 'DOMAIN-LITERAL-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
        )))
   'decision-trees
   0
   0
   '#((45 (33 (13 (= 9 6 err) (14 7 (32 err 6))) (40 (= 34 2 5) (42 (41 3
    err) (44 5 4)))) (63 (58 (= 46 4 5) (= 61 5 4)) (91 (= 64 4 5) (94 (92
    1 err) (127 5 err))))) err err err err (47 (40 (34 (33 err 5) (35 err
    5)) (44 (42 err 5) (= 45 5 err))) (64 (61 (58 5 err) (= 62 err 5)) (91
    (65 err 5) (94 err (127 5 err))))) (10 (9 err 6) (= 32 6 err)) (= 10 8
    err) (10 (9 err 9) (= 32 9 err)) (10 (9 err 9) (= 32 9 err)))
   '#((#f . #f) (6 . 6) (5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (#f . #f)
    (#f . #f) (0 . 0))))

) ; end of library

