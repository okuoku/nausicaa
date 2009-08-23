(library (email addresses lexer)
  (export
    address-table)
  (import (rnrs) (silex lexer)(lalr lr-driver))

;
; Table generated from the file lexer.l by SILex 1.0
;

(define address-table
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
         		(make-lexical-token
			 '*lexer-error*
			 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
			 yytext)

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
      			(make-lexical-token
			 'ATOM
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           		(let ((ch (string-ref yytext 0)))
			  (make-lexical-token
			   (case ch
			     ((#\@) 'AT)
			     ((#\.) 'DOT)
			     ((#\,) 'COMMA)
			     ((#\:) 'COLON)
			     ((#\;) 'SEMICOLON)
			     ((#\<) 'ANGLE-OPEN)
			     ((#\>) 'ANGLE-CLOSE))
			   (make-source-location #f yyline yycolumn yyoffset ch)
			   ch))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		(make-lexical-token
			 'COMMENT-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   	(make-lexical-token
			 'QUOTED-TEXT-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                      	(make-lexical-token
			 'DOMAIN-LITERAL-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)
        )))
   'decision-trees
   0
   0
   '#((44 (32 (11 (9 5 (10 6 7)) (= 13 7 5)) (35 (33 6 (34 5 2)) (= 40 3
    5))) (62 (47 (= 45 5 4) (58 5 (61 4 5))) (65 (= 63 5 4) (= 91 1 5))))
    err err err err (44 (32 (11 (9 5 err) (= 13 err 5)) (35 (= 33 5 err) (=
    40 err 5))) (62 (47 (= 45 5 err) (58 5 (61 err 5))) (65 (= 63 5 err) (=
    91 err 5)))) (10 (9 err 6) (= 32 6 err)) (13 (10 (9 err 8) (11 7 err))
    (32 (14 7 err) (33 8 err))) (10 (9 err 8) (= 32 8 err)))
   '#((#f . #f) (6 . 6) (5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (#f . #f)
    (0 . 0))))

) ; end of library

