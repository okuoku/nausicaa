(library (email address-lexer)
  (export
    email-address-table)
  (import (rnrs) (silex lexer))

;
; Table generated from the file address-lexer.l by SILex 1.0
;

(define email-address-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(begin #f)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(assertion-violation #f
			  (string-append "invalid token in email address text at line "
                                         (number->string yyline)
					 " column "
					 (number->string yycolumn)
					 " offset "
					 (number->string yyoffset)))
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
      			(cons 'atom yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           		(cons 'character (string-ref yytext 0))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              		(begin 'comment)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                  	(begin 'quoted-text)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                     	(begin 'domain-literal)

;; {domain-literal}	(cons 'domain-literal
;; 			      ;;Strip the square brackets.
;; 			      (let ((len (string-length yytext)))
;; 				(substring yytext 1 (- len 1))))
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

