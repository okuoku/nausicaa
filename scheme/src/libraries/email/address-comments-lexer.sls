(library (email address-comments-lexer)
  (export
    email-address-comments-table)
  (import (rnrs) (silex lexer))

;
; Table generated from the file address-comments.l by SILex 1.0
;

(define email-address-comments-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(assertion-violation #f
                          "found end of input while parsing comment")
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(assertion-violation #f
			  (string-append "invalid character in comment at line "
                                         (number->string yyline)
					 " column "
					 (number->string yycolumn)
					 " offset "
					 (number->string yyoffset))
			  yytext)
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                  	(begin 'comment)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                	(begin #f)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                	(begin yytext)
        )))
   'decision-trees
   0
   0
   '#((41 (40 1 3) (42 2 1)) (40 1 (42 err 1)) err err)
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

