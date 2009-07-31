(library (email address-domain-literals-lexer)
  (export
    email-address-domain-literals-table)
  (import (rnrs) (silex lexer))

;
; Table generated from the file address-domain-literals.l by SILex 1.0
;

(define email-address-domain-literals-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(assertion-violation #f
                          "found end of input while parsing domain literal")
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(assertion-violation #f
			  (string-append "invalid character in domain literal at line "
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
              		(begin #f)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
			(substring yytext 1 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
			(begin yytext)
        )))
   'decision-trees
   0
   0
   '#((92 (91 1 err) (93 2 (94 3 1))) (91 1 (94 err 1)) (92 (91 err 4) (=
    93 4 err)) err err)
   '#((#f . #f) (2 . 2) (2 . 2) (0 . 0) (1 . 1))))

) ; end of library

