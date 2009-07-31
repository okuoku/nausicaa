(library (email address-strings-lexer)
  (export
    email-address-strings-table)
  (import (rnrs) (silex lexer))

;
; Table generated from the file address-strings.l by SILex 1.0
;

(define email-address-strings-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(assertion-violation #f
                          "found end of input while parsing quoted text")
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(assertion-violation #f
			  (string-append "invalid character in quoted text at line "
                                         (number->string yyline)
					 " column "
					 (number->string yycolumn)
					 " offset "
					 (number->string yyoffset))
			  yytext)

;;; end of file
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
             		(begin yytext)
        )))
   'decision-trees
   0
   0
   '#((35 (34 1 3) (= 92 2 1)) (35 (34 1 err) (= 92 2 1)) (= 92 2 1) err)
   '#((#f . #f) (1 . 1) (1 . 1) (0 . 0))))

) ; end of library

