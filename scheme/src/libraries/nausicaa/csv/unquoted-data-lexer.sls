#!r6rs
(library (nausicaa csv unquoted-data-lexer)
  (export
    csv-unquoted-data-table)
  (import (rnrs)
(nausicaa silex input-system)
)

;
; Table generated from the file unquoted-data.l by SILex 1.0
;

(define csv-unquoted-data-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(begin #f)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         	(assertion-violation #f
                  (string-append "invalid token at line "
				 (number->string yyline)
				 " column "
				 (number->string yycolumn))
		  yytext)

;;; end of file
;; Local Variables:
;; mode: fundamental
;; End:
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
             	(begin 'eol)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              	(begin 'string)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(string-ref yytext 0)
        )))
   'decision-trees
   0
   0
   '#((13 (= 10 4 1) (34 (14 3 1) (35 2 1))) err err (11 (10 err 4) (= 13 4
    err)) (11 (10 err 4) (= 13 4 err)))
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))

) ; end of library

