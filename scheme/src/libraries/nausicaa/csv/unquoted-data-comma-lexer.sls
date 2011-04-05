#!r6rs
(library (nausicaa csv unquoted-data-comma-lexer)
  (export
    csv-unquoted-data-table/comma)
  (import (rnrs)
(nausicaa silex input-system)
)

;
; Table generated from the file unquoted-data-comma.l by SILex 1.0
;

(define csv-unquoted-data-table/comma
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
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       		(begin 'field)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(string-ref yytext 0)
        )))
   'decision-trees
   0
   0
   '#((14 (11 (10 1 5) (13 1 4)) (35 (34 1 3) (= 44 2 1))) err err err (11
    (10 err 5) (= 13 5 err)) (11 (10 err 5) (= 13 5 err)))
   '#((#f . #f) (3 . 3) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))

) ; end of library

