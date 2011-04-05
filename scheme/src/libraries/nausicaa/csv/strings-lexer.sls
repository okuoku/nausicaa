#!r6rs
(library (nausicaa csv strings-lexer)
  (export
    csv-strings-table)
  (import (rnrs)
(nausicaa silex input-system)
)

;
; Table generated from the file strings.l by SILex 1.0
;

(define csv-strings-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(assertion-violation #f
                  "while parsing string, found end of input before closing double-quote")
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
    		(begin #\")	;a double double-quote is a
				;nested double-quote
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
  		(begin #f)	;a single double-quote is the
				;end of string
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
 		(string-ref yytext 0)	;any character which is not a
					;double-quote
        )))
   'decision-trees
   0
   0
   '#((11 (10 1 err) (= 34 2 1)) err (= 34 3 err) err)
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

