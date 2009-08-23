(library (email addresses domain-literals-lexer)
  (export
    domain-literals-table)
  (import (rnrs) (silex lexer)(lalr lr-driver))

;
; Table generated from the file domain-literals.l by SILex 1.0
;

(define domain-literals-table
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
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)

;;; end of file
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              		(make-lexical-token
			 'DOMAIN-LITERAL-CLOSE
			 (make-source-location #f yyline yycolumn yyoffset
			                       (string-length yytext))
			 yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         		(let ((num (string->number yytext)))
			  (make-lexical-token
			   (if (< num 256) 'DOMAIN-LITERAL-INTEGER '*lexer-error*)
			   (make-source-location #f yyline yycolumn yyoffset
						 (string-length yytext))
			   yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     			(make-lexical-token
			 'DOT
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)
        )))
   'decision-trees
   0
   0
   '#((48 (= 46 1 err) (93 (58 2 err) (94 3 err))) err (48 err (58 2 err))
    err)
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

