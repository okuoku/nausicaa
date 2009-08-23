(library (email addresses comments-lexer)
  (export
    comments-table)
  (import (rnrs) (silex lexer)(lalr lr-driver))

;
; Table generated from the file comments.l by SILex 1.0
;

(define comments-table
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
                  	(quote COMMENT-OPEN)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                	(quote COMMENT-CLOSE)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                	yytext
        )))
   'decision-trees
   0
   0
   '#((41 (40 1 3) (42 2 1)) (40 1 (42 err 1)) err err)
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

