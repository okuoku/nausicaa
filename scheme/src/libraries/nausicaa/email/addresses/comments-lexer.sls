(library (nausicaa email addresses comments-lexer)
  (export
    comments-table)
  (import (rnrs)(nausicaa silex lexer)(nausicaa email addresses common)(nausicaa parser-tools lexical-token)(nausicaa parser-tools source-location))

;
; Table generated from the file comments.l by SILex 1.0
;

(define comments-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-<lexical-token> '*eoi*
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      (eof-object)
					      0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(make-<lexical-token> '*lexer-error*
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext
					      (string-length yytext))

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
                	(unquote-string yytext)
        )))
   'decision-trees
   0
   0
   '#((41 (40 1 3) (42 2 1)) (40 1 (42 err 1)) err err)
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

