(library (email addresses quoted-text-lexer)
  (export
    quoted-text-table)
  (import (rnrs) (silex lexer)(lalr lr-driver)(email addresses common))

;
; Table generated from the file quoted-text.l by SILex 1.0
;

(define quoted-text-table
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
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              		'QUOTED-TEXT-CLOSE
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
             		(unquote-string yytext)
        )))
   'decision-trees
   0
   0
   '#((35 (34 1 3) (= 92 2 1)) (35 (34 1 err) (= 92 2 1)) (= 92 2 1) err)
   '#((#f . #f) (1 . 1) (1 . 1) (0 . 0))))

) ; end of library

