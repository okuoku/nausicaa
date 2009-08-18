(library (email address-quoted-string-lexer)
  (export
    email-address-quoted-string-table)
  (import (rnrs) (silex lexer)(lalr lr-driver))

;
; Table generated from the file address-quoted-string.l by SILex 1.0
;

(define email-address-quoted-string-table
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
			 'INVALID-CHARACTER
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              		'QUOTED-STRING-CLOSE
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		yytext
        )))
   'decision-trees
   0
   0
   '#((35 (34 1 3) (= 92 2 1)) (35 (34 1 err) (= 92 2 1)) (= 92 2 1) err)
   '#((#f . #f) (1 . 1) (1 . 1) (0 . 0))))

) ; end of library

