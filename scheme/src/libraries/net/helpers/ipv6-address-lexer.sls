(library (net helpers ipv6-address-lexer)
  (export
    ipv6-address-lexer-table)
  (import (rnrs) (silex lexer)(silex default-error-handler)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file ipv6-address-lexer.l by SILex 1.0
;

(define ipv6-address-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(silex-default-error-handler)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-<lexical-token> 'HEXINT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))
        )))
   'decision-trees
   0
   0
   '#((58 (48 err 1) (59 2 err)) (48 err (58 3 err)) err (48 err (58 4
    err)) (48 err (58 5 err)) err)
   '#((#f . #f) (1 . 1) (0 . 0) (1 . 1) (1 . 1) (1 . 1))))

) ; end of library

