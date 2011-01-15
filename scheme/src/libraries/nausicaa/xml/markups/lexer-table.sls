(library (nausicaa xml tags lexer-table)
  (export
    xml-lexer-table)
  (import (rnrs)(nausicaa silex lexer)(nausicaa parser-tools lexical-token)(nausicaa parser-tools source-location)(nausicaa xml tags lexeme-processing))

;
; Table generated from the file lexer-table.l by SILex 1.0
;

(define xml-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		((open-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done
        )))
   'decision-trees
   0
   0
   '#((32 (11 (9 err 1) (= 13 1 err)) (65534 (55296 1 (57344 err 1)) (65536
    err (1114112 1 err)))) err)
   '#((#f . #f) (0 . 0))))

) ; end of library

