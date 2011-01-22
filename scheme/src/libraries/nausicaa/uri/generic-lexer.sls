(library (nausicaa uri generic-lexer)
  (export
    uri-generic-lexer-table)
  (import (rnrs)
(nausicaa silex input-system)
(nausicaa silex default-error-handler)
(nausicaa parser-tools lexical-token)
(nausicaa parser-tools source-location)
)

;
; Table generated from the file generic-lexer.l by SILex 1.0
;

(define uri-generic-lexer-table
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
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
       			(make-<lexical-token> 'SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\/ 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
          		(make-<lexical-token> 'QUESTION
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\? 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
             		(make-<lexical-token> 'NUMBER-SIGN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\# 1)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
           		(make-<lexical-token> 'COMPONENT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yystring (string-length yystring))
        )))
   'decision-trees
   0
   0
   '#((69 (58 (36 (35 1 2) (= 47 4 1)) (64 (59 5 (63 1 3)) (65 1 (68 err
    1)))) (81 (74 (= 70 1 err) (76 1 (80 err 1))) (124 (86 err (123 1 err))
    (= 125 err 1)))) (76 (69 (65 1 (68 err 1)) (71 (70 err 1) (74 err 1)))
    (123 (81 (80 err 1) (86 err 1)) (125 (124 err 1) (126 err 1)))) (76 (69
    (65 1 (68 err 1)) (71 (70 err 1) (74 err 1))) (123 (81 (80 err 1) (86
    err 1)) (125 (124 err 1) (126 err 1)))) (76 (69 (65 1 (68 err 1)) (71
    (70 err 1) (74 err 1))) (123 (81 (80 err 1) (86 err 1)) (125 (124 err
    1) (126 err 1)))) (76 (69 (65 1 (68 err 1)) (71 (70 err 1) (74 err 1)))
    (123 (81 (80 err 1) (86 err 1)) (125 (124 err 1) (126 err 1)))) (76 (69
    (65 1 (68 err 1)) (71 (70 err 1) (74 err 1))) (123 (81 (80 err 1) (86
    err 1)) (125 (124 err 1) (126 err 1)))))
   '#((#f . #f) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (0 . 0))))

) ; end of library

