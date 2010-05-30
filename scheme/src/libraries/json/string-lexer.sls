(library (json string-lexer)
  (export
    json-string-lexer-table)
  (import (rnrs) (silex lexer)(lalr lr-driver)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file string-lexer.l by SILex 1.0
;

(define json-string-lexer-table
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
         		(error 'json-lexer
                         (string-append "invalid character \"" yytext
			                "\" in JSON string at line " (number->string yyline)
					" column " (number->string yycolumn))
			 yytext)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
              		'QUOTED-TEXT-CLOSE
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
               		"\""
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                   	"\\"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
               		"/"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                   	"\b"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                  	"\f"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                  	"\n"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                	"\r"
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
             		"\t"
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                 	(begin
                         (string (integer->char (string->number (substring yytext 2 6) 16))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      			(begin yytext)
        )))
   'decision-trees
   0
   0
   '#((35 (34 1 3) (= 92 2 1)) (35 (34 1 err) (= 92 err 1)) (102 (48 (35
    (34 err 12) (47 err 10)) (93 (92 err 11) (= 98 9 err))) (114 (110 (103
    8 err) (111 7 err)) (116 (115 6 err) (117 5 (118 4 err))))) err (65 (48
    err (58 13 err)) (97 (71 13 err) (103 13 err))) err err err err err err
    err err (65 (48 err (58 14 err)) (97 (71 14 err) (103 14 err))) (65 (48
    err (58 15 err)) (97 (71 15 err) (103 15 err))) (65 (48 err (58 16
    err)) (97 (71 16 err) (103 16 err))) err)
   '#((#f . #f) (10 . 10) (#f . #f) (0 . 0) (#f . #f) (8 . 8) (7 . 7) (6 .
    6) (5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f .
    #f) (9 . 9))))

) ; end of library

