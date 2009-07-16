;
; Table generated from the file class.l by SILex 1.0
;

(define class-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
              (make-tok eof-tok    yytext yyline yycolumn)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (begin
         (display "Error: Invalid token.")
         (newline)
         'error)
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (make-tok rbrack-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (make-tok minus-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-spec-char     yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-digits-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-quoted-char   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
              (parse-ordinary-char yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\]) . 4) ((#f #\-) . 3) ((#f #\\) . 2) ((#t #\- #\\ #\]) . 1))
      ()
      (((#f #\n) . 8)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 7)
       ((#f #\-) . 6)
       ((#t #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\n) . 5))
      ()
      ()
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 10))
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 9))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 10)))
   '#((#f . #f) (6 . 6)   (6 . 6)   (1 . 1)   (0 . 0)   (5 . 5)   (5 . 5)
      (3 . 3)   (2 . 2)   (4 . 4)   (3 . 3))))
