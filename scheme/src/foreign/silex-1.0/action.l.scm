;
; Table generated from the file action.l by SILex 1.0
;

(define action-tables
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
          (make-tok hblank-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          (make-tok vblank-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          (make-tok char-tok   yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 4)
       ((#f #\;) . 3)
       ((#f #\newline) . 2)
       ((#t #\	 #\newline #\space #\;) . 1))
      (((#t #\newline) . 1))
      ()
      (((#t #\newline) . 3))
      (((#f #\	 #\space) . 4)
       ((#f #\;) . 3)
       ((#t #\	 #\newline #\space #\;) . 1)))
   '#((#f . #f) (2 . 2) (1 . 1) (0 . 0) (0 . 0))))
