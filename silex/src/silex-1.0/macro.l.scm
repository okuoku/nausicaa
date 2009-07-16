;
; Table generated from the file macro.l by SILex 1.0
;

(define macro-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         (make-tok eof-tok             yytext yyline yycolumn)
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
         (make-tok hblank-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok vblank-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok percent-percent-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (parse-id                     yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
         (make-tok illegal-tok         yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 8)
       ((#f #\;) . 7)
       ((#f #\newline) . 6)
       ((#f #\%) . 5)
       ((#f  #\! #\$ #\& #\* #\/ #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E
         #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U
         #\V #\W #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i
         #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
         #\z #\~)
        .
        4)
       ((#f #\+ #\-) . 3)
       ((#f #\.) . 2)
       ((#t        #\	       #\newline #\space   #\!       #\$
         #\%       #\&       #\*       #\+       #\-       #\.
         #\/       #\:       #\;       #\<       #\=       #\>
         #\?       #\A       #\B       #\C       #\D       #\E
         #\F       #\G       #\H       #\I       #\J       #\K
         #\L       #\M       #\N       #\O       #\P       #\Q
         #\R       #\S       #\T       #\U       #\V       #\W
         #\X       #\Y       #\Z       #\^       #\_       #\a
         #\b       #\c       #\d       #\e       #\f       #\g
         #\h       #\i       #\j       #\k       #\l       #\m
         #\n       #\o       #\p       #\q       #\r       #\s
         #\t       #\u       #\v       #\w       #\x       #\y
         #\z       #\~)
        .
        1))
      ()
      (((#f #\.) . 9))
      ()
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#f #\%) . 11)
       ((#f  #\! #\$ #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6
         #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G #\H
         #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
         #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
         #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      ()
      (((#t #\newline) . 12))
      ()
      (((#f #\.) . 13))
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        10))
      (((#t #\newline) . 12))
      ())
   '#((#f . #f) (4 . 4)   (4 . 4)   (3 . 3)   (3 . 3)   (3 . 3)   (1 . 1)
      (0 . 0)   (0 . 0)   (#f . #f) (3 . 3)   (2 . 2)   (0 . 0)   (3 . 3))))
