;
; Table generated from the file regexp.l by SILex 1.0
;

(define regexp-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok eof-tok           yytext yyline yycolumn)
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
                          (make-tok hblank-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok vblank-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok pipe-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok question-tok      yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok plus-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok star-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lpar-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok rpar-tok          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok dot-tok           yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-rbrack-tok yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-caret-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok lbrack-minus-tok  yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-id-ref               yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m              yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m-inf          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-power-m-n            yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok illegal-tok       yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok doublequote-tok   yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-spec-char            yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-digits-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-digits-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-quoted-char          yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok caret-tok         yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok dollar-tok        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (parse-ordinary-char        yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok <<EOF>>-tok       yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                          (make-tok <<ERROR>>-tok     yytext yyline yycolumn)
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\space) . 18)
       ((#f #\;) . 17)
       ((#f #\newline) . 16)
       ((#f #\|) . 15)
       ((#f #\?) . 14)
       ((#f #\+) . 13)
       ((#f #\*) . 12)
       ((#f #\() . 11)
       ((#f #\)) . 10)
       ((#f #\.) . 9)
       ((#f #\[) . 8)
       ((#f #\{) . 7)
       ((#f #\") . 6)
       ((#f #\\) . 5)
       ((#f #\^) . 4)
       ((#f #\$) . 3)
       ((#t        #\	       #\newline #\space   #\"       #\$
         #\(       #\)       #\*       #\+       #\.       #\;
         #\<       #\?       #\[       #\\       #\^       #\{
         #\|)
        .
        2)
       ((#f #\<) . 1))
      (((#f #\<) . 19))
      ()
      ()
      ()
      (((#f #\n) . 23)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 22)
       ((#f #\-) . 21)
       ((#t #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\n) . 20))
      ()
      (((#f  #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\A #\B #\C #\D
         #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
         #\U #\V #\W #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h
         #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
         #\y #\z #\~)
        .
        27)
       ((#f #\+ #\-) . 26)
       ((#f #\.) . 25)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 24))
      (((#f #\]) . 30) ((#f #\^) . 29) ((#f #\-) . 28))
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      ()
      (((#t #\newline) . 31))
      ()
      (((#f #\E) . 32))
      ()
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 33))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 34))
      ()
      (((#f #\}) . 36)
       ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 24)
       ((#f #\,) . 35))
      (((#f #\.) . 37))
      (((#f #\}) . 38))
      (((#f #\}) . 38)
       ((#f  #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
         #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\A #\B #\C #\D #\E #\F #\G
         #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
         #\X #\Y #\Z #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
         #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)
        .
        27))
      ()
      ()
      ()
      (((#t #\newline) . 31))
      (((#f #\O) . 40) ((#f #\R) . 39))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 33))
      (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 34))
      (((#f #\}) . 42) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 41))
      ()
      (((#f #\.) . 26))
      ()
      (((#f #\R) . 43))
      (((#f #\F) . 44))
      (((#f #\}) . 45) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 41))
      ()
      (((#f #\O) . 46))
      (((#f #\>) . 47))
      ()
      (((#f #\R) . 48))
      (((#f #\>) . 49))
      (((#f #\>) . 50))
      ()
      (((#f #\>) . 51))
      ())
   '#((#f . #f) (25 . 25) (25 . 25) (24 . 24) (23 . 23) (25 . 25) (18 . 18)
      (17 . 17) (9 . 9)   (8 . 8)   (7 . 7)   (6 . 6)   (5 . 5)   (4 . 4)
      (3 . 3)   (2 . 2)   (1 . 1)   (0 . 0)   (0 . 0)   (#f . #f) (22 . 22)
      (22 . 22) (20 . 20) (19 . 19) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
      (12 . 12) (11 . 11) (10 . 10) (0 . 0)   (#f . #f) (21 . 21) (20 . 20)
      (#f . #f) (14 . 14) (#f . #f) (13 . 13) (#f . #f) (#f . #f) (#f . #f)
      (15 . 15) (#f . #f) (#f . #f) (16 . 16) (#f . #f) (#f . #f) (#f . #f)
      (26 . 26) (#f . #f) (27 . 27))))
