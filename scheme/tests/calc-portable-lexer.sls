(library (calc-portable-lexer)
  (export
    calc-lexer-table/portable)
  (import (rnrs) (silex lexer))

;
; Table generated from the file #f by SILex 1.0
;

(define calc-lexer-table/portable
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
       		(begin #f)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
         	(assertion-violation #f
                  "invalid lexer token" yytext)
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	;; skip blanks, tabs and newlines
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number (string-append "+" yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
     		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
     		(string->number yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
          	(case (string-ref yytext 0)
		  ((#\+) +)
		  ((#\-) -)
		  ((#\*) *)
		  ((#\/) /)
		  ((#\%) mod)
		  ((#\^) expt)
		  ((#\\) div)
		  ((#\=) =)
		  ((#\<) <)
		  ((#\>) >))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
             	(cond
                  ((string=? yytext "<=") <=)
                  ((string=? yytext ">=") >=))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
        	(string->symbol yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
       		(begin cons)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	(begin #\()
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
        	(begin #\))
        )))
   'tagged-chars-lists
   0
   0
   '#((((#f #\	 #\newline #\ #\space) . 16) ((#f #\#) . 15) ((#f #\0 #\1
    #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 14) ((#f #\.) . 13) ((#f #\-) . 12)
    ((#f #\+) . 11) ((#f #\n) . 10) ((#f #\i) . 9) ((#f #\% #\* #\/ #\\
    #\^) . 8) ((#f #\<) . 7) ((#f #\>) . 6) ((#f #\=) . 5) ((#f #\! #\$ #\&
    #\: #\? #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g
    #\h #\j #\k #\l #\m #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\~) . 4) ((#f #\,) . 3) ((#f #\() . 2) ((#f #\)) . 1)) () () () (((#f
    #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\>
    #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g
    #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
    #\z #\~) . 4)) (((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a
    #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
    #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\=) . 17) ((#f #\! #\$
    #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\> #\? #\@ #\A
    #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
    #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
    #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) .
    4)) (((#f #\=) . 17) ((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9 #\: #\< #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a
    #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
    #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) () (((#f #\n) . 18) ((#f #\! #\$
    #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R
    #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i
    #\j #\k #\l #\m #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) .
    4)) (((#f #\a) . 19) ((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I
    #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_
    #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
    #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\n) . 21) ((#f #\i) .
    20)) (((#f #\n) . 23) ((#f #\i) . 22)) (((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9) . 24)) (((#f #\i) . 27) ((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9) . 14) ((#f #\.) . 26) ((#f #\E #\e) . 25)) (((#f #\B
    #\b) . 30) ((#f #\O #\o) . 29) ((#f #\X #\x) . 28)) (((#f #\	 #\newline
    #\ #\space) . 16)) (((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I
    #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
    #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\f) . 31) ((#f #\!
    #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\?
    #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q
    #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\g #\h #\i
    #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\~) . 4)) (((#f #\n) . 32) ((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4
    #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G
    #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y
    #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\o #\p #\q
    #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\n) . 33)) (((#f
    #\a) . 34)) (((#f #\n) . 35)) (((#f #\a) . 36)) (((#f #\i) . 27) ((#f
    #\E #\e) . 37) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 24))
    (((#f #\+ #\-) . 39) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) .
    38)) (((#f #\i) . 27) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) .
    24)) () (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D
    #\E #\F #\a #\b #\c #\d #\e #\f) . 40)) (((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7) . 41)) (((#f #\0 #\1) . 42)) (((#f #\.) . 43) ((#f #\! #\$ #\&
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B
    #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
    #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4))
    (((#f #\.) . 44) ((#f #\! #\$ #\& #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
    #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
    #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b
    #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
    #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\f) . 45)) (((#f #\n) . 46))
    (((#f #\f) . 47)) (((#f #\n) . 48)) (((#f #\+ #\-) . 50) ((#f #\0 #\1
    #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 49)) (((#f #\i) . 27) ((#f #\0 #\1
    #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 38)) (((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9) . 38)) (((#f #\i) . 27) ((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) . 40))
    (((#f #\i) . 27) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) . 41)) (((#f
    #\i) . 27) ((#f #\0 #\1) . 42)) (((#f #\0) . 51) ((#f #\! #\$ #\& #\.
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C
    #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U
    #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
    #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4))
    (((#f #\0) . 52) ((#f #\! #\$ #\& #\. #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
    #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
    #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b
    #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
    #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\.) . 53)) (((#f #\.) . 54))
    (((#f #\.) . 55)) (((#f #\.) . 56)) (((#f #\i) . 27) ((#f #\0 #\1 #\2
    #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 49)) (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9) . 49)) (((#f #\! #\$ #\& #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I
    #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
    #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\! #\$ #\& #\. #\0
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C
    #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U
    #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
    #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4))
    (((#f #\0) . 57)) (((#f #\0) . 58)) (((#f #\0) . 57)) (((#f #\0) . 58))
    () ())
   '#((#f . #f) (10 . 10) (9 . 9) (8 . 8) (7 . 7) (5 . 5) (5 . 5) (5 . 5)
    (5 . 5) (7 . 7) (7 . 7) (5 . 5) (5 . 5) (#f . #f) (2 . 2) (#f . #f) (0
    . 0) (6 . 6) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2
    . 2) (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (7 . 7) (7
    . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (#f .
    #f) (2 . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f . #f) (#f
    . #f) (#f . #f) (4 . 4) (3 . 3))))

) ; end of library

