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
       		(eof-object)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
         	(assertion-violation #f
                  "invalid lexer token")
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
   '#((((#f #\	 #\newline #\ #\space) . 15) ((#f #\#) . 14) ((#f #\0 #\1
    #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 13) ((#f #\.) . 12) ((#f #\-) . 11)
    ((#f #\+) . 10) ((#f #\n) . 9) ((#f #\i) . 8) ((#f #\% #\* #\/ #\= #\\
    #\^) . 7) ((#f #\<) . 6) ((#f #\>) . 5) ((#f #\A #\B #\C #\D #\E #\F
    #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
    #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\j #\k #\l #\m #\o #\p #\q
    #\r #\s #\t #\u #\v #\w #\x #\y #\z) . 4) ((#f #\,) . 3) ((#f #\() . 2)
    ((#f #\)) . 1)) () () () (((#f #\! #\$ #\& #\- #\. #\0 #\1 #\2 #\3 #\4
    #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G
    #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y
    #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p
    #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\=) . 16))
    (((#f #\=) . 16)) () (((#f #\n) . 17) ((#f #\! #\$ #\& #\- #\. #\0 #\1
    #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D
    #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
    #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\a) .
    18) ((#f #\! #\$ #\& #\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
    #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\b #\c #\d
    #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
    #\w #\x #\y #\z #\~) . 4)) (((#f #\n) . 20) ((#f #\i) . 19)) (((#f #\n)
    . 22) ((#f #\i) . 21)) (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) .
    23)) (((#f #\i) . 26) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) .
    13) ((#f #\.) . 25) ((#f #\E #\e) . 24)) (((#f #\B #\b) . 29) ((#f #\O
    #\o) . 28) ((#f #\X #\x) . 27)) (((#f #\	 #\newline #\ #\space) . 15))
    () (((#f #\f) . 30) ((#f #\! #\$ #\& #\- #\. #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H
    #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\_ #\a #\b #\c #\d #\e #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
    #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\n) . 31) ((#f #\!
    #\$ #\& #\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\>
    #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g
    #\h #\i #\j #\k #\l #\m #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\~) . 4)) (((#f #\n) . 32)) (((#f #\a) . 33)) (((#f #\n) . 34)) (((#f
    #\a) . 35)) (((#f #\i) . 26) ((#f #\E #\e) . 36) ((#f #\0 #\1 #\2 #\3
    #\4 #\5 #\6 #\7 #\8 #\9) . 23)) (((#f #\+ #\-) . 38) ((#f #\0 #\1 #\2
    #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 37)) (((#f #\i) . 26) ((#f #\0 #\1 #\2
    #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 23)) () (((#f #\0 #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) . 39))
    (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) . 40)) (((#f #\0 #\1) . 41))
    (((#f #\.) . 42) ((#f #\! #\$ #\& #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a
    #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s
    #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\.) . 43) ((#f #\! #\$
    #\& #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R
    #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i
    #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\~) . 4)) (((#f #\f) . 44)) (((#f #\n) . 45)) (((#f #\f) . 46)) (((#f
    #\n) . 47)) (((#f #\+ #\-) . 49) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9) . 48)) (((#f #\i) . 26) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9) . 37)) (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 37))
    (((#f #\i) . 26) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B
    #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) . 39)) (((#f #\i) . 26) ((#f
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) . 40)) (((#f #\i) . 26) ((#f #\0 #\1)
    . 41)) (((#f #\0) . 50) ((#f #\! #\$ #\& #\- #\. #\1 #\2 #\3 #\4 #\5
    #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H
    #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q
    #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\0) . 51) ((#f
    #\! #\$ #\& #\- #\. #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\>
    #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g
    #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y
    #\z #\~) . 4)) (((#f #\.) . 52)) (((#f #\.) . 53)) (((#f #\.) . 54))
    (((#f #\.) . 55)) (((#f #\i) . 26) ((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9) . 48)) (((#f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . 48))
    (((#f #\! #\$ #\& #\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\:
    #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d
    #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
    #\w #\x #\y #\z #\~) . 4)) (((#f #\! #\$ #\& #\- #\. #\0 #\1 #\2 #\3
    #\4 #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F
    #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
    #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
    #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~) . 4)) (((#f #\0) .
    56)) (((#f #\0) . 57)) (((#f #\0) . 56)) (((#f #\0) . 57)) () ())
   '#((#f . #f) (10 . 10) (9 . 9) (8 . 8) (7 . 7) (5 . 5) (5 . 5) (5 . 5)
    (7 . 7) (7 . 7) (5 . 5) (5 . 5) (#f . #f) (2 . 2) (#f . #f) (0 . 0) (6
    . 6) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2)
    (#f . #f) (2 . 2) (1 . 1) (#f . #f) (#f . #f) (#f . #f) (7 . 7) (7 . 7)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (#f . #f) (2
    . 2) (2 . 2) (2 . 2) (7 . 7) (7 . 7) (#f . #f) (#f . #f) (#f . #f) (#f
    . #f) (2 . 2) (#f . #f) (4 . 4) (3 . 3) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (4 . 4) (3 . 3))))

) ; end of library

