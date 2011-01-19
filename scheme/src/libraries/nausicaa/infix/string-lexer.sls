(library (nausicaa infix string-lexer)
  (export
    infix-string-lexer-table)
  (import (rnrs)
(nausicaa silex lexer)
(nausicaa lalr lr-driver)
(nausicaa parser-tools lexical-token)
(nausicaa parser-tools source-location)
)

;
; Table generated from the file string-lexer.l by SILex 1.0
;

(define infix-string-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       		(make-<lexical-token>
		 '*eoi*
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 (eof-object)
		 0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         	(assertion-violation #f
                  "invalid lexer token")

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
        	;; skip blanks, tabs and newlines
        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number (string-append "+" yytext))
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number yytext)
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
     		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +nan.0
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +inf.0
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      -inf.0
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(let ((position (make-<source-location> #f yyline yycolumn yyoffset))
		      (len	(string-length yytext))
                      (symbol	(string->symbol yytext)))
                  ;;These must be different categories to let us specify
                  ;;their precedence in the grammar.
		  (case symbol
		    ((+)	(make-<lexical-token> 'ADD	position '+ len))
		    ((-)	(make-<lexical-token> 'SUB	position '- len))
		    ((*)	(make-<lexical-token> 'MUL	position '* len))
		    ((/)	(make-<lexical-token> 'DIV	position '/ len))
		    ((%)	(make-<lexical-token> 'MOD	position 'mod len))
		    ((^)	(make-<lexical-token> 'EXPT	position 'expt len))
		    ((//)	(make-<lexical-token> 'DIV	position 'div len))
		    ((<)	(make-<lexical-token> 'LT	position '< len))
		    ((>)	(make-<lexical-token> 'GT	position '> len))
		    ((<=)	(make-<lexical-token> 'LE	position '<= len))
		    ((>=)	(make-<lexical-token> 'GE	position '>= len))
		    ((=)	(make-<lexical-token> 'EQ	position '= len))))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-<lexical-token> 'ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->symbol yytext)
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
          	(make-<lexical-token> 'QUESTION-ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      '?
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       		(make-<lexical-token> 'COLON-ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      ':
				      (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
       		(make-<lexical-token>
		 'COMMA
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'COMMA
		 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-<lexical-token>
		 'LPAREN
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'LPAREN
		 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        	(make-<lexical-token>
		 'RPAREN
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'RPAREN
		 (string-length yytext))
        )))
   'code
   (lambda (<<EOF>>-pre-action
            <<ERROR>>-pre-action
            rules-pre-action
            IS)
     (letrec
         ((user-action-<<EOF>> #f)
          (user-action-<<ERROR>> #f)
          (user-action-0 #f)
          (user-action-1 #f)
          (user-action-2 #f)
          (user-action-3 #f)
          (user-action-4 #f)
          (user-action-5 #f)
          (user-action-6 #f)
          (user-action-7 #f)
          (user-action-8 #f)
          (user-action-9 #f)
          (user-action-10 #f)
          (user-action-11 #f)
          (user-action-12 #f)
          (start-go-to-end    (:input-system-start-go-to-end	IS))
          (end-go-to-point    (:input-system-end-go-to-point	IS))
          (init-lexeme        (:input-system-init-lexeme	IS))
          (get-start-line     (:input-system-get-start-line	IS))
          (get-start-column   (:input-system-get-start-column	IS))
          (get-start-offset   (:input-system-get-start-offset	IS))
          (peek-left-context  (:input-system-peek-left-context	IS))
          (peek-char          (:input-system-peek-char		IS))
          (read-char          (:input-system-read-char		IS))
          (get-start-end-text (:input-system-get-start-end-text IS))
          (user-getc          (:input-system-user-getc		IS))
          (user-ungetc        (:input-system-user-ungetc	IS))
          (action-<<EOF>>
           (lambda (yyline yycolumn yyoffset)
             (user-action-<<EOF>> "" yyline yycolumn yyoffset)))
          (action-<<ERROR>>
           (lambda (yyline yycolumn yyoffset)
             (user-action-<<ERROR>> "" yyline yycolumn yyoffset)))
          (action-0
           (lambda (yyline yycolumn yyoffset)
             (start-go-to-end)
             (user-action-0 yyline yycolumn yyoffset)))
          (action-1
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-1 yytext yyline yycolumn yyoffset))))
          (action-2
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-2 yytext yyline yycolumn yyoffset))))
          (action-3
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-3 yytext yyline yycolumn yyoffset))))
          (action-4
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-4 yytext yyline yycolumn yyoffset))))
          (action-5
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-5 yytext yyline yycolumn yyoffset))))
          (action-6
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-6 yytext yyline yycolumn yyoffset))))
          (action-7
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-7 yytext yyline yycolumn yyoffset))))
          (action-8
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-8 yytext yyline yycolumn yyoffset))))
          (action-9
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-9 yytext yyline yycolumn yyoffset))))
          (action-10
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-10 yytext yyline yycolumn yyoffset))))
          (action-11
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-11 yytext yyline yycolumn yyoffset))))
          (action-12
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-12 yytext yyline yycolumn yyoffset))))
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 37)
                           (if (< c 14)
                               (if (< c 11)
                                   (if (< c 9)
                                       action
                                       (state-18 action))
                                   (if (< c 13)
                                       action
                                       (state-18 action)))
                               (if (< c 33)
                                   (if (< c 32)
                                       action
                                       (state-18 action))
                                   (if (= c 35)
                                       (state-17 action)
                                       action)))
                           (if (< c 43)
                               (if (< c 40)
                                   (if (< c 38)
                                       (state-10 action)
                                       action)
                                   (if (< c 41)
                                       (state-2 action)
                                       (if (< c 42)
                                           (state-1 action)
                                           (state-10 action))))
                               (if (< c 45)
                                   (if (< c 44)
                                       (state-13 action)
                                       (state-3 action))
                                   (if (< c 46)
                                       (state-14 action)
                                       (if (< c 47)
                                           (state-15 action)
                                           (state-7 action))))))
                       (if (< c 91)
                           (if (< c 61)
                               (if (< c 59)
                                   (if (< c 58)
                                       (state-16 action)
                                       (state-4 action))
                                   (if (< c 60)
                                       action
                                       (state-9 action)))
                               (if (< c 63)
                                   (if (< c 62)
                                       (state-10 action)
                                       (state-8 action))
                                   (if (< c 64)
                                       (state-5 action)
                                       (if (< c 65)
                                           action
                                           (state-6 action)))))
                           (if (< c 105)
                               (if (< c 95)
                                   (if (< c 94)
                                       action
                                       (state-10 action))
                                   (if (= c 96)
                                       action
                                       (state-6 action)))
                               (if (< c 110)
                                   (if (< c 106)
                                       (state-11 action)
                                       (state-6 action))
                                   (if (< c 111)
                                       (state-12 action)
                                       (if (< c 123)
                                           (state-6 action)
                                           action))))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-12))
          (state-2
           (lambda (action)
             (end-go-to-point)
             action-11))
          (state-3
           (lambda (action)
             (end-go-to-point)
             action-10))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-9))
          (state-5
           (lambda (action)
             (end-go-to-point)
             action-8))
          (state-6
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 37)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (< c 36)
                                   action-7
                                   (state-6 action-7)))
                           (if (< c 39)
                               (if (< c 38)
                                   action-7
                                   (state-6 action-7))
                               (if (< c 45)
                                   action-7
                                   (if (< c 47)
                                       (state-6 action-7)
                                       action-7))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 91)
                                   (state-6 action-7)
                                   (if (< c 95)
                                       action-7
                                       (state-6 action-7))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-7
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 47)
                       (state-10 action-6)
                       action-6)
                   action-6))))
          (state-8
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 61)
                       (state-10 action-6)
                       action-6)
                   action-6))))
          (state-9
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 61)
                       (state-10 action-6)
                       action-6)
                   action-6))))
          (state-10
           (lambda (action)
             (end-go-to-point)
             action-6))
          (state-11
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 59)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (= c 47)
                                   action-7
                                   (state-6 action-7))))
                       (if (< c 97)
                           (if (< c 91)
                               (if (< c 60)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 95)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 123)
                               (if (= c 110)
                                   (state-19 action-7)
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-12
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 59)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (= c 47)
                                   action-7
                                   (state-6 action-7))))
                       (if (< c 97)
                           (if (< c 91)
                               (if (< c 60)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 95)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 123)
                               (if (< c 98)
                                   (state-20 action-7)
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-13
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 106)
                       (if (< c 105)
                           action-6
                           (state-21 action-6))
                       (if (= c 110)
                           (state-22 action-6)
                           action-6))
                   action-6))))
          (state-14
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 106)
                       (if (< c 105)
                           action-6
                           (state-23 action-6))
                       (if (= c 110)
                           (state-24 action-6)
                           action-6))
                   action-6))))
          (state-15
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action
                       (if (< c 58)
                           (state-25 action)
                           action))
                   action))))
          (state-16
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 69)
                       (if (< c 47)
                           (if (< c 46)
                               action-2
                               (state-27 action-2))
                           (if (< c 48)
                               action-2
                               (if (< c 58)
                                   (state-16 action-2)
                                   action-2)))
                       (if (< c 102)
                           (if (< c 70)
                               (state-26 action-2)
                               (if (< c 101)
                                   action-2
                                   (state-26 action-2)))
                           (if (= c 105)
                               (state-28 action-2)
                               action-2)))
                   action-2))))
          (state-17
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 89)
                       (if (< c 79)
                           (if (= c 66)
                               (state-31 action)
                               action)
                           (if (< c 80)
                               (state-30 action)
                               (if (< c 88)
                                   action
                                   (state-29 action))))
                       (if (< c 111)
                           (if (= c 98)
                               (state-31 action)
                               action)
                           (if (< c 120)
                               (if (< c 112)
                                   (state-30 action)
                                   action)
                               (if (< c 121)
                                   (state-29 action)
                                   action))))
                   action))))
          (state-18
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 13)
                       (if (< c 9)
                           action-0
                           (if (< c 11)
                               (state-18 action-0)
                               action-0))
                       (if (< c 32)
                           (if (< c 14)
                               (state-18 action-0)
                               action-0)
                           (if (< c 33)
                               (state-18 action-0)
                               action-0)))
                   action-0))))
          (state-19
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 59)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (= c 47)
                                   action-7
                                   (state-6 action-7))))
                       (if (< c 97)
                           (if (< c 91)
                               (if (< c 60)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 95)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 123)
                               (if (= c 102)
                                   (state-32 action-7)
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-20
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 59)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (= c 47)
                                   action-7
                                   (state-6 action-7))))
                       (if (< c 97)
                           (if (< c 91)
                               (if (< c 60)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 95)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 123)
                               (if (= c 110)
                                   (state-33 action-7)
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-21
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 110)
                       (state-34 action)
                       action)
                   action))))
          (state-22
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 97)
                       (state-35 action)
                       action)
                   action))))
          (state-23
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 110)
                       (state-36 action)
                       action)
                   action))))
          (state-24
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 97)
                       (state-37 action)
                       action)
                   action))))
          (state-25
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 70)
                       (if (< c 58)
                           (if (< c 48)
                               action-2
                               (state-25 action-2))
                           (if (< c 69)
                               action-2
                               (state-38 action-2)))
                       (if (< c 102)
                           (if (< c 101)
                               action-2
                               (state-38 action-2))
                           (if (= c 105)
                               (state-28 action-2)
                               action-2)))
                   action-2))))
          (state-26
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 45)
                       (if (= c 43)
                           (state-40 action)
                           action)
                       (if (< c 48)
                           (if (< c 46)
                               (state-40 action)
                               action)
                           (if (< c 58)
                               (state-39 action)
                               action)))
                   action))))
          (state-27
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 58)
                       (if (< c 48)
                           action-2
                           (state-25 action-2))
                       (if (= c 105)
                           (state-28 action-2)
                           action-2))
                   action-2))))
          (state-28
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-29
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-41 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-41 action)
                               action)
                           (if (< c 103)
                               (state-41 action)
                               action)))
                   action))))
          (state-30
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action
                       (if (< c 56)
                           (state-42 action)
                           action))
                   action))))
          (state-31
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action
                       (if (< c 50)
                           (state-43 action)
                           action))
                   action))))
          (state-32
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 46)
                                   (state-6 action-7)
                                   (if (< c 47)
                                       (state-44 action-7)
                                       action-7))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 91)
                                   (state-6 action-7)
                                   (if (< c 95)
                                       action-7
                                       (state-6 action-7))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-33
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 46)
                                   (state-6 action-7)
                                   (if (< c 47)
                                       (state-45 action-7)
                                       action-7))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 91)
                                   (state-6 action-7)
                                   (if (< c 95)
                                       action-7
                                       (state-6 action-7))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-34
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 102)
                       (state-46 action)
                       action)
                   action))))
          (state-35
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 110)
                       (state-47 action)
                       action)
                   action))))
          (state-36
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 102)
                       (state-48 action)
                       action)
                   action))))
          (state-37
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 110)
                       (state-49 action)
                       action)
                   action))))
          (state-38
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 45)
                       (if (= c 43)
                           (state-51 action)
                           action)
                       (if (< c 48)
                           (if (< c 46)
                               (state-51 action)
                               action)
                           (if (< c 58)
                               (state-50 action)
                               action)))
                   action))))
          (state-39
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 58)
                       (if (< c 48)
                           action-2
                           (state-39 action-2))
                       (if (= c 105)
                           (state-28 action-2)
                           action-2))
                   action-2))))
          (state-40
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action
                       (if (< c 58)
                           (state-39 action)
                           action))
                   action))))
          (state-41
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 71)
                       (if (< c 58)
                           (if (< c 48)
                               action-2
                               (state-41 action-2))
                           (if (< c 65)
                               action-2
                               (state-41 action-2)))
                       (if (< c 103)
                           (if (< c 97)
                               action-2
                               (state-41 action-2))
                           (if (= c 105)
                               (state-28 action-2)
                               action-2)))
                   action-2))))
          (state-42
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 56)
                       (if (< c 48)
                           action-2
                           (state-42 action-2))
                       (if (= c 105)
                           (state-28 action-2)
                           action-2))
                   action-2))))
          (state-43
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 50)
                       (if (< c 48)
                           action-2
                           (state-43 action-2))
                       (if (= c 105)
                           (state-28 action-2)
                           action-2))
                   action-2))))
          (state-44
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 49)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 47)
                                   (state-6 action-7)
                                   (if (< c 48)
                                       action-7
                                       (state-52 action-7)))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 91)
                                   (state-6 action-7)
                                   (if (< c 95)
                                       action-7
                                       (state-6 action-7))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-45
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 49)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 36)
                                   (state-6 action-7)
                                   action-7))
                           (if (< c 45)
                               (if (< c 39)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 47)
                                   (state-6 action-7)
                                   (if (< c 48)
                                       action-7
                                       (state-53 action-7)))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-7)
                                   action-7)
                               (if (< c 91)
                                   (state-6 action-7)
                                   (if (< c 95)
                                       action-7
                                       (state-6 action-7))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-7
                                   (state-6 action-7))
                               (if (= c 126)
                                   (state-6 action-7)
                                   action-7))))
                   action-7))))
          (state-46
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-54 action)
                       action)
                   action))))
          (state-47
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-55 action)
                       action)
                   action))))
          (state-48
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-56 action)
                       action)
                   action))))
          (state-49
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-57 action)
                       action)
                   action))))
          (state-50
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 58)
                       (if (< c 48)
                           action-2
                           (state-50 action-2))
                       (if (= c 105)
                           (state-28 action-2)
                           action-2))
                   action-2))))
          (state-51
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       action
                       (if (< c 58)
                           (state-50 action)
                           action))
                   action))))
          (state-52
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 37)
                           (if (< c 34)
                               (if (< c 33)
                                   action-4
                                   (state-6 action-4))
                               (if (< c 36)
                                   action-4
                                   (state-6 action-4)))
                           (if (< c 39)
                               (if (< c 38)
                                   action-4
                                   (state-6 action-4))
                               (if (< c 45)
                                   action-4
                                   (if (< c 47)
                                       (state-6 action-4)
                                       action-4))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-4)
                                   action-4)
                               (if (< c 91)
                                   (state-6 action-4)
                                   (if (< c 95)
                                       action-4
                                       (state-6 action-4))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-4
                                   (state-6 action-4))
                               (if (= c 126)
                                   (state-6 action-4)
                                   action-4))))
                   action-4))))
          (state-53
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 37)
                           (if (< c 34)
                               (if (< c 33)
                                   action-3
                                   (state-6 action-3))
                               (if (< c 36)
                                   action-3
                                   (state-6 action-3)))
                           (if (< c 39)
                               (if (< c 38)
                                   action-3
                                   (state-6 action-3))
                               (if (< c 45)
                                   action-3
                                   (if (< c 47)
                                       (state-6 action-3)
                                       action-3))))
                       (if (< c 96)
                           (if (< c 60)
                               (if (< c 59)
                                   (state-6 action-3)
                                   action-3)
                               (if (< c 91)
                                   (state-6 action-3)
                                   (if (< c 95)
                                       action-3
                                       (state-6 action-3))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-3
                                   (state-6 action-3))
                               (if (= c 126)
                                   (state-6 action-3)
                                   action-3))))
                   action-3))))
          (state-54
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 48)
                       (state-58 action)
                       action)
                   action))))
          (state-55
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 48)
                       (state-59 action)
                       action)
                   action))))
          (state-56
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 48)
                       (state-60 action)
                       action)
                   action))))
          (state-57
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 48)
                       (state-59 action)
                       action)
                   action))))
          (state-58
           (lambda (action)
             (end-go-to-point)
             action-4))
          (state-59
           (lambda (action)
             (end-go-to-point)
             action-3))
          (state-60
           (lambda (action)
             (end-go-to-point)
             action-5))
          (start-automaton
           (lambda ()
             (if (peek-char)
                 (state-0 action-<<ERROR>>)
               action-<<EOF>>)))
          (final-lexer
           (lambda ()
             (init-lexeme)
             (let ((yyline (get-start-line))
                   (yycolumn (get-start-column))
                   (yyoffset (get-start-offset)))
               ((start-automaton) yyline yycolumn yyoffset)))))
       (set! user-action-<<EOF>> (<<EOF>>-pre-action
                                  final-lexer user-getc user-ungetc))
       (set! user-action-<<ERROR>> (<<ERROR>>-pre-action
                                    final-lexer user-getc user-ungetc))
       (set! user-action-0 ((vector-ref rules-pre-action 1)
                            final-lexer user-getc user-ungetc))
       (set! user-action-1 ((vector-ref rules-pre-action 3)
                            final-lexer user-getc user-ungetc))
       (set! user-action-2 ((vector-ref rules-pre-action 5)
                            final-lexer user-getc user-ungetc))
       (set! user-action-3 ((vector-ref rules-pre-action 7)
                            final-lexer user-getc user-ungetc))
       (set! user-action-4 ((vector-ref rules-pre-action 9)
                            final-lexer user-getc user-ungetc))
       (set! user-action-5 ((vector-ref rules-pre-action 11)
                            final-lexer user-getc user-ungetc))
       (set! user-action-6 ((vector-ref rules-pre-action 13)
                            final-lexer user-getc user-ungetc))
       (set! user-action-7 ((vector-ref rules-pre-action 15)
                            final-lexer user-getc user-ungetc))
       (set! user-action-8 ((vector-ref rules-pre-action 17)
                            final-lexer user-getc user-ungetc))
       (set! user-action-9 ((vector-ref rules-pre-action 19)
                            final-lexer user-getc user-ungetc))
       (set! user-action-10 ((vector-ref rules-pre-action 21)
                             final-lexer user-getc user-ungetc))
       (set! user-action-11 ((vector-ref rules-pre-action 23)
                             final-lexer user-getc user-ungetc))
       (set! user-action-12 ((vector-ref rules-pre-action 25)
                             final-lexer user-getc user-ungetc))
       final-lexer))))

) ; end of library

