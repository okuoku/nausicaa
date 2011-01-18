(library (nausicaa silex macro.l)
  (export
    macro-tables)
  (import (rnrs)(nausicaa silex lexer)
(nausicaa silex semantic)
)

;
; Table generated from the file macro.l by SILex 1.0
;

(define macro-tables
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-tok eof-tok		yytext yyline yycolumn)

;;; end of file
;; Local Variables:
;; mode: scheme
;; coding: utf-8-unix
;; End:
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (assertion-violation #f "invalid token")
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-tok hblank-tok		yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
        		(make-tok vblank-tok		yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                 	(parse-percent-include		yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                 	(make-tok percent-percent-tok	yytext yyline yycolumn)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
    			(parse-id			yytext yyline yycolumn)

;;everything else is invalid
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
   			(make-tok illegal-tok		yytext yyline yycolumn)
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
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-0 yytext yyline yycolumn yyoffset))))
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
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 48)
                       (if (< c 37)
                           (if (< c 32)
                               (if (< c 10)
                                   (if (< c 9)
                                       (state-1 action)
                                       (state-10 action))
                                   (if (< c 11)
                                       (state-8 action)
                                       (state-1 action)))
                               (if (< c 34)
                                   (if (< c 33)
                                       (state-10 action)
                                       (state-6 action))
                                   (if (< c 36)
                                       (state-1 action)
                                       (state-6 action))))
                           (if (< c 43)
                               (if (< c 39)
                                   (if (< c 38)
                                       (state-7 action)
                                       (state-6 action))
                                   (if (< c 42)
                                       (state-1 action)
                                       (state-6 action)))
                               (if (< c 45)
                                   (if (< c 44)
                                       (state-4 action)
                                       (state-1 action))
                                   (if (< c 46)
                                       (state-2 action)
                                       (if (< c 47)
                                           (state-3 action)
                                           (state-6 action))))))
                       (if (< c 93)
                           (if (< c 64)
                               (if (< c 59)
                                   (if (< c 58)
                                       (state-1 action)
                                       (state-6 action))
                                   (if (< c 60)
                                       (state-9 action)
                                       (state-6 action)))
                               (if (< c 91)
                                   (if (< c 65)
                                       (state-1 action)
                                       (state-6 action))
                                   (if (< c 92)
                                       (state-1 action)
                                       (state-5 action))))
                           (if (< c 123)
                               (if (< c 96)
                                   (if (< c 94)
                                       (state-1 action)
                                       (state-6 action))
                                   (if (< c 97)
                                       (state-1 action)
                                       (state-6 action)))
                               (if (< c 55296)
                                   (if (< c 126)
                                       (state-1 action)
                                       (state-6 action))
                                   (if (< c 57344)
                                       (state-1 action)
                                       (if (< c 1114112)
                                           (state-6 action)
                                           (state-1 action)))))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-5))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 62)
                       (state-11 action-4)
                       action-4)
                   action-4))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-12 action-5)
                       action-5)
                   action-5))))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-4))
          (state-5
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-13 action-5)
                       action-5)
                   action-5))))
          (state-6
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-4
                                   (state-15 action-4))
                               (if (< c 36)
                                   action-4
                                   (if (< c 39)
                                       (state-15 action-4)
                                       action-4)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-15 action-4)
                                   action-4)
                               (if (= c 59)
                                   action-4
                                   (state-15 action-4))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-4
                                   (state-14 action-4))
                               (if (< c 94)
                                   action-4
                                   (if (< c 96)
                                       (state-15 action-4)
                                       action-4)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-15 action-4)
                                   (if (< c 126)
                                       action-4
                                       (state-15 action-4)))
                               (if (< c 57344)
                                   action-4
                                   (if (< c 1114112)
                                       (state-15 action-4)
                                       action-4)))))
                   action-4))))
          (state-7
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-4
                                   (state-15 action-4))
                               (if (< c 36)
                                   action-4
                                   (if (< c 37)
                                       (state-15 action-4)
                                       (state-16 action-4))))
                           (if (< c 44)
                               (if (< c 39)
                                   (state-15 action-4)
                                   (if (< c 42)
                                       action-4
                                       (state-15 action-4)))
                               (if (< c 45)
                                   action-4
                                   (if (< c 59)
                                       (state-15 action-4)
                                       action-4))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 91)
                                   (state-15 action-4)
                                   (if (< c 92)
                                       (state-17 action-4)
                                       (state-14 action-4)))
                               (if (< c 94)
                                   action-4
                                   (if (< c 96)
                                       (state-15 action-4)
                                       action-4)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-15 action-4)
                                   (if (< c 126)
                                       action-4
                                       (state-15 action-4)))
                               (if (< c 57344)
                                   action-4
                                   (if (< c 1114112)
                                       (state-15 action-4)
                                       action-4)))))
                   action-4))))
          (state-8
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-9
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 10)
                       action-0
                       (state-18 action-0))
                   action-0))))
          (state-10
           (lambda (action)
             (end-go-to-point)
             action-0))
          (state-11
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-4
                                   (state-11 action-4))
                               (if (< c 36)
                                   action-4
                                   (if (< c 39)
                                       (state-11 action-4)
                                       action-4)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-11 action-4)
                                   action-4)
                               (if (= c 59)
                                   action-4
                                   (state-11 action-4))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-4
                                   (state-19 action-4))
                               (if (< c 94)
                                   action-4
                                   (if (< c 96)
                                       (state-11 action-4)
                                       action-4)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-11 action-4)
                                   (if (< c 126)
                                       action-4
                                       (state-11 action-4)))
                               (if (< c 57344)
                                   action-4
                                   (if (< c 1114112)
                                       (state-11 action-4)
                                       action-4)))))
                   action-4))))
          (state-12
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-20 action)
                       action)
                   action))))
          (state-13
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-21 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-21 action)
                               action)
                           (if (< c 103)
                               (state-21 action)
                               action)))
                   action))))
          (state-14
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-22 action)
                       action)
                   action))))
          (state-15
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-4
                                   (state-15 action-4))
                               (if (< c 36)
                                   action-4
                                   (if (< c 39)
                                       (state-15 action-4)
                                       action-4)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-15 action-4)
                                   action-4)
                               (if (= c 59)
                                   action-4
                                   (state-15 action-4))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-4
                                   (state-14 action-4))
                               (if (< c 94)
                                   action-4
                                   (if (< c 96)
                                       (state-15 action-4)
                                       action-4)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-15 action-4)
                                   (if (< c 126)
                                       action-4
                                       (state-15 action-4)))
                               (if (< c 57344)
                                   action-4
                                   (if (< c 1114112)
                                       (state-15 action-4)
                                       action-4)))))
                   action-4))))
          (state-16
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-3
                                   (state-15 action-3))
                               (if (< c 36)
                                   action-3
                                   (if (< c 39)
                                       (state-15 action-3)
                                       action-3)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-15 action-3)
                                   action-3)
                               (if (= c 59)
                                   action-3
                                   (state-15 action-3))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-3
                                   (state-14 action-3))
                               (if (< c 94)
                                   action-3
                                   (if (< c 96)
                                       (state-15 action-3)
                                       action-3)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-15 action-3)
                                   (if (< c 126)
                                       action-3
                                       (state-15 action-3)))
                               (if (< c 57344)
                                   action-3
                                   (if (< c 1114112)
                                       (state-15 action-3)
                                       action-3)))))
                   action-3))))
          (state-17
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 93)
                       action
                       (state-23 action))
                   action))))
          (state-18
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 10)
                       action-0
                       (state-18 action-0))
                   action-0))))
          (state-19
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-24 action)
                       action)
                   action))))
          (state-20
           (lambda (action)
             (end-go-to-point)
             action-4))
          (state-21
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-21 action))
                           (if (< c 59)
                               action
                               (state-15 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-21 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-21 action)
                                   action))))
                   action))))
          (state-22
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-25 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-25 action)
                               action)
                           (if (< c 103)
                               (state-25 action)
                               action)))
                   action))))
          (state-23
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 93)
                       (state-26 action)
                       (state-23 action))
                   action))))
          (state-24
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-27 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-27 action)
                               action)
                           (if (< c 103)
                               (state-27 action)
                               action)))
                   action))))
          (state-25
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-25 action))
                           (if (< c 59)
                               action
                               (state-15 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-25 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-25 action)
                                   action))))
                   action))))
          (state-26
           (lambda (action)
             (end-go-to-point)
             action-2))
          (state-27
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-27 action))
                           (if (< c 59)
                               action
                               (state-11 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-27 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-27 action)
                                   action))))
                   action))))
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
       final-lexer))))

) ; end of library

