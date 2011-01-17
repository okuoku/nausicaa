(library (nausicaa silex macro.l)
  (export
    macro-tables)
  (import (rnrs)(nausicaa silex lexer)(nausicaa silex semantic))

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
       (assertion-violation #f "invalid token")
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
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 46)
                       (if (< c 36)
                           (if (< c 11)
                               (if (< c 9)
                                   (state-1 action)
                                   (if (< c 10)
                                       (state-8 action)
                                       (state-6 action)))
                               (if (< c 33)
                                   (if (< c 32)
                                       (state-1 action)
                                       (state-8 action))
                                   (if (< c 34)
                                       (state-4 action)
                                       (state-1 action))))
                           (if (< c 42)
                               (if (< c 38)
                                   (if (< c 37)
                                       (state-4 action)
                                       (state-5 action))
                                   (if (< c 39)
                                       (state-4 action)
                                       (state-1 action)))
                               (if (< c 44)
                                   (if (< c 43)
                                       (state-4 action)
                                       (state-3 action))
                                   (if (< c 45)
                                       (state-1 action)
                                       (state-3 action)))))
                       (if (< c 65)
                           (if (< c 58)
                               (if (< c 47)
                                   (state-2 action)
                                   (if (< c 48)
                                       (state-4 action)
                                       (state-1 action)))
                               (if (< c 60)
                                   (if (< c 59)
                                       (state-4 action)
                                       (state-7 action))
                                   (if (< c 64)
                                       (state-4 action)
                                       (state-1 action))))
                           (if (< c 97)
                               (if (< c 94)
                                   (if (< c 91)
                                       (state-4 action)
                                       (state-1 action))
                                   (if (< c 96)
                                       (state-4 action)
                                       (state-1 action)))
                               (if (< c 126)
                                   (if (< c 123)
                                       (state-4 action)
                                       (state-1 action))
                                   (if (< c 127)
                                       (state-4 action)
                                       (state-1 action))))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-4))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-9 action-4)
                       action-4)
                   action-4))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             action-3))
          (state-4
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 39)
                           (if (< c 34)
                               (if (< c 33)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 36)
                                   action-3
                                   (state-10 action-3)))
                           (if (< c 44)
                               (if (< c 42)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 45)
                                   action-3
                                   (if (< c 59)
                                       (state-10 action-3)
                                       action-3))))
                       (if (< c 96)
                           (if (< c 65)
                               (if (< c 64)
                                   (state-10 action-3)
                                   action-3)
                               (if (< c 91)
                                   (state-10 action-3)
                                   (if (< c 94)
                                       action-3
                                       (state-10 action-3))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-3
                                   (state-10 action-3))
                               (if (= c 126)
                                   (state-10 action-3)
                                   action-3))))
                   action-3))))
          (state-5
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 59)
                       (if (< c 38)
                           (if (< c 34)
                               (if (< c 33)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 36)
                                   action-3
                                   (if (< c 37)
                                       (state-10 action-3)
                                       (state-11 action-3))))
                           (if (< c 42)
                               (if (< c 39)
                                   (state-10 action-3)
                                   action-3)
                               (if (= c 44)
                                   action-3
                                   (state-10 action-3))))
                       (if (< c 94)
                           (if (< c 64)
                               (if (< c 60)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 65)
                                   action-3
                                   (if (< c 91)
                                       (state-10 action-3)
                                       action-3)))
                           (if (< c 123)
                               (if (= c 96)
                                   action-3
                                   (state-10 action-3))
                               (if (= c 126)
                                   (state-10 action-3)
                                   action-3))))
                   action-3))))
          (state-6
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-7
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 10)
                       action-0
                       (state-12 action-0))
                   action-0))))
          (state-8
           (lambda (action)
             (end-go-to-point)
             action-0))
          (state-9
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-13 action)
                       action)
                   action))))
          (state-10
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 39)
                           (if (< c 34)
                               (if (< c 33)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 36)
                                   action-3
                                   (state-10 action-3)))
                           (if (< c 44)
                               (if (< c 42)
                                   action-3
                                   (state-10 action-3))
                               (if (< c 45)
                                   action-3
                                   (if (< c 59)
                                       (state-10 action-3)
                                       action-3))))
                       (if (< c 96)
                           (if (< c 65)
                               (if (< c 64)
                                   (state-10 action-3)
                                   action-3)
                               (if (< c 91)
                                   (state-10 action-3)
                                   (if (< c 94)
                                       action-3
                                       (state-10 action-3))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-3
                                   (state-10 action-3))
                               (if (= c 126)
                                   (state-10 action-3)
                                   action-3))))
                   action-3))))
          (state-11
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 39)
                           (if (< c 34)
                               (if (< c 33)
                                   action-2
                                   (state-10 action-2))
                               (if (< c 36)
                                   action-2
                                   (state-10 action-2)))
                           (if (< c 44)
                               (if (< c 42)
                                   action-2
                                   (state-10 action-2))
                               (if (< c 45)
                                   action-2
                                   (if (< c 59)
                                       (state-10 action-2)
                                       action-2))))
                       (if (< c 96)
                           (if (< c 65)
                               (if (< c 64)
                                   (state-10 action-2)
                                   action-2)
                               (if (< c 91)
                                   (state-10 action-2)
                                   (if (< c 94)
                                       action-2
                                       (state-10 action-2))))
                           (if (< c 123)
                               (if (< c 97)
                                   action-2
                                   (state-10 action-2))
                               (if (= c 126)
                                   (state-10 action-2)
                                   action-2))))
                   action-2))))
          (state-12
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 10)
                       action-0
                       (state-12 action-0))
                   action-0))))
          (state-13
           (lambda (action)
             (end-go-to-point)
             action-3))
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
       final-lexer))))

) ; end of library

