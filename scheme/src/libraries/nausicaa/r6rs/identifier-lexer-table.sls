(library (nausicaa r6rs identifier-lexer-table)
  (export
    r6rs-identifier-lexer-table)
  (import (rnrs) (nausicaa silex lexer)(nausicaa silex default-error-handler)(nausicaa parser-tools lexical-token)(nausicaa parser-tools source-location))

;
; Table generated from the file identifier-lexer-table.l by SILex 1.0
;

(define r6rs-identifier-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(silex-default-eof-handler)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(silex-default-error-handler)

;;; end of file
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
            		(string->symbol yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                     	(string->symbol yytext)
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
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 44)
                           (if (< c 36)
                               (if (= c 33)
                                   (state-5 action)
                                   action)
                               (if (< c 42)
                                   (if (< c 39)
                                       (state-5 action)
                                       action)
                                   (if (< c 43)
                                       (state-5 action)
                                       (state-3 action))))
                           (if (< c 47)
                               (if (< c 45)
                                   action
                                   (if (< c 46)
                                       (state-1 action)
                                       (state-2 action)))
                               (if (< c 58)
                                   (if (< c 48)
                                       (state-5 action)
                                       action)
                                   (if (< c 59)
                                       (state-5 action)
                                       action))))
                       (if (< c 96)
                           (if (< c 91)
                               (if (= c 64)
                                   action
                                   (state-5 action))
                               (if (< c 93)
                                   (if (< c 92)
                                       action
                                       (state-4 action))
                                   (if (< c 94)
                                       action
                                       (state-5 action))))
                           (if (< c 126)
                               (if (< c 97)
                                   action
                                   (if (< c 123)
                                       (state-5 action)
                                       action))
                               (if (< c 57344)
                                   (if (< c 55296)
                                       (state-5 action)
                                       action)
                                   (if (< c 1114112)
                                       (state-5 action)
                                       action)))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (= c 62)
                       (state-6 action-1)
                       action-1)
                   action-1))))
          (state-2
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-7 action)
                       action)
                   action))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-4
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-8 action)
                       action)
                   action))))
          (state-5
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-0
                                   (state-5 action-0))
                               (if (< c 36)
                                   action-0
                                   (if (< c 39)
                                       (state-5 action-0)
                                       action-0)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-5 action-0)
                                   action-0)
                               (if (= c 59)
                                   action-0
                                   (state-5 action-0))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-0
                                   (state-9 action-0))
                               (if (< c 94)
                                   action-0
                                   (if (< c 96)
                                       (state-5 action-0)
                                       action-0)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-5 action-0)
                                   (if (< c 126)
                                       action-0
                                       (state-5 action-0)))
                               (if (< c 57344)
                                   action-0
                                   (if (< c 1114112)
                                       (state-5 action-0)
                                       action-0)))))
                   action-0))))
          (state-6
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 91)
                       (if (< c 42)
                           (if (< c 34)
                               (if (< c 33)
                                   action-1
                                   (state-6 action-1))
                               (if (< c 36)
                                   action-1
                                   (if (< c 39)
                                       (state-6 action-1)
                                       action-1)))
                           (if (< c 45)
                               (if (< c 44)
                                   (state-6 action-1)
                                   action-1)
                               (if (= c 59)
                                   action-1
                                   (state-6 action-1))))
                       (if (< c 97)
                           (if (< c 93)
                               (if (< c 92)
                                   action-1
                                   (state-10 action-1))
                               (if (< c 94)
                                   action-1
                                   (if (< c 96)
                                       (state-6 action-1)
                                       action-1)))
                           (if (< c 55296)
                               (if (< c 123)
                                   (state-6 action-1)
                                   (if (< c 126)
                                       action-1
                                       (state-6 action-1)))
                               (if (< c 57344)
                                   action-1
                                   (if (< c 1114112)
                                       (state-6 action-1)
                                       action-1)))))
                   action-1))))
          (state-7
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 46)
                       (state-3 action)
                       action)
                   action))))
          (state-8
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-11 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-11 action)
                               action)
                           (if (< c 103)
                               (state-11 action)
                               action)))
                   action))))
          (state-9
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-12 action)
                       action)
                   action))))
          (state-10
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 120)
                       (state-13 action)
                       action)
                   action))))
          (state-11
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-11 action))
                           (if (< c 59)
                               action
                               (state-5 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-11 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-11 action)
                                   action))))
                   action))))
          (state-12
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-14 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-14 action)
                               action)
                           (if (< c 103)
                               (state-14 action)
                               action)))
                   action))))
          (state-13
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-15 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-15 action)
                               action)
                           (if (< c 103)
                               (state-15 action)
                               action)))
                   action))))
          (state-14
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-14 action))
                           (if (< c 59)
                               action
                               (state-5 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-14 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-14 action)
                                   action))))
                   action))))
          (state-15
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-15 action))
                           (if (< c 59)
                               action
                               (state-6 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-15 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-15 action)
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
       final-lexer))))

) ; end of library

