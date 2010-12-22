(library (r6rs lexer-table)
  (export
    r6rs-lexer-table)
  (import (rnrs) (silex lexer)(silex default-error-handler)(parser-tools lexical-token)(parser-tools source-location))

;
; Table generated from the file lexer-table.l by SILex 1.0
;

(define r6rs-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		(silex-default-error-handler)

;;; end of file
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
     			;ignore it
        (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
    			;ignore it

        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
      			(make-<lexical-token>
			 'ATOM
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
           		(let ((ch (string-ref yytext 0)))
			  (make-<lexical-token>
			   (case ch
			     ((#\@) 'AT)
			     ((#\.) 'DOT)
			     ((#\,) 'COMMA)
			     ((#\:) 'COLON)
			     ((#\;) 'SEMICOLON)
			     ((#\<) 'ANGLE-OPEN)
			     ((#\>) 'ANGLE-CLOSE))
			   (make-<source-location> #f yyline yycolumn yyoffset)
			   ch
			   1))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
               		(make-<lexical-token>
			 'COMMENT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   	(make-<lexical-token>
			 'QUOTED-TEXT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                      	(make-<lexical-token>
			 'DOMAIN-LITERAL-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext
			 1)
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
             (start-go-to-end)
             (user-action-1 yyline yycolumn yyoffset)))
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
          (state-0
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 45)
                       (if (< c 33)
                           (if (< c 13)
                               (if (= c 9)
                                   (state-6 action)
                                   action)
                               (if (< c 14)
                                   (state-7 action)
                                   (if (< c 32)
                                       action
                                       (state-6 action))))
                           (if (< c 40)
                               (if (= c 34)
                                   (state-2 action)
                                   (state-5 action))
                               (if (< c 42)
                                   (if (< c 41)
                                       (state-3 action)
                                       action)
                                   (if (< c 44)
                                       (state-5 action)
                                       (state-4 action)))))
                       (if (< c 63)
                           (if (< c 58)
                               (if (= c 46)
                                   (state-4 action)
                                   (state-5 action))
                               (if (= c 61)
                                   (state-5 action)
                                   (state-4 action)))
                           (if (< c 91)
                               (if (= c 64)
                                   (state-4 action)
                                   (state-5 action))
                               (if (< c 94)
                                   (if (< c 92)
                                       (state-1 action)
                                       action)
                                   (if (< c 127)
                                       (state-5 action)
                                       action)))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             action-6))
          (state-2
           (lambda (action)
             (end-go-to-point)
             action-5))
          (state-3
           (lambda (action)
             (end-go-to-point)
             action-4))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-3))
          (state-5
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 47)
                       (if (< c 40)
                           (if (< c 34)
                               (if (< c 33)
                                   action-2
                                   (state-5 action-2))
                               (if (< c 35)
                                   action-2
                                   (state-5 action-2)))
                           (if (< c 44)
                               (if (< c 42)
                                   action-2
                                   (state-5 action-2))
                               (if (= c 45)
                                   (state-5 action-2)
                                   action-2)))
                       (if (< c 64)
                           (if (< c 61)
                               (if (< c 58)
                                   (state-5 action-2)
                                   action-2)
                               (if (= c 62)
                                   action-2
                                   (state-5 action-2)))
                           (if (< c 91)
                               (if (< c 65)
                                   action-2
                                   (state-5 action-2))
                               (if (< c 94)
                                   action-2
                                   (if (< c 127)
                                       (state-5 action-2)
                                       action-2)))))
                   action-2))))
          (state-6
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 10)
                       (if (< c 9)
                           action-1
                           (state-6 action-1))
                       (if (= c 32)
                           (state-6 action-1)
                           action-1))
                   action-1))))
          (state-7
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (= c 10)
                       (state-8 action)
                       action)
                   action))))
          (state-8
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 10)
                       (if (< c 9)
                           action
                           (state-9 action))
                       (if (= c 32)
                           (state-9 action)
                           action))
                   action))))
          (state-9
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 10)
                       (if (< c 9)
                           action-0
                           (state-9 action-0))
                       (if (= c 32)
                           (state-9 action-0)
                           action-0))
                   action-0))))
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
       final-lexer))))

) ; end of library

