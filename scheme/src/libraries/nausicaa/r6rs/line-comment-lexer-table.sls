(library (nausicaa r6rs line-comment-lexer-table)
  (export
    r6rs-line-comment-lexer-table)
  (import (rnrs) (nausicaa silex lexer)(nausicaa r6rs lexeme-processing)(nausicaa parser-tools lexical-token)(nausicaa parser-tools source-location))

;
; Table generated from the file line-comment-lexer-table.l by SILex 1.0
;

(define r6rs-line-comment-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
         		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
             		yytext
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   	(let ((ch (dynamic-wind
				      (lambda () #f)
				      (lambda ()
					(yygetc))
				      (lambda () (yyungetc)))))
			  (if (eof-object? ch)
			      yytext
			    ((lexical-error-token-maker)
			     yygetc yyungetc yytext yyline yycolumn yyoffset)))
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
                   (if (= c 59)
                       (state-1 action)
                       action)
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 14)
                       (if (< c 11)
                           (if (< c 10)
                               (state-1 action-1)
                               (state-4 action-1))
                           (if (< c 13)
                               (state-1 action-1)
                               (state-2 action-1)))
                       (if (< c 134)
                           (if (< c 133)
                               (state-1 action-1)
                               (state-3 action-1))
                           (if (< c 8232)
                               (state-1 action-1)
                               (if (< c 8234)
                                   (state-3 action-1)
                                   (state-1 action-1)))))
                   action-1))))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 14)
                       (if (< c 11)
                           (if (< c 10)
                               (state-5 action-0)
                               (state-4 action-0))
                           (if (< c 13)
                               (state-5 action-0)
                               (state-2 action-0)))
                       (if (< c 134)
                           (if (< c 133)
                               (state-5 action-0)
                               (state-3 action-0))
                           (if (< c 8232)
                               (state-5 action-0)
                               (if (< c 8234)
                                   (state-3 action-0)
                                   (state-5 action-0)))))
                   action-0))))
          (state-3
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 14)
                       (if (< c 11)
                           (if (< c 10)
                               (state-5 action-0)
                               (state-4 action-0))
                           (if (< c 13)
                               (state-5 action-0)
                               (state-2 action-0)))
                       (if (< c 134)
                           (if (< c 133)
                               (state-5 action-0)
                               (state-3 action-0))
                           (if (< c 8232)
                               (state-5 action-0)
                               (if (< c 8234)
                                   (state-3 action-0)
                                   (state-5 action-0)))))
                   action-0))))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-0))
          (state-5
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 14)
                       (if (< c 11)
                           (if (< c 10)
                               (state-5 action)
                               (state-4 action))
                           (if (< c 13)
                               (state-5 action)
                               (state-2 action)))
                       (if (< c 134)
                           (if (< c 133)
                               (state-5 action)
                               (state-3 action))
                           (if (< c 8232)
                               (state-5 action)
                               (if (< c 8234)
                                   (state-3 action)
                                   (state-5 action)))))
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

