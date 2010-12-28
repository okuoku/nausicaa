(library (nausicaa r6rs string-lexer-table)
  (export
    r6rs-string-lexer-table)
  (import (rnrs) (nausicaa silex lexer)(nausicaa silex default-error-handler)(nausicaa parser-tools lexical-token)(nausicaa parser-tools source-location))

;
; Table generated from the file string-lexer-table.l by SILex 1.0
;

(define r6rs-string-lexer-table
  (vector
   'all
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (eof-object)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline yycolumn yyoffset)
       (assertion-violation #f "invalid token")
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
             			(make-<lexical-token> 'DOUBLEQUOTE
						      (make-<source-location> #f yyline yycolumn yyoffset)
						      #\" 1)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                        	(make-<lexical-token> 'ESCAPED-STRING-ELEMENT
						      (make-<source-location> #f yyline yycolumn yyoffset)
						      (case yytext
                                                        (("\\a")	#x7)
                                                        (("\\b")	#x8)
                                                        (("\\t")	#x9)
                                                        (("\\n")	#xA)
                                                        (("\\v")	#xB)
                                                        (("\\f")	#xC)
                                                        (("\\r")	#xD)
                                                        (("\\\"")	#x22)
                                                        (("\\\\")	#x5C))
                                                      (string-length yytext))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline yycolumn yyoffset)
                      		;;ignored

        (yycontinue)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                   		(make-<lexical-token> 'INLINE_HEX_ESCAPE
						      (make-<source-location> #f yyline yycolumn yyoffset)
						      yytext (string-length yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline yycolumn yyoffset)
                		(make-<lexical-token> 'STRING_ELEMENT
						      (make-<source-location> #f yyline yycolumn yyoffset)
						      yytext (string-length yytext))

;;; end of file
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
             (start-go-to-end)
             (user-action-0 yyline yycolumn yyoffset)))
          (action-1
           (lambda (yyline yycolumn yyoffset)
             (let ((yytext (get-start-end-text)))
               (start-go-to-end)
               (user-action-1 yytext yyline yycolumn yyoffset))))
          (action-2
           (lambda (yyline yycolumn yyoffset)
             (start-go-to-end)
             (user-action-2 yyline yycolumn yyoffset)))
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
                   (if (< c 5760)
                       (if (< c 34)
                           (if (< c 10)
                               (if (< c 9)
                                   (state-1 action)
                                   (state-2 action))
                               (if (= c 32)
                                   (state-2 action)
                                   (state-1 action)))
                           (if (< c 93)
                               (if (< c 35)
                                   (state-4 action)
                                   (if (< c 92)
                                       (state-1 action)
                                       (state-3 action)))
                               (if (= c 160)
                                   (state-2 action)
                                   (state-1 action))))
                       (if (< c 8239)
                           (if (< c 6159)
                               (if (< c 5761)
                                   (state-2 action)
                                   (if (< c 6158)
                                       (state-1 action)
                                       (state-2 action)))
                               (if (< c 8192)
                                   (state-1 action)
                                   (if (< c 8203)
                                       (state-2 action)
                                       (state-1 action))))
                           (if (< c 8288)
                               (if (< c 8240)
                                   (state-2 action)
                                   (if (< c 8287)
                                       (state-1 action)
                                       (state-2 action)))
                               (if (= c 12288)
                                   (state-2 action)
                                   (state-1 action)))))
                   action))))
          (state-1
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 35)
                       (if (< c 34)
                           (state-1 action-4)
                           action-4)
                       (if (= c 92)
                           action-4
                           (state-1 action-4)))
                   action-4))))
          (state-2
           (lambda (action)
             (end-go-to-point)
             (let ((c (read-char)))
               (if c
                   (if (< c 35)
                       (if (< c 34)
                           (state-1 action-2)
                           action-2)
                       (if (= c 92)
                           action-2
                           (state-1 action-2)))
                   action-2))))
          (state-3
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 110)
                       (if (< c 93)
                           (if (< c 35)
                               (if (< c 34)
                                   action
                                   (state-6 action))
                               (if (< c 92)
                                   action
                                   (state-6 action)))
                           (if (< c 99)
                               (if (< c 97)
                                   action
                                   (state-6 action))
                               (if (= c 102)
                                   (state-6 action)
                                   action)))
                       (if (< c 117)
                           (if (< c 114)
                               (if (< c 111)
                                   (state-6 action)
                                   action)
                               (if (= c 115)
                                   action
                                   (state-6 action)))
                           (if (< c 119)
                               (if (< c 118)
                                   action
                                   (state-6 action))
                               (if (= c 120)
                                   (state-5 action)
                                   action))))
                   action))))
          (state-4
           (lambda (action)
             (end-go-to-point)
             action-0))
          (state-5
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 65)
                       (if (< c 48)
                           action
                           (if (< c 58)
                               (state-7 action)
                               action))
                       (if (< c 97)
                           (if (< c 71)
                               (state-7 action)
                               action)
                           (if (< c 103)
                               (state-7 action)
                               action)))
                   action))))
          (state-6
           (lambda (action)
             (end-go-to-point)
             action-1))
          (state-7
           (lambda (action)
             (let ((c (read-char)))
               (if c
                   (if (< c 60)
                       (if (< c 58)
                           (if (< c 48)
                               action
                               (state-7 action))
                           (if (< c 59)
                               action
                               (state-8 action)))
                       (if (< c 71)
                           (if (< c 65)
                               action
                               (state-7 action))
                           (if (< c 97)
                               action
                               (if (< c 103)
                                   (state-7 action)
                                   action))))
                   action))))
          (state-8
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

