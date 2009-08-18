(library (email addresse-parser)
  (export make-email-address-parser
          make-source-location
          source-location?
          source-location-line
          source-location-input
          source-location-column
          source-location-offset
          source-location-length
          make-lexical-token
          lexical-token?
          lexical-token-value
          lexical-token-category
          lexical-token-source
          lexical-token?/end-of-input)
  (import (rnrs) (lalr lr-driver) (lalr common) (sentinel))
  (define (make-email-address-parser)
    (lr-driver
      '#(((*default* . -4) (ATOM . 1))
         ((*default* . -4) (ATOM . 1))
         ((*default* . -2))
         ((*default* . *error*) (*eoi* . 5))
         ((*default* . -3))
         ((*default* . -1) (*eoi* . accept)))
      (vector '((2 . 2) (1 . 3)) '((2 . 4)) '() '() '() '())
      (vector
        '()
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 1 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 2 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 2 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 4 (make-domain (cons $1 $2)) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 5 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 5 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $9
                  $8
                  $7
                  $6
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push
            9
            6
            (make-domain-literal $2 $4 $6 $8)
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push
            2
            7
            (make-local-part (cons $1 $2))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 8 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 8 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 9 (make-addr-spec $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 10 (make-route $1) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 11 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 11 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 5 12 (make-route-address $1 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 12 (make-route-address #f $1) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 13 (make-mailbox $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 13 (make-mailbox "" $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $5
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push
            5
            14
            (make-group $1 (cons $3 $4))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $4
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push
            4
            14
            (make-group "" (cons $2 $3))
            yy-stack-states
            yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 15 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 15 '() yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 16 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 16 (cons $1 $2) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 17 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push
                  yypushback
                  yycustom
                  yy-stack-states
                  $3
                  $2
                  $1
                  .
                  yy-stack-values)
          (yy-reduce-pop-and-push 3 17 (cons $2 $3) yy-stack-states yy-stack-values))
        (lambda (yy-reduce-pop-and-push yypushback yycustom yy-stack-states . yy-stack-values)
          (yy-reduce-pop-and-push 0 17 '() yy-stack-states yy-stack-values))))))