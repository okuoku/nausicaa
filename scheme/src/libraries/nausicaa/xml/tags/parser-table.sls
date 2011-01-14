(library (nausicaa xml tags parser-table)
  (export make-xml-parser)
  (import
    (rnrs)
    (nausicaa lalr lr-driver)
    (nausicaa parser-tools source-location)
    (nausicaa parser-tools lexical-token)
    (nausicaa language sentinel)
    (nausicaa xml tags datum-processing))
  (define (make-xml-parser)
    (lr-driver
      '#(((*default* . *error*) (OTAG . 1))
         ((*default* . -3))
         ((*default* . *error*) (OTAG . 4))
         ((*default* . *error*) (*eoi* . 6))
         ((*default* . -4))
         ((*default* . *error*) (OTAG . 7))
         ((*default* . -1) (*eoi* . accept))
         ((*default* . -6) (OTAG . 7)) ((*default* . -5))
         ((*default* . -2)) ((*default* . -7)))
      (vector '((2 . 2) (1 . 3)) '() '((3 . 5)) '() '()
        '((5 . 8) (4 . 9)) '() '((5 . 10)) '() '() '())
      (vector '()
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $3 $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 3 1
            ((document-datum-maker) yypushback yycustom $1
              $2 $3)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 2 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 3 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 4
            ((misc-datum-maker) yypushback yycustom $1)
            yy-stack-states yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $1 . yy-stack-values)
          (yy-reduce-pop-and-push 1 5 $1 yy-stack-states
            yy-stack-values))
        (lambda
          (yy-reduce-pop-and-push yypushback yycustom
           yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 5 (cons $1 $2)
            yy-stack-states yy-stack-values))))))
