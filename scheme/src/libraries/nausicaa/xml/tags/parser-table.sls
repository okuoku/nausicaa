(library (nausicaa xml tags parser-table)
  (export make-xml-parser)
  (import (rnrs) (nausicaa lalr lr-driver)
    (nausicaa parser-tools source-location)
    (nausicaa parser-tools lexical-token)
    (nausicaa language sentinel)
    (nausicaa xmltags datum-processing))
  (define (make-xml-parser)
    (lr-driver
      '#(((*default* . *error*) (OTAG . 2))
         ((*default* . *error*) (*eoi* . 3))
         ((*default* . *error*) (CTAG . 4))
         ((*default* . -1) (*eoi* . accept)) ((*default* . -2)))
      (vector '((1 . 1)) '() '() '() '())
      (vector
        '()
        (lambda (yy-reduce-pop-and-push yypushback yycustom
                 yy-stack-states $2 $1 . yy-stack-values)
          $1)
        (lambda (yy-reduce-pop-and-push yypushback yycustom
                 yy-stack-states $2 $1 . yy-stack-values)
          (yy-reduce-pop-and-push 2 1
            ((tag-token-maker) yypushback yycustom $1) yy-stack-states
            yy-stack-values))))))
