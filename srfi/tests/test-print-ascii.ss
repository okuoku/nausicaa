;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is  hereby granted, free of charge,  to any person
;;; obtaining   a   copy   of   this  software   and   associated
;;; documentation files (the "Software"), to deal in the Software
;;; without restriction, including  without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the  Software, and to permit persons to
;;; whom  the Software  is furnished  to  do so,  subject to  the
;;; following conditions:
;;;
;;; The above  copyright notice and this  permission notice shall
;;; be  included in  all copies  or substantial  portions  of the
;;; Software.
;;;
;;; Except as contained in this  notice, the name(s) of the above
;;; copyright  holders  shall  not  be  used  in  advertising  or
;;; otherwise to promote the sale,  use or other dealings in this
;;; Software without prior written authorization.
;;;
;;; THE  SOFTWARE IS PROVIDED  "AS IS",  WITHOUT WARRANTY  OF ANY
;;; KIND, EXPRESS  OR IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE
;;; WARRANTIES  OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR
;;; PURPOSE AND  NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS
;;; OR  COPYRIGHT HOLDERS  BE LIABLE  FOR ANY  CLAIM,  DAMAGES OR
;;; OTHER LIABILITY,  WHETHER IN AN  ACTION OF CONTRACT,  TORT OR
;;; OTHERWISE,  ARISING FROM, OUT  OF OR  IN CONNECTION  WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Test format import of implementation
;;; specific routine: pretty-print

#|
LARCENY USAGE:
==> larceny -r6rs -program print-ascii.ss

IKARUS USAGE
==> ikarus --r6rs-script print-ascii.ss

|#

(import (rnrs (6))
        (srfi format))

(define pa
 '(define (print-ascii-chart . radix+port)
  (let ( (radix (if (null? radix+port) 16 (car radix+port)))
         (port  (if (or (null? radix+port) (null? (cdr radix+port)))
                  (current-output-port)
                  (cadr radix+port)))
         (max-row    15)
         (max-col     7)
         (max-ascii 127)
         (max-control 31)  ; [0..31] are control codes
       )

    (define (printable? N) ; N.B.: integer input
      (< max-control N max-ascii)) ; control or DEL

    (define (print-a-char N)
      (if (printable? N)
        (begin
          (display #\'               port)
          (display (integer->char N) port)
          (display #\'               port)
          )
        (cond ; print a control character
         ((= N max-ascii) (display "DEL" port))
         (else
          (display #\^   port)
          (display (integer->char (+ (char->integer #\@) N)) port)
          ) )     )
      (display " = "                    port)
      (display (number->string N radix) port)
      (display #\space                  port)
      (display #\space                  port)
      (display #\space                  port)
      )

    ; output the chart...
    (newline port)
    (let row-loop ( (row 0) )
      (if (> row max-row)
        (newline port)  ; done
        (let column-loop ( (col 0) )
          (print-a-char (+ row (* col (+ max-row 1))))
          (if (< col max-col)
            (column-loop (+ col 1))
            (begin
              (newline  port)
              (row-loop (+ row 1))
              )   )
          )   )
      )) )
)

(format #t "~Y~%" pa)

;;; end of file
