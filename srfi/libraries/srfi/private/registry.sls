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

#!r6rs
(library (srfi private registry)
  (export
    available-features)
  (import 
    (rnrs)
    (srfi private implementation-features))
  
  (define srfi-features
    (map 
     (lambda (x)
       (list `(srfi ,(car x)) 
             (string->symbol (string-append "srfi-" (number->string (cadr x))))))
     ;  name     SRFI code number
     '([cond-expand             0]
       [lists                   1]
       [and-let*                2]
       [string-ports            6]
       [receive                 8]
       [records                 9]
       [let-values             11]
       [strings                13]
       [char-set               14]
       [case-lambda            16]
       [time                   19]
       [error-reporting        23]
       [cut                    26]
       [random                 27]
       [rec                    31]
       [args-fold              37]
       [sharing                38]
       [parameters             39]
       [streams                41]
       [eager-comprehensions   42]
       [vectors                43]
       [format                 48]
       [general-cond           61]
       [compare                67]
       [lightweight-testing    78])))
  
  (define available-features
    (apply append
           '(R6RS r6rs)
           implementation-features
	   OS-features
           srfi-features))
  
)
