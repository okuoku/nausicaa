;;;Copyright 2010 Derick Eddington.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

#!r6rs
(library (nausicaa times-and-dates compat)
  (export host:time-resolution
	  host:current-time
	  host:time-nanosecond
	  host:time-second
	  host:time-gmt-offset)
  (import (rnrs)
    (only (scheme)
	  current-inexact-milliseconds
	  date-time-zone-offset
	  seconds->date
	  current-seconds))

  (define host:time-resolution #e1e6)
  (define (millis->repr x)
    (let-values (((d m) (div-and-mod x 1000)))
      (cons d (* m #e1e6))))
  (define (host:current-time)
    (millis->repr (exact (floor (current-inexact-milliseconds)))))
  (define host:time-nanosecond cdr)
  (define host:time-second     car)
  (define (host:time-gmt-offset t)
    (date-time-zone-offset (seconds->date (current-seconds)))))

;;; end of file
