;;;Copyright (c) 2008, 2009 Derick Eddington
;;;Modified by Marco Maggi <marcomaggi@gna.org>
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


(library (times-and-dates compat)
  (export
    host:time-resolution
    host:current-time
    host:time-nanosecond
    host:time-second
    host:time-gmt-offset)
  (import (rnrs)
    (primitives r5rs:require current-utc-time timezone-offset))

  (define dummy (r5rs:require 'time))

  (define-record-type time
    (fields secs usecs))

  ;; Larceny uses gettimeofday() which gives microseconds,
  ;; so our resolution is 1000 nanoseconds
  (define host:time-resolution 1000)

  (define (host:current-time)
    (let-values (((secs usecs) (current-utc-time)))
      (make-time secs usecs)))

  (define (host:time-nanosecond t)
    (* (time-usecs t) 1000))

  (define (host:time-second t)
    (time-secs t))

  (define (host:time-gmt-offset t)
    (timezone-offset (time-secs t))))

;;; end of file
