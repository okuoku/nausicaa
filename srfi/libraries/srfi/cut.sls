;;;Reference implementation for SRFI-26 "cut"
;;;
;;;Copyright (c) 2002 Sebastian.Egner@philips.com, 5-Jun-2002.
;;;Copyright (c) 2002 Al Petrofsky <al@petrofsky.org>
;;;Copyright (c) 2008 Derick Eddington
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
(library (srfi cut)
  (export cut cute)
  (import (rnrs))

  (define-syntax srfi-26-internal-cut
    (syntax-rules (<> <...>)
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
       (lambda (slot-name ...) ((begin proc) arg ...)))
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
       (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
       (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
       (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

  (define-syntax srfi-26-internal-cute
    (syntax-rules (<> <...>)
      ((srfi-26-internal-cute
	(slot-name ...) nse-bindings (proc arg ...))
       (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
      ((srfi-26-internal-cute
	(slot-name ...) nse-bindings (proc arg ...) <...>)
       (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))
      ((srfi-26-internal-cute
	(slot-name ...)         nse-bindings  (position ...)   <>  . se)
       (srfi-26-internal-cute
	(slot-name ... x)       nse-bindings  (position ... x)     . se))
      ((srfi-26-internal-cute
	slot-names              nse-bindings  (position ...)   nse . se)
       (srfi-26-internal-cute
	slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

  (define-syntax cut
    (syntax-rules ()
      ((cut . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs))))

  (define-syntax cute
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs)))))

;;; end of file
