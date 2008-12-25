;;;Reference implementation for SRFI-26 "cut"
;;;
;;;Sebastian.Egner@philips.com, 5-Jun-2002.
;;;adapted from the posting by Al Petrofsky <al@petrofsky.org>
;;;placed in the public domain
;;;
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


;;;The code to handle the variable argument case was originally proposed
;;;by Michael  Sperber and  has been  adapted to the  new syntax  of the
;;;macro using  an explicit rest-slot  symbol. The code to  evaluate the
;;;non-slots  for cute has  been proposed  by Dale  Jordan. The  code to
;;;allow  a slot for  the procedure  position and  to process  the macro
;;;using an internal macro is based on a suggestion by Al Petrofsky. The
;;;code found below  is, with exception of this  header and some changes
;;;in variable names, entirely written by Al Petrofsky.
;;;
;;;compliance: Scheme R5RS (including macros).
;;;
;;;Loading this file into Scheme 48 0.57: ,load cut.scm
;;;
;;;History of this file:
;;;   SE,  6-Feb-2002: initial version as 'curry' with ". <>" notation
;;;   SE, 14-Feb-2002: revised for <...>
;;;   SE, 27-Feb-2002: revised for 'cut'
;;;   SE, 03-Jun-2002: revised for proc-slot, cute
;;;   SE, 04-Jun-2002: rewritten with internal transformer (no "loop" pattern)
;;;   SE, 05-Jun-2002: replace my code by Al's; substituted "constant" etc.
;;;     to match the convention in the SRFI-document
;;;
;;; (srfi-26-internal-cut slot-names combination . se)
;;;   transformer used internally
;;;     slot-names  : the internal names of the slots
;;;     combination : procedure being specialized, followed by its arguments
;;;     se          : slots-or-exprs, the qualifiers of the macro



#!r6rs
(library (srfi cut)
  (export cut cute)
  (import (rnrs))

  (define-syntax srfi-26-internal-cut
    (syntax-rules (<> <...>)

      ;; construct fixed- or variable-arity procedure:
      ;;   (begin proc) throws an error if proc is not an <expression>
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
       (lambda (slot-name ...) ((begin proc) arg ...)))
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
       (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))

      ;; process one slot-or-expr
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
       (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
       (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

;;; (srfi-26-internal-cute slot-names nse-bindings combination . se)
;;;   transformer used internally
;;;     slot-names     : the internal names of the slots
;;;     nse-bindings   : let-style bindings for the non-slot expressions.
;;;     combination    : procedure being specialized, followed by its arguments
;;;     se             : slots-or-exprs, the qualifiers of the macro

  (define-syntax srfi-26-internal-cute
    (syntax-rules (<> <...>)

      ;; If there are no slot-or-exprs to process, then:
      ;; construct a fixed-arity procedure,
      ((srfi-26-internal-cute
	(slot-name ...) nse-bindings (proc arg ...))
       (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
      ;; or a variable-arity procedure
      ((srfi-26-internal-cute
	(slot-name ...) nse-bindings (proc arg ...) <...>)
       (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))

      ;; otherwise, process one slot:
      ((srfi-26-internal-cute
	(slot-name ...)         nse-bindings  (position ...)   <>  . se)
       (srfi-26-internal-cute
	(slot-name ... x)       nse-bindings  (position ... x)     . se))
      ;; or one non-slot expression
      ((srfi-26-internal-cute
	slot-names              nse-bindings  (position ...)   nse . se)
       (srfi-26-internal-cute
	slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

;;; exported syntax

  (define-syntax cut
    (syntax-rules ()
      ((cut . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs))))

  (define-syntax cute
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs)))))

;;; end of file
