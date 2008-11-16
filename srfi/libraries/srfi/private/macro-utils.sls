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
(library (srfi private macro-utils)
  (export
    duplicate-id
    unique-ids?
    unique-ids?/raise
    formals-ok?)
  (import
    (rnrs))
  
  (define (duplicate-id ls)
    (if (null? ls)
      #f
      (or
        (let loop ([x (car ls)] [rest (cdr ls)])
          (if (null? rest)
            #f
            (if (bound-identifier=? x (car rest))
              x
              (loop x (cdr rest)))))
        (duplicate-id (cdr ls)))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define unique-ids?/raise
    (case-lambda
      [(ids stx msg)
       (let ([dup (duplicate-id ids)])
         (if dup
           (syntax-violation #f msg stx dup)
           #t))]
      [(ids stx)
       (unique-ids?/raise ids stx "duplicate identifier")]))
  
  (define (formals-ok? frmls-stx orig-stx)
    (syntax-case frmls-stx ()
      [(arg* ... . rest)
       (and (or (null? (syntax->datum #'rest))
                (identifier? #'rest)
                (syntax-violation #f "not an identifier" orig-stx #'rest))
            (for-all (lambda (id)
                       (or (identifier? id)
                           (syntax-violation #f "not an identifier" orig-stx id)))
                     #'(arg* ...))
            (unique-ids?/raise 
              (append
                #'(arg* ...)
                (if (identifier? #'rest) (list #'rest) '())) 
              orig-stx))]))
)
