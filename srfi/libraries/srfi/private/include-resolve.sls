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
(library (srfi private include-resolve)
  (export 
    include/resolve)
  (import 
    (rnrs) 
    (for (srfi private include-resolve compat) expand))
  
  (define-syntax include/resolve
    (lambda (stx)
      (define (include/lexical-context ctxt filename)
        (with-exception-handler
          (lambda (ex)
            (raise
             (condition
              (make-error)
              (make-who-condition 'include/resolve)
              (make-message-condition "error while trying to include")
              (make-irritants-condition (list filename))
              (if (condition? ex) ex (make-irritants-condition (list ex))))))
          (lambda ()
            (call-with-input-file filename
              (lambda (fip)
                (let loop ([x (read fip)] [a '()])
                  (if (eof-object? x)
                    (datum->syntax ctxt `(begin . ,(reverse a)))
                    (loop (read fip) (cons x a)))))))))
      (syntax-case stx ()
        [(kw (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(file-path lib-path* ...)))
         (let* ([sep "/"]
                [lp*/sep (apply string-append (map (lambda (ps) (string-append ps sep)) 
                                                   (syntax->datum #'(lib-path* ...))))]
                [fp (syntax->datum #'file-path)])
           (let loop ([search (search-paths)])
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths"
                      (string-append lp*/sep fp) (search-paths))
               (let ([full (string-append (car search) sep lp*/sep fp)])
                 (if (file-exists? full)
                   (include/lexical-context #'kw full)
                   (loop (cdr search)))))))])))
)
