#!r6rs
(library (xitomatl srfi private include-resolve)
  (export 
    include/resolve)
  (import 
    (rnrs) 
    (for (xitomatl srfi private include-resolve compat) expand))
  
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
