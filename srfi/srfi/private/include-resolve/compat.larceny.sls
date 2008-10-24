(library (xitomatl srfi private include-resolve compat)
  (export
    search-paths)
  (import
    (rnrs base)
    (primitives current-require-path getenv absolute-path-string?))
  
  (define (search-paths)
    (let ([larceny-root (getenv "LARCENY_ROOT")])
      (map (lambda (crp)
             (if (absolute-path-string? crp)
               crp
               (string-append larceny-root "/" crp)))
           (current-require-path))))

)
