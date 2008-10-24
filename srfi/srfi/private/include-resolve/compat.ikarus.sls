(library (xitomatl srfi private include-resolve compat)
  (export
    search-paths)
  (import
    (rnrs base)
    (only (ikarus) library-path))

  (define (search-paths)
    (library-path))
)
