(library (xitomatl srfi format compat)
  (export
    pretty-print
    ascii-tab)
  (import
    (rnrs base)
    (primitives pretty-print))
  
  (define ascii-tab #\tab)
)
