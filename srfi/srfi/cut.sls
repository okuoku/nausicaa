#!r6rs
(library (xitomatl srfi cut)
  (export cut cute)
  (import (rnrs) (xitomatl srfi private include-resolve))
  
  (include/resolve ("xitomatl" "srfi" "cut") "cut.scm")  
)
