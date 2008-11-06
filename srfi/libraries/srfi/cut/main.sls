#!r6rs
(library (srfi cut)
  (export cut cute)
  (import (rnrs) (srfi private include-resolve))
  
  (include/resolve ("srfi" "cut") "cut.sls")  
)
