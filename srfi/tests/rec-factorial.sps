#!r6rs
(import (rnrs) (xitomatl srfi rec))

(display 
 ((rec (F N) 
    (if (zero? N) 1 
      (* N (F (- N 1)))))
  10))
(newline)
