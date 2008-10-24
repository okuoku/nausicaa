#!r6rs
(import (rnrs) (xitomatl srfi random))

(do ((i 0 (+ i 1)))
  ((= i 10) 'done)
  (display (random-integer 100))
  (newline))

(do ((i 0 (+ i 1)))
  ((= i 10) 'done)
  (display (random-real))
  (newline))
