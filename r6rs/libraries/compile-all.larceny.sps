;;; compile-all.sps --
;;;

(import (rnrs)
  (larceny compiler))

(compile-library "r6rs.sls" "r6rs.larceny.slfasl")

; end of file
