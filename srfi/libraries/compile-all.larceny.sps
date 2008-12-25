;;; compile-all.sps --
;;;

(import (rnrs)
  (larceny compiler))

(compile-library "srfi/and-let-star.sls"
		 "srfi/and-let-star.larceny.slfasl")
(compile-library "srfi/parameters.larceny.sls"
		 "srfi/parameters.larceny.slfasl")
(compile-library "srfi/error-reporting.sls"
		 "srfi/error-reporting.larceny.slfasl")
;;(compile-library "srfi/cond-expand.sls" "srfi/cond-expand.larceny.slfasl")
;;(compile-library "srfi/lists.sls" "srfi/lists.larceny.slfasl")
;;(compile-library "srfi/string-ports.sls" "srfi/string-ports.larceny.slfasl")
(compile-library "srfi/receive.sls"
		 "srfi/receive.larceny.slfasl")
(compile-library "srfi/records.sls"
		 "srfi/records.larceny.slfasl")
(compile-library "srfi/let-values.sls"
		 "srfi/let-values.larceny.slfasl")
;;(compile-library "srfi/strings.sls" "srfi/strings.larceny.slfasl")
;;(compile-library "srfi/char-set.sls" "srfi/char-set.larceny.slfasl")
(compile-library "srfi/case-lambda.sls"
		 "srfi/case-lambda.larceny.slfasl")
;;(compile-library "srfi/time.sls" "srfi/time.larceny.slfasl")
;;(compile-library "srfi/cut.sls" "srfi/cut.larceny.slfasl")
;;(compile-library "srfi/random.sls" "srfi/random.larceny.slfasl")
(compile-library "srfi/rec.sls"
		 "srfi/rec.larceny.slfasl")
;;(compile-library "srfi/args-fold.sls" "srfi/args-fold.larceny.slfasl")
(compile-library "srfi/sharing.sls"
		 "srfi/sharing.larceny.slfasl")
(compile-library "srfi/streams.sls"
		 "srfi/streams.larceny.slfasl")
;;(compile-library "srfi/eager-comprehensions.sls" "srfi/eager-comprehensions.larceny.slfasl")
;;(compile-library "srfi/vectors.sls" "srfi/vectors.larceny.slfasl")
;;(compile-library "srfi/format.sls" "srfi/format.larceny.slfasl")
(compile-library "srfi/general-cond.sls"
		 "srfi/general-cond.larceny.slfasl")
;;(compile-library "srfi/compare.sls" "srfi/compare.larceny.slfasl")
;;(compile-library "srfi/lightweight-testing.sls" "srfi/lightweight-testing.larceny.slfasl")

;; (compile-library "list-lib.sls" "list-lib.larceny.slfasl")
;; (compile-library "string-lib.sls" "string-lib.larceny.slfasl")
;; (compile-library "vector-lib.sls" "vector-lib.larceny.slfasl")


; end of file
