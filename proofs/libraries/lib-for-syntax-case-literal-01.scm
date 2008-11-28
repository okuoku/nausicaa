;;; lib-for-syntax-case-literal-01.sls --
;;;

(library (lib-for-syntax-case-literal-01)
	 (export doit-beta)
	 (import (rnrs))
	 (define-syntax doit-beta
	   (syntax-rules (beta)
	     ((_ ?arg1 (beta ?arg2))
	      (list ?arg1 ?arg2)))))

;;; end of file
