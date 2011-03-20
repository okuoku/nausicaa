;;; guile-config.scm --

(define-module (guile-r6rs-setup))

(set! %load-extensions '(".guile.sls" ".sls" ".scm" ""))
(debug-enable 'backtrace 'debug)

;;; end of file
