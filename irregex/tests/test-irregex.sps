;; test-irregex.sls --
;;

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(import (ikarus)
	(irregex)
	(srfi lightweight-testing))

(printf "test file: ~a~%" (car (command-line)))
(check-set-mode! 'report-failed)

(define-syntax check-for-true
  (syntax-rules ()
    [(_ ?form)
     (check (if ?form #t #f) => #t)]))

(define-syntax check-for-false
  (syntax-rules ()
    [(_ ?form)
     (check (if ?form #t #f) => #f)]))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Tests.
;; ------------------------------------------------------------

(check-for-true (irregex-search "[a-z]+" "123abc456"))

(check-for-false (irregex-search "[a-z]+" "123456"))

(check-for-false (irregex-search "foobar" "abcFOOBARdef"))

(check-for-true (irregex-search (string->irregex "foobar" 'case-insensitive)
				"abcFOOBARdef"))

;; ------------------------------------------------------------

(check-for-false (irregex-match '(w/nocase "foobar") "abcFOOBARdef"))
(check-for-true (irregex-match '(w/nocase "foobar") "FOOBAR"))

;; ------------------------------------------------------------

(check
 (irregex-match-substring (irregex-search "ciao" "hello ciao salut")
			  0)
 => "ciao")

(check
 (irregex-match-substring (irregex-search "ciao[0-9]"
					  "hello ciao0 salut ciao1, ciao2")
			  0)
 => "ciao0")

(pretty-print (irregex-match-substring
	       (irregex-search '(w/nocase "[0-9]")
			       "123456") 0))
#!eof
(check
 (irregex-match-substring (irregex-search '("[0-9]+" (submatch-named ciao "34"))
					  "123456,9")
			  0)
 => "ciao1")

(check
 (irregex-match-substring (irregex-search "ciao[0-9]"
					  "hello ciao0 salut ciao1, ciao2")
			  1)
 => "ciao1")

(check
 (irregex-match-substring (irregex-search "ciao[0-9]"
					  "hello ciao0 salut ciao1, ciao2")
			  2)
 => "ciao2")

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

(check-report)

;;; end of file



