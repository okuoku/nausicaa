;;; test-irregex.sls --
;;


;;;; setup

(import (rnrs)
  (irregex)
  (uriel test))

(check-set-mode! 'report-failed)
;;;(check-set-mode! 'report)



;;;; tests

(let ((rex "[a-z]+"))
  (check-for-true (irregex-search rex "123abc456"))
  (check (irregex-search rex "123456") => #f))

(let ((rex "foobar"))
  (check-for-true (irregex-search rex "abcfoobardef"))
  (check (irregex-search rex "abcFOOBARdef") => #f))

(let ((rex (string->irregex "[a-z]+")))
  (check-for-true (irregex-search rex "123abc456"))
  (check (irregex-search rex "123456") => #f))

(let ((rex (string->irregex "foobar" 'case-insensitive)))
  (check-for-true (irregex-search rex "abcfoobardef"))
  (check-for-true (irregex-search rex "abcFOOBARdef")))

(let ((rex '(w/nocase "foobar")))
  (check-for-true (irregex-search rex "abcFOOBARdef"))
  (check-for-true (irregex-search rex "foobar"))
  (check-for-true (irregex-search rex "FOOBAR")))

;;IRREGEX-MATCH performs a  search anchored to the beginning  and end of
;;the string.
(let ((rex '(w/nocase "foobar")))
  (check (irregex-match rex "abcFOOBARdef") => #f)
  (check-for-true (irregex-match rex "foobar"))
  (check-for-true (irregex-match rex "FOOBAR")))

(let ((match (irregex-search "ciao" "hello ciao salut")))
  (check (irregex-match-substring match) => "ciao")
  (check (irregex-match-substring match 0) => "ciao")
  (check (irregex-match-substring match 1) => #f))

;;Grouping parentheses.
(let ((match (irregex-search "c(i(a(o)))"
			     "hello ciao salut")))
  (check (irregex-match-substring match) => "ciao")
  (check (irregex-match-substring match 0) => "ciao")
  (check (irregex-match-substring match 1) => "iao")
  (check (irregex-match-substring match 2) => "ao")
  (check (irregex-match-substring match 3) => "o"))

;;Non-grouping parentheses.
(let ((match (irregex-search "c(i(?:a(o)))"
			     "hello ciao salut")))
  (check (irregex-match-substring match) => "ciao")
  (check (irregex-match-substring match 0) => "ciao")
  (check (irregex-match-substring match 1) => "iao")
  (check (irregex-match-substring match 2) => "o"))

;;Submatch.
;; (let ((match (irregex-search '("ciao"
;; 			       (submatch "iao")
;; 			       (submatch "ao")
;; 			       (submatch "o")
;; 			       (submatch "a"))
;; 			     "hello ciao salut")))
;;   (check 'this (irregex-match-substring match) => "ciao")
;;   (check (irregex-match-substring match 1) => "iao")
;;   (check (irregex-match-substring match 2) => "ao")
;;   (check (irregex-match-substring match 3) => "o")
;;   (check (irregex-match-substring match 4) => "a"))

;;Named submatch.
;; (let ((match (irregex-search '("ciao"
;; 			       (submatch-named "first" "iao")
;; 			       (submatch-named "second" "ao")
;; 			       (submatch-named "third" "o"))
;; 			     "hello ciao salut")))
;;   (write match)
;;   (check 'this (irregex-match-substring match) => "ciao")
;;   (check (irregex-match-substring match "iao") => "iao")
;;   (check (irregex-match-substring match "second") => "ao")
;;   (check (irregex-match-substring match "third") => "o"))



;;;; done

(check-report)

;;; end of file



