;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the email addresses data types
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (nausicaa)
  (checks)
  (email addresses common))

(check-set-mode! 'report-failed)
(display "*** testing email addresses records\n")


(parameterise ((check-test-name 'common))

  (check
      (unquote-string "")
    => "")

  (check
      (unquote-string "\\")
    => "\\")

  (check
      (unquote-string "\\\\")
    => "\\")

  (check
      (unquote-string "\\\\\\")
    => "\\\\")

  (check
      (unquote-string "ciao")
    => "ciao")

  (check
      (unquote-string "ci\\ao")
    => "ciao")

  (check
      (unquote-string "\\c\\i\\a\\o")
    => "ciao")

  (check
      (unquote-string "ciao\\")
    => "ciao\\")

  #t)


;;;; domain data type

(check
    (domain? (make-domain #f '("alpha" "beta")))
  => #t)

(check
    (domain->string (make-domain #f '("alpha" "beta" "gamma")))
  => "alpha.beta.gamma")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-display (make-domain #f '("alpha" "beta" "gamma")) port)
      (getter))
  => "#<domain -- alpha.beta.gamma>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-write (make-domain #f '("alpha" "beta" "gamma")) port)
      (getter))
  => "(make-domain #f (quote (\"alpha\" \"beta\" \"gamma\")))")

;;; --------------------------------------------------------------------

(check
    (domain? (make-domain #t '("1" "2" "3" "4")))
  => #t)

(check
    (domain->string (make-domain #t '("1" "2" "3" "4")))
  => "[1.2.3.4]")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-display (make-domain #t '("1" "2" "3" "4")) port)
      (getter))
  => "#<domain -- [1.2.3.4]>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-write (make-domain #t '("1" "2" "3" "4")) port)
      (getter))
  => "(make-domain #t (quote (\"1\" \"2\" \"3\" \"4\")))")


;;;; local-part data type

(check
    (local-part? (make-local-part '("alpha" "beta")))
  => #t)

(check
    (local-part->string (make-local-part '("alpha" "beta" "gamma")))
  => "alpha.beta.gamma")

(check
    (let-values (((port getter) (open-string-output-port)))
      (local-part-display (make-local-part '("alpha" "beta" "gamma")) port)
      (getter))
  => "#<local-part -- alpha.beta.gamma>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (local-part-write (make-local-part '("alpha" "beta" "gamma")) port)
      (getter))
  => "(make-local-part (quote (\"alpha\" \"beta\" \"gamma\")))")


;;;; addr-spec data type

(check
    (addr-spec? (make-addr-spec (make-local-part '("alpha" "beta"))
				(make-domain #f '("delta" "org"))))
  => #t)

(check
    (addr-spec->string (make-addr-spec (make-local-part '("alpha" "beta"))
				       (make-domain #f '("delta" "org"))))
  => "alpha.beta@delta.org")

(check
    (let-values (((port getter) (open-string-output-port)))
      (addr-spec-display (make-addr-spec (make-local-part '("alpha" "beta"))
					 (make-domain #f '("delta" "org"))) port)
      (getter))
  => "#<addr-spec -- alpha.beta@delta.org>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (addr-spec-write (make-addr-spec (make-local-part '("alpha" "beta"))
				       (make-domain #f '("delta" "org"))) port)
      (getter))
  => "(make-addr-spec (make-local-part (quote (\"alpha\" \"beta\"))) (make-domain #f (quote (\"delta\" \"org\"))))")


;;;; route data type

(check
    (route? (make-route (list (make-domain #f '("alpha" "org"))
			      (make-domain #t '("1" "2" "3" "4"))
			      (make-domain #f '("beta" "com")))))
  => #t)

(check
    (route->string (make-route (list (make-domain #f '("alpha" "org"))
				     (make-domain #t '("1" "2" "3" "4"))
				     (make-domain #f '("beta" "com")))))
  => "alpha.org,[1.2.3.4],beta.com")

(check
    (let-values (((port getter) (open-string-output-port)))
      (route-display (make-route (list (make-domain #f '("alpha" "org"))
				       (make-domain #t '("1" "2" "3" "4"))
				       (make-domain #f '("beta" "com"))))
		     port)
      (getter))
  => "#<route -- alpha.org,[1.2.3.4],beta.com>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (route-write (make-route (list (make-domain #f '("alpha" "org"))
				     (make-domain #t '("1" "2" "3" "4"))
				     (make-domain #f '("beta" "com"))))
		   port)
      (getter))
  => "(make-route (list (make-domain #f (quote (\"alpha\" \"org\"))) (make-domain #t (quote (\"1\" \"2\" \"3\" \"4\"))) (make-domain #f (quote (\"beta\" \"com\"))) ))")


;;;; mailbox data type

(let ((the-route	(make-route (list (make-domain #f '("alpha" "org"))
					  (make-domain #t '("1" "2" "3" "4"))
					  (make-domain #f '("beta" "com")))))
      (the-phrase	"the phrase")
      (the-addr-spec	(make-addr-spec (make-local-part '("alpha" "beta"))
					(make-domain #f '("delta" "org")))))

  (check
      (mailbox? (make-mailbox the-phrase the-route the-addr-spec))
    => #t)

  (check
      (mailbox->string (make-mailbox the-phrase the-route the-addr-spec))
    => "the phrase <alpha.org,[1.2.3.4],beta.com:alpha.beta@delta.org>")

  (check
      (let-values (((port getter) (open-string-output-port)))
	(mailbox-display (make-mailbox the-phrase the-route the-addr-spec) port)
	(getter))
    => "#<mailbox -- the phrase <alpha.org,[1.2.3.4],beta.com:alpha.beta@delta.org>>")

  (check
      (let-values (((port getter) (open-string-output-port)))
	(mailbox-write (make-mailbox the-phrase the-route the-addr-spec) port)
	(getter))
    => "(make-mailbox \"the phrase\" (make-route (list (make-domain #f (quote (\"alpha\" \"org\"))) (make-domain #t (quote (\"1\" \"2\" \"3\" \"4\"))) (make-domain #f (quote (\"beta\" \"com\"))) )) (make-addr-spec (make-local-part (quote (\"alpha\" \"beta\"))) (make-domain #f (quote (\"delta\" \"org\")))))")

  #t)


;;;; done

(check-report)

;;; end of file
