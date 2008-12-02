;;;
;;;Part of: Uriel libraries
;;;Contents: tests for the POSIX interface
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-12-02 18:20:26 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (uriel posix)
  (uriel glibc)
  (srfi receive))

(check-set-mode! 'report-failed)


;;;; environment variables

(check
    (let ()
      (setenv 'CIAO 'pasta 1)
      (getenv 'CIAO))
  => "pasta")

(check
    (let ()
      (setenv 'SALUT 'pasta 1)
      (setenv 'SALUT 'fusillo 0)
      (getenv 'CIAO))
  => "pasta")

;; (check
;;     (let ()
;;       (setenv 'CIAO 'pasta 1)
;;       (let ((v (getenv 'CIAO)))
;; 	(unsetenv 'CIAO)
;; 	(list v (getenv 'CIAO))))
;;   => '("pasta" #f))

;; (check
;;     (let ()
;;       (setenv 'CIAO 'pasta 1)
;;       (let ((v (getenv 'CIAO)))
;; 	(unsetenv 'CIAO)
;; 	(list v (getenv 'CIAO))))
;;   => '("pasta" ""))



;;;; working directory

(check
    (let ((dirname '/))
      (chdir dirname))
  => 0)

(check
    (let ((dirname '/usr/local/bin))
      (chdir dirname))
  => 0)

(check
    (let ((dirname '/usr/local/bin))
      (chdir dirname)
      (getcwd))
  => "/usr/local/bin")

(check
    (let ((dirname '/bin))
      (chdir dirname)
      (pwd))
  => "/bin")



;;;; done

(check-report)

;;; end of file
