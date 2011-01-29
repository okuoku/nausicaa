;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the users functions
;;;Date: Tue Dec  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa strings)
  (nausicaa ffi errno)
  (for (nausicaa posix typedefs) expand run)
  (prefix (nausicaa posix system) px.)
  (prefix (nausicaa glibc streams) glibc.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing users functions\n")


(parameterize ((check-test-name	'accessors)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in persona accessors" E))
    (lambda ()

      (check
	  (uid? (px.getuid))
	=> #t)

      (check
	  (gid? (px.getgid))
	=> #t)

      (check
	  (uid? (px.geteuid))
	=> #t)

      (check
	  (gid? (px.getegid))
	=> #t)

      (check
	  (for-all gid? (px.getgroups))
	=> #t)

      (check
	  (for-all gid? (px.getgrouplist "marco" (px.getgid)))
	=> #t)

      #t)))


(parameterize ((check-test-name	'mutators)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in persona mutators" E))
    (lambda ()

      (check
	  (px.setuid (px.getuid))
	=> 0)

      (check
	  (px.setgid (px.getgid))
	=> 0)

      (check
	  (px.seteuid (px.geteuid))
	=> 0)

      (check
	  (px.setegid (px.getegid))
	=> 0)

      (check
	  ;;requires privileges
	  (guard (E ((errno-condition? E)
		     (errno-symbolic-value E))
		    (else
		     #f))
	    (px.setgroups (px.getgroups)))
	=> 'EPERM)

      #t)))


(parameterize ((check-test-name	'login)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in login" E))
    (lambda ()

      (check
	  (px.getlogin)
	=> "marco")

      (check
	  (px.cuserid)
	=> "marco")

      #t)))


(parameterize ((check-test-name	'passwd)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in struct passwd" E))
    (lambda ()

      (check
	  (let ((record (px.getpwuid (px.getuid))))
	    (<passwd>? record))
	=> #t)

      (check
	  (let ((pwd (px.getpwuid (px.getuid))))
	    (with-fields* ((name <passwd-rtd> pwd))
	      pwd.name))
	=> "marco")

      (check
	  (let ((pwd (px.getpwuid (px.getuid))))
	    (with-fields* ((name <passwd-rtd> pwd))
	      (let ((pwd (px.getpwnam pwd.name)))
		(with-fields* ((name <passwd-rtd> pwd))
		  pwd.name))))
	=> "marco")

      (check
	  (let ((pwds (with-compensations
			(letrec ((file* (compensate
					    (glibc.fopen "/etc/passwd" "r")
					  (with
					   (glibc.fclose file*)))))
			  (let loop ((pwds '()))
			    (let ((pwd (px.fgetpwent file*)))
			      (if pwd
				  (loop (cons pwd pwds))
				pwds)))))))
	    (<passwd>-name (car (memp (lambda (pwd)
					       (with-fields* (((name uid) <passwd-rtd> pwd))
						 (= 0 (uid->integer pwd.uid))))
					     pwds))))
	=> "root")

      #t)))


(parameterize ((check-test-name	'groups)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in struct group" E))
    (lambda ()

      (check
	  (let ((record (px.getgrgid (px.getgid))))
	    (<group>? record))
	=> #t)

      (check
	  (let ((grp (px.getgrgid (px.getgid))))
	    (with-fields* ((name <group-rtd> grp))
	      grp.name))
	=> "marco")

      (check
	  (let ((grp (px.getgrgid (px.getgid))))
	    (with-fields* ((name <group-rtd> grp))
	      (let ((grp (px.getgrnam grp.name)))
		(with-fields* ((name <group-rtd> grp))
		  grp.name))))
	=> "marco")

      (check
      	  (let ((grps (with-compensations
      			(letrec ((file* (compensate
      					    (glibc.fopen "/etc/group" "r")
      					  (with
      					   (glibc.fclose file*)))))
      			  (let loop ((grps '()))
      			    (let ((grp (px.fgetgrent file*)))
      			      (if grp
      				  (loop (cons grp grps))
      				grps)))))))
      	    (<group>-name (car (memp (lambda (grp)
					      (with-fields* (((name gid) <group-rtd> grp))
						(= 0 (gid->integer grp.gid))))
					    grps))))
      	=> "root")

      #t)))


;;;; done

(check-report)

;;; end of file
