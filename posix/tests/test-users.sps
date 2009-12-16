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
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (strings)
  (compensations)
  (deferred-exceptions)
  (records)
  (foreign errno)
  (for (foreign posix typedefs) expand run)
  (prefix (foreign posix system) posix:)
  (prefix (foreign glibc streams) glibc:)
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
	  (uid? (posix:getuid))
	=> #t)

      (check
	  (gid? (posix:getgid))
	=> #t)

      (check
	  (uid? (posix:geteuid))
	=> #t)

      (check
	  (gid? (posix:getegid))
	=> #t)

      (check
	  (for-all gid? (posix:getgroups))
	=> #t)

      (check
	  (for-all gid? (posix:getgrouplist "marco" (posix:getgid)))
	=> #t)

      #t)))


(parameterize ((check-test-name	'mutators)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in persona mutators" E))
    (lambda ()

      (check
	  (posix:setuid (posix:getuid))
	=> 0)

      (check
	  (posix:setgid (posix:getgid))
	=> 0)

      (check
	  (posix:seteuid (posix:geteuid))
	=> 0)

      (check
	  (posix:setegid (posix:getegid))
	=> 0)

      (check
	  ;;requires privileges
	  (guard (E ((errno-condition? E)
		     (errno-symbolic-value E))
		    (else
		     #f))
	    (posix:setgroups (posix:getgroups)))
	=> 'EPERM)

      #t)))


(parameterize ((check-test-name	'login)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in login" E))
    (lambda ()

      (check
	  (posix:getlogin)
	=> "marco")

      (check
	  (posix:cuserid)
	=> "marco")

      #t)))


(parameterize ((check-test-name	'passwd)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in struct passwd" E))
    (lambda ()

      (check
	  (let ((record (posix:getpwuid (posix:getuid))))
	    (<struct-passwd>? record))
	=> #t)

      (check
	  (let ((pwd (posix:getpwuid (posix:getuid))))
	    (with-fields* ((name <struct-passwd-rtd> pwd))
	      pwd.name))
	=> "marco")

      (check
	  (let ((pwd (posix:getpwuid (posix:getuid))))
	    (with-fields* ((name <struct-passwd-rtd> pwd))
	      (let ((pwd (posix:getpwnam pwd.name)))
		(with-fields* ((name <struct-passwd-rtd> pwd))
		  pwd.name))))
	=> "marco")

      (check
	  (let ((pwds (with-compensations
			(letrec ((file* (compensate
					    (glibc:fopen "/etc/passwd" "r")
					  (with
					   (glibc:fclose file*)))))
			  (let loop ((pwds '()))
			    (let ((pwd (posix:fgetpwent file*)))
			      (if pwd
				  (loop (cons pwd pwds))
				pwds)))))))
	    (<struct-passwd>-name (car (memp (lambda (pwd)
					       (with-fields* (((name uid) <struct-passwd-rtd> pwd))
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
	  (let ((record (posix:getgrgid (posix:getgid))))
	    (<struct-group>? record))
	=> #t)

      (check
	  (let ((grp (posix:getgrgid (posix:getgid))))
	    (with-fields* ((name <struct-group-rtd> grp))
	      grp.name))
	=> "marco")

      (check
	  (let ((grp (posix:getgrgid (posix:getgid))))
	    (with-fields* ((name <struct-group-rtd> grp))
	      (let ((grp (posix:getgrnam grp.name)))
		(with-fields* ((name <struct-group-rtd> grp))
		  grp.name))))
	=> "marco")

      (check
      	  (let ((grps (with-compensations
      			(letrec ((file* (compensate
      					    (glibc:fopen "/etc/group" "r")
      					  (with
      					   (glibc:fclose file*)))))
      			  (let loop ((grps '()))
      			    (let ((grp (posix:fgetgrent file*)))
      			      (if grp
      				  (loop (cons grp grps))
      				grps)))))))
      	    (<struct-group>-name (car (memp (lambda (grp)
					      (with-fields* (((name gid) <struct-group-rtd> grp))
						(= 0 (gid->integer grp.gid))))
					    grps))))
      	=> "root")

      #t)))


;;;; done

(check-report)

;;; end of file
