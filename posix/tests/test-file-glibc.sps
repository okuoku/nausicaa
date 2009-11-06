;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the glibc file functions
;;;Date: Fri Nov  6, 2009
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
  (strings)
  (checks)
  (deferred-exceptions)
  (compensations)
  (foreign ffi)
  (foreign memory)
  (foreign errno)
  (foreign cstrings)

  (prefix (foreign glibc file) glibc:)

  (foreign posix sizeof)
  (prefix (foreign posix process) posix:)
  (prefix (foreign posix fd) posix:)
  (prefix (foreign posix file) posix:)
  (prefix (foreign posix stat) posix:)
  (prefix (foreign posix stat record-types) posix:))

(check-set-mode! 'report-failed)
(display "*** testing Glibc file\n")


;;;; test hierarchy

(define TMPDIR
  (or (get-environment-variable "TMPDIR")
      "/tmp"))

(define the-root	(string-join (list TMPDIR "root-dir") "/"))
(define the-file	(string-join (list the-root "name.ext") "/"))
(define the-subdir-1	(string-join (list the-root "dir-1") "/"))
(define the-file-10	(string-join (list the-subdir-1 "name-10.ext") "/"))
(define the-file-11	(string-join (list the-subdir-1 "name-11.ext") "/"))
(define the-subdir-2	(string-join (list the-root "dir-2") "/"))
(define the-file-2	(string-join (list the-subdir-2 "name-2.ext") "/"))
(define the-subdir-3	(string-join (list the-root "dir-3") "/"))

(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")

;;The hierarchy looks like this:
;;
;; $TMPDIR/root-dir/
;; $TMPDIR/root-dir/dir-1/
;; $TMPDIR/root-dir/dir-1/name-10.ext
;; $TMPDIR/root-dir/dir-1/name-11.ext
;; $TMPDIR/root-dir/dir-2/
;; $TMPDIR/root-dir/dir-2/name-2.ext
;; $TMPDIR/root-dir/dir-3/
;; $TMPDIR/root-dir/name.ext
;;
(define the-layout
  (list the-root
	the-subdir-1 the-file-10 the-file-11
	the-subdir-2 the-file-2
	the-subdir-3
	the-file))

(define (make-test-hierarchy)
  (posix:system (string-append "mkdir --mode=0700 " the-root))
  (posix:system (string-append "mkdir --mode=0700 " the-subdir-1))
  (posix:system (string-append "mkdir --mode=0700 " the-subdir-2))
  (posix:system (string-append "mkdir --mode=0700 " the-subdir-3))
  (posix:system (string-append "umask 0027; echo -n \"" the-string "\" >" the-file))
  (posix:system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-10))
  (posix:system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-11))
  (posix:system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-2)))

(define (clean-test-hierarchy)
  (posix:system (string-append "rm -fr " the-root)))


(parametrise ((check-test-name	'times)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in times" E))
    (lambda ()
      (guard (E (else (debug-print-condition "times condition" E)))

	(define (get-times pathname)
	  (let ((record (posix:stat the-file)))
	    (list (posix:<struct-stat>-atime record)
		  (posix:<struct-stat>-mtime record))))

	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

;;; --------------------------------------------------------------------

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
		(glibc:utimes the-file
			      #e1e3 ;access time
			      #e1e4
			      #e2e3 ;modification time
			      #e2e4)
		(get-times the-file))
	    => '(#e1e3 #e2e3))

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
		(glibc:utimes the-file))
	    => 0)

;;; --------------------------------------------------------------------

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
		(glibc:lutimes the-file
			       #e1e3 ;access time
			       #e1e4
			       #e2e3 ;modification time
			       #e2e4)
		(get-times the-file))
	    => '(#e1e3 #e2e3))

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
		(glibc:lutimes the-file))
	    => 0)

;;; --------------------------------------------------------------------

	  (check
	      (with-compensations
	  	(posix:chmod the-file S_IRWXU)
	  	(letrec ((fd (compensate
	  			 (posix:open the-file O_WRONLY 0)
	  		       (with
	  			(posix:close fd)))))
	  	  (glibc:futimes fd
	  			 #e1e3 ;access time
	  			 #e1e4
	  			 #e2e3 ;modification time
	  			 #e2e4)
	  	  (get-times the-file)))
	    => '(#e1e3 #e2e3))

	  (check
	      (with-compensations
	  	(posix:chmod the-file S_IRWXU)
	  	(letrec ((fd (compensate
	  			 (posix:open the-file O_WRONLY 0)
	  		       (with
	  			(posix:close fd)))))
	  	  (glibc:futimes fd)))
	    => 0)

	  #f)))))


;;;; done

(check-report)

;;; end of file
