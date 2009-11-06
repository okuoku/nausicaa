;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the stat POSIX functions
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
  (prefix (foreign posix process) posix:)
  (prefix (foreign posix fd) posix:)
  (prefix (foreign posix file) posix:)
  (prefix (foreign posix stat) posix:)
  (prefix (foreign posix stat record-types) posix:)
  (foreign posix sizeof))

(check-set-mode! 'report-failed)
(display "*** testing POSIX stat\n")


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


(parametrise ((check-test-name	'stat)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in stat" E))
    (lambda ()
      (guard (E (else (debug-print-condition "stat condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (letrec ((the-other	(string-join (list the-root "other.ext") "/"))
		   (fd		(compensate
				    (posix:open the-file O_RDONLY 0)
				  (with
				   (posix:close fd)))))
	    (compensate
		(posix:symlink the-file the-other)
	      (with
	       (delete-file the-other)))

	    (check
		(posix:<struct-stat>? (posix:stat the-file))
	      => #t)

	    (check
		(posix:<struct-stat>? (posix:fstat fd))
	      => #t)

	    (check
		(with-compensations
		  (posix:<struct-stat>? (posix:lstat the-other)))
	      => #t)

;;; --------------------------------------------------------------------

	    (check
		(list (posix:file-is-directory? the-subdir-1)
		      (posix:file-is-directory? the-file)
		      (posix:file-is-directory? the-other)
		      (posix:file-is-directory? fd))
	      => '(#t #f #f #f))

	    (check
		(list (posix:file-is-character-special? the-subdir-1)
		      (posix:file-is-character-special? the-file)
		      (posix:file-is-character-special? the-other)
		      (posix:file-is-character-special? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-block-special? the-subdir-1)
		      (posix:file-is-block-special? the-file)
		      (posix:file-is-block-special? the-other)
		      (posix:file-is-block-special? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-regular? the-subdir-1)
		      (posix:file-is-regular? the-file)
		      (posix:file-is-regular? the-other)
		      (posix:file-is-regular? fd))
	      => '(#f #t #t #t))

	    (check
		(list (posix:file-is-fifo? the-subdir-1)
		      (posix:file-is-fifo? the-file)
		      (posix:file-is-fifo? the-other)
		      (posix:file-is-fifo? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-symbolic-link? the-subdir-1)
		      (posix:file-is-symbolic-link? the-file)
		      (posix:file-is-symbolic-link? the-other))
	      => '(#f #f #t))

	    (check
		(list (posix:file-is-socket? the-subdir-1)
		      (posix:file-is-socket? the-file)
		      (posix:file-is-socket? the-other)
		      (posix:file-is-socket? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-semaphore? the-subdir-1)
		      (posix:file-is-semaphore? the-file)
		      (posix:file-is-semaphore? the-other)
		      (posix:file-is-semaphore? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-shared-memory? the-subdir-1)
		      (posix:file-is-shared-memory? the-file)
		      (posix:file-is-shared-memory? the-other)
		      (posix:file-is-shared-memory? fd))
	      => '(#f #f #f #f))

	    (check
		(list (posix:file-is-message-queue? the-subdir-1)
		      (posix:file-is-message-queue? the-file)
		      (posix:file-is-message-queue? the-other)
		      (posix:file-is-message-queue? fd))
	      => '(#f #f #f #f))

;;; --------------------------------------------------------------------

	    (check
		(= 0 (bitwise-ior S_IRUSR (posix:<struct-stat>-mode (posix:stat the-file))))
	      => #f)

	    (check
		(= 0 (bitwise-ior S_IROTH (posix:<struct-stat>-mode (posix:stat the-file))))
	      => #f)

	    (check
		(list (posix:file-user-readable? the-file)
		      (posix:file-user-writable? the-file)
		      (posix:file-user-executable? the-file)
		      (posix:file-group-readable? the-file)
		      (posix:file-group-writable? the-file)
		      (posix:file-group-executable? the-file)
		      (posix:file-other-readable? the-file)
		      (posix:file-other-writable? the-file)
		      (posix:file-other-executable? the-file)
		      (posix:file-setuid? the-file)
		      (posix:file-setgid? the-file)
		      (posix:file-sticky? the-file))
	      => '(#t #t #f
		      #t #f #f
		      #f #f #f
		      #f #f #f))

	    (check
		(list (posix:file-user-readable? fd)
		      (posix:file-user-writable? fd)
		      (posix:file-user-executable? fd)
		      (posix:file-group-readable? fd)
		      (posix:file-group-writable? fd)
		      (posix:file-group-executable? fd)
		      (posix:file-other-readable? fd)
		      (posix:file-other-writable? fd)
		      (posix:file-other-executable? fd)
		      (posix:file-setuid? fd)
		      (posix:file-setgid? fd)
		      (posix:file-sticky? fd))
	      => '(#t #t #f
		      #t #f #f
		      #f #f #f
		      #f #f #f))

	    (check
		(list (posix:lfile-user-readable? the-other)
		      (posix:lfile-user-writable? the-other)
		      (posix:lfile-user-executable? the-other)
		      (posix:lfile-group-readable? the-other)
		      (posix:lfile-group-writable? the-other)
		      (posix:lfile-group-executable? the-other)
		      (posix:lfile-other-readable? the-other)
		      (posix:lfile-other-writable? the-other)
		      (posix:lfile-other-executable? the-other)
		      (posix:lfile-setuid? the-other)
		      (posix:lfile-setgid? the-other)
		      (posix:lfile-sticky? the-other))
	      => '(#t #t #t
		      #t #t #t
		      #t #t #t
		      #f #f #f))

	    #f))))))




;;;; done

(check-report)

;;; end of file
