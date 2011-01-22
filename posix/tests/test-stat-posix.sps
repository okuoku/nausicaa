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
  (nausicaa checks)
  (nausicaa ffi)
  (nausicaa ffi memory)
  (nausicaa ffi errno)
  (nausicaa ffi cstrings)
  (nausicaa posix sizeof)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix process) px.)
  (prefix (nausicaa posix fd) px.)
  (prefix (nausicaa posix file) px.))

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
  (px.system (string-append "mkdir --mode=0700 " the-root))
  (px.system (string-append "mkdir --mode=0700 " the-subdir-1))
  (px.system (string-append "mkdir --mode=0700 " the-subdir-2))
  (px.system (string-append "mkdir --mode=0700 " the-subdir-3))
  (px.system (string-append "umask 0027; echo -n \"" the-string "\" >" the-file))
  (px.system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-10))
  (px.system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-11))
  (px.system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-2)))

(define (clean-test-hierarchy)
  (px.system (string-append "rm -fr " the-root)))


(parametrise ((check-test-name	'stat)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in stat" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

	(letrec ((the-other	(string-join (list the-root "other.ext") "/"))
		 (fd		(compensate
				    (px.open the-file O_RDONLY 0)
				  (with
				   (px.close fd)))))
	  (compensate
	      (px.symlink the-file the-other)
	    (with
	     (delete-file the-other)))

	  (check
	      (<stat>? (px.stat the-file))
	    => #t)

	  (check
	      (<stat>? (px.fstat fd))
	    => #t)

	  (check
	      (with-compensations
		(<stat>? (px.lstat the-other)))
	    => #t)

;;; --------------------------------------------------------------------

	  (check
	      (list (px.file-is-directory? the-subdir-1)
		    (px.file-is-directory? the-file)
		    (px.file-is-directory? the-other)
		    (px.file-is-directory? fd))
	    => '(#t #f #f #f))

	  (check
	      (list (px.file-is-character-special? the-subdir-1)
		    (px.file-is-character-special? the-file)
		    (px.file-is-character-special? the-other)
		    (px.file-is-character-special? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-block-special? the-subdir-1)
		    (px.file-is-block-special? the-file)
		    (px.file-is-block-special? the-other)
		    (px.file-is-block-special? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-regular? the-subdir-1)
		    (px.file-is-regular? the-file)
		    (px.file-is-regular? the-other)
		    (px.file-is-regular? fd))
	    => '(#f #t #t #t))

	  (check
	      (list (px.file-is-fifo? the-subdir-1)
		    (px.file-is-fifo? the-file)
		    (px.file-is-fifo? the-other)
		    (px.file-is-fifo? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-symbolic-link? the-subdir-1)
		    (px.file-is-symbolic-link? the-file)
		    (px.file-is-symbolic-link? the-other))
	    => '(#f #f #t))

	  (check
	      (list (px.file-is-socket? the-subdir-1)
		    (px.file-is-socket? the-file)
		    (px.file-is-socket? the-other)
		    (px.file-is-socket? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-semaphore? the-subdir-1)
		    (px.file-is-semaphore? the-file)
		    (px.file-is-semaphore? the-other)
		    (px.file-is-semaphore? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-shared-memory? the-subdir-1)
		    (px.file-is-shared-memory? the-file)
		    (px.file-is-shared-memory? the-other)
		    (px.file-is-shared-memory? fd))
	    => '(#f #f #f #f))

	  (check
	      (list (px.file-is-message-queue? the-subdir-1)
		    (px.file-is-message-queue? the-file)
		    (px.file-is-message-queue? the-other)
		    (px.file-is-message-queue? fd))
	    => '(#f #f #f #f))

;;; --------------------------------------------------------------------

	  (check
	      (= 0 (bitwise-ior S_IRUSR (<stat>-mode (px.stat the-file))))
	    => #f)

	  (check
	      (= 0 (bitwise-ior S_IROTH (<stat>-mode (px.stat the-file))))
	    => #f)

	  (check
	      (list (px.file-user-readable? the-file)
		    (px.file-user-writable? the-file)
		    (px.file-user-executable? the-file)
		    (px.file-group-readable? the-file)
		    (px.file-group-writable? the-file)
		    (px.file-group-executable? the-file)
		    (px.file-other-readable? the-file)
		    (px.file-other-writable? the-file)
		    (px.file-other-executable? the-file)
		    (px.file-setuid? the-file)
		    (px.file-setgid? the-file)
		    (px.file-sticky? the-file))
	    => '(#t #t #f
		    #t #f #f
		    #f #f #f
		    #f #f #f))

	  (check
	      (list (px.file-user-readable? fd)
		    (px.file-user-writable? fd)
		    (px.file-user-executable? fd)
		    (px.file-group-readable? fd)
		    (px.file-group-writable? fd)
		    (px.file-group-executable? fd)
		    (px.file-other-readable? fd)
		    (px.file-other-writable? fd)
		    (px.file-other-executable? fd)
		    (px.file-setuid? fd)
		    (px.file-setgid? fd)
		    (px.file-sticky? fd))
	    => '(#t #t #f
		    #t #f #f
		    #f #f #f
		    #f #f #f))

	  (check
	      (list (px.lfile-user-readable? the-other)
		    (px.lfile-user-writable? the-other)
		    (px.lfile-user-executable? the-other)
		    (px.lfile-group-readable? the-other)
		    (px.lfile-group-writable? the-other)
		    (px.lfile-group-executable? the-other)
		    (px.lfile-other-readable? the-other)
		    (px.lfile-other-writable? the-other)
		    (px.lfile-other-executable? the-other)
		    (px.lfile-setuid? the-other)
		    (px.lfile-setgid? the-other)
		    (px.lfile-sticky? the-other))
	    => '(#t #t #t
		    #t #t #t
		    #t #t #t
		    #f #f #f))

	  #f)))))


;;;; done

(check-report)

;;; end of file
