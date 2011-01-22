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
  (prefix (nausicaa glibc file) glibc:)
  (prefix (only (nausicaa glibc streams) fclose) glibc:)
  (prefix (nausicaa glibc file platform) glibc:platform:)
  (prefix (nausicaa posix process) px.)
  (prefix (nausicaa posix fd) px.)
  (prefix (nausicaa posix file) px.))

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


(parametrise ((check-test-name	'directory-access)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in directory access" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

	(check
	    (glibc:scandir the-root
			   (glibc:make-scandir-selector-callback
			    (lambda (struct-dirent)
			      #t))
			   glibc:platform:alphasort)
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	(check
	    (glibc:scandir the-root
			   (glibc:make-scandir-selector-callback
			    (lambda (struct-dirent)
			      #t))
			   glibc:platform:versionsort)
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	(check
	    (glibc:scandir the-root
			   (glibc:make-scandir-selector-callback
			    (lambda (struct-dirent)
			      (let ((name (px.dirent-name->string struct-dirent)))
				(not (or (string=? "."  name)
					 (string=? ".." name))))))
			   glibc:platform:alphasort)
	  => '("dir-1" "dir-2" "dir-3" "name.ext"))

	(check
	    (glibc:scandir the-root
			   (glibc:make-scandir-selector-callback
			    (lambda (struct-dirent)
			      #t))
			   (glibc:make-scandir-compare-callback
			    (lambda (a b)
			      (let ((a (px.dirent-name->string a))
				    (b (px.dirent-name->string b)))
				(cond ((string<? a b) 1)
				      ((string>? a b) -1)
				      (else 0))))))
	  => (reverse '("." ".." "dir-1" "dir-2" "dir-3" "name.ext")))


	#f))))


(parametrise ((check-test-name	'times)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in times" E))
    (lambda ()
      (define (get-times pathname)
	(let ((record (px.stat the-file)))
	  (list (<stat>-atime record)
		(<stat>-mtime record))))

      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

;;; --------------------------------------------------------------------

	(check
	    (begin
	      (px.chmod the-file S_IRWXU)
	      (glibc:lutimes the-file
			     #e1e3 ;access time
			     #e1e4
			     #e2e3 ;modification time
			     #e2e4)
	      (get-times the-file))
	  => '(#e1e3 #e2e3))

	(check
	    (begin
	      (px.chmod the-file S_IRWXU)
	      (glibc:lutimes the-file))
	  => 0)

;;; --------------------------------------------------------------------

	(check
	    (with-compensations
	      (px.chmod the-file S_IRWXU)
	      (letrec ((fd (compensate
			       (px.open the-file O_WRONLY 0)
			     (with
			      (px.close fd)))))
		(glibc:futimes fd
			       #e1e3 ;access time
			       #e1e4
			       #e2e3 ;modification time
			       #e2e4)
		(get-times the-file)))
	  => '(#e1e3 #e2e3))

	(check
	    (with-compensations
	      (px.chmod the-file S_IRWXU)
	      (letrec ((fd (compensate
			       (px.open the-file O_WRONLY 0)
			     (with
			      (px.close fd)))))
		(glibc:futimes fd)))
	  => 0)

	#f))))


(parametrise ((check-test-name	'tempfile)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in tmpfile" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

	(check		;mktemp
	    (let ((pathname (glibc:mktemp (string-join (list TMPDIR "XXXXXX") "/"))))
	      (list (string? pathname)
		    (file-exists? pathname)))
	  => '(#t #f))

	(check		;tempnam
	    (let ((pathname (glibc:tempnam #f #f)))
	      (list (string? pathname)
		    (file-exists? pathname)))
	  => '(#t #f))

	(check		;tmpnam
	    (let ((pathname (glibc:tmpnam)))
	      (list (string? pathname)
		    (file-exists? pathname)))
	  => '(#t #f))

	(check		;tmpfile
	    (let ((FILE* (glibc:tmpfile)))
	      (glibc:fclose FILE*))
	  => 0)

	#f))))


(parametrise ((check-test-name	'links)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in links" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))
	(let ((the-other (string-join (list the-root "other.ext") "/")))


	  #f)))))


;;;; done

(check-report)

;;; end of file
