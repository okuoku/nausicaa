;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the file system functions
;;;Date: Fri Jan  2, 2009
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



;;;; setup

(import (except (r6rs)
		read write remove)
  (uriel lang)
  (uriel foreign)
  (only (string-lib) string-join)
  (uriel test)
  (posix process)
  (posix fd)
  (posix file)
  (posix file stat)
  (posix sizeof)
  (env-lib))

(check-set-mode! 'report-failed)



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
  (system (string-append "mkdir --mode=0700 " the-root))
  (system (string-append "mkdir --mode=0700 " the-subdir-1))
  (system (string-append "mkdir --mode=0700 " the-subdir-2))
  (system (string-append "mkdir --mode=0700 " the-subdir-3))
  (system (string-append "umask 0027; echo -n \"" the-string "\" >" the-file))
  (system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-10))
  (system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-11))
  (system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file-2)))

(define (clean-test-hierarchy)
  (system (string-append "rm -fr " the-root)))



(parameterize ((testname	'working-directory)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))

	(check
	    (let ((dirname '/))
	      (chdir dirname))
	  => 0)

	(check
	    (let ((dirname '/usr/local/bin))
	      (chdir dirname))
	  => 0)

	(check
	    (let ((dirname '/scrappy/dappy/doo))
	      (guard (exc (else
			   (list (errno-condition? exc)
				 (condition-who exc)
				 (errno-symbolic-value exc))))
		(chdir dirname)))
	  => '(#t primitive-chdir ENOENT))

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

	))))



(parameterize ((testname	'directory-access)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (with-compensations
		(let ((dir	(opendir/c the-root))
		      (layout	'()))
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  (check
	      (with-compensations
		(let ((dir	(opendir/c the-subdir-1))
		      (layout	'()))
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".." "name-10.ext" "name-11.ext"))

	  (check
	      (with-compensations
		(let ((dir	(opendir/c the-subdir-3))
		      (layout	'()))
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".."))

	  (check
	      (list-sort string<? (directory-list the-root))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  (check
	      (list-sort string<? (directory-list the-subdir-1))
	    => '("." ".." "name-10.ext" "name-11.ext"))

	  (check
	      (list-sort string<? (directory-list the-subdir-3))
	    => '("." ".."))

	  ;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	  ;;compensation (weird but I have tested it, believe me!).
	  (check
	      (letrec ((fd (open the-root O_RDONLY 0)))
		(list-sort string<? (directory-list/fd fd)))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  ;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	  ;;compensation (weird but I have tested it, believe me!).
	  (when (number? O_NOATIME)
	    (check
		(with-compensations
		  (letrec ((fd (open the-root O_RDONLY 0)))
		    (list-sort string<? (directory-list/fd fd))))
	      => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext")))

	  (check
	      (with-compensations
		(let ((dir	(opendir/c the-root))
		      (layout2	'())
		      (layout1	'()))
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout1
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout1)))
		  (rewinddir dir)
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout2
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout2)))
		  (append (list-sort string<? layout1)
			  (list-sort string<? layout2))))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"
		 "." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  (check
	      (with-compensations
		(let ((dir	(opendir/c the-root))
		      (layout	'()))
		  (do ((entry (readdir dir) (readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cons (cstring->string (struct-dirent-d_name-ref entry))
				      (telldir entry))
				layout)))
		  (map car (list-sort (lambda (a b)
					(string<? (car a) (car b)))
				      layout))))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  )))))



(parameterize ((testname	'links)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))
	  (let ((the-other (string-join (list the-root "other.ext") "/")))

	    (check
		(with-compensations
		    (compensate
			(link the-file the-other)
		      (with
		       (delete-file the-other)))
		  (with-input-from-file the-other
		    (lambda ()
		      (get-string-all (current-input-port)))))
	      => the-string)

	    (check
		(with-compensations
		    (compensate
			(symlink the-file the-other)
		      (with
		       (delete-file the-other)))
		  (with-input-from-file the-other
		    (lambda ()
		      (get-string-all (current-input-port)))))
	      => the-string)

	    (check
		(with-compensations
		    (compensate
			(symlink the-file the-other)
		      (with
		       (delete-file the-other)))
		  (realpath the-other))
	      => the-file)

	    ))))))




(parameterize ((testname	'remove)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (begin
		(unlink the-file)
		(file-exists? the-file))
	    => #f)

	  (check
	      (begin
		(remove the-file-2)
		(file-exists? the-file-2))
	    => #f)

	  (check
	      (begin
		(rmdir the-subdir-3)
		(file-exists? the-subdir-3))
	    => #f)

	  (check
	      (guard (exc (else
			   (list (condition-who exc)
				 (errno-condition? exc)
				 (errno-symbolic-value exc))))
		(rmdir the-subdir-1))
	    => '(primitive-rmdir #t ENOTEMPTY))

	  (check
	      (begin
		(unlink the-file-10)
		(unlink the-file-11)
		(rmdir the-subdir-1)
		(file-exists? the-subdir-1))
	    => #f)


	  )))))



(parameterize ((testname	'rename)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (let ((the-other-file (string-join (list the-root "other.ext") "/"))
		(the-other-dir  (string-join (list the-root "dir-4") "/")))

	    (check
		(begin
		  (rename the-file the-other-file)
		  (list (file-exists? the-file)
			(file-exists? the-other-file)))
	      => '(#f #t))

	    (check
		(begin
		  (rename the-subdir-1 the-other-dir)
		  (list (file-exists? the-subdir-1)
			(file-exists? the-other-dir)))
	      => '(#f #t))


	    ))))))



(parameterize ((testname	'mkdir)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (let ((the-other-dir  (string-join (list the-root "dir-4") "/")))

	    (check
		(list
		  (file-exists? the-other-dir)
		  (begin
		    (mkdir the-other-dir #o700)
		    (file-exists? the-other-dir)))
	      => '(#f #t))

	    ))))))



(parameterize ((testname	'tmpfile)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (let ((pathname (tmpnam)))
;;;		(debug "tmpnam: ~s" pathname)
		(list (string? pathname)
		      (file-exists? pathname)))
	    => '(#t #f))

	  (check
	      (let ((pathname (mktemp (string-join
				       (list TMPDIR "XXXXXX")
				       "/"))))
;;;		(debug "mktemp: ~s" pathname)
		(list (string? pathname)
		      (file-exists? pathname)))
	    => '(#t #f))

	  (check
	      (let-values (((fd pathname)
			    (mkstemp (string-join
				      (list TMPDIR "XXXXXX")
				      "/"))))
;;;		(debug "mkstemp: ~s" pathname)
		(close fd)
		(list (string? pathname)
		      (begin0
			  (file-exists? pathname)
			(delete-file pathname))))
	    => '(#t #t))

	  )))))




(parameterize ((testname	'stat)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (exc)
	(debug-print-condition "deferred condition" exc))
    (lambda ()
      (guard (exc (else
		   (debug-print-condition "sync condition" exc)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (letrec ((the-other
		    (string-join (list the-root "other.ext") "/"))
		   (fd
		    (compensate
			(open the-file O_RDONLY 0)
		      (with
		       (close fd)))))

	    (compensate
		(symlink the-file the-other)
	      (with
	       (delete-file the-other)))

	    (check
		(struct-stat? (stat the-file))
	      => #t)

	    (check
		(struct-stat? (fstat fd))
	      => #t)

	    (check
		(with-compensations
		  (struct-stat? (lstat the-other)))
	      => #t)

;;; --------------------------------------------------------------------

	    (check
		(list (file-is-directory? the-subdir-1)
		      (file-is-directory? the-file)
		      (file-is-directory? the-other)
		      (file-is-directory? fd))
	      => '(#t #f #f #f))

	    (check
		(list (file-is-character-special? the-subdir-1)
		      (file-is-character-special? the-file)
		      (file-is-character-special? the-other)
		      (file-is-character-special? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-block-special? the-subdir-1)
		      (file-is-block-special? the-file)
		      (file-is-block-special? the-other)
		      (file-is-block-special? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-regular? the-subdir-1)
		      (file-is-regular? the-file)
		      (file-is-regular? the-other)
		      (file-is-regular? fd))
	      => '(#f #t #t #t))

	    (check
		(list (file-is-fifo? the-subdir-1)
		      (file-is-fifo? the-file)
		      (file-is-fifo? the-other)
		      (file-is-fifo? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-symbolic-link? the-subdir-1)
		      (file-is-symbolic-link? the-file)
		      (file-is-symbolic-link? the-other))
	      => '(#f #f #t))

	    (check
		(list (file-is-socket? the-subdir-1)
		      (file-is-socket? the-file)
		      (file-is-socket? the-other)
		      (file-is-socket? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-semaphore? the-subdir-1)
		      (file-is-semaphore? the-file)
		      (file-is-semaphore? the-other)
		      (file-is-semaphore? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-shared-memory? the-subdir-1)
		      (file-is-shared-memory? the-file)
		      (file-is-shared-memory? the-other)
		      (file-is-shared-memory? fd))
	      => '(#f #f #f #f))

	    (check
		(list (file-is-message-queue? the-subdir-1)
		      (file-is-message-queue? the-file)
		      (file-is-message-queue? the-other)
		      (file-is-message-queue? fd))
	      => '(#f #f #f #f))

;;; --------------------------------------------------------------------

	    (check
		(= 0 (bitwise-ior S_IRUSR
				  (struct-stat-mode (stat the-file))))
	      => #f)

	    (check
		(= 0 (bitwise-ior S_IROTH
				  (struct-stat-mode (stat the-file))))
	      => #f)

	    (check
		(list (file-user-readable? the-file)
		      (file-user-writable? the-file)
		      (file-user-executable? the-file)
		      (file-group-readable? the-file)
		      (file-group-writable? the-file)
		      (file-group-executable? the-file)
		      (file-other-readable? the-file)
		      (file-other-writable? the-file)
		      (file-other-executable? the-file)
		      (file-setuid? the-file)
		      (file-setgid? the-file)
		      (file-sticky? the-file))
	      => '(#t #t #f
		      #t #f #f
		      #f #f #f
		      #f #f #f))

	    (check
		(list (file-user-readable? fd)
		      (file-user-writable? fd)
		      (file-user-executable? fd)
		      (file-group-readable? fd)
		      (file-group-writable? fd)
		      (file-group-executable? fd)
		      (file-other-readable? fd)
		      (file-other-writable? fd)
		      (file-other-executable? fd)
		      (file-setuid? fd)
		      (file-setgid? fd)
		      (file-sticky? fd))
	      => '(#t #t #f
		      #t #f #f
		      #f #f #f
		      #f #f #f))

	    (check
		(list (lfile-user-readable? the-other)
		      (lfile-user-writable? the-other)
		      (lfile-user-executable? the-other)
		      (lfile-group-readable? the-other)
		      (lfile-group-writable? the-other)
		      (lfile-group-executable? the-other)
		      (lfile-other-readable? the-other)
		      (lfile-other-writable? the-other)
		      (lfile-other-executable? the-other)
		      (lfile-setuid? the-other)
		      (lfile-setgid? the-other)
		      (lfile-sticky? the-other))
	      => '(#t #t #t
		      #t #t #t
		      #t #t #t
		      #f #f #f))

	    ))))))



;;;; done

(check-report)

;;; end of file
