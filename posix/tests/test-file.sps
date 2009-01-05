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
  (system (string-append "umask 0077; echo -n \"" the-string "\" >" the-file))
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

	  (let ((the-other (string-join (list the-root "other.ext") "/")))

	    (check
		(struct-stat? (stat the-file))
	      => #t)

	    (check
		(with-compensations
		  (letrec ((fd (compensate
				   (open the-file O_RDONLY 0)
				 (with
				  (close fd)))))
		    (struct-stat? (fstat fd))))
	      => #t)

	    (check
		(with-compensations
		    (compensate
			(symlink the-file the-other)
		      (with
		       (delete-file the-other)))
		  (struct-stat? (lstat the-other)))
	      => #t)

	    ))))))



;;;; done

(check-report)

;;; end of file
