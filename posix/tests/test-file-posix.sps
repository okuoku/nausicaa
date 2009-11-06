;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the POSIX file system functions
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
  (only (foreign posix file platform) struct-dirent-d_name-ptr-ref)
  (prefix (foreign posix stat) posix:)
  (prefix (foreign posix stat record-types) posix:)
  (foreign posix sizeof))

(check-set-mode! 'report-failed)
(display "*** testing POSIX file\n")


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


(parametrise ((check-test-name	'working-directory)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in working directory" E))
    (lambda ()
      (guard (E (else (debug-print-condition "working directory condition" E)))

	(check
	    (let ((dirname '/))
	      (posix:chdir dirname))
	  => 0)

	(check
	    (let ((dirname '/usr/local/bin))
	      (posix:chdir dirname))
	  => 0)

	(check
	    (let ((dirname '/scrappy/dappy/doo))
	      (guard (E (else (list (errno-condition? E)
				    (condition-who E)
				    (errno-symbolic-value E))))
		(posix:chdir dirname)))
	  => '(#t chdir ENOENT))

	(check
	    (let ((dirname '/usr/local/bin))
	      (posix:chdir dirname)
	      (posix:getcwd))
	  => "/usr/local/bin")

	(check
	    (let ((dirname '/bin))
	      (posix:chdir dirname)
	      (posix:pwd))
	  => "/bin")

	#f))))


(parametrise ((check-test-name	'directory-access)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in directory access" E))
    (lambda ()
      (guard (E (else (debug-print-condition "directory access condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))
;;;(posix:system (string-append "ls -l " the-root))
	  (check 'this
	      (with-compensations
		(let ((dir	(posix:opendir/c the-root))
		      (layout	'()))
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
		      ((pointer-null? entry))
;; (write (list (struct-dirent-d_name-ptr-ref entry)
;; 	     (struct-dirent-d_name-ref entry)
;; 	     (cstring->string (struct-dirent-d_name-ref entry))
;; 	     ))(newline)
		    (set! layout
			  (cons (cstring->string
				 (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  (check
	      (with-compensations
		(let ((dir	(posix:opendir/c the-subdir-1))
		      (layout	'()))
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".." "name-10.ext" "name-11.ext"))

	  (check
	      (with-compensations
		(let ((dir	(posix:opendir/c the-subdir-3))
		      (layout	'()))
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout)))
		  (list-sort string<? layout)))
	    => '("." ".."))

	  (check
	      (list-sort string<? (posix:directory-list the-root))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  (check
	      (list-sort string<? (posix:directory-list the-subdir-1))
	    => '("." ".." "name-10.ext" "name-11.ext"))

	  (check
	      (list-sort string<? (posix:directory-list the-subdir-3))
	    => '("." ".."))

	  ;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	  ;;compensation (weird but I have tested it, believe me!).
	  (check
	      (letrec ((fd (posix:open the-root O_RDONLY 0)))
		(list-sort string<? (posix:directory-list/fd fd)))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  ;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	  ;;compensation (weird but I have tested it, believe me!).
	  (when (number? O_NOATIME)
	    (check
		(with-compensations
		  (letrec ((fd (posix:open the-root O_RDONLY 0)))
		    (list-sort string<? (posix:directory-list/fd fd))))
	      => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext")))

	  (check
	      (with-compensations
		(let ((dir	(posix:opendir/c the-root))
		      (layout2	'())
		      (layout1	'()))
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
		      ((pointer-null? entry))
		    (set! layout1
			  (cons (cstring->string (struct-dirent-d_name-ref entry))
				layout1)))
		  (posix:rewinddir dir)
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
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
		(let ((dir	(posix:opendir/c the-root))
		      (layout	'()))
		  (do ((entry (posix:readdir dir) (posix:readdir dir)))
		      ((pointer-null? entry))
		    (set! layout
			  (cons (cons (cstring->string (struct-dirent-d_name-ref entry))
				      (posix:telldir entry))
				layout)))
		  (map car (list-sort (lambda (a b)
					(string<? (car a) (car b)))
				      layout))))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	  #f)))))


(parametrise ((check-test-name	'links)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in links" E))
    (lambda ()
      (guard (E (else (debug-print-condition "links condition" E)))
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
			(posix:link the-file the-other)
		      (with
		       (delete-file the-other)))
		  (with-input-from-file the-other
		    (lambda ()
		      (get-string-all (current-input-port)))))
	      => the-string)

	    (check
		(with-compensations
		    (compensate
			(posix:symlink the-file the-other)
		      (with
		       (delete-file the-other)))
		  (with-input-from-file the-other
		    (lambda ()
		      (get-string-all (current-input-port)))))
	      => the-string)

	    (check
		(with-compensations
		    (compensate
			(posix:symlink the-file the-other)
		      (with
		       (delete-file the-other)))
		  (posix:realpath the-other))
	      => the-file)

	    #f))))))


(parametrise ((check-test-name	'remove)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in remove" E))
    (lambda ()
      (guard (E (else (debug-print-condition "remove condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (begin
		(posix:unlink the-file)
		(file-exists? the-file))
	    => #f)

	  (check
	      (begin
		(posix:remove the-file-2)
		(file-exists? the-file-2))
	    => #f)

	  (check
	      (begin
		(posix:rmdir the-subdir-3)
		(file-exists? the-subdir-3))
	    => #f)

	  (check
	      (guard (E (else (list (condition-who E)
				    (errno-condition? E)
				    (errno-symbolic-value E))))
		(posix:rmdir the-subdir-1))
	    => '(rmdir #t ENOTEMPTY))

	  (check
	      (begin
		(posix:unlink the-file-10)
		(posix:unlink the-file-11)
		(posix:rmdir the-subdir-1)
		(file-exists? the-subdir-1))
	    => #f)

	  #f)))))


(parametrise ((check-test-name	'rename)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in rename" E))
    (lambda ()
      (guard (E (else (debug-print-condition "rename condition" E)))
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
		  (posix:rename the-file the-other-file)
		  (list (file-exists? the-file)
			(file-exists? the-other-file)))
	      => '(#f #t))

	    (check
		(begin
		  (posix:rename the-subdir-1 the-other-dir)
		  (list (file-exists? the-subdir-1)
			(file-exists? the-other-dir)))
	      => '(#f #t))

	    #f))))))


(parametrise ((check-test-name	'mkdir)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in mkdir" E))
    (lambda ()
      (guard (E (else (debug-print-condition "mkdir condition" E)))
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
		    (posix:mkdir the-other-dir #o700)
		    (file-exists? the-other-dir)))
	      => '(#f #t))

	    #f))))))


(parametrise ((check-test-name	'tmpfile)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in tmpfile" E))
    (lambda ()
      (guard (E (else (debug-print-condition "tmpfile condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (let ((pathname (posix:tmpnam)))
;;;(debug "tmpnam: ~s" pathname)
		(list (string? pathname)
		      (file-exists? pathname)))
	    => '(#t #f))

	  (check
	      (let ((pathname (posix:mktemp (string-join (list TMPDIR "XXXXXX") "/"))))
;;;(debug "mktemp: ~s" pathname)
		(list (string? pathname)
		      (file-exists? pathname)))
	    => '(#t #f))

	  (check
	      (let-values (((fd pathname) (posix:mkstemp (string-join (list TMPDIR "XXXXXX") "/"))))
;;;(debug "mkstemp: ~s" pathname)
		(posix:close fd)
		(list (string? pathname)
		      (begin0
			  (file-exists? pathname)
			(delete-file pathname))))
	    => '(#t #t))

	  #f)))))


(parametrise ((check-test-name	'chown)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in chown" E))
    (lambda ()
      (guard (E (else (debug-print-condition "chown condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (let* ((record	(posix:stat the-file))
		     (uid	(posix:<struct-stat>-uid record))
		     (gid	(posix:<struct-stat>-gid record)))
		(posix:chown the-file uid gid))
	    => 0)

	  (check
	      (with-compensations
		(letrec ((fd (compensate
				 (posix:open the-file O_RDONLY 0)
			       (with
				(posix:close fd)))))
		  (let* ((record	(posix:fstat fd))
			 (uid		(posix:<struct-stat>-uid record))
			 (gid		(posix:<struct-stat>-gid record)))
		    (posix:fchown fd uid gid))))
	    => 0)

	  #f)))))


(parametrise ((check-test-name	'chmod)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in chmod" E))
    (lambda ()
      (guard (E (else (debug-print-condition "chmod condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (posix:umask #o0027)

	  (check
	      (posix:getumask)
	    => #o0027)

	  (check
	      (let ()
		(posix:umask 0)
		(posix:chmod the-file (bitwise-ior S_IRUSR S_IXUSR
						   S_IRGRP S_IXGRP
						   S_IROTH S_IXOTH))
		(posix:file-permissions the-file))
	    => (bitwise-ior S_IRUSR S_IXUSR
			    S_IRGRP S_IXGRP
			    S_IROTH S_IXOTH))

	  (check
	      (with-compensations
		(letrec ((fd (compensate
				 (posix:open the-file O_RDONLY 0)
			       (with
				(posix:close fd)))))
		  (posix:umask 0)
		  (posix:fchmod fd #o123)
		  (posix:file-permissions fd)))
	    => #o123)

	  #f)))))


(parametrise ((check-test-name	'access)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in access" E))
    (lambda ()
      (guard (E (else (debug-print-condition "access condition" E)))
	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
		(posix:access the-file F_OK))
	    => #t)

	  (check
	      (let ((the-other (string-join (list the-root "other.ext") "/")))
		(posix:access the-other F_OK))
	    => #f)

	  (check
	      (begin
		(posix:chmod the-file S_IRUSR)
		(posix:access the-file R_OK))
	    => #t)

	  (check
	      (begin
		(posix:chmod the-file 0)
		(posix:access the-file R_OK))
	    => #f)

	  (check
	      (begin
		(posix:chmod the-file S_IWUSR)
		(posix:access the-file W_OK))
	    => #t)

	  (check
	      (begin
		(posix:chmod the-file 0)
		(posix:access the-file W_OK))
	    => #f)

	  ;;I am not testing this on my system because "/tmp" is mounted
	  ;;with the NOEXEC attribute.
	  ;;
	  ;; (check
	  ;;     (with-compensations
	  ;; 	  (compensate
	  ;; 	      (posix:umask 0)
	  ;; 	    (with
	  ;; 	     (posix:umask (bitwise-ior S_IWGRP S_IRWXG))))
	  ;; 	(posix:chmod the-file S_IXUSR)
	  ;; 	(posix:access the-file X_OK))
	  ;;   => #t)

	  (check
	      (posix:access the-subdir-1 X_OK)
	    => #t)

	  (check
	      (begin
		(posix:chmod the-file 0)
		(posix:access the-file X_OK))
	    => #f)

	  #f)))))


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

	  ;;;The SUS says  that an implementation may choose  not to set
	  ;;;the  access time  of the  file in  the "struct  stat", even
	  ;;;though it is requested by the "utime()" call.
	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
;;;(posix:system (string-append "stat --printf='access=%X, modification=%Y, change=%Z\n' " the-file))
;;;		(debug "debug: times ~s" (get-times the-file))
		(posix:utime the-file
			     #e1e9 ;access time
			     #e2e9);modification time
;;;(posix:system (string-append "stat --printf='access=%X, modification=%Y, change=%Z\n' " the-file))
;;;		(debug "debug: times ~s" (get-times the-file))
		(get-times the-file))
	    => '(#e1e9 #e2e9))

	  (check
	      (begin
		(posix:chmod the-file S_IRWXU)
;;;		(debug "debug: times ~s" (get-times the-file))
		(begin0
		    (posix:utime the-file)
;;;		  (debug "debug: times ~s" (get-times the-file))
		  ))
	    => 0)

	  #f)))))


(parametrise ((check-test-name	'size)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in size" E))
    (lambda ()
      (guard (E (else (debug-print-condition "size condition" E)))

	(with-compensations
	  (clean-test-hierarchy)
	    (compensate
		(make-test-hierarchy)
	      (with
	       (clean-test-hierarchy)))

;;;(posix:system (string-append "ls -l " the-file))
	  (check
	      (posix:file-size the-file)
	    => (string-length the-string))

	  (check
	      (begin
		(posix:ftruncate the-file 5)
		(posix:file-size the-file))
	    => 5)

	  #f)))))


;;;; done

(check-report)

;;; end of file
