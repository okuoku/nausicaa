;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the POSIX file system functions
;;;Date: Fri Jan  2, 2009
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


(parametrise ((check-test-name	'working-directory)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in working directory" E))
    (lambda ()

      (check
	  (let ((dirname '/))
	    (px.chdir dirname))
	=> 0)

      (check
	  (let ((dirname '/usr/local/bin))
	    (px.chdir dirname))
	=> 0)

      (check
	  (let ((dirname '/scrappy/dappy/doo))
	    (guard (E (else (list (errno-condition? E)
				  (condition-who E)
				  (errno-symbolic-value E))))
	      (px.chdir dirname)))
	=> '(#t chdir ENOENT))

      (check
	  (let ((dirname '/usr/local/bin))
	    (px.chdir dirname)
	    (px.getcwd))
	=> "/usr/local/bin")

      (check
	  (let ((dirname '/bin))
	    (px.chdir dirname)
	    (px.pwd))
	=> "/bin")

      #f)))


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

;;;(px.system (string-append "ls -l " the-root))

	(check
	    (with-compensations
	      (let ((dir (px.opendir/c the-root)))
		(list-sort string<?
			   (let loop ((entry	(px.readdir dir))
				      (layout	'()))
			     (if (pointer-null? entry)
				 layout
			       (loop (px.readdir dir)
				     (cons (px.dirent-name->string entry) layout)))))))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	(check
	    (with-compensations
	      (let ((dir	(px.opendir/c the-subdir-1))
		    (layout	'()))
		(do ((entry (px.readdir dir) (px.readdir dir)))
		    ((pointer-null? entry))
		  (set! layout (cons (px.dirent-name->string entry) layout)))
		(list-sort string<? layout)))
	  => '("." ".." "name-10.ext" "name-11.ext"))

	(check
	    (with-compensations
	      (let ((dir	(px.opendir/c the-subdir-3))
		    (layout	'()))
		(do ((entry (px.readdir dir) (px.readdir dir)))
		    ((pointer-null? entry))
		  (set! layout (cons (px.dirent-name->string entry) layout)))
		(list-sort string<? layout)))
	  => '("." ".."))

;;; --------------------------------------------------------------------

	(check	;readdir_r
	    (with-compensations
	      (let ((dir	(px.opendir/c the-root))
		    (layout	'()))
		(do ((entry (px.readdir_r dir) (px.readdir_r dir)))
		    ((pointer-null? entry))
		  (set! layout  (cons (px.dirent-name->string entry) layout)))
		(list-sort string<? layout)))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

;;; --------------------------------------------------------------------

	(check
	    (list-sort string<? (px.directory-entries the-root))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	(check
	    (list-sort string<? (px.directory-entries the-subdir-1))
	  => '("." ".." "name-10.ext" "name-11.ext"))

	(check
	    (list-sort string<? (px.directory-entries the-subdir-3))
	  => '("." ".."))

;;; --------------------------------------------------------------------

	;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	;;compensation (weird but I have tested it, believe me!).
	(check
	    (letrec ((fd (px.open the-root O_RDONLY 0)))
	      (list-sort string<? (px.directory-entries/fd fd)))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	;;We DO  NOT close fd  here, it is  closed by CLOSEDIR  in the
	;;compensation (weird but I have tested it, believe me!).
	(when (number? O_NOATIME)
	  (check
	      (with-compensations
		(letrec ((fd (px.open the-root O_RDONLY 0)))
		  (list-sort string<? (px.directory-entries/fd fd))))
	    => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext")))

;;; --------------------------------------------------------------------

	(check	;telldir
	    (with-compensations
	      (let ((dir	(px.opendir/c the-root))
		    (layout2	'())
		    (layout1	'()))
		(do ((entry (px.readdir dir) (px.readdir dir)))
		    ((pointer-null? entry))
		  (set! layout1 (cons (px.dirent-name->string entry) layout1)))
		(px.rewinddir dir)
		(do ((entry (px.readdir dir) (px.readdir dir)))
		    ((pointer-null? entry))
		  (set! layout2 (cons (px.dirent-name->string entry) layout2)))
		(append (list-sort string<? layout1)
			(list-sort string<? layout2))))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"
	       "." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

;;; --------------------------------------------------------------------

	(check	;rewinddir
	    (with-compensations
	      (let ((dir	(px.opendir/c the-root))
		    (layout	'()))
		(do ((entry (px.readdir dir) (px.readdir dir)))
		    ((pointer-null? entry))
		  (set! layout (cons (cons (px.dirent-name->string entry) (px.telldir entry))
				     layout)))
		(map car (list-sort (lambda (a b)
				      (string<? (car a) (car b)))
				    layout))))
	  => '("." ".." "dir-1" "dir-2" "dir-3" "name.ext"))

	#f))))


(parametrise ((check-test-name	'tree-walk)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in tree walking" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

;;;(px.system (string-append "ls -l " the-root))

	(check
	    (list-sort string<?
		       (begin0-let ((result '()))
			 (px.ftw the-root
				    (px.make-ftw-callback
				     (lambda (pathname stat flag)
;;;				       (write (list pathname stat))(newline)
				       (set-cons! result pathname)
				       0))
				    5)))
	  => (map (lambda (item)
		    (string-append TMPDIR item))
	       '("/root-dir"
		 "/root-dir/dir-1"
		 "/root-dir/dir-1/name-10.ext"
		 "/root-dir/dir-1/name-11.ext"
		 "/root-dir/dir-2"
		 "/root-dir/dir-2/name-2.ext"
		 "/root-dir/dir-3"
		 "/root-dir/name.ext")))

	(check
	    (list-sort string<?
		       (begin0-let ((result '()))
			 (px.nftw the-root
				     (px.make-nftw-callback
				      (lambda (pathname stat flag base level)
;;;				        (write stat)(newline)
;;;					(write (list base level))(newline)
					(set-cons! result pathname)
					0))
				     5 0)))
	  => (map (lambda (item)
		    (string-append TMPDIR item))
	       '("/root-dir"
		 "/root-dir/dir-1"
		 "/root-dir/dir-1/name-10.ext"
		 "/root-dir/dir-1/name-11.ext"
		 "/root-dir/dir-2"
		 "/root-dir/dir-2/name-2.ext"
		 "/root-dir/dir-3"
		 "/root-dir/name.ext")))

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

	  (check		;link
	      (with-compensations
		  (compensate
		      (px.link the-file the-other)
		    (with
		     (delete-file the-other)))
		(with-input-from-file the-other
		  (lambda ()
		    (get-string-all (current-input-port)))))
	    => the-string)

	  (check	;symlink
	      (with-compensations
		  (compensate
		      (px.symlink the-file the-other)
		    (with
		     (delete-file the-other)))
		(with-input-from-file the-other
		  (lambda ()
		    (get-string-all (current-input-port)))))
	    => the-string)

	  (check	;realpath
	      (with-compensations
		  (compensate
		      (px.symlink the-file the-other)
		    (with
		     (delete-file the-other)))
		(px.realpath the-other))
	    => the-file)

	  (check	;readlink
	      (with-compensations
		  (compensate
		      (px.symlink the-file the-other)
		    (with
		     (delete-file the-other)))
		(px.readlink the-other))
	    => the-file)

	  #f)))))


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
		(px.unlink the-file)
		(file-exists? the-file))
	    => #f)

	  (check
	      (begin
		(px.remove the-file-2)
		(file-exists? the-file-2))
	    => #f)

	  (check
	      (begin
		(px.rmdir the-subdir-3)
		(file-exists? the-subdir-3))
	    => #f)

	  (check
	      (guard (E (else (list (condition-who E)
				    (errno-condition? E)
				    (errno-symbolic-value E))))
		(px.rmdir the-subdir-1))
	    => '(rmdir #t ENOTEMPTY))

	  (check
	      (begin
		(px.unlink the-file-10)
		(px.unlink the-file-11)
		(px.rmdir the-subdir-1)
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
		  (px.rename the-file the-other-file)
		  (list (file-exists? the-file)
			(file-exists? the-other-file)))
	      => '(#f #t))

	    (check
		(begin
		  (px.rename the-subdir-1 the-other-dir)
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
		    (px.mkdir the-other-dir #o700)
		    (file-exists? the-other-dir)))
	      => '(#f #t))

	    #f))))))


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
	      (let* ((record	(px.stat the-file))
		     (uid	(<stat>-uid record))
		     (gid	(<stat>-gid record)))
		(px.chown the-file uid gid))
	    => 0)

	  (check
	      (with-compensations
		(letrec ((fd (compensate
				 (px.open the-file O_RDONLY 0)
			       (with
				(px.close fd)))))
		  (let* ((record	(px.fstat fd))
			 (uid		(<stat>-uid record))
			 (gid		(<stat>-gid record)))
		    (px.fchown fd uid gid))))
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

	  (px.umask #o0027)

	  (check
	      (px.getumask)
	    => #o0027)

	  (check
	      (let ()
		(px.umask 0)
		(px.chmod the-file (bitwise-ior S_IRUSR S_IXUSR
						   S_IRGRP S_IXGRP
						   S_IROTH S_IXOTH))
		(px.file-permissions the-file))
	    => (bitwise-ior S_IRUSR S_IXUSR
			    S_IRGRP S_IXGRP
			    S_IROTH S_IXOTH))

	  (check
	      (let ()
		(px.umask 0)
		(px.chmod the-file
			     (access-permissions user-read user-exec
						 group-read group-exec
						 other-read other-exec))
		(px.file-permissions the-file))
	    => (bitwise-ior S_IRUSR S_IXUSR
			    S_IRGRP S_IXGRP
			    S_IROTH S_IXOTH))

	  (check
	      (with-compensations
		(letrec ((fd (compensate
				 (px.open the-file O_RDONLY 0)
			       (with
				(px.close fd)))))
		  (px.umask 0)
		  (px.fchmod fd #o123)
		  (px.file-permissions fd)))
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
		(px.chmod the-file S_IRWXU)
		(px.access the-file F_OK))
	    => #t)

	  (check
	      (let ((the-other (string-join (list the-root "other.ext") "/")))
		(px.access the-other F_OK))
	    => #f)

	  (check
	      (begin
		(px.chmod the-file S_IRUSR)
		(px.access the-file R_OK))
	    => #t)

	  (check
	      (begin
		(px.chmod the-file 0)
		(px.access the-file R_OK))
	    => #f)

	  (check
	      (begin
		(px.chmod the-file S_IWUSR)
		(px.access the-file W_OK))
	    => #t)

	  (check
	      (begin
		(px.chmod the-file 0)
		(px.access the-file W_OK))
	    => #f)

	  ;;I am not testing this on my system because "/tmp" is mounted
	  ;;with the NOEXEC attribute.
	  ;;
	  ;; (check
	  ;;     (with-compensations
	  ;; 	  (compensate
	  ;; 	      (px.umask 0)
	  ;; 	    (with
	  ;; 	     (px.umask (bitwise-ior S_IWGRP S_IRWXG))))
	  ;; 	(px.chmod the-file S_IXUSR)
	  ;; 	(px.access the-file X_OK))
	  ;;   => #t)

	  (check
	      (px.access the-subdir-1 X_OK)
	    => #t)

	  (check
	      (begin
		(px.chmod the-file 0)
		(px.access the-file X_OK))
	    => #f)

	  (check
	      (begin
		(px.chmod the-file S_IRWXU)
		(px.access the-file (access-flags existence)))
	    => #t)

	  (check
	      (begin
		(px.chmod the-file S_IRWXU)
		(px.access the-file (access-flags read)))
	    => #t)

	  #f)))))


(parametrise ((check-test-name	'times)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in times" E))
    (lambda ()
      (guard (E (else (debug-print-condition "times condition" E)))

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

	  ;;;The SUS says  that an implementation may choose  not to set
	  ;;;the  access time  of the  file in  the "struct  stat", even
	  ;;;though it is requested by the "utime()" call.
	  (check
	      (begin
		(px.chmod the-file S_IRWXU)
;;;(px.system (string-append "stat --printf='access=%X, modification=%Y, change=%Z\n' " the-file))
;;;		(debug "debug: times ~s" (get-times the-file))
		(px.utime the-file
			     #e1e9 ;access time
			     #e2e9);modification time
;;;(px.system (string-append "stat --printf='access=%X, modification=%Y, change=%Z\n' " the-file))
;;;		(debug "debug: times ~s" (get-times the-file))
		(get-times the-file))
	    => '(#e1e9 #e2e9))

	  (check
	      (begin
		(px.chmod the-file S_IRWXU)
;;;		(debug "debug: times ~s" (get-times the-file))
		(begin0
		    (px.utime the-file)
;;;		  (debug "debug: times ~s" (get-times the-file))
		  ))
	    => 0)

;;; --------------------------------------------------------------------

	(check
	    (begin
	      (px.chmod the-file S_IRWXU)
	      (px.utimes the-file
			    #e1e3 ;access time
			    #e1e4
			    #e2e3 ;modification time
			    #e2e4)
	      (get-times the-file))
	  => '(#e1e3 #e2e3))

	(check
	    (begin
	      (px.chmod the-file S_IRWXU)
	      (px.utimes the-file))
	  => 0)

	  #f)))))


(parametrise ((check-test-name	'size)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in size" E))
    (lambda ()
      (with-compensations
	(clean-test-hierarchy)
	  (compensate
	      (make-test-hierarchy)
	    (with
	     (clean-test-hierarchy)))

;;;(px.system (string-append "ls -l " the-file))
	(check
	    (px.file-size the-file)
	  => (string-length the-string))

	(check
	    (begin
	      (px.ftruncate the-file 10)
	      (px.file-size the-file))
	  => 10)

	(check
	    (begin
	      (px.truncate the-file 5)
	      (px.file-size the-file))
	  => 5)

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

	(check		;mkstemp
	    (let-values (((fd pathname) (px.mkstemp (string-join (list TMPDIR "XXXXXX") "/"))))
	      (px.close fd)
	      (list (string? pathname)
		    (begin0
			(file-exists? pathname)
		      (delete-file pathname))))
	  => '(#t #t))

	(check		;mkdtemp
	    (let ((pathname (px.mkdtemp (string-join (list TMPDIR "XXXXXX") "/"))))
	      (list (string? pathname)
		    (file-exists? pathname)
		    (px.file-is-regular? pathname)
		    (px.file-is-directory? pathname)))
	  => '(#t #t #f #t))

	#f))))


;;;; done

(check-report)

;;; end of file
