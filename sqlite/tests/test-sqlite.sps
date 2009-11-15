;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: tests for high-level interface
;;;Date: Fri Nov 13, 2009
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
  (compensations)
  (checks)
  (foreign databases sqlite)
  (foreign databases sqlite compensated))

(check-set-mode! 'report-failed)
(display "*** testing SQLite compound\n")


;;;; helpers

(define-syntax with-database
  (syntax-rules ()
    ((_ (?name ?pathname) ?form ...)
     (let ((?name ?pathname))
       (dynamic-wind
	   (lambda ()
	     (when (file-exists? ?name)
	       (delete-file ?name)))
	   (lambda ()
	     ?form ...)
	   (lambda ()
	     (when (file-exists? ?name)
	       (delete-file ?name))))))))


(parametrise ((check-test-name	'version))

  (check
      (sqlite-libversion-number)
    => SQLITE_VERSION_NUMBER)

  #t)


(parametrise ((check-test-name	'open-close))

  (check
      (let ((db (sqlite-open ':memory:)))
	(sqlite-close db))
    => 0)

  (check
      (with-database (pathname "database-0.db")
		     (let ((db (sqlite-open pathname)))
		       (sqlite-close db)))
    => 0)

  (check
      (with-database (pathname "database-0.db")
  		     (let ((db (sqlite-open-v2 pathname
  					       (sqlite-open-flags CREATE
								  READWRITE
								  NOMUTEX))))
  		       (sqlite-close db)))
    => 0)

  #t)


(parametrise ((check-test-name	'open-compensated))

  (check
      (with-compensations
	(let ((session (sqlite-open/c ':memory:)))
	  #t))
    => #t)

  (check
      (with-database (pathname "database-0.db")
		     (with-compensations
		       (let ((session (sqlite-open-v2/c pathname
							(sqlite-open-flags CREATE
									   READWRITE
									   NOMUTEX))))
			 #t)))
    => #t)

  #t)


(parametrise ((check-test-name	'callback))

  (check
      (let* ((session	(sqlite-open ":memory:"))
	     (result	'())
	     (cback	(lambda (column-names column-values)
			  (set-cons! result (list column-names column-values))
			  SQLITE_OK)))
	(sqlite-exec session "create table accounts
				(id INTEGER PRIMARY KEY,
				 nickname TEXT,
				 password TEXT);")
	(sqlite-exec session "insert into accounts (nickname, password)
				  values ('ichigo', 'abcde');
                              insert into accounts (nickname, password)
				  values ('rukia', '12345');
			      insert into accounts (nickname, password)
				  values ('chad', 'fist');")
	(let ((code (sqlite-exec session "select * from accounts;" cback)))
	  (sqlite-close session)
	  (list code (reverse result))))
    => `(,SQLITE_OK
	 ((("id" "nickname" "password")
	   ("1" "ichigo" "abcde"))
	  (("id" "nickname" "password")
	   ("2" "rukia" "12345"))
	  (("id" "nickname" "password")
	   ("3" "chad" "fist")))))

  (check
      (let ((session (sqlite-open ":memory:")))
	(guard (E ((sqlite-querying-error-condition? E)
		   #t)
		  (else #f))
	  (sqlite-exec session "create table")))
    => #t)

  (check
      (let ((session (sqlite-open ":memory:")))
	(guard (E ((warning? E)
		   (condition-irritants E))
		  (else #f))
	  (sqlite-exec session "create table accounts
				(id INTEGER PRIMARY KEY,
				 nickname TEXT,
				 password TEXT);")
	  (sqlite-exec session "insert into accounts (nickname, password)
				  values ('ichigo', 'abcde');")
	  (sqlite-exec session "select * from accounts;"
		       (lambda (n v)
			 (raise (condition (make-warning)
					   (make-irritants-condition 123)))))))
    => 123)

  #t)


(parametrise ((check-test-name	'get-table))

  (check
      (let ((session (sqlite-open ":memory:")))
	(sqlite-exec session "create table accounts
				(id INTEGER PRIMARY KEY,
				 nickname TEXT,
				 password TEXT);")
	(sqlite-exec session "insert into accounts (nickname, password)
				  values ('ichigo', 'abcde');
                              insert into accounts (nickname, password)
				  values ('rukia', '12345');
			      insert into accounts (nickname, password)
				  values ('chad', 'fist');")
	(let ((table (sqlite-get-table session "select * from accounts;")))
	  (sqlite-close session)
	  table))
    => '(("id" "nickname" "password")
	 ("1" "ichigo" "abcde")
	 ("2" "rukia" "12345")
	 ("3" "chad" "fist")))

  #t)


(parametrise ((check-test-name	'step))

  (define sql-create
    "create table accounts (id INTEGER PRIMARY KEY, nickname TEXT, password TEXT);")

  (define sql-insert
    "insert into accounts (nickname, password) values ('ichigo', 'abcde');
     insert into accounts (nickname, password) values ('rukia', '12345');
     insert into accounts (nickname, password) values ('chad', 'fist');")

  (define sql-query
    "select * from accounts;")

  (check
      (guard (E (else E))
	(let* ((session	(sqlite-open ":memory:"))
	       (result	'()))

	  (let ((ell1 (sqlite-prepare-v2 session sql-create)))
	    (for-each (lambda (statement)
			(let loop ((code (sqlite-step session statement)))
			  (unless (= code SQLITE_DONE)
			    (loop (sqlite-step session statement))))
			(sqlite-finalise session statement))
	      ell1))

	  (let ((ell2 (sqlite-prepare-v2 session sql-insert)))
	    (for-each (lambda (statement)
			(let loop ((code (sqlite-step session statement)))
			  (unless (= code SQLITE_DONE)
			    (loop (sqlite-step session statement))))
	  		(sqlite-finalise session statement))
	      ell2))

	  (let ((ell3 (sqlite-prepare-v2 session sql-query)))
	    (for-each
		(lambda (statement)
		  (let loop ()
		    (let ((code (sqlite-step session statement)))
		      (cond ((= code SQLITE_ROW)
			     (when (null? result)
			       (set-cons! result (list (sqlite-column-count statement)
						       (sqlite-column-name statement 0)
						       (sqlite-column-name statement 1)
						       (sqlite-column-name statement 2))))
			     (set-cons! result (list (sqlite-column-int statement 0)
						     (sqlite-column-text statement 1)
						     (sqlite-column-text statement 2)))
			     (loop))
			    ((= code SQLITE_DONE)
			     (sqlite-finalise session statement))
			    (else
			     (loop))))))
	      ell3))

	  (reverse result)))
    => '((3 "id" "nickname" "password")
	 (1 "ichigo" "abcde")
	 (2 "rukia" "12345")
	 (3 "chad" "fist")))

  #t)


;;;; done

(check-report)

;;; end of file
