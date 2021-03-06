;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: message catalog for internationalisation
;;;Date: Tue May 18, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa msgcat)
  (export
    mc mcmax
    load-catalog load-catalog-from-file
    current-catalog en_GB)
  (import (nausicaa)
    (only (nausicaa strings) string-tokenize)
    (only (nausicaa char-sets)
	  char-set-complement char-set char-set:full))


(define-syntax mc
  (syntax-rules ()
    ((_ ?string)
     ((current-catalog) ?string))))

(define (mcmax . strings)
  (if (null? strings)
      0
    (max (map (lambda (S)
		(string-length ((current-catalog) S)))
	   strings))))

(define en_GB
  (case-lambda
   ((string)
    string)
   ((string default)
    string)))

(define current-catalog
  (make-parameter en_GB
    (lambda (catalog)
      (if (procedure? catalog)
	  catalog
	(assertion-violation 'current-catalog "expected procedure as msgcat catalog" catalog)))))

(define-constant $catalogs-directory-environment-variable
  "NAUSICAA_MSGCAT")

(define-constant $catalogs-directories
  (let ((path (get-environment-variable $catalogs-directory-environment-variable)))
    (if path
	(string-tokenize path (char-set-complement (char-set #\:) char-set:full))
      '())))

(define (load-catalog name)
  (let ((name (cond ((string? name)
		     name)
		    ((symbol? name)
		     (symbol->string name))
		    (else
		     (assertion-violation 'load-catalog
		       "expected string or symbol as msgcat catalog name" name)))))
    (let loop ((path $catalogs-directories))
      (if (null? path)
	  (error 'load-catalog "msgcat catalog does not exist" name)
	(let ((pathname (string-append (car path) "/" name ".cat")))
	  (if (file-exists? pathname)
	      (load-catalog-from-file pathname)
	    (loop (cdr path))))))))

(define (load-catalog-from-file pathname)
  (define who 'load-catalog-from-file)
  (unless (file-exists? pathname)
    (error who "missing catalogue file for messages translation" pathname))
  (let ((table (make-hashtable string-hash string=?)))
    (match (with-input-from-file pathname read)
      (('msgcat (1) ?catalog-name (?src ?dst) ...)
       (for-each (lambda (src dst)
		   (hashtable-set! table src dst))
	 ?src ?dst)
       (case-lambda
	((S)
	 (hashtable-ref table S S))
	((S default)
	 (hashtable-ref table S default))))
      (_
       (error who "invalid format of message catalog" pathname)))))


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
