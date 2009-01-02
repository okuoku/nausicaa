;;;
;;;Part of: Nausicaa/Uriparser
;;;Contents: test for URI parsing
;;;Date: Tue Dec 23, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(import (r6rs)
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (uriparser)
  (uriparser sizeof))

(check-set-mode! 'report-failed)



(parameterize ((testname 'condition))

  (check
      (guard (exc (else
		   (list (uriparser-condition? exc)
			 (condition-message exc)
			 (uriparser-symbolic-value exc)
			 (uriparser-numeric-value exc))))
	(raise-uriparser-error 'woppa URI_ERROR_MALLOC))
    => (list #t "requested memory could not be allocated"
	     'URI_ERROR_MALLOC URI_ERROR_MALLOC))

  )



(define the-uri "file:///home/marco/share/sounds/giorgia/giorgia--gocce-di-memoria.flv")
(define the-url "http://www.here.it/the/path/to/file.ext")

(check
    (with-compensations
      (let ((uri	(compensate
			    (malloc-block/c sizeof-UriUriA)
			  (with
			   (uriFreeUriMembersA uri)))))
	(uri-parser uri the-uri)
	(uri->string uri)))
  => the-uri)


;;;; done

(check-report)

;;; end of file