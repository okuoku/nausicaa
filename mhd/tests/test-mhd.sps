;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: tests
;;;Date: Wed Dec  2, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (foreign ffi)
  (foreign memory)
  (foreign cstrings)
  (net mhd)
  (foreign net curl)
  (foreign net curl)
  (foreign net curl compensated)
  (prefix (posix typedefs) px:)
  (prefix (posix process) px:)
  (prefix (posix fd) px:)
  (prefix (posix sizeof) px:)
  (prefix (glibc time) px:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing GNU Libmicrohttpd\n")


(define (curl-get-page address-string)
  (with-compensations
    (let* ((handle	(curl-easy-init/c))
	   (out	"")
	   (cb	(lambda (buffer item-size item-count)
		  (let ((len (* item-size item-count)))
		    (set! out (string-append out (cstring->string buffer len)))
		    len))))
      (curl-easy-setopt handle CURLOPT_URL address-string)
      (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
      (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
      (curl-easy-perform handle)
      out)))




(parametrise ((check-test-name	'basic))

  (define port 8888)
  (define page
    "<html><body>Hello, browser!</body></html>")

  (define (access-contents-callback connection url method http-version
				    upload-data.ptr upload-data-len.ptr*
				    new?)
    (with-compensations
      (let* ((page-cstr	(string->cstring/c page)))
	(letrec ((response (compensate
			       (mhd-create-response-from-data (make-<memblock>
							       page-cstr (strlen page-cstr)  #f)
							      MHD_NO ;must free
							      MHD_YES) ;must copy
			     (with
			      (mhd-destroy-response response)))))
	  (mhd-queue-response connection MHD_HTTP_OK response)
	  MHD_YES))))

  (check
      (with-compensations
	(letrec ((daemon	(compensate
				    (mhd-start-daemon (mhd-flags use-debug)
						      8080
						      (make-mhd-accept-policy-callback
						       (lambda args MHD_YES))
						      (make-mhd-access-contents-callback
						       access-contents-callback)
						      (mhd-daemon-config
						       (connection-timeout 10)))
				  (with
				   (mhd-stop-daemon daemon))))
		 (multi		(curl-multi-init/c))
		 (easy		(curl-easy-init/c)))

	  (define result "")
	  (define (%curl-write-callback buffer item-size item-count)
	    (begin0-let ((len (* item-size item-count)))
	      (set! result (string-append result (cstring->string buffer len)))))

	  (curl-easy-setopt easy CURLOPT_URL "http://localhost:8080/index.html")
	  (curl-easy-setopt easy CURLOPT_WRITEFUNCTION
			    (curl-make-write-callback %curl-write-callback))
	  (curl-easy-setopt easy CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-setopt easy CURLOPT_FAILONERROR #t)
	  (curl-easy-setopt easy CURLOPT_HTTP_VERSION CURL_HTTP_VERSION_1_1)
	  (curl-easy-setopt easy CURLOPT_TIMEOUT 150)
	  (curl-easy-setopt easy CURLOPT_CONNECTTIMEOUT 15)

	  (curl-multi-add-handle multi easy)

	  (let ((rd-set	(px:make-fdset malloc-block/c))
		(wr-set	(px:make-fdset malloc-block/c))
		(ex-set	(px:make-fdset malloc-block/c)))

	    (define (%poll)
	      (px:FD_ZERO rd-set)
	      (px:FD_ZERO wr-set)
	      (px:FD_ZERO ex-set)
	      (mhd-get-fdset daemon rd-set wr-set ex-set)
	      (curl-multi-fdset multi
				(px:fdset->pointer rd-set)
				(px:fdset->pointer wr-set)
				(px:fdset->pointer ex-set))
	      (px:select px:FD_SETSIZE rd-set wr-set ex-set pointer-null))

	    (let loop ()
	      (receive (code running)
		  (curl-multi-perform multi)
		(if (= 0 running)
		    (let ((msgs (curl-multi-info-read multi)))
		      (if (null? msgs)
			  #f
			(eq? 'DONE (<curl-message>-code (car msgs)))))
		  (begin
		    (%poll)
		    (mhd-run daemon)
		    (loop)))))
	    (curl-multi-remove-handle/all multi)
	    result)))
    => page)

  #t)


;;;; done

(check-report)

;;; end of file
