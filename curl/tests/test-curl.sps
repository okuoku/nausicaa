;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: tests for cURL compound library
;;;Date: Fri Nov 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (foreign net curl)
  (foreign net curl compensated)
  (foreign memory)
  (foreign cstrings)
  (strings)
  (irregex)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing cURL\n")

(check
    (curl-global-init CURL_GLOBAL_ALL)
  => CURLE_OK)


(parametrise ((check-test-name	'version))

  (check
      (let ((s (curl-version)))
	(display s)
	(newline)
	(string? s))
    => #t)

  (check
      (let ((s (curl-version-info)))
	(display "version-info age:\t\t")
	(write (<curl-version-info>-age s))
	(newline)

	(display "version-info version:\t\t")
	(write (<curl-version-info>-version s))
	(newline)

	(display "version-info version-num:\t")
	(write (number->string (<curl-version-info>-version-num s) 16))
	(newline)

	(display "version-info host:\t\t")
	(write (<curl-version-info>-host s))
	(newline)

	(display "version-info features:\n\t")
	(write (<curl-version-info>-features s))
	(newline)

	(display "version-info ssl-version:\t")
	(write (<curl-version-info>-ssl-version s))
	(newline)

	(display "version-info ssl-version-num:\t")
	(write (<curl-version-info>-ssl-version-num s))
	(newline)

	(display "version-info libz-version:\t")
	(write (<curl-version-info>-libz-version s))
	(newline)

	(display "version-info protocols:\n\t")
	(write (<curl-version-info>-protocols s))
	(newline)

	(display "version-info ares:\t\t")
	(write (<curl-version-info>-ares s))
	(newline)

	(display "version-info ares-num:\t\t")
	(write (<curl-version-info>-ares-num s))
	(newline)

	(display "version-info iconv:\t\t")
	(write (<curl-version-info>-iconv s))
	(newline)

	(display "version-info libssh-version:\t")
	(write (<curl-version-info>-libssh-version s))
	(newline)

	(<curl-version-info>? s))
    => #t)

  #t)


(parametrise ((check-test-name	'easy-perform))

  (check
      (with-compensations
	(let* ((handle	(curl-easy-init/c))
	       (out	"")
	       (cb	(lambda (buffer item-size item-count)
			  (let ((len (* item-size item-count)))
			    (set! out (string-append out (cstring->string buffer len)))
			    len))))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/index.html")
	  (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
	  (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-perform handle)
	  out))
    => "<html><body><p>proof page</p></body></html>\n")

  (check
      (with-compensations
	(let* ((handle	(curl-easy-init/c))
	       (clone	(curl-easy-duphandle/c handle))
	       (out	"")
	       (cb	(lambda (buffer item-size item-count)
			  (let ((len (* item-size item-count)))
			    (set! out (string-append out (cstring->string buffer len)))
			    len))))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/index.html")
	  (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
	  (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-perform handle)
	  out))
    => "<html><body><p>proof page</p></body></html>\n")

  (check
      (with-compensations
	(let* ((handle	(curl-easy-init/c))
	       (out	"")
	       (cb	(lambda (buffer item-size item-count)
			  (let ((len (* item-size item-count)))
			    (set! out (string-append out (cstring->string buffer len)))
			    len))))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/index.html")
	  (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
	  (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-pause handle (curl-pause-mask ALL))
	  (curl-easy-perform handle)
	  out))
    => "<html><body><p>proof page</p></body></html>\n")

  #t)


(parametrise ((check-test-name	'easy-send/recv))

;;; --------------------------------------------------------------------

  (check		;send/recv string
      (with-compensations
	(let* ((handle	(curl-easy-init/c)))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/")
	  (curl-easy-setopt handle CURLOPT_CONNECT_ONLY #t)
;;;	  (curl-easy-setopt handle CURLOPT_VERBOSE #t)
	  (curl-easy-perform handle)
	  (curl-easy-send handle "GET index.html HTTP/1.1\r\nHost: localhost\r\n\r\n")
	  (let loop ()
	    (let ((s (curl-easy-recv/string handle 256)))
	      (if (not s)
		  (loop)
		(irregex-match-data?
		 (irregex-search "<html><body><p>proof page</p></body></html>\n" s)))))))
    => #t)

  (check		;send/recv bytevector
      (with-compensations
	(let* ((handle	(curl-easy-init/c)))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/")
	  (curl-easy-setopt handle CURLOPT_CONNECT_ONLY #t)
;;;	  (curl-easy-setopt handle CURLOPT_VERBOSE #t)
	  (curl-easy-perform handle)
	  (curl-easy-send handle (string->utf8 "GET index.html HTTP/1.1\r\nHost: localhost\r\n\r\n"))
	  (let loop ()
	    (let ((s (curl-easy-recv/bytevector handle 256)))
	      (if (not s)
		  (loop)
		(irregex-match-data?
		 (irregex-search "<html><body><p>proof page</p></body></html>\n"
				 (utf8->string s))))))))
    => #t)

  (check		;send/recv memblock
      (with-compensations
	(let* ((handle	(curl-easy-init/c)))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/")
	  (curl-easy-setopt handle CURLOPT_CONNECT_ONLY #t)
;;;	  (curl-easy-setopt handle CURLOPT_VERBOSE #t)
	  (curl-easy-perform handle)
	  (curl-easy-send handle
			  (bytevector->memblock
			   (string->utf8 "GET index.html HTTP/1.1\r\nHost: localhost\r\n\r\n")
			   malloc-block/c))
	  (let loop ()
	    (let ((s (curl-easy-recv/memblock handle 256)))
	      (if (not s)
		  (loop)
		(irregex-match-data?
		 (irregex-search "<html><body><p>proof page</p></body></html>\n"
				 (utf8->string (memblock->bytevector s)))))))))
    => #t)

  #t)


(parametrise ((check-test-name	'easy-getinfo))

  (check
      (with-compensations
	(let* ((handle	(curl-easy-init/c))
	       (out	"")
	       (cb	(lambda (buffer item-size item-count)
			  (let ((len (* item-size item-count)))
			    (set! out (string-append out (cstring->string buffer len)))
			    len))))
	  (curl-easy-setopt handle CURLOPT_URL "http://localhost:8080/index.html")
	  (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
	  (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-perform handle)
	  (list (curl-easy-getinfo handle CURLINFO_EFFECTIVE_URL)
		(curl-easy-getinfo handle CURLINFO_RESPONSE_CODE))))
    => '("http://localhost:8080/index.html" 200))

  #t)


(parametrise ((check-test-name	'escaping))

  (check
      (with-compensations
	(let ((handle	(curl-easy-init/c)))
	  (curl-easy-escape handle "http://www.marco.it/")))
    => "http%3A%2F%2Fwww%2Emarco%2Eit%2F")

  (check
      (with-compensations
	(let ((handle	(curl-easy-init/c)))
	  (curl-easy-escape handle "ciao")))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let ((handle	(curl-easy-init/c)))
	  (curl-easy-unescape handle "http%3A%2F%2Fwww%2Emarco%2Eit%2F")))
    => "http://www.marco.it/")

  (check
      (with-compensations
	(let ((handle	(curl-easy-init/c)))
	  (curl-easy-unescape handle "ciao")))
    => "ciao")

  #t)


;;;; done

(check-report)

;;; end of file
