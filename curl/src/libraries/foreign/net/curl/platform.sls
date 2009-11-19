;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: map to foreign functions
;;;Date: Mon Nov 16, 2009
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


(library (foreign net curl platform)
  (export
    )
  (import (rnrs)
    (foreign ffi)
    (foreign net curl sizeof)
    (foreign net curl shared-object))

  (define dummy
    (shared-object curl-shared-object))


(define int*					'pointer)
(define CURL*					'pointer)
(define struct-curl_httppost*			'pointer)
(define struct-curl_httppost**			'pointer)
(define struct-curl_slist*			'pointer)


;;This is variadic.
;;
;; (define-c-function curl_formadd
;;   (CURLFORMcode curl_formadd (struct-curl_httppost** struct-curl_httppost** ...)))


(define-c-function curl_formget
  (int curl_formget (struct-curl_httppost* void* curl_formget_callback)))

(define-c-function curl_formfree
  (void curl_formfree (struct-curl_httppost*)))

(define-c-function curl_getenv
  (char* curl_getenv (char*)))

(define-c-function curl_version
  (char* curl_version (void)))

(define-c-function curl_easy_escape
  (char* curl_easy_escape (CURL* handle char* int)))

(define-c-function curl_escape
  (char* curl_escape (char* int)))

(define-c-function curl_easy_unescape
  (char* curl_easy_unescape (CURL*  char* int int*)))

(define-c-function curl_unescape
  (char* curl_unescape (char* int)))

(define-c-function curl_free
  (void curl_free (void*)))

(define-c-function curl_global_init
  (CURLcode curl_global_init (long)))

(define-c-function curl_global_init_mem
  (CURLcode curl_global_init_mem (long curl_malloc_callback curl_free_callback
				       curl_realloc_callback curl_strdup_callback
				       curl_calloc_callback)))

(define-c-function curl_global_cleanup
  (void curl_global_cleanup (void)))

(define-c-function curl_slist_append
  (struct-curl_slist* curl_slist_append (struct-curl_slist* char*)))

(define-c-function curl_slist_free_all
  (void curl_slist_free_all (struct-curl_slist*)))

;;*FIXME* How do they implement "time_t"?
(define time_t 'long)
(define-c-function curl_getdate
  (time_t curl_getdate (char* time_t*)))



;;;; done

)

;;; end of file
