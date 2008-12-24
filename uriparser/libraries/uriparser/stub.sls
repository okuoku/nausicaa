;;;
;;;Part of: Nausicaa/Uriparser
;;;Contents: stub library with foreign functions interface
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

(library (uriparser stub)
  (export
    uriParseUriExA
    uriParseUriA
    uriFreeUriMembersA
    uriEscapeExA
    uriEscapeA
    uriUnescapeInPlaceExA
    uriUnescapeInPlaceA
    uriAddBaseUriA
    uriRemoveBaseUriA
    uriEqualsUriA
    uriToStringCharsRequiredA
    uriToStringA
    uriNormalizeSyntaxMaskRequiredA
    uriNormalizeSyntaxExA
    uriNormalizeSyntaxA
    uriUnixFilenameToUriStringA
    uriWindowsFilenameToUriStringA
    uriUriStringToUnixFilenameA
    uriUriStringToWindowsFilenameA
    uriComposeQueryCharsRequiredA
    uriComposeQueryCharsRequiredExA
    uriComposeQueryA
    uriComposeQueryExA
    uriComposeQueryMallocA
    uriComposeQueryMallocExA
    uriDissectQueryMallocA
    uriDissectQueryMallocExA
    uriFreeQueryListA)
  (import (r6rs)
    (uriel foreign)
    (uriparser sizeof))

  (define uriparser-lib
    (let ((o (open-shared-object 'liburiparser.so)))
      (shared-object o)
      o))


;;;; functions

(define-c-function uriParseUriExA
  (int uriParseUriExA (pointer char* char*)))

(define-c-function uriParseUriA
  (int uriParseUriA (pointer char*)))

(define-c-function uriFreeUriMembersA
  (void uriFreeUriMembersA (pointer)))

(define-c-function  uriEscapeExA
  (char* uriEscapeExA (char* char* char* UriBool UriBool)))

(define-c-function  uriEscapeA
  (char* uriEscapeA (char* char* UriBool UriBool)))

(define-c-function  uriUnescapeInPlaceExA
  (char* uriUnescapeInPlaceExA (char* UriBool UriBreakConversion)))

(define-c-function  uriUnescapeInPlaceA
  (char* uriUnescapeInPlaceA (char*)))

(define-c-function uriAddBaseUriA
  (int uriAddBaseUriA (pointer pointer pointer)))

(define-c-function uriRemoveBaseUriA
  (int uriRemoveBaseUriA (pointer pointer pointer UriBool)))

(define-c-function uriEqualsUriA
  (UriBool uriEqualsUriA (pointer pointer)))

(define-c-function uriToStringCharsRequiredA
  (int uriToStringCharsRequiredA (pointer pointer)))

(define-c-function uriToStringA
  (int uriToStringA (char* pointer int pointer)))

(define-c-function uriNormalizeSyntaxMaskRequiredA
  (unsigned-int uriNormalizeSyntaxMaskRequiredA (pointer)))

(define-c-function uriNormalizeSyntaxExA
  (int uriNormalizeSyntaxExA (pointer unsigned-int)))

(define-c-function uriNormalizeSyntaxA
  (int uriNormalizeSyntaxA (pointer)))

(define-c-function uriUnixFilenameToUriStringA
  (int uriUnixFilenameToUriStringA (char* char*)))

(define-c-function uriWindowsFilenameToUriStringA
  (int uriWindowsFilenameToUriStringA (char* char*)))

(define-c-function uriUriStringToUnixFilenameA
  (int uriUriStringToUnixFilenameA (char* char*)))

(define-c-function uriUriStringToWindowsFilenameA
  (int uriUriStringToWindowsFilenameA (char* char*)))

(define-c-function uriComposeQueryCharsRequiredA
  (int uriComposeQueryCharsRequiredA (pointer pointer)))

(define-c-function uriComposeQueryCharsRequiredExA
  (int uriComposeQueryCharsRequiredExA (pointer pointer UriBool UriBool)))

(define-c-function uriComposeQueryA
  (int uriComposeQueryA (char* pointer int pointer)))

(define-c-function uriComposeQueryExA
  (int uriComposeQueryExA (char* pointer int pointer UriBool UriBool)))

(define-c-function uriComposeQueryMallocA
  (int uriComposeQueryMallocA (pointer pointer)))

(define-c-function uriComposeQueryMallocExA
  (int uriComposeQueryMallocExA (pointer pointer UriBool UriBool)))

(define-c-function uriDissectQueryMallocA
  (int uriDissectQueryMallocA (pointer pointer char* char*)))

(define-c-function uriDissectQueryMallocExA
  (int uriDissectQueryMallocExA (pointer pointer char* char* UriBool UriBreakConversion)))

(define-c-function uriFreeQueryListA
  (void uriFreeQueryListA (pointer)))



;;;; done

)

;;; end of file
