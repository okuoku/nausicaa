;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: library of cstring functions
;;;Date: Thu Dec 18, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa glibc cstrings platform)
  (export

    ;; length
    strlen		wcslen
    strnlen		wcsnlen

    ;; copying and concatenation
    memcpy		wmemcpy
    mempcpy		wmempcpy
    memmove		wmemmove
    memccpy
    memset		wmemset
    strcpy		wcscpy
    strncpy		wcsncpy
    strdup		wcsdup
    strndup
    stpcpy		wcpcpy
    stpncpy		wcpncpy
;;;    strdupa
;;;    strndupa
    strcat		wcscat
    strncat		wcsncat
    bcopy
    bzero

    ;; string/array comparison
    memcmp		wmemcmp
    strcmp		wcscmp
    strcasecmp		wcscasecmp
    strncmp		wcsncmp
    strncasecmp		wcsncasecmp
    strverscmp

    ;; collation
    strcoll		wcscoll
    strxfrm		wcsxfrm

    ;; search
    memchr		wmemchr
    rawmemchr
    memrchr
    strchr		wcschr
    strchrnul		wcschrnul
    strrchr		wcsrchr
    strstr		wcsstr
    strcasestr
    memmem
    strspn		wcsspn
    strcspn		wcscspn
    strpbrk		wcspbrk

    ;; finding tokens
    strtok		wcstok
    strtok_r
    strsep
    basename
    dirname

    ;; misc
    strfry
    memfrob
    l64a		a64l)
  (import (rnrs)
    (nausicaa ffi)
    (nausicaa ffi sizeof)
    (nausicaa posix sizeof))

  (define wchar_t* 'void*)

  ;; length
  (define-c-functions libc-shared-object
    (strlen		(size_t strlen (char*)))
    (wcslen		(size_t wcslen (wchar_t*)))
    (strnlen		(size_t strnlen (char* size_t)))
    (wcsnlen		(size_t wcsnlen (wchar_t* size_t))))

  ;; copying and concatenation
  (define-c-functions libc-shared-object
    (memcpy		(void* memcpy (void* void* size_t)))
    (wmemcpy		(wchar_t* wmemcpy (wchar_t* wchar_t* size_t)))
    (mempcpy		(void* mempcpy (void* void* size_t)))
    (wmempcpy		(wchar_t* wmempcpy (wchar_t* wchar_t* size_t)))
    (memmove		(void* memmove (void* void* size_t)))
    (wmemmove		(wchar_t* wmemmove (wchar_t* wchar_t* size_t)))
    (memccpy		(void* memccpy (void* void* int size_t)))
    (memset		(void* memset (void* int size_t)))
    (wmemset		(wchar_t* wmemset (wchar_t* wchar_t size_t)))
    (strcpy		(char* strcpy (char* char*)))
    (wcscpy		(wchar_t* wcscpy (wchar_t* wchar_t*)))
    (strncpy		(char* strncpy (char* char* size_t)))
    (wcsncpy		(wchar_t* wcsncpy (wchar_t* wchar_t* size_t)))
    (strdup		(char* strdup (char*)))
    (wcsdup		(wchar_t* wcsdup (wchar_t*)))
    (strndup		(char* strndup (char* size_t)))
    (stpcpy		(char* stpcpy (char* char*)))
    (wcpcpy		(wchar_t* wcpcpy (wchar_t* wchar_t*)))
    (stpncpy		(char* stpncpy (char* char* size_t)))
    (wcpncpy		(wchar_t* wcpncpy (wchar_t* wchar_t* size_t)))
;;; These are macros.
;;;    (strdupa		(char* strdupa (char*)))
;;;    (strndupa		(char* strndupa (char* size_t)))
    (strcat		(char* strcat (char* char*)))
    (wcscat		(wchar_t* wcscat (wchar_t* wchar_t*)))
    (strncat		(char* strncat (char* char* size_t)))
    (wcsncat		(wchar_t* wcsncat (wchar_t* wchar_t* size_t)))
    (bcopy		(void bcopy (void* void* size_t)))
    (bzero		(void bzero (void* size_t))))

  ;; string/array comparison
  (define-c-functions libc-shared-object
    (memcmp		(int memcmp (void* void* size_t)))
    (wmemcmp		(int wmemcmp (wchar_t* wchar_t* size_t)))
    (strcmp		(int strcmp (char* char*)))
    (wcscmp		(int wcscmp (wchar_t* wchar_t*)))
    (strcasecmp		(int strcasecmp (char* char*)))
    (wcscasecmp		(int wcscasecmp (wchar_t* wchar_t*)))
    (strncmp		(int strncmp (char* char* size_t)))
    (wcsncmp		(int wcsncmp (wchar_t* wchar_t* size_t)))
    (strncasecmp	(int strncasecmp (char* char* size_t)))
    (wcsncasecmp	(int wcsncasecmp (wchar_t* wchar_t* size_t)))
    (strverscmp		(int strverscmp (char* char*))))

  ;; collation
  (define-c-functions libc-shared-object
    (strcoll		(int strcoll (char* char*)))
    (wcscoll		(int wcscoll (wchar_t* wchar_t*)))
    (strxfrm		(size_t strxfrm (char* char* size_t)))
    (wcsxfrm		(size_t wcsxfrm (wchar_t* wchar_t* size_t))))

  ;; search
  (define-c-functions libc-shared-object
    (memchr		(char* memchr (char* int size_t)))
    (wmemchr		(wchar_t* wmemchr (wchar_t* wchar_t size_t)))
    (rawmemchr		(void* rawmemchr (void* int)))
    (memrchr		(void* memrchr (void* int size_t)))
    (strchr		(char* strchr (char* int)))
    (wcschr		(wchar_t* wcschr (wchar_t* int)))
    (strchrnul		(char* strchrnul (char* int)))
    (wcschrnul		(wchar_t* wcschrnul (wchar_t* wchar_t)))
    (strrchr		(char* strrchr (char* int)))
    (wcsrchr		(wchar_t* wcsrchr (wchar_t* wchar_t)))
    (strstr		(char* strstr (char* char*)))
    (wcsstr		(wchar_t* wcsstr (wchar_t* wchar_t*)))
    (strcasestr		(char* strcasestr (char* char*)))
    (memmem		(void* memmem (void* size_t void* size_t)))
    (strspn		(size_t strspn (char* char*)))
    (wcsspn		(size_t wcsspn (wchar_t* wchar_t*)))
    (strcspn		(size_t strcspn (char* char*)))
    (wcscspn		(size_t wcscspn (wchar_t* wchar_t*)))
    (strpbrk		(char* strpbrk (char* char*)))
    (wcspbrk		(wchar_t* wcspbrk (wchar_t* wchar_t*))))

  ;; finding tokens
  (define-c-functions libc-shared-object
    (strtok		(char* strtok (char* char*)))
    (wcstok		(wchar_t* wcstok (wchar_t* wchar_t*)))
    (strtok_r		(char* strtok_r (char* char* void*)))
    (strsep		(char* strsep (void* char*)))
    (basename		(char* basename (char*)))
    (dirname		(char* dirname (char*))))

  ;; misc
  (define-c-functions libc-shared-object
    (strfry		(char* strfry (char*)))
    (memfrob		(void* memfrob (void* size_t)))
    (l64a		(char* l64a (long)))
    (a64l		(long a64l (char*)))))

;;; end of file
