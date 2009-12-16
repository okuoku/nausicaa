;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: library of cstring functions
;;;Date: Thu Dec 18, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (glibc cstrings platform)
  (export
    memchr		memrchr
    strchr		strrchr
    strstr		memmem)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof))


(define-c-functions libc-shared-object
  (memchr		(char* memchr (char* int size_t)))
  (memrchr		(char* memrchr (char* int size_t)))
  (strchr		(char* strchr (char* int)))
  (strrchr		(char* strchr (char* int)))
  (strstr		(char* strstr (char* int)))
  (memmem		(void* memmem (void* size_t void* size_t))))


;;;; done

)

;;; end of file
