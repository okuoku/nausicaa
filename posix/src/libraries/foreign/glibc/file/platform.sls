;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions that handle files
;;;Date: Thu Jan  1, 2009
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


(library (foreign glibc file platform)
  (export

    ;; times
    lutimes		futimes

    ;; directory access
    scandir		alphasort	versionsort

    ;; temporary files
    mktemp		tempnam		tmpnam		tmpfile

    ;; miscellaneous
    mknod)
  (import (except (rnrs)
		  remove truncate)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign posix shared-object)
    (foreign posix sizeof))

  (define-c-functions/with-errno libc-shared-object
    (lutimes		(int lutimes (char* pointer)))
    (futimes		(int futimes (int pointer)))
    (mktemp		(char* mktemp (char*)))
    (tempnam		(char* tempnam (char* char*)))
    (tmpnam		(char* tmpnam (char*)))
    (tmpfile		(FILE* tmpfile (void))))

  (define-c-functions/with-errno libnausicaa-posix
    (scandir		(int nausicaa_posix_scandir (char* pointer callback callback)))
    (mknod		(int nausicaa_posix_mknod (char* int int))))

  (define alphasort
    (lookup-shared-object* libnausicaa-posix 'nausicaa_posix_alphasort))

  (define versionsort
    (lookup-shared-object* libnausicaa-posix 'nausicaa_posix_versionsort)))

;;; end of file
