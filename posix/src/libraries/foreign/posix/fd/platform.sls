;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: platform interface to POSIX fd functions
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


(library (foreign posix fd platform)
  (export
    open	close
    read	write
    pread	pwrite
    lseek
    sync	fsync
    fdatasync
    fcntl	ioctl
    dup	dup2
    pipe	mkfifo)
  (import (except (rnrs) read write)
    (foreign ffi)
    (foreign posix shared-object)
    (foreign posix sizeof))

  (define-c-functions/with-errno libc-shared-object
    (open		(int open (char* int mode_t)))
    (close		(int close (int)))
    (read		(ssize_t read (int void* size_t)))
    (write		(ssize_t write (int void* size_t)))
    (sync		(int sync (void)))
    (fsync		(int fsync (int)))
    (fdatasync		(int fdatasync (int)))
    (fcntl		(int fcntl (int int int)))
    (ioctl		(int ioctl (int int int)))
    (dup		(int dup (int)))
    (dup2		(int dup2 (int int)))
    (pipe		(int pipe (pointer)))
    (mkfifo		(int mkfifo (char* mode_t))))

  (define-c-functions/with-errno libnausicaa-posix
    (pread		(ssize_t nausicaa_posix_pread (int void* size_t off_t)))
    (pwrite		(ssize_t nausicaa_posix_pwrite (int void* size_t off_t)))
    (lseek		(off_t nausicaa_posix_lseek (int off_t int)))))

;;; end of file
