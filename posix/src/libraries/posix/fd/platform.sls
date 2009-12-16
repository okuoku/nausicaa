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


(library (posix fd platform)
  (export
    open	close
    read	write
    pread	pwrite
    lseek
    sync	fsync
    fdatasync
    fcntl	fcntl/ptr	ioctl
    dup	dup2
    pipe	mkfifo
    readv	writev
    mmap	munmap		msync		mremap

    select FD_ZERO FD_SET FD_CLR FD_ISSET
;;;    aio_read aio_write aio_error aio_return aio_fsync aio_suspend aio_cancel lio_listio
    )
  (import (except (rnrs) read write)
    (foreign ffi)
    (posix shared-object)
    (posix sizeof))

  (define-c-functions/with-errno libc-shared-object
    (open		(int open (char* int mode_t)))
    (close		(int close (int)))
    (read		(ssize_t read (int void* size_t)))
    (write		(ssize_t write (int void* size_t)))
    (sync		(int sync (void)))
    (fsync		(int fsync (int)))
    (fdatasync		(int fdatasync (int)))
    (fcntl		(int fcntl (int int int)))
    (fcntl/ptr		(int fcntl (int int void*)))
    (ioctl		(int ioctl (int int int)))
    (dup		(int dup (int)))
    (dup2		(int dup2 (int int)))
    (pipe		(int pipe (pointer)))
    (mkfifo		(int mkfifo (char* mode_t)))
    (readv		(ssize_t readv (int void* int)))
    (writev		(ssize_t writev (int void* int)))
    (munmap		(int munmap (void* size_t)))
    (msync		(int msync (void* size_t int)))
    (mremap		(int mremap (void* size_t size_t int)))
    (select		(int select (int void* void* void* void*)))
    )

  (define-c-functions/with-errno libnausicaa-posix
    (pread		(ssize_t nausicaa_posix_pread (int void* size_t off_t)))
    (pwrite		(ssize_t nausicaa_posix_pwrite (int void* size_t off_t)))
    (lseek		(off_t nausicaa_posix_lseek (int off_t int)))
    (mmap		(void* nausicaa_posix_mmap (void* size_t int int int off_t)))
    ;; (aio_read		(int nausicaa_posix_aio_read (void*)))
    ;; (aio_write		(int nausicaa_posix_aio_write (void*)))
    ;; (aio_error		(int nausicaa_posix_aio_error (void*)))
    ;; (aio_return		(int nausicaa_posix_aio_return (void*)))
    ;; (aio_fsync		(int nausicaa_posix_aio_fsync (int void*)))
    ;; (aio_suspend	(int nausicaa_posix_aio_suspend (void* int void*)))
    ;; (aio_cancel		(int nausicaa_posix_aio_cancel (void* void*)))
    ;; (lio_listio		(int nausicaa_posix_lio_listio (int void* int void*)))
    )

  (define-c-functions libnausicaa-posix
    (FD_ZERO		(void nausicaa_posix_FD_ZERO (void*)))
    (FD_SET		(void nausicaa_posix_FD_SET (int void*)))
    (FD_CLR		(void nausicaa_posix_FD_CLR (int void*)))
    (FD_ISSET		(int nausicaa_posix_FD_ISSET (int void*))))
  )

;;; end of file
