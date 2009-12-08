;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: low level interface to users and groups functions
;;;Date: Tue Jan  6, 2009
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


(library (foreign posix users platform)
  (export
    getuid		getgid
    geteuid		getegid

    setuid		seteuid
    setgid		setegid

    getgroups		setgroups
    initgroups		getgrouplist

    getlogin		cuserid
    )
  (import (rnrs)
    (foreign ffi)
    (foreign posix shared-object)
    (foreign posix sizeof))

  (define-c-functions libc-shared-object
    (getuid		(uid_t getuid (void)))
    (getgid		(gid_t getgid (void)))
    (geteuid		(uid_t geteuid (void)))
    (getegid		(uid_t getegid (void)))
    (getlogin		(char* getlogin (void)))
    (cuserid		(char* cuserid (char*)))
    )

  (define-c-functions/with-errno libc-shared-object
    (setuid		(int setuid (uid_t)))
    (seteuid		(int seteuid (uid_t)))
    (setgid		(int setgid (uid_t)))
    (setegid		(int setegid (uid_t)))
    (getgroups		(int getgroups (int void*)))
    (getgrouplist	(int getgrouplist (char* gid_t void* void*)))
    (setgroups		(int setgroups (int void*)))
    (initgroups		(int initgroups (char* gid_t)))
    ))

;;; end of file
