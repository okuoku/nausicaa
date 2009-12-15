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

    getpwuid		getpwuid_r
    getpwnam		getpwnam_r
    fgetpwent		fgetpwent_r

    getgrgid		getgrgid_r
    getgrnam		getgrnam_r
    fgetgrent		fgetgrent_r
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
    (getpwuid		(void* getpwuid (uid_t)))
    (getpwuid_r		(void* getpwuid_r (uid_t void* char* size_t void*)))
    (getpwnam		(void* getpwnam (char*)))
    (getpwnam_r		(void* getpwnam_r (char* void* char* size_t void*)))
    (fgetpwent		(void* fgetpwent (FILE*)))
    (fgetpwent_r	(int fgetpwent_r (FILE* void* char* size_t void*)))
    (getgrgid		(void* getgrgid (uid_t)))
    (getgrgid_r		(void* getgrgid_r (uid_t void* char* size_t void*)))
    (getgrnam		(void* getgrnam (char*)))
    (getgrnam_r		(void* getgrnam_r (char* void* char* size_t void*)))
    (fgetgrent		(void* fgetgrent (FILE*)))
    (fgetgrent_r	(int fgetgrent_r (FILE* void* char* size_t void*)))
    ))

;;; end of file
