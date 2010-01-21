;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: file system functions
;;;Date: Thu Jan  1, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix file)
  (export

    ;; working directory
    getcwd			getcwd-function
    chdir			chdir-function
    fchdir			fchdir-function
    (rename (getcwd pwd))

    ;; directory access
    opendir			opendir-function
    fdopendir			fdopendir-function
    dirfd			dirfd-function

    closedir			closedir-function
    readdir			readdir-function
    readdir_r			readdir_r-function
    rewinddir			rewinddir-function

    telldir			telldir-function
    seekdir			seekdir-function

    (rename (primitive:dirent-name->string	dirent-name->string))
    opendir/c			opendir/c-function
    fdopendir/c			fdopendir/c-function
    directory-entries		directory-entries-function
    directory-entries/fd	directory-entries/fd-function

    ftw				ftw-function
    nftw			nftw-function
    (rename (primitive:make-ftw-callback	make-ftw-callback)
	    (primitive:make-nftw-callback	make-nftw-callback))

    ;; links
    link			link-function
    symlink			symlink-function
    readlink			readlink-function
    realpath			realpath-function

    ;; removing
    unlink			unlink-function
    rmdir			rmdir-function
    remove			remove-function

    ;; renaming
    rename			rename-function

    ;; mkdir
    mkdir			mkdir-function

    ;; changing owner
    chown			chown-function
    fchown			fchown-function

    ;; changing permissions
    umask			umask-function
    chmod			chmod-function
    fchmod			fchmod-function
    (rename (primitive:getumask	getumask))

    ;; access test
    access			access-function

    ;; file times
    utime			utime-function
    utimes			utimes-function

    ;; file size
    file-size			file-size-function
    ftruncate			ftruncate-function
    truncate			truncate-function

    ;; temporary files
    mkstemp			mkstemp-function
    mkdtemp			mkdtemp-function

    ;; stat
    stat		stat-function
    lstat		lstat-function
    fstat		fstat-function

    (rename (primitive:S_ISDIR		S_ISDIR)
	    (primitive:S_ISCHR		S_ISCHR)
	    (primitive:S_ISBLK		S_ISBLK)
	    (primitive:S_ISREG		S_ISREG)
	    (primitive:S_ISFIFO		S_ISFIFO)
	    (primitive:S_ISLNK		S_ISLNK)
	    (primitive:S_ISSOCK		S_ISSOCK)

	    (primitive:S_TYPEISMQ	S_TYPEISMQ)
	    (primitive:S_TYPEISSEM	S_TYPEISSEM)
	    (primitive:S_TYPEISSHM	S_TYPEISSHM))

    file-is-directory?			file-is-directory?-function
    file-is-character-special?		file-is-character-special?-function
    file-is-block-special?		file-is-block-special?-function
    file-is-regular?			file-is-regular?-function
    file-is-fifo?			file-is-fifo?-function
    file-is-symbolic-link?		file-is-symbolic-link?-function
    file-is-socket?			file-is-socket?-function
    file-is-message-queue?		file-is-message-queue?-function
    file-is-semaphore?			file-is-semaphore?-function
    file-is-shared-memory?		file-is-shared-memory?-function

    file-user-readable?			file-user-readable?-function
    file-user-writable?			file-user-writable?-function
    file-user-executable?		file-user-executable?-function
    file-group-readable?		file-group-readable?-function
    file-group-writable?		file-group-writable?-function
    file-group-executable?		file-group-executable?-function
    file-other-readable?		file-other-readable?-function
    file-other-writable?		file-other-writable?-function
    file-other-executable?		file-other-executable?-function
    file-setuid?			file-setuid?-function
    file-setgid?			file-setgid?-function
    file-sticky?			file-sticky?-function

    lfile-user-readable?		lfile-user-readable?-function
    lfile-user-writable?		lfile-user-writable?-function
    lfile-user-executable?		lfile-user-executable?-function
    lfile-group-readable?		lfile-group-readable?-function
    lfile-group-writable?		lfile-group-writable?-function
    lfile-group-executable?		lfile-group-executable?-function
    lfile-other-readable?		lfile-other-readable?-function
    lfile-other-writable?		lfile-other-writable?-function
    lfile-other-executable?		lfile-other-executable?-function
    lfile-setuid?			lfile-setuid?-function
    lfile-setgid?			lfile-setgid?-function
    lfile-sticky?			lfile-sticky?-function

    file-permissions			file-permissions-function
    lfile-permissions			lfile-permissions-function
    )
  (import (except (rnrs) remove truncate)
    (receive)
    (compensations)
    (only (foreign ffi pointers) pointer-null?)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (posix sizeof)
    (posix typedefs)
    (posix helpers)
    (prefix (posix file primitives) primitive:)
    (only (posix fd) open close lseek)
    (prefix (only (posix file platform) stat fstat sizeof-stat) platform:))


;; working directory

(define-parametrised getcwd)
(define-parametrised chdir pathname)
(define-parametrised fchdir fd)

;; directory access

(define-parametrised opendir pathname)
(define-parametrised fdopendir fd)
(define-parametrised opendir/c pathname)
(define-parametrised fdopendir/c fd)
(define-parametrised dirfd stream)
(define-parametrised closedir stream)
(define-parametrised readdir stream)
(define-parametrised readdir_r stream)
(define-parametrised rewinddir stream)
(define-parametrised telldir stream)
(define-parametrised seekdir stream position)

(define-parametrised ftw pathname selector-callback descriptors)
(define-parametrised nftw pathname selector-callback descriptors flag)
(define-parametrised directory-entries pathname)
(define-parametrised directory-entries/fd fd)

;; links

(define-parametrised link oldname newname)
(define-parametrised symlink oldname newname)
(define-parametrised readlink pathname)
(define-parametrised realpath pathname)

;; changing owner

(define-parametrised chown pathname owner-id group-id)
(define-parametrised fchown fd owner-id group-id)

;; changing permissions

(define-parametrised umask mask)
(define-parametrised chmod pathname mode)
(define-parametrised fchmod fd mode)

;; testing access

(define-parametrised access fd mask)

;; file times

(define-parametrised utime	((pathname access-time modification-time)
				 (pathname)))
(define-parametrised utimes
  ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
   (pathname)))

;; file size

(define-parametrised file-size obj)
(define-parametrised ftruncate obj length)
(define-parametrised truncate pathname length)

;; removing

(define-parametrised unlink pathname)
(define-parametrised remove pathname)
(define-parametrised rmdir pathname)

;; renaming

(define-parametrised rename oldname newname)

;; making directories

(define-parametrised mkdir pathname mode)

;; temporary files

(define-parametrised mkstemp template)
(define-parametrised mkdtemp template)



;;;; stat functions

(define-parametrised stat pathname)
(define-parametrised lstat pathname)
(define-parametrised fstat fd)

(define-parametrised file-is-directory? obj)
(define-parametrised file-is-character-special? obj)
(define-parametrised file-is-block-special? obj)
(define-parametrised file-is-regular? obj)
(define-parametrised file-is-fifo? obj)
(define-parametrised file-is-socket? obj)
(define-parametrised file-is-symbolic-link? mode/pathname)
(define-parametrised file-is-message-queue? obj)
(define-parametrised file-is-semaphore? obj)
(define-parametrised file-is-shared-memory? obj)
(define-parametrised file-user-readable? obj)
(define-parametrised file-user-writable? obj)
(define-parametrised file-user-executable? obj)
(define-parametrised file-group-readable? obj)
(define-parametrised file-group-writable? obj)
(define-parametrised file-group-executable? obj)
(define-parametrised file-other-readable? obj)
(define-parametrised file-other-writable? obj)
(define-parametrised file-other-executable? obj)
(define-parametrised file-setuid? obj)
(define-parametrised file-setgid? obj)
(define-parametrised file-sticky? obj)
(define-parametrised lfile-user-readable? obj)
(define-parametrised lfile-user-writable? obj)
(define-parametrised lfile-user-executable? obj)
(define-parametrised lfile-group-readable? obj)
(define-parametrised lfile-group-writable? obj)
(define-parametrised lfile-group-executable? obj)
(define-parametrised lfile-other-readable? obj)
(define-parametrised lfile-other-writable? obj)
(define-parametrised lfile-other-executable? obj)
(define-parametrised lfile-setuid? obj)
(define-parametrised lfile-setgid? obj)
(define-parametrised lfile-sticky? obj)
(define-parametrised file-permissions obj)
(define-parametrised lfile-permissions obj)


;;;; done

)

;;; end of file
