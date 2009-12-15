;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: entity record wrappers
;;;Date: Sun Dec  6, 2009
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


(library (foreign posix typedefs)
  (export

    ;; wrappers
    file-descriptor file-descriptor? integer->file-descriptor file-descriptor->integer
    FILE* FILE*? pointer->FILE* FILE*->pointer
    fdset fdset? make-fdset pointer->fdset fdset->pointer
    uid uid? integer->uid uid->integer
    gid gid? integer->gid gid->integer
    struct-flock make-struct-flock struct-flock? struct-flock->pointer pointer->struct-flock
    struct-timeval make-struct-timeval struct-timeval? struct-timeval->pointer pointer->struct-timeval

;;; --------------------------------------------------------------------

    <struct-passwd>		<struct-passwd-rtd>
    make-<struct-passwd>	<struct-passwd>?
    <struct-passwd>-name	<struct-passwd>-name-set!
    <struct-passwd>-passwd	<struct-passwd>-passwd-set!
    <struct-passwd>-uid		<struct-passwd>-uid-set!
    <struct-passwd>-gid		<struct-passwd>-gid-set!
    <struct-passwd>-gecos	<struct-passwd>-gecos-set!
    <struct-passwd>-dir		<struct-passwd>-dir-set!
    <struct-passwd>-shell	<struct-passwd>-shell-set!
    pointer->struct-passwd

;;; --------------------------------------------------------------------

    <struct-group>		<struct-group-rtd>
    make-<struct-group>		<struct-group>?
    <struct-group>-name		<struct-group>-name-set!
    <struct-group>-gid		<struct-group>-gid-set!
    <struct-group>-mem		<struct-group>-mem-set!
    pointer->struct-group

;;; --------------------------------------------------------------------

    <struct-utsname>		<struct-utsname-rtd>
    make-<struct-utsname>	<struct-utsname>?
    <struct-utsname>-sysname	<struct-utsname>-sysname-set!
    <struct-utsname>-release	<struct-utsname>-release-set!
    <struct-utsname>-version	<struct-utsname>-version-set!
    <struct-utsname>-machine	<struct-utsname>-machine-set!
    pointer->struct-utsname

;;; --------------------------------------------------------------------

    <struct-fstab>		<struct-fstab-rtd>
    make-<struct-fstab>		<struct-fstab>?
    <struct-fstab>-spec		<struct-fstab>-spec-set!
    <struct-fstab>-file		<struct-fstab>-file-set!
    <struct-fstab>-vfstype	<struct-fstab>-vfstype-set!
    <struct-fstab>-mntops	<struct-fstab>-mntops-set!
    <struct-fstab>-type		<struct-fstab>-type-set!
    <struct-fstab>-freq		<struct-fstab>-freq-set!
    <struct-fstab>-passno	<struct-fstab>-passno-set!
    pointer->struct-fstab

;;; --------------------------------------------------------------------

    <struct-mtab>		<struct-mtab-rtd>
    <struct-mtab>-fsname	<struct-mtab>-fsname-set!
    <struct-mtab>-dir		<struct-mtab>-dir-set!
    <struct-mtab>-type		<struct-mtab>-type-set!
    <struct-mtab>-opts		<struct-mtab>-opts-set!
    <struct-mtab>-freq		<struct-mtab>-freq-set!
    <struct-mtab>-passno	<struct-mtab>-passno-set!
    pointer->struct-mtab

    )
  (import (rnrs)
    (only (foreign cstrings)
	  cstring->string
	  argv->strings)
    (only (foreign ffi pointers)
	  pointer-null?)
    (foreign posix sizeof))


(define-record-type (file-descriptor integer->file-descriptor file-descriptor?)
  (nongenerative nausicaa:posix:file-descriptor)
  (fields (immutable object file-descriptor->integer)))

(define-record-type (FILE* pointer->FILE* FILE*?)
  (nongenerative nausicaa:posix:FILE*)
  (fields (immutable object FILE*->pointer)))

(define-record-type (uid integer->uid uid?)
  (nongenerative nausicaa:posix:uid)
  (fields (immutable object uid->integer)))

(define-record-type (gid integer->gid gid?)
  (nongenerative nausicaa:posix:gid)
  (fields (immutable object gid->integer)))

;;; --------------------------------------------------------------------

(define-record-type (fdset pointer->fdset fdset?)
  (nongenerative nausicaa:posix:fdset)
  (fields (immutable object fdset->pointer)))

(define (make-fdset malloc)
  (pointer->fdset (malloc sizeof-fdset)))

;;; --------------------------------------------------------------------

(define-record-type (struct-flock pointer->struct-flock struct-flock?)
  (nongenerative nausicaa:posix:struct-flock)
  (fields (immutable object struct-flock->pointer)))

(define (make-struct-flock malloc)
  (pointer->struct-flock (malloc sizeof-flock)))


(define-record-type (struct-timeval pointer->struct-timeval struct-timeval?)
  (nongenerative nausicaa:posix:struct-timeval)
  (fields (immutable object struct-timeval->pointer)))

(define (make-struct-timeval malloc)
  (pointer->struct-timeval (malloc sizeof-timeval)))


(define-record-type <struct-passwd>
  (nongenerative nausicaa:posix:struct-passwd)
  (fields (mutable name)
	  (mutable passwd)
	  (mutable uid)
	  (mutable gid)
	  (mutable gecos)
	  (mutable dir)
	  (mutable shell)))

(define <struct-passwd-rtd>
  (record-type-descriptor <struct-passwd>))

(define (pointer->struct-passwd passwd*)
  (make-<struct-passwd> (cstring->string (struct-passwd-pw_name-ref passwd*))
			(cstring->string (struct-passwd-pw_passwd-ref passwd*))
			(integer->uid (struct-passwd-pw_uid-ref passwd*))
			(integer->gid (struct-passwd-pw_gid-ref passwd*))
			(cstring->string (struct-passwd-pw_gecos-ref passwd*))
			(let ((p (struct-passwd-pw_dir-ref passwd*)))
			  (if (pointer-null? p)
			      #f
			    (cstring->string p)))
			(let ((p (struct-passwd-pw_shell-ref passwd*)))
			  (if (pointer-null? p)
			      #f
			    (cstring->string p)))))


(define-record-type <struct-group>
  (nongenerative nausicaa:posix:struct-group)
  (fields (mutable name)
	  (mutable gid)
	  (mutable mem)))

(define <struct-group-rtd>
  (record-type-descriptor <struct-group>))

(define (pointer->struct-group group*)
  (make-<struct-group> (cstring->string (struct-group-gr_name-ref group*))
		       (integer->gid (struct-group-gr_gid-ref group*))
		       (argv->strings (struct-group-gr_mem-ref group*))))


(define-record-type <struct-utsname>
  (nongenerative nausicaa:posix:struct-utsname)
  (fields (mutable sysname)
	  (mutable release)
	  (mutable version)
	  (mutable machine)))

(define <struct-utsname-rtd>
  (record-type-descriptor <struct-utsname>))

(define (pointer->struct-utsname utsname*)
  (make-<struct-utsname> (cstring->string (struct-utsname-sysname-ref utsname*))
			 (cstring->string (struct-utsname-release-ref utsname*))
			 (cstring->string (struct-utsname-version-ref utsname*))
			 (cstring->string (struct-utsname-machine-ref utsname*))))


(define-record-type <struct-fstab>
  (nongenerative nausicaa:posix:struct-fstab)
  (fields (mutable spec)
	  (mutable file)
	  (mutable vfstype)
	  (mutable mntops)
	  (mutable type)
	  (mutable freq)
	  (mutable passno)))

(define <struct-fstab-rtd>
  (record-type-descriptor <struct-fstab>))

(define (pointer->struct-fstab fstab*)
  (make-<struct-fstab> (cstring->string (struct-fstab-fs_spec-ref fstab*))
		       (cstring->string (struct-fstab-fs_file-ref fstab*))
		       (cstring->string (struct-fstab-fs_vfstype-ref fstab*))
		       (cstring->string (struct-fstab-fs_mntops-ref fstab*))
		       (cstring->string (struct-fstab-fs_type-ref fstab*))
		       (struct-fstab-fs_freq-ref fstab*)
		       (struct-fstab-fs_passno-ref fstab*)))


(define-record-type <struct-mtab>
  (nongenerative nausicaa:posix:struct-mtab)
  (fields (mutable fsname)
	  (mutable dir)
	  (mutable type)
	  (mutable opts)
	  (mutable freq)
	  (mutable passno)))

(define <struct-mtab-rtd>
  (record-type-descriptor <struct-mtab>))

(define (pointer->struct-mtab mtab*)
  (make-<struct-mtab> (cstring->string (struct-mtab-mnt_fsname-ref mtab*))
		      (cstring->string (struct-mtab-mnt_dir-ref mtab*))
		      (cstring->string (struct-mtab-mnt_type-ref mtab*))
		      (cstring->string (struct-mtab-mnt_opts-ref mtab*))
		      (struct-mtab-mnt_freq-ref mtab*)
		      (struct-mtab-mnt_passno-ref mtab*)))


;;;; done

)

;;; end of file
