;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling functions for glibc system inspection
;;;Date: Tue Dec 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa glibc system primitives)
  (export
    setfsent		endfsent
    getfsent		getfsspec		getfsfile
    setmntent		endmntent
    getmntent		addmntent

    unsetenv		(rename (platform.clearenv clearenv))
    putenv		putenv*

    pointer-><fstab>
    pointer-><mntent>	<mntent>->pointer
    )
  (import (rnrs)
    (nausicaa language extensions)
    (nausicaa language compensations)
    (only (nausicaa strings) string-index)
    (prefix (nausicaa ffi memory) mem.)
    (nausicaa ffi cstrings)
    (nausicaa ffi errno)
    (prefix (nausicaa posix sizeof) so.)
    (nausicaa posix typedefs)
    (prefix (nausicaa glibc system platform) platform.))


;;;; reading fstab

(define (pointer-><fstab> fstab*)
  (make-<fstab> (cstring->string (struct-fstab-fs_spec-ref fstab*))
		       (cstring->string (struct-fstab-fs_file-ref fstab*))
		       (cstring->string (struct-fstab-fs_vfstype-ref fstab*))
		       (cstring->string (struct-fstab-fs_mntops-ref fstab*))
		       (cstring->string (struct-fstab-fs_type-ref fstab*))
		       (struct-fstab-fs_freq-ref fstab*)
		       (struct-fstab-fs_passno-ref fstab*)))


(define (setfsent)
  (let ((result (platform.setfsent)))
    (if (= 0 result)
	(error 'setfsent "error initialising fstab file iteration")
      result)))

(define (endfsent)
  (let ((result (platform.endfsent)))
    (if (= 0 result)
	(error 'endfsent "error finalising fstab file iteration")
      result)))

(define (getfsent)
  (with-compensations
    (let ((fstab* (platform.getfsent)))
      (if (pointer-null? fstab*)
	  #f
	(pointer-><fstab> fstab*)))))

(define (getfsspec spec)
  (with-compensations
    (let ((fstab* (platform.getfsspec (string->cstring/c spec))))
      (if (pointer-null? fstab*)
	  #f
	(pointer-><fstab> fstab*)))))

(define (getfsfile file)
  (with-compensations
    (let ((fstab* (platform.getfsfile (string->cstring/c file))))
      (if (pointer-null? fstab*)
	  #f
	(pointer-><fstab> fstab*)))))


;;;; reading mtab

(define (pointer-><mntent> mntent*)
  (make-<mntent> (cstring->string (struct-mntent-mnt_fsname-ref mntent*))
			(cstring->string (struct-mntent-mnt_dir-ref mntent*))
			(cstring->string (struct-mntent-mnt_type-ref mntent*))
			(cstring->string (struct-mntent-mnt_opts-ref mntent*))
			(struct-mntent-mnt_freq-ref mntent*)
			(struct-mntent-mnt_passno-ref mntent*)))

(define (<mntent>->pointer mntent malloc)
  (let ((mntent* (malloc sizeof-mntent)))
    (struct-mntent-mnt_fsname-set! mntent* (string->cstring (<mntent>-fsname mntent) malloc))
    (struct-mntent-mnt_dir-set!    mntent* (string->cstring (<mntent>-dir mntent)    malloc))
    (struct-mntent-mnt_type-set!   mntent* (string->cstring (<mntent>-type mntent)   malloc))
    (struct-mntent-mnt_opts-set!   mntent* (string->cstring (<mntent>-opts mntent)   malloc))
    (struct-mntent-mnt_freq-set!   mntent* (<mntent>-freq mntent))
    (struct-mntent-mnt_passno-set! mntent* (<mntent>-passno mntent))
    mntent*))


(define (setmntent file-name open-mode)
  (with-compensations
    (receive (file* errno)
	(platform.setmntent (string->cstring/c file-name)
			    (string->cstring/c open-mode))
      (if (pointer-null? file*)
	  (raise-errno-error 'setmntent errno (list file-name open-mode))
	(pointer->FILE* file*)))))

(define (endmntent stream)
  (when (= 0 (platform.endmntent (FILE*->pointer stream)))
    (error 'endmntent "error finalising mtab file iteration" stream)))

(define (getmntent stream)
  (with-compensations
    (let* ((mntent*	(mem.malloc-block/c sizeof-mntent))
	   (buf.len	4096) ;let's try to play it safe
	   (buf.ptr	(mem.malloc-block/c buf.len)))
      (let ((result* (platform.getmntent_r (FILE*->pointer stream) mntent* buf.ptr buf.len)))
	(if (pointer-null? result*)
	    #f
	  (pointer-><mntent> mntent*))))))

(define (addmntent stream mntent)
  (with-compensations
    (receive (result errno)
	(platform.addmntent (FILE*->pointer stream) (<mntent>->pointer mntent mem.malloc-block/c))
      (if (= 0 result)
	  result
	(raise-errno-error 'addmntent errno (list stream mntent))))))


;;;; environment variables

(define (unsetenv name)
  (with-compensations
    (platform.unsetenv (string->cstring/c name))))

(define (%normalise s)
  (if (symbol? s)
      (symbol->string s)
    s))

(define (putenv assignment)
  (let ((s (%normalise assignment)))
    (if (string-index s #\=)
	(platform.putenv (string->cstring s mem.malloc))
      (assertion-violation 'putenv
	"missing equal sign in process' environment variable assignment"
	assignment))))

(define (putenv* name value)
  (putenv (string-append (%normalise name) "=" (%normalise value))))


;;;; done

)

;;; end of file
