;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the stat functions
;;;Date: Fri Jan  2, 2009
;;;
;;;Abstract
;;;
;;;	This is an interface  to "stat()", "fstat()" and "lstat()" which
;;;	makes use of the  stubs functions in "libnausicaa-posix.so" from
;;;	the  Nausicaa/Stubs project.
;;;
;;;	  It is a misfortune that a stubs library is needed but invoking
;;;	"dlsym()"  on  the stat  functions  fails  with  all the  Scheme
;;;	implementations.  If someone has a solution: email me!!!
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
(library (foreign posix file stat)
  (export
    platform-stat
    platform-lstat
    platform-fstat

    stat	primitive-stat		primitive-stat-function
    lstat	primitive-lstat		primitive-lstat-function
    fstat	primitive-fstat		primitive-fstat-function

    S_ISDIR	file-is-directory?
    S_ISCHR	file-is-character-special?
    S_ISBLK	file-is-block-special?
    S_ISREG	file-is-regular?
    S_ISFIFO	file-is-fifo?
    S_ISLNK	file-is-symbolic-link?
    S_ISSOCK	file-is-socket?

    S_TYPEISMQ	file-is-message-queue?
    S_TYPEISSEM	file-is-semaphore?
    S_TYPEISSHM	file-is-shared-memory?

    file-user-readable?		file-user-writable?	file-user-executable?
    file-group-readable?	file-group-writable?	file-group-executable?
    file-other-readable?	file-other-writable?	file-other-executable?
    file-setuid?		file-setgid?		file-sticky?

    lfile-user-readable?	lfile-user-writable?	lfile-user-executable?
    lfile-group-readable?	lfile-group-writable?	lfile-group-executable?
    lfile-other-readable?	lfile-other-writable?	lfile-other-executable?
    lfile-setuid?		lfile-setgid?		lfile-sticky?

    file-permissions		lfile-permissions

    make-struct-stat struct-stat?

    struct-stat-mode
    struct-stat-ino
    struct-stat-dev
    struct-stat-nlink
    struct-stat-uid
    struct-stat-gid
    struct-stat-size
    struct-stat-atime
    struct-stat-atime_usec
    struct-stat-mtime
    struct-stat-mtime_usec
    struct-stat-ctime
    struct-stat-ctime_usec
    struct-stat-blocks
    struct-stat-blksize
    )
  (import (nausicaa)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (except (foreign posix sizeof)
	    sizeof-struct-stat)
    (compensations))

  (define stub-lib
    (let ((o (open-shared-object 'libnausicaa-posix1.so)))
      (shared-object o)
      o))


;;;; stat record

(define-record-type struct-stat
  (fields (immutable mode)
	  (immutable ino)
	  (immutable dev)
	  (immutable nlink)
	  (immutable uid)
	  (immutable gid)
	  (immutable size)
	  (immutable atime)
	  (immutable atime_usec)
	  (immutable mtime)
	  (immutable mtime_usec)
	  (immutable ctime)
	  (immutable ctime_usec)
	  (immutable blocks)
	  (immutable blksize)))

(define (struct-stat->record *struct-stat)
  ;;Some "struct  stat" field  may be unimplemented,  so we  default its
  ;;value to #f.
  (let-syntax ((get	(syntax-rules ()
			  ((_ ?getter)
			   (guard (exc (else #f))
			     (?getter *struct-stat))))))
    (make-struct-stat
     (get struct-stat-st_mode-ref)
     (get struct-stat-st_ino-ref)
     (get struct-stat-st_dev-ref)
     (get struct-stat-st_nlink-ref)
     (get struct-stat-st_uid-ref)
     (get struct-stat-st_gid-ref)
     (get struct-stat-st_size-ref)
     (get struct-stat-st_atime-ref)
     (get struct-stat-st_atime_usec-ref)
     (get struct-stat-st_mtime-ref)
     (get struct-stat-st_mtime_usec-ref)
     (get struct-stat-st_ctime-ref)
     (get struct-stat-st_ctime_usec-ref)
     (get struct-stat-st_blocks-ref)
     (get struct-stat-st_blksize-ref))))


;;; low level interface

;;;FIXME For some reason it looks  like the size of "struct stat" is not
;;;determined correctly by the  Autoconf macros.  Dunno why.  This error
;;;causes segmentation faults in the REAL-PRIMITIVE-STAT function.
;;;
;;;The fix below solves the  problem.
;;;
;;;Note that  on my  i686-pc-linux-gnu adding the  size reported  by the
;;;Autoconf  macro  is 88,  while  the  value  returned by  the  foreign
;;;function is 96.
(define-c-function platform-sizeof-stat
  (int nausicaa_posix_sizeof_stat (void)))

(define sizeof-struct-stat
  (platform-sizeof-stat))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-stat
  (int nausicaa_posix_stat (char* pointer)))

(define-c-function/with-errno platform-fstat
  (int nausicaa_posix_fstat (int pointer)))

(define-c-function/with-errno platform-lstat
  (int nausicaa_posix_lstat (char* pointer)))

;;; --------------------------------------------------------------------

(define-c-function S_ISDIR
  (int nausicaa_posix_stat_is_dir (mode_t)))

(define-c-function S_ISCHR
  (int nausicaa_posix_stat_is_chr (mode_t)))

(define-c-function S_ISBLK
  (int nausicaa_posix_stat_is_blk (mode_t)))

(define-c-function S_ISREG
  (int nausicaa_posix_stat_is_reg (mode_t)))

(define-c-function S_ISFIFO
  (int nausicaa_posix_stat_is_fifo (mode_t)))

(define-c-function S_ISLNK
  (int nausicaa_posix_stat_is_lnk (mode_t)))

(define-c-function S_ISSOCK
  (int nausicaa_posix_stat_is_sock (mode_t)))

;;; --------------------------------------------------------------------

(define-c-function S_TYPEISMQ
  (int nausicaa_posix_stat_typeismq (pointer)))

(define-c-function S_TYPEISSEM
  (int nausicaa_posix_stat_typeissem(pointer)))

(define-c-function S_TYPEISSHM
  (int nausicaa_posix_stat_typeisshm(pointer)))



;;;; stat functions

(define (real-primitive-stat func funcname pathname)
  (with-compensations
    (let ((*struct-stat	(malloc-block/c sizeof-struct-stat)))
      (receive (result errno)
	  (func (string->cstring/c pathname) *struct-stat)
	(when (= -1 result)
	  (raise-errno-error funcname errno pathname))
	(struct-stat->record *struct-stat)))))

(define (primitive-stat pathname)
  (real-primitive-stat platform-stat 'primitive-stat pathname))

(define (primitive-lstat pathname)
  (real-primitive-stat platform-lstat 'primitive-lstat pathname))

(define (primitive-fstat fd)
  (with-compensations
    (let ((*struct-stat	(malloc-block/c sizeof-struct-stat)))
      (receive (result errno)
	  (platform-fstat fd *struct-stat)
	(when (= -1 result)
	  (raise-errno-error 'primitive-fstat errno fd))
	(struct-stat->record *struct-stat)))))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-stat-function primitive-stat)

(define-primitive-parameter
  primitive-fstat-function primitive-fstat)

(define-primitive-parameter
  primitive-lstat-function primitive-lstat)

;;; --------------------------------------------------------------------

(define (stat pathname)
  ((primitive-stat-function) pathname))

(define (lstat pathname)
  ((primitive-lstat-function) pathname))

(define (fstat fd)
  ((primitive-fstat-function) fd))


;;;; type inspection

(define (type-inspection funcname pred obj)
  (cond

   ;; file descriptor
   ((and (integer? obj) (< -1 obj))
    (pred (fstat obj)))

   ;; pathname
   ((or (string? obj) (symbol? obj))
    (pred (stat obj)))

   (else
    (error funcname
      "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-is-directory? obj)
  (type-inspection
   'file-is-directory?
   (lambda (record)
     (not (= 0 (S_ISDIR (struct-stat-mode record)))))
   obj))

(define (file-is-character-special? obj)
  (type-inspection
   'file-is-character-special?
   (lambda (record)
     (not (= 0 (S_ISCHR (struct-stat-mode record)))))
   obj))

(define (file-is-block-special? obj)
  (type-inspection
   'file-is-block-special?
   (lambda (record)
     (not (= 0 (S_ISBLK (struct-stat-mode record)))))
   obj))

(define (file-is-regular? obj)
  (type-inspection
   'file-is-regular?
   (lambda (record)
     (not (= 0 (S_ISREG (struct-stat-mode record)))))
   obj))

(define (file-is-fifo? obj)
  (type-inspection
   'file-is-fifo?
   (lambda (record)
     (not (= 0 (S_ISFIFO (struct-stat-mode record)))))
   obj))

(define (file-is-socket? obj)
  (type-inspection
   'file-is-socket?
   (lambda (record)
     (not (= 0 (S_ISSOCK (struct-stat-mode record)))))
   obj))

(define (file-is-symbolic-link? pathname)
  (not (= 0 (S_ISLNK (struct-stat-mode (lstat pathname))))))

;;; --------------------------------------------------------------------

(define (pointer-type-inspection funcname getter obj)

  (cond

   ;; file descriptor
   ((and (integer? obj) (< -1 obj))
    (with-compensations
      (let ((*struct-stat (malloc-block/c sizeof-struct-stat)))
	(receive (result errno)
	    (platform-fstat obj *struct-stat)
	  (when (= -1 result)
	    (raise-errno-error funcname errno obj))
	  (not (= 0 (getter *struct-stat)))))))

   ;; pathname
   ((or (string? obj) (symbol? obj))
    (with-compensations
      (let ((*struct-stat (malloc-block/c sizeof-struct-stat)))
	(receive (result errno)
	    (platform-stat (string->cstring/c obj) *struct-stat)
	  (when (= -1 result)
	    (raise-errno-error funcname errno obj))
	  (not (= 0 (getter *struct-stat)))))))

   (else
    (error funcname
      "expected file descriptor or file pathname" obj))))

(define (file-is-message-queue? obj)
  (pointer-type-inspection 'file-is-message-queue? S_TYPEISMQ obj))

(define (file-is-semaphore? obj)
  (pointer-type-inspection 'file-is-semaphore? S_TYPEISSEM obj))

(define (file-is-shared-memory? obj)
  (pointer-type-inspection 'file-is-semaphore? S_TYPEISSHM obj))



;;;; mode inspection

(define (mode-inspection the-stat funcname mask obj)

  (define (set? record)
    (not (= 0 (bitwise-and mask (struct-stat-mode record)))))

  (cond

   ;; file descriptor
   ((and (eq? the-stat stat) (integer? obj) (< -1 obj))
    (set? (fstat obj)))

   ;; pathname
   ((or (string? obj) (symbol? obj))
    (set? (the-stat obj)))

   (else
    (error funcname
      "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-user-readable? obj)
  (mode-inspection stat 'file-user-readable? S_IRUSR obj))

(define (file-user-writable? obj)
  (mode-inspection stat 'file-user-writable? S_IWUSR obj))

(define (file-user-executable? obj)
  (mode-inspection stat 'file-user-executable? S_IXUSR obj))

(define (file-group-readable? obj)
  (mode-inspection stat 'file-user-readable? S_IRGRP obj))

(define (file-group-writable? obj)
  (mode-inspection stat 'file-group-writable? S_IWGRP obj))

(define (file-group-executable? obj)
  (mode-inspection stat 'file-group-executable? S_IXGRP obj))

(define (file-other-readable? obj)
  (mode-inspection stat 'file-other-readable? S_IROTH obj))

(define (file-other-writable? obj)
  (mode-inspection stat 'file-other-writable? S_IWOTH obj))

(define (file-other-executable? obj)
  (mode-inspection stat 'file-other-executable? S_IXOTH obj))

(define (file-setuid? obj)
  (mode-inspection stat 'file-setuid? S_ISUID obj))

(define (file-setgid? obj)
  (mode-inspection stat 'file-setgid? S_ISGID obj))

(define (file-sticky? obj)
  (mode-inspection stat 'file-sticky? S_ISVTX obj))

;;; --------------------------------------------------------------------

(define (lfile-user-readable? obj)
  (mode-inspection lstat 'lfile-user-readable? S_IRUSR obj))

(define (lfile-user-writable? obj)
  (mode-inspection lstat 'lfile-user-writable? S_IWUSR obj))

(define (lfile-user-executable? obj)
  (mode-inspection lstat 'lfile-user-executable? S_IXUSR obj))

(define (lfile-group-readable? obj)
  (mode-inspection lstat 'lfile-user-readable? S_IRGRP obj))

(define (lfile-group-writable? obj)
  (mode-inspection lstat 'lfile-group-writable? S_IWGRP obj))

(define (lfile-group-executable? obj)
  (mode-inspection lstat 'lfile-group-executable? S_IXGRP obj))

(define (lfile-other-readable? obj)
  (mode-inspection lstat 'lfile-other-readable? S_IROTH obj))

(define (lfile-other-writable? obj)
  (mode-inspection lstat 'lfile-other-writable? S_IWOTH obj))

(define (lfile-other-executable? obj)
  (mode-inspection lstat 'lfile-other-executable? S_IXOTH obj))

(define (lfile-setuid? obj)
  (mode-inspection lstat 'lfile-setuid? S_ISUID obj))

(define (lfile-setgid? obj)
  (mode-inspection lstat 'lfile-setgid? S_ISGID obj))

(define (lfile-sticky? obj)
  (mode-inspection lstat 'lfile-sticky? S_ISVTX obj))

;;; --------------------------------------------------------------------

(define (file-permissions obj)

  (define (get-mode record)
    (let ((mode (struct-stat-mode record)))
      (bitwise-and
       (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
       mode)))

  (cond

   ;; file descriptor
   ((and (integer? obj) (< -1 obj))
    (get-mode (fstat obj)))

   ;; pathname
   ((or (string? obj) (symbol? obj))
    (get-mode (stat obj)))

   (else
    (error 'file-permissions
      "expected file descriptor or file pathname" obj))))

(define (lfile-permissions obj)
  (bitwise-and
   (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
   (struct-stat-mode (lstat obj))))



;;;; done

)

;;; end of file
