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



;;;; setup

(library (posix file stat)
  (export
    platform-stat
    platform-lstat
    platform-fstat

    stat	primitive-stat		primitive-stat-function
    lstat	primitive-lstat		primitive-lstat-function
    fstat	primitive-fstat		primitive-fstat-function

    make-struct-stat struct-stat? struct-stat->record
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
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof))

  (define stub-lib
    (let ((o (open-shared-object 'libnausicaa-posix.so)))
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


;;; interface

(define-c-function/with-errno platform-stat
  (int nausicaa_posix_stat (char* pointer)))

(define-c-function/with-errno platform-fstat
  (int nausicaa_posix_fstat (int pointer)))

(define-c-function/with-errno platform-lstat
  (int nausicaa_posix_lstat (char* pointer)))

;;; --------------------------------------------------------------------

;;;FIXME For some reason it looks  like the size of "struct stat" is not
;;;determined correctly by the  Autoconf macros.  Dunno why.  This error
;;;causes segmentation faults in the REAL-PRIMITIVE-STAT function.
;;;
;;;The fix below solves the  problem.  Note that on my i686-pc-linux-gnu
;;;adding 5 bytes to SIZEOF-STRUCT-STAT solves the problem, but adding 4
;;;bytes is not enough.
(define the-size-of-struct-stat 1024)

(define (real-primitive-stat func funcname pathname)
  (with-compensations
    (let ((*struct-stat	(malloc-block/c the-size-of-struct-stat)))
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
    (let ((*struct-stat	(malloc-block/c the-size-of-struct-stat)))
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



;;;; done

)

;;; end of file
