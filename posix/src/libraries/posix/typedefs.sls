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


(library (posix typedefs)
  (export

    ;; simple wrappers
    file-descriptor file-descriptor? integer->file-descriptor file-descriptor->integer
    FILE* FILE*? pointer->FILE* FILE*->pointer
    fdset fdset? make-fdset pointer->fdset fdset->pointer
    uid uid? integer->uid uid->integer
    gid gid? integer->gid gid->integer
    pid pid? integer->pid pid->integer

    pid-zero? pid=? pid<? pid<=? pid>? pid>=? pid<>?

    struct-flock
    make-struct-flock		struct-flock?
    struct-flock->pointer	pointer->struct-flock

    struct-timeval
    make-struct-timeval		struct-timeval?
    struct-timeval->pointer	pointer->struct-timeval

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

;;; --------------------------------------------------------------------

    <struct-group>		<struct-group-rtd>
    make-<struct-group>		<struct-group>?
    <struct-group>-name		<struct-group>-name-set!
    <struct-group>-gid		<struct-group>-gid-set!
    <struct-group>-mem		<struct-group>-mem-set!

;;; --------------------------------------------------------------------

    <struct-utsname>		<struct-utsname-rtd>
    make-<struct-utsname>	<struct-utsname>?
    <struct-utsname>-sysname	<struct-utsname>-sysname-set!
    <struct-utsname>-release	<struct-utsname>-release-set!
    <struct-utsname>-version	<struct-utsname>-version-set!
    <struct-utsname>-machine	<struct-utsname>-machine-set!

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

;;; --------------------------------------------------------------------

    <struct-mntent>		<struct-mntent-rtd>
    make-<struct-mntent>	<struct-mntent>?
    <struct-mntent>-fsname	<struct-mntent>-fsname-set!
    <struct-mntent>-dir		<struct-mntent>-dir-set!
    <struct-mntent>-type	<struct-mntent>-type-set!
    <struct-mntent>-opts	<struct-mntent>-opts-set!
    <struct-mntent>-freq	<struct-mntent>-freq-set!
    <struct-mntent>-passno	<struct-mntent>-passno-set!

;;; --------------------------------------------------------------------

    <process-term-status>	<process-term-status-rtd>
    make-<process-term-status>	<process-term-status>?
    WIFEXITED?			WEXITSTATUS?
    WIFSIGNALED?		WTERMSIG?
    WCOREDUMP?			WIFSTOPPED?
    WSTOPSIG?

;;; --------------------------------------------------------------------

    <struct-stat>		<struct-stat-rtd>
    make-<struct-stat>		<struct-stat>?
    <struct-stat>-mode
    <struct-stat>-ino
    <struct-stat>-dev
    <struct-stat>-nlink
    <struct-stat>-uid
    <struct-stat>-gid
    <struct-stat>-size
    <struct-stat>-atime
    <struct-stat>-atime_usec
    <struct-stat>-mtime
    <struct-stat>-mtime_usec
    <struct-stat>-ctime
    <struct-stat>-ctime_usec
    <struct-stat>-blocks
    <struct-stat>-blksize

;;; --------------------------------------------------------------------

    <struct-tms>			<struct-tms-rtd>
    make-<struct-tms>			<struct-tms>?
    <struct-tms>-utime			<struct-tms>-utime-set!
    <struct-tms>-stime			<struct-tms>-stime-set!
    <struct-tms>-cutime			<struct-tms>-cutime-set!
    <struct-tms>-cstime			<struct-tms>-cstime-set!

    <struct-timeval>			<struct-timeval-rtd>
    make-<struct-timeval>		<struct-timeval>?
    <struct-timeval>-sec		<struct-timeval>-sec-set!
    <struct-timeval>-usec		<struct-timeval>-usec-set!

    <struct-timespec>			<struct-timespec-rtd>
    make-<struct-timespec>		<struct-timespec>?
    <struct-timespec>-sec		<struct-timespec>-sec-set!
    <struct-timespec>-nsec		<struct-timespec>-nsec-set!

    <struct-timezone>			<struct-timezone-rtd>
    make-<struct-timezone>		<struct-timezone>?
    <struct-timezone>-minuteswest	<struct-timezone>-minuteswest-set!
    <struct-timezone>-dsttime		<struct-timezone>-dsttime-set!

    <struct-tm>				<struct-tm-rtd>
    make-<struct-tm>			<struct-tm>?
    <struct-tm>-sec			<struct-tm>-sec-set!
    <struct-tm>-min			<struct-tm>-min-set!
    <struct-tm>-hour			<struct-tm>-hour-set!
    <struct-tm>-mday			<struct-tm>-mday-set!
    <struct-tm>-mon			<struct-tm>-mon-set!
    <struct-tm>-year			<struct-tm>-year-set!
    <struct-tm>-wday			<struct-tm>-wday-set!
    <struct-tm>-yday			<struct-tm>-yday-set!
    <struct-tm>-isdst			<struct-tm>-isdst-set!
    <struct-tm>-gmtoff			<struct-tm>-gmtoff-set!
    <struct-tm>-zone			<struct-tm>-zone-set!

    <struct-ntptimeval>			<struct-ntptimeval-rtd>
    make-<struct-ntptimeval>		<struct-ntptimeval>?
    <struct-ntptimeval>-time		<struct-ntptimeval>-time-set!
    <struct-ntptimeval>-maxerror	<struct-ntptimeval>-maxerror-set!
    <struct-ntptimeval>-esterror	<struct-ntptimeval>-esterror-set!

    <struct-timex>			<struct-timex-rtd>
    make-<struct-timex>			<struct-timex>?
    <struct-timex>-modes		<struct-timex>-modes-set!
    <struct-timex>-offset		<struct-timex>-offset-set!
    <struct-timex>-freq			<struct-timex>-freq-set!
    <struct-timex>-maxerror		<struct-timex>-maxerror-set!
    <struct-timex>-esterror		<struct-timex>-esterror-set!
    <struct-timex>-status		<struct-timex>-status-set!
    <struct-timex>-constant		<struct-timex>-constant-set!
    <struct-timex>-precision		<struct-timex>-precision-set!
    <struct-timex>-tolerance		<struct-timex>-tolerance-set!
    <struct-timex>-time			<struct-timex>-time-set!
    <struct-timex>-tick			<struct-timex>-tick-set!
    <struct-timex>-ppsfreq		<struct-timex>-ppsfreq-set!
    <struct-timex>-jitter		<struct-timex>-jitter-set!
    <struct-timex>-shift		<struct-timex>-shift-set!
    <struct-timex>-stabil		<struct-timex>-stabil-set!
    <struct-timex>-jitcnt		<struct-timex>-jitcnt-set!
    <struct-timex>-calcnt		<struct-timex>-calcnt-set!
    <struct-timex>-errcnt		<struct-timex>-errcnt-set!
    <struct-timex>-stbcnt		<struct-timex>-stbcnt-set!

    <struct-itimerval>			<struct-itimerval-rtd>
    make-<struct-itimerval>		<struct-itimerval>?
    <struct-itimerval>-interval		<struct-itimerval>-interval-set!
    <struct-itimerval>-value		<struct-itimerval>-value-set!

    )
  (import (rnrs)
    (posix sizeof))


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

(define-record-type (pid integer->pid pid?)
  (nongenerative nausicaa:posix:pid)
  (fields (immutable object pid->integer)))

(define (pid-zero? obj)
  (and (pid? obj) (= 0 (pid->integer obj))))

(let-syntax ((define-pidop (syntax-rules ()
			     ((_ ?name ?op)
			      (define (?name a b)
				(?op (pid->integer a) (pid->integer b)))))))
  (define-pidop pid=?  =)
  (define-pidop pid<?  <)
  (define-pidop pid<=? <=)
  (define-pidop pid>?  >)
  (define-pidop pid>=? >=))

(define (pid<>? a b)
  (not (= (pid->integer a) (pid->integer b))))

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

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

(define-record-type <struct-group>
  (nongenerative nausicaa:posix:struct-group)
  (fields (mutable name)
	  (mutable gid)
	  (mutable mem)))

(define <struct-group-rtd>
  (record-type-descriptor <struct-group>))


(define-record-type <struct-utsname>
  (nongenerative nausicaa:posix:struct-utsname)
  (fields (mutable sysname)
	  (mutable release)
	  (mutable version)
	  (mutable machine)))

(define <struct-utsname-rtd>
  (record-type-descriptor <struct-utsname>))


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

;;; --------------------------------------------------------------------

(define-record-type <struct-mntent>
  (nongenerative nausicaa:posix:struct-mntent)
  (fields (mutable fsname)
	  (mutable dir)
	  (mutable type)
	  (mutable opts)
	  (mutable freq)
	  (mutable passno)))

(define <struct-mntent-rtd>
  (record-type-descriptor <struct-mntent>))


(define-record-type <process-term-status>
  (fields (immutable WIFEXITED		WIFEXITED?)
	  (immutable WEXITSTATUS	WEXITSTATUS?)
	  (immutable WIFSIGNALED	WIFSIGNALED?)
	  (immutable WTERMSIG		WTERMSIG?)
	  (immutable WCOREDUMP		WCOREDUMP?)
	  (immutable WIFSTOPPED		WIFSTOPPED?)
	  (immutable WSTOPSIG		WSTOPSIG?)))

(define <process-term-status-rtd>
  (record-type-descriptor <process-term-status>))


(define-record-type <struct-stat>
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

(define <struct-stat-rtd>
  (record-type-descriptor <struct-stat>))


(define-record-type <struct-tms>
  (fields (mutable utime)
	  (mutable stime)
	  (mutable cutime)
	  (mutable cstime)))

(define <struct-tms-rtd>
  (record-type-descriptor <struct-tms>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timeval>
  (fields (mutable sec)
	  (mutable usec)))

(define <struct-timeval-rtd>
  (record-type-descriptor <struct-timeval>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timespec>
  (fields (mutable sec)
	  (mutable nsec)))

(define <struct-timespec-rtd>
  (record-type-descriptor <struct-timespec>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timezone>
  (fields (mutable minuteswest)
	  (mutable dsttime)))

(define <struct-timezone-rtd>
  (record-type-descriptor <struct-timezone>))

;;; --------------------------------------------------------------------

(define-record-type <struct-tm>
  (fields (mutable sec)
	  (mutable min)
	  (mutable hour)
	  (mutable mday)
	  (mutable mon)
	  (mutable year)
	  (mutable wday)
	  (mutable yday)
	  (mutable isdst)
	  (mutable gmtoff)
	  (mutable zone)))

(define <struct-tm-rtd>
  (record-type-descriptor <struct-tm>))

;;; --------------------------------------------------------------------

(define-record-type <struct-ntptimeval>
  (fields (mutable time)
	  (mutable maxerror)
	  (mutable esterror)))

(define <struct-ntptimeval-rtd>
  (record-type-descriptor <struct-ntptimeval>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timex>
  (fields (mutable modes)
	  (mutable offset)
	  (mutable freq)
	  (mutable maxerror)
	  (mutable esterror)
	  (mutable status)
	  (mutable constant)
	  (mutable precision)
	  (mutable tolerance)
	  (mutable time)
	  (mutable tick)
	  (mutable ppsfreq)
	  (mutable jitter)
	  (mutable shift)
	  (mutable stabil)
	  (mutable jitcnt)
	  (mutable calcnt)
	  (mutable errcnt)
	  (mutable stbcnt)))

(define <struct-timex-rtd>
  (record-type-descriptor <struct-timex>))

;;; --------------------------------------------------------------------

(define-record-type <struct-itimerval>
  (fields (mutable interval)
	  (mutable value)))

(define <struct-itimerval-rtd>
  (record-type-descriptor <struct-itimerval>))


;;;; done

)

;;; end of file
