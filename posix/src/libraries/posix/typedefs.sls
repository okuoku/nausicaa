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


(library (posix typedefs)
  (export

    ;; simple wrappers
    file-descriptor file-descriptor? integer->file-descriptor file-descriptor->integer
    FILE* FILE*? pointer->FILE* FILE*->pointer
    fdset fdset? make-fdset pointer->fdset fdset->pointer
    uid uid? integer->uid uid->integer
    gid gid? integer->gid gid->integer
    pid pid? integer->pid pid->integer

    uid=? gid=?
    pid-zero? pid=? pid<? pid<=? pid>? pid>=? pid<>?

    struct-flock
    make-struct-flock		struct-flock?
    struct-flock->pointer	pointer->struct-flock

    struct-timeval
    make-struct-timeval		struct-timeval?
    struct-timeval->pointer	pointer->struct-timeval

    struct-in-addr		struct-in-addr?
    struct-in-addr->pointer	pointer->struct-in-addr

    struct-in6-addr		struct-in6-addr?
    struct-in6-addr->pointer	pointer->struct-in6-addr

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

;;; --------------------------------------------------------------------

    <socket>				<socket-rtd>
    make-<socket>			<socket>?
    <socket>-namespace
    <socket>-style
    <socket>-protocol

    <struct-sockaddr-in>		<struct-sockaddr-in-rtd>
    make-<struct-sockaddr-in>		<struct-sockaddr-in>?
    <struct-sockaddr-in>-family		<struct-sockaddr-in>-family-set!
    <struct-sockaddr-in>-addr		<struct-sockaddr-in>-addr-set!
    <struct-sockaddr-in>-port		<struct-sockaddr-in>-port-set!

    <struct-sockaddr-in6>		<struct-sockaddr-in6-rtd>
    make-<struct-sockaddr-in6>		<struct-sockaddr-in6>?
    <struct-sockaddr-in6>-family	<struct-sockaddr-in6>-family-set!
    <struct-sockaddr-in6>-addr		<struct-sockaddr-in6>-addr-set!
    <struct-sockaddr-in6>-port		<struct-sockaddr-in6>-port-set!

    <struct-sockaddr-un>		<struct-sockaddr-un-rtd>
    make-<struct-sockaddr-un>		<struct-sockaddr-un>?
    <struct-sockaddr-un>-family		<struct-sockaddr-un>-family-set!
    <struct-sockaddr-un>-path		<struct-sockaddr-un>-path-set!

    <struct-if-nameindex>		<struct-if-nameindex-rtd>
    make-<struct-if-nameindex>		<struct-if-nameindex>?
    <struct-if-nameindex>-index		<struct-if-nameindex>-index-set!
    <struct-if-nameindex>-name		<struct-if-nameindex>-name-set!

    <struct-netent>			<struct-netent>?
    <struct-netent>-name		<struct-netent>-name-set!
    <struct-netent>-aliases		<struct-netent>-aliases-set!
    <struct-netent>-addrtype		<struct-netent>-addrtype-set!
    <struct-netent>-net			<struct-netent>-net-set!

;;; --------------------------------------------------------------------

    enum-unix-signals			unix-signals
    unix-signal->value			value->unix-signal
    unix-signal-symbol->value		value->unix-signal-symbol
    unix-signal

;;; --------------------------------------------------------------------

    enum-socket-namespace		socket-namespace
    socket-namespace->value		value->socket-namespace
    socket-namespace->socklen

;;; --------------------------------------------------------------------

    enum-socket-address-format		socket-address-format
    socket-address-format->value	value->socket-address-format

;;; --------------------------------------------------------------------

    enum-socket-style			socket-style
    socket-style->value			value->socket-style

;;; --------------------------------------------------------------------

    enum-socket-protocol		socket-protocol
    socket-protocol->value		value->socket-protocol

;;; --------------------------------------------------------------------

    enum-shutdown-mode			shutdown-mode
    shutdown-mode->value		value->shutdown-mode

;;; --------------------------------------------------------------------

    enum-socket-data-options		socket-data-options
    socket-data-options->value		value->socket-data-options

;;; --------------------------------------------------------------------

    enum-socket-option			socket-option
    socket-option->symbol
    socket-option->value		value->socket-option

    )
  (import (rnrs)
    (enumerations)
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

(define (uid=? a b)
  (= (uid->integer a) (uid->integer b)))

(define (gid=? a b)
  (= (gid->integer a) (gid->integer b)))

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
  (nongenerative nausicaa:posix:<struct-passwd>)
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
  (nongenerative nausicaa:posix:<struct-group>)
  (fields (mutable name)
	  (mutable gid)
	  (mutable mem)))

(define <struct-group-rtd>
  (record-type-descriptor <struct-group>))


(define-record-type <struct-utsname>
  (nongenerative nausicaa:posix:<struct-utsname>)
  (fields (mutable sysname)
	  (mutable release)
	  (mutable version)
	  (mutable machine)))

(define <struct-utsname-rtd>
  (record-type-descriptor <struct-utsname>))


(define-record-type <struct-fstab>
  (nongenerative nausicaa:posix:<struct-fstab>)
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
  (nongenerative nausicaa:posix:<struct-mntent>)
  (fields (mutable fsname)
	  (mutable dir)
	  (mutable type)
	  (mutable opts)
	  (mutable freq)
	  (mutable passno)))

(define <struct-mntent-rtd>
  (record-type-descriptor <struct-mntent>))


(define-record-type <process-term-status>
  (nongenerative nausicaa:posix:<process-term-status>)
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
  (nongenerative nausicaa:posix:<struct-stat>)
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
  (nongenerative nausicaa:posix:<struct-tms>)
  (fields (mutable utime)
	  (mutable stime)
	  (mutable cutime)
	  (mutable cstime)))

(define <struct-tms-rtd>
  (record-type-descriptor <struct-tms>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timeval>
  (nongenerative nausicaa:posix:<struct-timeval>)
  (fields (mutable sec)
	  (mutable usec)))

(define <struct-timeval-rtd>
  (record-type-descriptor <struct-timeval>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timespec>
  (nongenerative nausicaa:posix:<struct-timespec>)
  (fields (mutable sec)
	  (mutable nsec)))

(define <struct-timespec-rtd>
  (record-type-descriptor <struct-timespec>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timezone>
  (nongenerative nausicaa:posix:<struct-timezone>)
  (fields (mutable minuteswest)
	  (mutable dsttime)))

(define <struct-timezone-rtd>
  (record-type-descriptor <struct-timezone>))

;;; --------------------------------------------------------------------

(define-record-type <struct-tm>
  (nongenerative nausicaa:posix:<struct-tm>)
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
  (nongenerative nausicaa:posix:<struct-ntptimeval>)
  (fields (mutable time)
	  (mutable maxerror)
	  (mutable esterror)))

(define <struct-ntptimeval-rtd>
  (record-type-descriptor <struct-ntptimeval>))

;;; --------------------------------------------------------------------

(define-record-type <struct-timex>
  (nongenerative nausicaa:posix:<struct-timex>)
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
  (nongenerative nausicaa:posix:<struct-itimerval>)
  (fields (mutable interval)
	  (mutable value)))

(define <struct-itimerval-rtd>
  (record-type-descriptor <struct-itimerval>))


(define-record-type <struct-sockaddr-in>
  (nongenerative nausicaa:posix:<struct-sockaddr-in>)
  (fields (mutable family)
	  (mutable addr)
	  (mutable port)))

(define <struct-sockaddr-in-rtd>
  (record-type-descriptor <struct-sockaddr-in>))

;;; --------------------------------------------------------------------

(define-record-type <struct-sockaddr-in6>
  (nongenerative nausicaa:posix:<struct-sockaddr-in6>)
  (fields (mutable family)
	  (mutable addr)
	  (mutable port)))

(define <struct-sockaddr-in6-rtd>
  (record-type-descriptor <struct-sockaddr-in6>))

;;; --------------------------------------------------------------------

(define-record-type (<struct-sockaddr-un> %make-<struct-sockaddr-un> <struct-sockaddr-un>?)
  (nongenerative nausicaa:posix:<struct-sockaddr-un>)
  (fields (mutable family)
	  (mutable path)))

(define <struct-sockaddr-un-rtd>
  (record-type-descriptor <struct-sockaddr-un>))

(define (make-<struct-sockaddr-un> pathname)
  (%make-<struct-sockaddr-un> AF_LOCAL pathname))

;;; --------------------------------------------------------------------

(define-record-type <struct-if-nameindex>
  (nongenerative nausicaa:posix:<struct-if-nameindex>)
  (fields (mutable index)
	  (mutable name)))

(define <struct-if-nameindex-rtd>
  (record-type-descriptor <struct-if-nameindex>))

;;; --------------------------------------------------------------------

(define-record-type <struct-netent>
  (nongenerative nausicaa:posix:<struct-netent>)
  (fields (mutable name)
	  (mutable aliases)
	  (mutable addrtype)
	  (mutable net)))

(define <struct-netent-rtd>
  (record-type-descriptor <struct-netent>))


(define-record-type struct-in-addr
  (nongenerative nausicaa:posix:struct-in-addr)
  (fields (immutable pointer struct-in-addr->pointer)))

(define pointer->struct-in-addr make-struct-in-addr)

;;; --------------------------------------------------------------------

(define-record-type struct-in6-addr
  (nongenerative nausicaa:posix:struct-in6-addr)
  (fields (immutable pointer struct-in6-addr->pointer)))

(define pointer->struct-in6-addr make-struct-in6-addr)

;;; --------------------------------------------------------------------

(define-record-type <socket>
  (nongenerative nausicaa:posix:<socket>)
  (parent file-descriptor)
  (fields (immutable namespace)
	  (immutable style)
	  (immutable protocol)))

(define <socket-rtd>
  (record-type-descriptor <socket>))


(define-enumeration enum-unix-signals
  (SIGABRT	SIGALRM		SIGBUS		SIGCHLD
   SIGCLD	SIGCONT		SIGEMT		SIGFPE
   SIGHUP	SIGILL		SIGINFO		SIGINT
   SIGIO	SIGIOT		SIGKILL		SIGLOST
   SIGPIPE	SIGPOLL		SIGPROF		SIGQUIT
   SIGSEGV	SIGSTOP		SIGSYS		SIGTERM
   SIGTRAP	SIGTSTP		SIGTTIN		SIGTTOU
   SIGURG	SIGUSR1		SIGUSR2		SIGVRALRM
   SIGWINCH	SIGXCPU		SIGXSFZ)
  unix-signals)

(define %unix-signals-universe
  (enum-set-universe (unix-signals)))

(define-syntax unix-signal
  (syntax-rules ()
    ((_ ?name)
     (unix-signals ?name))))

(define (value->unix-signal signum)
  ((enum-set-constructor %unix-signals-universe) (list (value->unix-signal-symbol signum))))

(define (value->unix-signal-symbol signum)
  ;;We have to remember that some of the signals are NOT defined on some
  ;;platforms,  in this  case the  binding is  to #f  rather than  to an
  ;;integer.
  ;;
  (cond ((and SIGABRT	(= signum SIGABRT))	'SIGABRT)
	((and SIGALRM	(= signum SIGALRM))	'SIGALRM)
	((and SIGBUS	(= signum SIGBUS))	'SIGBUS)
  	((and SIGCHLD	(= signum SIGCHLD))	'SIGCHLD)
	((and SIGCLD	(= signum SIGCLD))	'SIGCLD)
	((and SIGCONT	(= signum SIGCONT))	'SIGCONT)
	((and SIGEMT	(= signum SIGEMT))	'SIGEMT)
	((and SIGFPE	(= signum SIGFPE))	'SIGFPE)
	((and SIGHUP	(= signum SIGHUP))	'SIGHUP)
	((and SIGILL	(= signum SIGILL))	'SIGILL)
	((and SIGINFO	(= signum SIGINFO))	'SIGINFO)
	((and SIGINT	(= signum SIGINT))	'SIGINT)
	((and SIGIO	(= signum SIGIO))	'SIGIO)
	((and SIGIOT	(= signum SIGIOT))	'SIGIOT)
	((and SIGKILL	(= signum SIGKILL))	'SIGKILL)
	((and SIGLOST	(= signum SIGLOST))	'SIGLOST)
	((and SIGPIPE	(= signum SIGPIPE))	'SIGPIPE)
	((and SIGPOLL	(= signum SIGPOLL))	'SIGPOLL)
	((and SIGPROF	(= signum SIGPROF))	'SIGPROF)
	((and SIGQUIT	(= signum SIGQUIT))	'SIGQUIT)
	((and SIGSEGV	(= signum SIGSEGV))	'SIGSEGV)
	((and SIGSTOP	(= signum SIGSTOP))	'SIGSTOP)
	((and SIGSYS	(= signum SIGSYS))	'SIGSYS)
	((and SIGTERM	(= signum SIGTERM))	'SIGTERM)
	((and SIGTRAP	(= signum SIGTRAP))	'SIGTRAP)
	((and SIGTSTP	(= signum SIGTSTP))	'SIGTSTP)
	((and SIGTTIN	(= signum SIGTTIN))	'SIGTTIN)
	((and SIGTTOU	(= signum SIGTTOU))	'SIGTTOU)
	((and SIGURG	(= signum SIGURG))	'SIGURG)
	((and SIGUSR1	(= signum SIGUSR1))	'SIGUSR1)
	((and SIGUSR2	(= signum SIGUSR2))	'SIGUSR2)
	((and SIGVRALRM	(= signum SIGVRALRM))	'SIGVRALRM)
	((and SIGWINCH	(= signum SIGWINCH))	'SIGWINCH)
	((and SIGXCPU	(= signum SIGXCPU))	'SIGXCPU)
	((and SIGXSFZ	(= signum SIGXSFZ))	'SIGXSFZ)
	(else
	 (assertion-violation 'unix-signal->symbol "unknown unix signal number" signum))))

(define (unix-signal->value set)
  (assert (enum-set-subset? set %unix-signals-universe))
  (let ((ell (enum-set->list set)))
    (assert (= 1 (length ell)))
    (unix-signal-symbol->value (car ell))))

(define (unix-signal-symbol->value symbol)
  ;;We have to remember that some of the signals are NOT defined on some
  ;;platforms,  in this  case the  binding is  to #f  rather than  to an
  ;;integer.
  ;;
  (case symbol
    ((SIGABRT)		SIGABRT)
    ((SIGALRM)		SIGALRM)
    ((SIGBUS)		SIGBUS)
    ((SIGCHLD)		SIGCHLD)
    ((SIGCLD)		SIGCLD)
    ((SIGCONT)		SIGCONT)
    ((SIGEMT)		SIGEMT)
    ((SIGFPE)		SIGFPE)
    ((SIGHUP)		SIGHUP)
    ((SIGILL)		SIGILL)
    ((SIGINFO)		SIGINFO)
    ((SIGINT)		SIGINT)
    ((SIGIO)		SIGIO)
    ((SIGIOT)		SIGIOT)
    ((SIGKILL)		SIGKILL)
    ((SIGLOST)		SIGLOST)
    ((SIGPIPE)		SIGPIPE)
    ((SIGPOLL)		SIGPOLL)
    ((SIGPROF)		SIGPROF)
    ((SIGQUIT)		SIGQUIT)
    ((SIGSEGV)		SIGSEGV)
    ((SIGSTOP)		SIGSTOP)
    ((SIGSYS)		SIGSYS)
    ((SIGTERM)		SIGTERM)
    ((SIGTRAP)		SIGTRAP)
    ((SIGTSTP)		SIGTSTP)
    ((SIGTTIN)		SIGTTIN)
    ((SIGTTOU)		SIGTTOU)
    ((SIGURG)		SIGURG)
    ((SIGUSR1)		SIGUSR1)
    ((SIGUSR2)		SIGUSR2)
    ((SIGVRALRM)	SIGVRALRM)
    ((SIGWINCH)		SIGWINCH)
    ((SIGXCPU)		SIGXCPU)
    ((SIGXSFZ)		SIGXSFZ)
    (else (assertion-violation 'unix-signal-symbol->value "unknown unix signal symbol" symbol))))


(define-c-flags socket-style
  (SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_SEQPACKET)
  (stream datagram raw seqpacket))

(define-c-flags socket-protocol
  (0)
  (zero))

(define-c-flags socket-namespace
  ;; PF_LOCAL, PF_UNIX and PF_FILE are synonims.
  (PF_LOCAL PF_UNIX PF_FILE PF_INET PF_INET6 PF_UNSPEC)
  (local unix file inet inet6 unspec))

(define (socket-namespace->socklen set)
  ;;Given a set built with  SOCKET-NAMESPACE return the size in bytes of
  ;;the sockaddr structure required to represent an address of the given
  ;;namespace.  It may not be significant.
  ;;
  (cond ((enum-set-subset? set (%socket-namespace local unix file))
	 sizeof-sockaddr_un)
	((enum-set=? set (socket-namespace inet))
	 sizeof-sockaddr_in)
	((enum-set=? set (socket-namespace inet6))
	 sizeof-sockaddr_in6)
	((enum-set=? set (socket-namespace unspec))
	 (error 'socket-namespace->socklen
	   "unknown socket address structure size for selected namespace"
	   set))
	(else
	 (assertion-violation 'socket-namespace->socklen
	   "invalid socket address namespace" set))))

(define-c-flags socket-address-format
  (AF_LOCAL AF_UNIX AF_FILE AF_INET AF_INET6 AF_UNSPEC)
  (local unix file inet inet6 unspec))

(define-c-flags shutdown-mode
  (SHUT_RD SHUT_WR SHUT_RDWR)
  (read write both))

(define-c-ior-flags socket-data-options
  (MSG_OOB MSG_PEEK MSG_DONTROUTE)
  (oob peek dontroute))

(define-c-flags socket-option
  (SO_DEBUG		SO_REUSEADDR
   SO_KEEPALIVE		SO_DONTROUTE
   SO_LINGER		SO_BROADCAST
   SO_OOBINLINE		SO_SNDBUF
   SO_RCVBUF		SO_STYLE
   SO_TYPE		SO_ERROR)
  (debug		reuseaddr
   keepalive		dontroute
   linger		broadcast
   oobinline		sndbuf
   rcvbuf		style
   type			error))

(define (socket-option->symbol set)
  (car (enum-set->list set)))


;;;; done

)

;;; end of file
