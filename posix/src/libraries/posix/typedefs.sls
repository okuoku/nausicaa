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
    <fd> <fd>? integer-><fd> <fd>->integer
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

    <passwd>			<passwd-rtd>
    make-<passwd>		<passwd>?
    <passwd>-name		<passwd>-name-set!
    <passwd>-passwd		<passwd>-passwd-set!
    <passwd>-uid		<passwd>-uid-set!
    <passwd>-gid		<passwd>-gid-set!
    <passwd>-gecos		<passwd>-gecos-set!
    <passwd>-dir		<passwd>-dir-set!
    <passwd>-shell		<passwd>-shell-set!

;;; --------------------------------------------------------------------

    <group>			<group-rtd>
    make-<group>		<group>?
    <group>-name		<group>-name-set!
    <group>-gid			<group>-gid-set!
    <group>-mem			<group>-mem-set!

;;; --------------------------------------------------------------------

    <utsname>			<utsname-rtd>
    make-<utsname>		<utsname>?
    <utsname>-sysname		<utsname>-sysname-set!
    <utsname>-release		<utsname>-release-set!
    <utsname>-version		<utsname>-version-set!
    <utsname>-machine		<utsname>-machine-set!

;;; --------------------------------------------------------------------

    <fstab>			<fstab-rtd>
    make-<fstab>		<fstab>?
    <fstab>-spec		<fstab>-spec-set!
    <fstab>-file		<fstab>-file-set!
    <fstab>-vfstype		<fstab>-vfstype-set!
    <fstab>-mntops		<fstab>-mntops-set!
    <fstab>-type		<fstab>-type-set!
    <fstab>-freq		<fstab>-freq-set!
    <fstab>-passno		<fstab>-passno-set!

;;; --------------------------------------------------------------------

    <mntent>			<mntent-rtd>
    make-<mntent>		<mntent>?
    <mntent>-fsname		<mntent>-fsname-set!
    <mntent>-dir		<mntent>-dir-set!
    <mntent>-type		<mntent>-type-set!
    <mntent>-opts		<mntent>-opts-set!
    <mntent>-freq		<mntent>-freq-set!
    <mntent>-passno		<mntent>-passno-set!

;;; --------------------------------------------------------------------

    <process-term-status>	<process-term-status-rtd>
    make-<process-term-status>	<process-term-status>?
    WIFEXITED?			WEXITSTATUS?
    WIFSIGNALED?		WTERMSIG?
    WCOREDUMP?			WIFSTOPPED?
    WSTOPSIG?

;;; --------------------------------------------------------------------

    <stat>			<stat-rtd>
    make-<stat>			<stat>?
    <stat>-mode
    <stat>-ino
    <stat>-dev
    <stat>-nlink
    <stat>-uid
    <stat>-gid
    <stat>-size
    <stat>-atime
    <stat>-atime_usec
    <stat>-mtime
    <stat>-mtime_usec
    <stat>-ctime
    <stat>-ctime_usec
    <stat>-blocks
    <stat>-blksize

;;; --------------------------------------------------------------------

    <tms>			<tms-rtd>
    make-<tms>			<tms>?
    <tms>-utime			<tms>-utime-set!
    <tms>-stime			<tms>-stime-set!
    <tms>-cutime		<tms>-cutime-set!
    <tms>-cstime		<tms>-cstime-set!

    <timeval>			<timeval-rtd>
    make-<timeval>		<timeval>?
    <timeval>-sec		<timeval>-sec-set!
    <timeval>-usec		<timeval>-usec-set!

    <timespec>			<timespec-rtd>
    make-<timespec>		<timespec>?
    <timespec>-sec		<timespec>-sec-set!
    <timespec>-nsec		<timespec>-nsec-set!

    <timezone>			<timezone-rtd>
    make-<timezone>		<timezone>?
    <timezone>-minuteswest	<timezone>-minuteswest-set!
    <timezone>-dsttime		<timezone>-dsttime-set!

    <tm>			<tm-rtd>
    make-<tm>			<tm>?
    <tm>-sec			<tm>-sec-set!
    <tm>-min			<tm>-min-set!
    <tm>-hour			<tm>-hour-set!
    <tm>-mday			<tm>-mday-set!
    <tm>-mon			<tm>-mon-set!
    <tm>-year			<tm>-year-set!
    <tm>-wday			<tm>-wday-set!
    <tm>-yday			<tm>-yday-set!
    <tm>-isdst			<tm>-isdst-set!
    <tm>-gmtoff			<tm>-gmtoff-set!
    <tm>-zone			<tm>-zone-set!

    <ntptimeval>		<ntptimeval-rtd>
    make-<ntptimeval>		<ntptimeval>?
    <ntptimeval>-time		<ntptimeval>-time-set!
    <ntptimeval>-maxerror	<ntptimeval>-maxerror-set!
    <ntptimeval>-esterror	<ntptimeval>-esterror-set!

    <timex>			<timex-rtd>
    make-<timex>		<timex>?
    <timex>-modes		<timex>-modes-set!
    <timex>-offset		<timex>-offset-set!
    <timex>-freq		<timex>-freq-set!
    <timex>-maxerror		<timex>-maxerror-set!
    <timex>-esterror		<timex>-esterror-set!
    <timex>-status		<timex>-status-set!
    <timex>-constant		<timex>-constant-set!
    <timex>-precision		<timex>-precision-set!
    <timex>-tolerance		<timex>-tolerance-set!
    <timex>-time		<timex>-time-set!
    <timex>-tick		<timex>-tick-set!
    <timex>-ppsfreq		<timex>-ppsfreq-set!
    <timex>-jitter		<timex>-jitter-set!
    <timex>-shift		<timex>-shift-set!
    <timex>-stabil		<timex>-stabil-set!
    <timex>-jitcnt		<timex>-jitcnt-set!
    <timex>-calcnt		<timex>-calcnt-set!
    <timex>-errcnt		<timex>-errcnt-set!
    <timex>-stbcnt		<timex>-stbcnt-set!

    <itimerval>			<itimerval-rtd>
    make-<itimerval>		<itimerval>?
    <itimerval>-interval	<itimerval>-interval-set!
    <itimerval>-value		<itimerval>-value-set!

;;; --------------------------------------------------------------------

    <socket>			<socket-rtd>
    make-<socket>		<socket>?
    <socket>-namespace
    <socket>-style
    <socket>-protocol

    <sockaddr>			<sockaddr-rtd>
    make-<sockaddr>		<sockaddr>?
    <sockaddr>-family

    <sockaddr-in>		<sockaddr-in-rtd>
    make-<sockaddr-in>		<sockaddr-in>?
    <sockaddr-in>-family
    <sockaddr-in>-addr		<sockaddr-in>-addr-set!
    <sockaddr-in>-port		<sockaddr-in>-port-set!

    <sockaddr-in6>		<sockaddr-in6-rtd>
    make-<sockaddr-in6>		<sockaddr-in6>?
    <sockaddr-in6>-family
    <sockaddr-in6>-addr		<sockaddr-in6>-addr-set!
    <sockaddr-in6>-port		<sockaddr-in6>-port-set!

    <sockaddr-un>		<sockaddr-un-rtd>
    make-<sockaddr-un>		<sockaddr-un>?
    <sockaddr-un>-family
    <sockaddr-un>-pathname	<sockaddr-un>-pathname-set!

    <if-nameindex>		<if-nameindex-rtd>
    make-<if-nameindex>		<if-nameindex>?
    <if-nameindex>-index
    <if-nameindex>-name

;;; --------------------------------------------------------------------

    <hostent>			<hostent-rtd>
    make-<hostent>		<hostent>?
    <hostent>-name
    <hostent>-aliases
    <hostent>-addrtype
    <hostent>-addrlist

    <netent>			<netent-rtd>
    make-<netent>		<netent>?
    <netent>-name
    <netent>-aliases
    <netent>-addrtype
    <netent>-net

    <protoent>			<protoent-rtd>
    make-<protoent>		<protoent>?
    <protoent>-name
    <protoent>-aliases
    <protoent>-proto

    <servent>			<servent-rtd>
    make-<servent>		<servent>?
    <servent>-name
    <servent>-aliases
    <servent>-port
    <servent>-proto

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

    ;; enum-socket-protocol		socket-protocol
    ;; socket-protocol->value		value->socket-protocol

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


(define-record-type (<fd> integer-><fd> <fd>?)
  (nongenerative nausicaa:posix:<fd>)
  (fields (immutable object <fd>->integer)))

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


(define-record-type <passwd>
  (nongenerative nausicaa:posix:<passwd>)
  (fields (mutable name)
	  (mutable passwd)
	  (mutable uid)
	  (mutable gid)
	  (mutable gecos)
	  (mutable dir)
	  (mutable shell)))

(define <passwd-rtd>
  (record-type-descriptor <passwd>))

;;; --------------------------------------------------------------------

(define-record-type <group>
  (nongenerative nausicaa:posix:<group>)
  (fields (mutable name)
	  (mutable gid)
	  (mutable mem)))

(define <group-rtd>
  (record-type-descriptor <group>))


(define-record-type <utsname>
  (nongenerative nausicaa:posix:<utsname>)
  (fields (mutable sysname)
	  (mutable release)
	  (mutable version)
	  (mutable machine)))

(define <utsname-rtd>
  (record-type-descriptor <utsname>))


(define-record-type <fstab>
  (nongenerative nausicaa:posix:<fstab>)
  (fields (mutable spec)
	  (mutable file)
	  (mutable vfstype)
	  (mutable mntops)
	  (mutable type)
	  (mutable freq)
	  (mutable passno)))

(define <fstab-rtd>
  (record-type-descriptor <fstab>))

;;; --------------------------------------------------------------------

(define-record-type <mntent>
  (nongenerative nausicaa:posix:<mntent>)
  (fields (mutable fsname)
	  (mutable dir)
	  (mutable type)
	  (mutable opts)
	  (mutable freq)
	  (mutable passno)))

(define <mntent-rtd>
  (record-type-descriptor <mntent>))


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


(define-record-type <stat>
  (nongenerative nausicaa:posix:<stat>)
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

(define <stat-rtd>
  (record-type-descriptor <stat>))


(define-record-type <tms>
  (nongenerative nausicaa:posix:<tms>)
  (fields (mutable utime)
	  (mutable stime)
	  (mutable cutime)
	  (mutable cstime)))

(define <tms-rtd>
  (record-type-descriptor <tms>))

;;; --------------------------------------------------------------------

(define-record-type <timeval>
  (nongenerative nausicaa:posix:<timeval>)
  (fields (mutable sec)
	  (mutable usec)))

(define <timeval-rtd>
  (record-type-descriptor <timeval>))

;;; --------------------------------------------------------------------

(define-record-type <timespec>
  (nongenerative nausicaa:posix:<timespec>)
  (fields (mutable sec)
	  (mutable nsec)))

(define <timespec-rtd>
  (record-type-descriptor <timespec>))

;;; --------------------------------------------------------------------

(define-record-type <timezone>
  (nongenerative nausicaa:posix:<timezone>)
  (fields (mutable minuteswest)
	  (mutable dsttime)))

(define <timezone-rtd>
  (record-type-descriptor <timezone>))

;;; --------------------------------------------------------------------

(define-record-type <tm>
  (nongenerative nausicaa:posix:<tm>)
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

(define <tm-rtd>
  (record-type-descriptor <tm>))

;;; --------------------------------------------------------------------

(define-record-type <ntptimeval>
  (nongenerative nausicaa:posix:<ntptimeval>)
  (fields (mutable time)
	  (mutable maxerror)
	  (mutable esterror)))

(define <ntptimeval-rtd>
  (record-type-descriptor <ntptimeval>))

;;; --------------------------------------------------------------------

(define-record-type <timex>
  (nongenerative nausicaa:posix:<timex>)
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

(define <timex-rtd>
  (record-type-descriptor <timex>))

;;; --------------------------------------------------------------------

(define-record-type <itimerval>
  (nongenerative nausicaa:posix:<itimerval>)
  (fields (mutable interval)
	  (mutable value)))

(define <itimerval-rtd>
  (record-type-descriptor <itimerval>))


(define-record-type <sockaddr>
  (nongenerative nausicaa:posix:<sockaddr>)
  (fields (immutable family)))

(define <sockaddr-rtd>
  (record-type-descriptor <sockaddr>))

;;; --------------------------------------------------------------------

(define-record-type (<sockaddr-in> %make-<sockaddr-in> <sockaddr-in>?)
  (nongenerative nausicaa:posix:<sockaddr-in>)
  (parent <sockaddr>)
  (fields (mutable addr)
	  (mutable port)))

(define <sockaddr-in-rtd>
  (record-type-descriptor <sockaddr-in>))

(define (make-<sockaddr-in> addr port)
  (%make-<sockaddr-in> (socket-address-format inet) addr port))

(define <sockaddr-in>-family		<sockaddr>-family)

;;; --------------------------------------------------------------------

(define-record-type (<sockaddr-in6> %make-<sockaddr-in6> <sockaddr-in6>?)
  (nongenerative nausicaa:posix:<sockaddr-in6>)
  (parent <sockaddr>)
  (fields (mutable addr)
	  (mutable port)))

(define <sockaddr-in6-rtd>
  (record-type-descriptor <sockaddr-in6>))

(define (make-<sockaddr-in6> addr port)
  (%make-<sockaddr-in6> (socket-address-format inet6) addr port))

(define <sockaddr-in6>-family		<sockaddr>-family)

;;; --------------------------------------------------------------------

(define-record-type (<sockaddr-un> %make-<sockaddr-un> <sockaddr-un>?)
  (nongenerative nausicaa:posix:<sockaddr-un>)
  (parent <sockaddr>)
  (fields (mutable pathname)))

(define <sockaddr-un-rtd>
  (record-type-descriptor <sockaddr-un>))

(define (make-<sockaddr-un> pathname)
  (%make-<sockaddr-un> (socket-address-format local) pathname))

(define <sockaddr-un>-family		<sockaddr>-family)

;;; --------------------------------------------------------------------

(define-record-type <if-nameindex>
  (nongenerative nausicaa:posix:<if-nameindex>)
  (fields (immutable index)
	  (immutable name)))

(define <if-nameindex-rtd>
  (record-type-descriptor <if-nameindex>))


(define-record-type <netent>
  (nongenerative nausicaa:posix:<netent>)
  (fields (immutable name)
	  (immutable aliases)
	  (immutable addrtype)
	  (immutable net)))

(define <netent-rtd>
  (record-type-descriptor <netent>))

;;; --------------------------------------------------------------------

(define-record-type <hostent>
  (nongenerative nausicaa:posix:<hostent>)
  (fields (immutable name)
	  (immutable aliases)
	  (immutable addrtype)
	  (immutable addrlist)))

(define <hostent-rtd>
  (record-type-descriptor <hostent>))

;;; --------------------------------------------------------------------

(define-record-type <protoent>
  (nongenerative nausicaa:posix:<protoent>)
  (fields (immutable name)
	  (immutable aliases)
	  (immutable proto)))

(define <protoent-rtd>
  (record-type-descriptor <protoent>))

;;; --------------------------------------------------------------------

(define-record-type <servent>
  (nongenerative nausicaa:posix:<servent>)
  (fields (immutable name)
	  (immutable aliases)
	  (immutable port)
	  (immutable proto)))

(define <servent-rtd>
  (record-type-descriptor <servent>))


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
  (parent <fd>)
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

;; (define-c-flags socket-protocol
;;   (0)
;;   (zero))

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
