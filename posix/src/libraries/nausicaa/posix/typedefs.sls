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


(library (nausicaa posix typedefs)
  (export

    fd				fd?
    integer->fd			fd->integer

    FILE*			FILE*?
    pointer->FILE*		FILE*->pointer

    fdset
    make-fdset			fdset?
    pointer->fdset		fdset->pointer

    uid				uid?
    integer->uid		uid->integer

    gid				gid?
    integer->gid		gid->integer
    pid				pid?
    integer->pid		pid->integer

    uid=? gid=?
    pid-zero? pid=? pid<? pid<=? pid>? pid>=? pid<>?

;;; --------------------------------------------------------------------

    <process-term-status>
    make-<process-term-status>	<process-term-status>?
    WIFEXITED?			WEXITSTATUS?
    WIFSIGNALED?		WTERMSIG?
    WCOREDUMP?			WIFSTOPPED?
    WSTOPSIG?

;;; --------------------------------------------------------------------

    <stat>
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

    <socket>
    make-<socket>		<socket>?
    <socket>-namespace
    <socket>-style
    <socket>-protocol

;;; --------------------------------------------------------------------

    enum-access-permissions		access-permissions
    access-permissions->value		value->access-permissions

;;; --------------------------------------------------------------------

    enum-open-mode			open-mode
    open-mode->value			value->open-mode

;;; --------------------------------------------------------------------

    enum-pipe2-flags			pipe2-flags
    pipe2-flags->value			value->pipe2-flags

;;; --------------------------------------------------------------------

    enum-access-flags			access-flags
    access-flags->value			value->access-flags

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
  (import (nausicaa)
    (nausicaa enumerations)
    (nausicaa posix sizeof))


(define-class (fd integer->fd fd?)
  (nongenerative nausicaa:posix:fd)
  (fields (immutable object fd->integer)))

(define-class (FILE* pointer->FILE* FILE*?)
  (nongenerative nausicaa:posix:FILE*)
  (fields (immutable object FILE*->pointer)))

(define-class (uid integer->uid uid?)
  (nongenerative nausicaa:posix:uid)
  (fields (immutable object uid->integer)))

(define-class (gid integer->gid gid?)
  (nongenerative nausicaa:posix:gid)
  (fields (immutable object gid->integer)))

(define-class (pid integer->pid pid?)
  (nongenerative nausicaa:posix:pid)
  (fields (immutable object pid->integer)))

(define (uid=? a b)
  (= (uid->integer a) (uid->integer b)))

(define (gid=? a b)
  (= (gid->integer a) (gid->integer b)))

(define (pid-zero? obj)
  (and (pid? obj) (= 0 (pid->integer obj))))

(let-syntax
    ((define-pid-operation (syntax-rules ()
			     ((_ ?name ?op)
			      (define (?name a b)
				(?op (pid->integer a) (pid->integer b)))))))
  (define-pid-operation pid=?  =)
  (define-pid-operation pid<?  <)
  (define-pid-operation pid<=? <=)
  (define-pid-operation pid>?  >)
  (define-pid-operation pid>=? >=))

(define (pid<>? a b)
  (not (= (pid->integer a) (pid->integer b))))

;;; --------------------------------------------------------------------

(define-class (fdset pointer->fdset fdset?)
  (nongenerative nausicaa:posix:fdset)
  (fields (immutable object fdset->pointer)))

(define (make-fdset malloc)
  (pointer->fdset (malloc (c-sizeof struct-fdset))))

;;; --------------------------------------------------------------------

(define-class (struct-flock pointer->struct-flock struct-flock?)
  (nongenerative nausicaa:posix:struct-flock)
  (fields (immutable object struct-flock->pointer)))

(define (make-struct-flock malloc)
  (pointer->struct-flock (malloc (c-sizeof struct-flock))))


(define-class <process-term-status>
  (nongenerative nausicaa:posix:<process-term-status>)
  (fields (immutable WIFEXITED		WIFEXITED?)
	  (immutable WEXITSTATUS	WEXITSTATUS?)
	  (immutable WIFSIGNALED	WIFSIGNALED?)
	  (immutable WTERMSIG		WTERMSIG?)
	  (immutable WCOREDUMP		WCOREDUMP?)
	  (immutable WIFSTOPPED		WIFSTOPPED?)
	  (immutable WSTOPSIG		WSTOPSIG?)))


(define-class <stat>
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


(define-class <socket>
  (nongenerative nausicaa:posix:<socket>)
  (inherit fd)
  (fields (immutable namespace)
	  (immutable style)
	  (immutable protocol)))


(define-c-ior-flags access-permissions
  (S_IRUSR S_IWUSR S_IXUSR
   S_IRGRP S_IWGRP S_IXGRP
   S_IROTH S_IWOTH S_IXOTH
   S_ISUID S_ISGID S_ISVTX)
  (user-read user-write user-exec
   group-read group-write group-exec
   other-read other-write other-exec
   setuid setgid sticky))

(define-c-ior-flags open-mode
  (O_ACCMODE	O_APPEND	O_ASYNC
   O_CREAT	O_EXCL		O_EXEC
   O_EXLOCK	O_FSYNC		O_IGNORE_CTTY
   O_NDELAY	O_NOATIME	O_NOCTTY
   O_NOLINK	O_NONBLOCK	O_NOTRANS
   O_RDONLY	O_RDWR		O_READ
   O_SHLOCK	O_SYNC		O_TRUNC
   O_WRITE	O_WRONLY

   O_DIRECT	O_DIRECTORY	O_LARGEFILE
   O_NOFOLLOW)
  (accmode	append		async
   creat	excl		exec
   exlock	fsync		ignore_ctty
   ndelay	noatime		noctty
   nolink	nonblock	notrans
   rdonly	rdwr		read
   shlock	sync		trunc
   write	wronly

   direct	directory	largefile
   nofollow))

(define-c-ior-flags pipe2-flags
  (O_NONBLOCK	O_CLOEXEC)
  (nonblock	cloexec))

(define-c-ior-flags access-flags
  (F_OK R_OK W_OK X_OK)
  (existence read write exec))


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
	 (c-sizeof struct-sockaddr_un))
	((enum-set=? set (socket-namespace inet))
	 (c-sizeof struct-sockaddr_in))
	((enum-set=? set (socket-namespace inet6))
	 (c-sizeof struct-sockaddr_in6))
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
