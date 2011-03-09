;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: read timezone information files
;;;Date: Thu Jul 15, 2010
;;;
;;;Abstract
;;;
;;;	Read a timezone information files in the format described by the
;;;	manual page tzinfo(5) available on GNU+Linux systems.
;;;
;;;---------------------------------------------------------------------
;;;
;;;TZFILE(5)    Linux Programmer's Manual       TZFILE(5)
;;;
;;;NAME
;;;       tzfile - timezone information
;;;
;;;SYNOPSIS
;;;       #include <tzfile.h>
;;;
;;;DESCRIPTION
;;;
;;;       The timezone information files used by tzset(3) begin with the
;;;       magic   characters  "TZif"  to   identify  then   as  timezone
;;;       information  files,  followed by  sixteen  bytes reserved  for
;;;       future  use, followed by  six four-byte  values of  type long,
;;;       written in a "standard" byte order (the high-order byte of the
;;;       value is written first).  These values are, in order:
;;;
;;;       tzh_ttisgmtcnt
;;;              The number of UTC/local indicators stored in the file.
;;;
;;;       tzh_ttisstdcnt
;;;              The  number of standard/wall  indicators stored  in the
;;;              file.
;;;
;;;       tzh_leapcnt
;;;              The number of leap seconds  for which data is stored in
;;;              the file.
;;;
;;;       tzh_timecnt
;;;              The  number of  "transition  times" for  which data  is
;;;              stored in the file.
;;;
;;;       tzh_typecnt
;;;              The  number of  "local time  types" for  which  data is
;;;              stored in the file (must not be zero).
;;;
;;;       tzh_charcnt
;;;              The  number  of  characters of  "timezone  abbreviation
;;;              strings" stored in the file.
;;;
;;;       The above header is followed by "tzh_timecnt" four-byte values
;;;       of  type long, sorted  in ascending  order.  These  values are
;;;       written  in  "standard"  byte   order.   Each  is  used  as  a
;;;       transition time  (as returned by  time(2)) at which  the rules
;;;       for  computing  local time  change.   Next come  "tzh_timecnt"
;;;       one-byte values of type  "unsigned char"; each one tells which
;;;       of the different types of  "local time" types described in the
;;;       file  is  associated with  the  same-indexed transition  time.
;;;       These  values  serve as  indices  into  an  array of  "ttinfo"
;;;       structures that appears next in the file; these structures are
;;;       defined as follows:
;;;
;;;           struct ttinfo {
;;;               long         tt_gmtoff;
;;;               int          tt_isdst;
;;;               unsigned int tt_abbrind;
;;;           };
;;;
;;;       Each structure is written as a four-byte value for "tt_gmtoff"
;;;       of  type "long",  in  a  standard byte  order,  followed by  a
;;;       one-byte  value  for  "tt_isdst"  and  a  one-byte  value  for
;;;       "tt_abbrind".  In each structure, "tt_gmtoff" gives the number
;;;       of  seconds  to be  added  to  UTC,  "tt_isdst" tells  whether
;;;       "tm_isdst"  should be  set by  localtime(3),  and "tt_abbrind"
;;;       serves  as an index  into the  array of  timezone abbreviation
;;;       characters that follow the "ttinfo" structure(s) in the file.
;;;
;;;       Then  there  are  "tzh_leapcnt"  pairs  of  four-byte  values,
;;;       written in standard  byte order; the first value  of each pair
;;;       gives the time (as returned by time(2)) at which a leap second
;;;       occurs; the second gives the "total" number of leap seconds to
;;;       be  applied after  the given  time.  The  pairs of  values are
;;;       sorted in ascending order by time.
;;;
;;;       Then there are "tzh_ttisstdcnt" standard/wall indicators, each
;;;       stored as  a one-byte value; they tell  whether the transition
;;;       times  associated  with local  time  types  were specified  as
;;;       standard time or wall clock time, and are used when a timezone
;;;       file  is  used in  handling  POSIX-style timezone  environment
;;;       variables.
;;;
;;;       Finally, there are "tzh_ttisgmtcnt" UTC/local indicators, each
;;;       stored as  a one-byte value; they tell  whether the transition
;;;       times associated  with local time types were  specified as UTC
;;;       or local  time, and are used  when a timezone file  is used in
;;;       handling POSIX-style timezone environment variables.
;;;
;;;       "Localtime" uses the first standard-time "ttinfo" structure in
;;;       the  file  (or simply  the  first  "ttinfo"  structure in  the
;;;       absence of a  standard-time structure) if either "tzh_timecnt"
;;;       is zero or the time argument is less than the first transition
;;;       time recorded in the file.
;;;
;;;COLOPHON
;;;       This  page is  part of  release  3.15 of  the Linux  man-pages
;;;       project.  A description of  the project, and information about
;;;       reporting bugs, can be found at:
;;;
;;;                     http://www.kernel.org/doc/man-pages/
;;;
;;;                             1996-06-05                     TZFILE(5)
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;The author hereby grant  permission to use, copy, modify, distribute,
;;;and  license this  software and  its documentation  for  any purpose,
;;;provided that  existing copyright notices are retained  in all copies
;;;and that  this notice is  included verbatim in any  distributions. No
;;;written agreement, license, or royalty fee is required for any of the
;;;authorized uses.   Modifications to this software  may be copyrighted
;;;by their  authors and need  not follow the licensing  terms described
;;;here, provided that the new  terms are clearly indicated on the first
;;;page of each file where they apply.
;;;
;;;IN NO EVENT  SHALL THE AUTHOR OR DISTRIBUTORS BE  LIABLE TO ANY PARTY
;;;FOR DIRECT,  INDIRECT, SPECIAL, INCIDENTAL,  OR CONSEQUENTIAL DAMAGES
;;;ARISING OUT  OF THE USE OF  THIS SOFTWARE, ITS  DOCUMENTATION, OR ANY
;;;DERIVATIVES  THEREOF, EVEN  IF THE  AUTHOR HAVE  BEEN ADVISED  OF THE
;;;POSSIBILITY OF SUCH DAMAGE.
;;;
;;;THE  AUTHOR AND  DISTRIBUTORS SPECIFICALLY  DISCLAIM  ANY WARRANTIES,
;;;INCLUDING,   BUT  NOT   LIMITED   TO,  THE   IMPLIED  WARRANTIES   OF
;;;MERCHANTABILITY,    FITNESS   FOR    A   PARTICULAR    PURPOSE,   AND
;;;NON-INFRINGEMENT.  THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, AND
;;;THE   AUTHOR  AND   DISTRIBUTORS  HAVE   NO  OBLIGATION   TO  PROVIDE
;;;MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
;;;


#!r6rs
(import (rnrs))

(define (main pathname)
  (define zoneinfo
    (load-zoneinfo-file pathname))
  (define zih
    (make-<zoneinfo-header> zoneinfo))
  (define transition-times
    (zoneinfo-read-transition-times zoneinfo zih))
  (define transition-times-indexes
    (zoneinfo-read-transition-times-indexes zoneinfo zih))
  (define time-types-infos
    (zoneinfo-read-time-type-infos zoneinfo zih))

  (<zoneinfo-header>-pretty-print zih (current-error-port))
  (write (length transition-times))(newline)
  (write transition-times)(newline)
  (write transition-times-indexes)(newline)
  (vector-for-each (lambda (ttinfo)
		     (<time-type-info>-pretty-print ttinfo (current-error-port)))
		   time-types-infos)

  (exit 0))


(define (load-zoneinfo-file pathname)
  ;;Load  the  file referenced  by  the  string  PATHNAME and  return  a
  ;;bytevector  of its  whole contents.   Validate the  file  header and
  ;;raise and exception if it is invalid.
  ;;
  (let ((port (open-file-input-port pathname (file-options no-create no-truncate))))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (let ((zoneinfo (get-bytevector-all port)))
	    (validate-head zoneinfo)
	    zoneinfo))
	(lambda ()
	  (close-input-port port)))))

(define (validate-head zoneinfo)
  ;;The  first 4  bytes of  a zoneinfo  file must  be the  ASCII encoded
  ;;string "TZif"; raise an exception if they are not.
  ;;
  (let ((head (let-values (((port getter) (open-string-output-port)))
		(do ((i 0 (+ 1 i)))
		    ((= i 4)
		     (getter))
		  (put-char port (integer->char (bytevector-u8-ref zoneinfo i)))))))
    (unless (string=? "TZif" head)
      (error 'validate-head
	(string-append "error: head of file is '" head "' rather than 'TZif'\n")))))


(define-record-type <zoneinfo-header>
  (nongenerative zoneinfo:<zoneinfo-header>)
  (fields (immutable ttisgmtcnt)
		;the number of UTC/local indicators stored in the file
	  (immutable ttisstdcnt)
		;the  number of standard/wall  indicators stored  in the
		;file
	  (immutable leapcnt)
		;the number of leap seconds  for which data is stored in
		;the file
	  (immutable timecnt)
		;the  number of  "transition  times" for  which data  is
		;stored in the file
	  (immutable typecnt)
		;the  number of  "local time  types" for  which  data is
		;stored in the file (must not be zero)
	  (immutable charcnt))
		;the  number  of  characters of  "timezone  abbreviation
		;strings" stored in the file
  (protocol (lambda (make-record)
	      (lambda (zoneinfo)
		(make-record (%extract-long zoneinfo $tzh-ttisgmtcnt-offset)
			     (%extract-long zoneinfo $tzh-ttisstdcnt-offset)
			     (%extract-long zoneinfo $tzh-leapcnt-offset)
			     (%extract-long zoneinfo $tzh-timecnt-offset)
			     (%extract-long zoneinfo $tzh-typecnt-offset)
			     (%extract-long zoneinfo $tzh-charcnt-offset))))))

(define (<zoneinfo-header>-pretty-print zih port)
  (define-syntax %display
    (syntax-rules ()
      ((_ ?v) (display ?v port))))
  (define (%print-field name accessor)
    (%display (string-append " " name "="))
    (%display (number->string (accessor zih))))
  (%display "#<<zoneinfo-header>")
  (%print-field "ttisgmtcnt"	<zoneinfo-header>-ttisgmtcnt)
  (%print-field "ttisstdcnt"	<zoneinfo-header>-ttisstdcnt)
  (%print-field "leapcnt"	<zoneinfo-header>-leapcnt)
  (%print-field "timecnt"	<zoneinfo-header>-timecnt)
  (%print-field "typecnt"	<zoneinfo-header>-typecnt)
  (%print-field "charcnt"	<zoneinfo-header>-charcnt)
  (%display ">\n"))


(define (zoneinfo-read-transition-times zoneinfo zih)
  ;;Return  the list  of  local-time rule-changing  transition times  in
  ;;ascending order.
  ;;
  (assert (bytevector? zoneinfo))
  (assert (<zoneinfo-header>? zih))
  (let ((count (<zoneinfo-header>-timecnt zih)))
    (let loop ((i      count)
	       (offset (+ $first-rule-transition-time-offset (* 4 (- count 1))))
	       (times  '()))
      (if (zero? i)
	  times
	(loop (- i 1) (- offset 4) (cons (%extract-long zoneinfo offset) times))))))

(define (zoneinfo-read-transition-times-indexes zoneinfo zih)
  ;;Return the list of indexes  into the array of time types associating
  ;;the selected array element with a transition time.
  ;;
  (assert (bytevector? zoneinfo))
  (assert (<zoneinfo-header>? zih))
  (let ((count (<zoneinfo-header>-timecnt zih)))
    (let loop ((i      count)
	       (offset (+ count $first-rule-transition-time-offset (* 4 count)))
	       (times  '()))
      (if (zero? i)
	  times
	(loop (- i 1) (- offset 1) (cons (%extract-unsigned-char zoneinfo offset) times))))))


(define-record-type <time-type-info>
  (nongenerative zoneinfo:<time-type-info>)
  (fields (immutable gmtoff)
	  (immutable isdst)
	  (immutable abbrind))
  (protocol (lambda (make-record)
	      (lambda (zoneinfo offset)
		(make-record (%extract-long zoneinfo offset)
			     (not (zero? (bytevector-s8-ref zoneinfo (+ $sizeof-long offset))))
			     (bytevector-u8-ref zoneinfo (+ 1 $sizeof-long offset)))))))

(define (<time-type-info>-pretty-print ttinfo port)
  (define-syntax %display
    (syntax-rules ()
      ((_ ?v) (display ?v port))))
  (define (%print-field name accessor)
    (%display (string-append " " name "="))
    (let ((v (accessor ttinfo)))
      (%display (if (number? v)
		    (number->string v)
		  v))))
  (%display "#<<time-type-info>")
  (%print-field "gmtoff"	<time-type-info>-gmtoff)
  (%print-field "isdst"		<time-type-info>-isdst)
  (%print-field "abbrind"	<time-type-info>-abbrind)
  (%display ">\n"))

(define (zoneinfo-read-time-type-infos zoneinfo zih)
  ;;Return  the vector of  <time-type-info> records  describing timezone
  ;;rules.
  ;;
  (assert (bytevector? zoneinfo))
  (assert (<zoneinfo-header>? zih))
  (let* ((count		(<zoneinfo-header>-typecnt zih))
	 (ttinfos	(make-vector count)))
    (let loop ((i	0)
	       (offset	($first-rule-transition-time-index-offset zih)))
      (if (= i count)
	  ttinfos
	(begin
	  (vector-set! ttinfos i (make-<time-type-info> zoneinfo offset))
	  (loop (+ 1 i)
		(+ offset $sizeof-ttinfo)))))))


;;;; data offsets in zoneinfo file

(define $sizeof-long	4)
(define $sizeof-ttinfo	6)

(define $tzh-ttisgmtcnt-offset	20)
(define $tzh-ttisstdcnt-offset	24)
(define $tzh-leapcnt-offset	28)
(define $tzh-timecnt-offset	32)
(define $tzh-typecnt-offset	36)
(define $tzh-charcnt-offset	40)

(define $first-rule-transition-time-offset	44)
(define ($last-rule-transition-time-offset zih)
  (assert (<zoneinfo-header>? zih))
  (+ $first-rule-transition-time-offset
     (* $sizeof-long (- (<zoneinfo-header>-timecnt zih) 1))))

(define ($first-rule-transition-time-index-offset zih)
  (assert (<zoneinfo-header>? zih))
  (+ $sizeof-long ($last-rule-transition-time-offset zih)))

(define ($last-rule-transition-time-index-offset zih)
  (+ ($first-rule-transition-time-index-offset zih)
     (* $sizeof-ttinfo (- (<zoneinfo-header>-typecnt zih) 1))))



;;;; value extractors from the zoneinfo bytevector

(define (%extract-long zoneinfo offset)
  (bytevector-uint-ref zoneinfo offset (endianness big) 4))

(define (%extract-unsigned-char zoneinfo offset)
  (bytevector-u8-ref zoneinfo offset))



;;;; do it

(main (cadr (command-line)))

;;; end of file
