;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level functions for URI handling
;;;Date: Fri Jun  4, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (uri low)
  (export

    ;; percent character encoding/decoding
    to-utf8			to-string
    percent-encode		percent-decode
    normalise-percent-encoded-string
    )
  (import (nausicaa)
    (rnrs mutable-strings))


;;;; percent encoding/decoding of characters

(define-constant $int-%		(char->integer #\%))
(define-constant $int-a		(char->integer #\a))
(define-constant $int-z		(char->integer #\z))
(define-constant $int-A		(char->integer #\A))
(define-constant $int-Z		(char->integer #\Z))
(define-constant $int-0		(char->integer #\0))
(define-constant $int-9		(char->integer #\9))
(define-constant $int-dash	(char->integer #\-))
(define-constant $int-dot	(char->integer #\.))
(define-constant $int-underscore(char->integer #\_))
(define-constant $int-tilde	(char->integer #\~))

(define-constant $percent-encoder-table
  '#("%00" "%01" "%02" "%03" "%04" "%05" "%06" "%07" "%08" "%09" "%0a" "%0b" "%0c" "%0d" "%0e" "%0f"
     "%10" "%11" "%12" "%13" "%14" "%15" "%16" "%17" "%18" "%19" "%1a" "%1b" "%1c" "%1d" "%1e" "%1f"
     "%20" "%21" "%22" "%23" "%24" "%25" "%26" "%27" "%28" "%29" "%2a" "%2b" "%2c" "%2d" "%2e" "%2f"
     "%30" "%31" "%32" "%33" "%34" "%35" "%36" "%37" "%38" "%39" "%3a" "%3b" "%3c" "%3d" "%3e" "%3f"
     "%40" "%41" "%42" "%43" "%44" "%45" "%46" "%47" "%48" "%49" "%4a" "%4b" "%4c" "%4d" "%4e" "%4f"
     "%50" "%51" "%52" "%53" "%54" "%55" "%56" "%57" "%58" "%59" "%5a" "%5b" "%5c" "%5d" "%5e" "%5f"
     "%60" "%61" "%62" "%63" "%64" "%65" "%66" "%67" "%68" "%69" "%6a" "%6b" "%6c" "%6d" "%6e" "%6f"
     "%70" "%71" "%72" "%73" "%74" "%75" "%76" "%77" "%78" "%79" "%7a" "%7b" "%7c" "%7d" "%7e" "%7f"
     "%80" "%81" "%82" "%83" "%84" "%85" "%86" "%87" "%88" "%89" "%8a" "%8b" "%8c" "%8d" "%8e" "%8f"
     "%90" "%91" "%92" "%93" "%94" "%95" "%96" "%97" "%98" "%99" "%9a" "%9b" "%9c" "%9d" "%9e" "%9f"
     "%a0" "%a1" "%a2" "%a3" "%a4" "%a5" "%a6" "%a7" "%a8" "%a9" "%aa" "%ab" "%ac" "%ad" "%ae" "%af"
     "%b0" "%b1" "%b2" "%b3" "%b4" "%b5" "%b6" "%b7" "%b8" "%b9" "%ba" "%bb" "%bc" "%bd" "%be" "%bf"
     "%c0" "%c1" "%c2" "%c3" "%c4" "%c5" "%c6" "%c7" "%c8" "%c9" "%ca" "%cb" "%cc" "%cd" "%ce" "%cf"
     "%d0" "%d1" "%d2" "%d3" "%d4" "%d5" "%d6" "%d7" "%d8" "%d9" "%da" "%db" "%dc" "%dd" "%de" "%df"
     "%e0" "%e1" "%e2" "%e3" "%e4" "%e5" "%e6" "%e7" "%e8" "%e9" "%ea" "%eb" "%ec" "%ed" "%ee" "%ef"
     "%f0" "%f1" "%f2" "%f3" "%f4" "%f5" "%f6" "%f7" "%f8" "%f9" "%fa" "%fb" "%fc" "%fd" "%fe" "%ff"))

(define (unreserved-char? obj)
  ;;Return true  if OBJ represents an unreserved  character according to
  ;;the RFC.  OBJ can be either a character or an integer representing a
  ;;character according to CHAR->INTEGER.
  ;;
  (let ((chi (cond ((char? obj)
		    (char->integer obj))
		   ((and (integer? obj) (exact? obj))
		    obj)
		   (else
		    #f))))
    (and obj
	 (or (<= $int-a chi $int-z)
	     (<= $int-A chi $int-Z)
	     (<= $int-0 chi $int-9)
	     (= chi $int-dash)
	     (= chi $int-dot)
	     (= chi $int-underscore)
	     (= chi $int-tilde)))))

(define (not-unreserved-char? obj)
  (not (unreserved-char? obj)))

(define (to-utf8 obj)
  ;;Convert the  string OBJ  to a bytevector  representation; characters
  ;;are taken from OBJ and  inserted into the resulting bytevector after
  ;;decoding  them,  char  to byte,  as  ASCII.   If  OBJ is  already  a
  ;;bytevector: OBJ itself is returned.
  ;;
  (if (bytevector? obj)
      obj
    (let* ((len (string-length obj))
	   (bv  (make-bytevector len)))
      (dotimes (i len bv)
	(bytevector-u8-set! bv i (char->integer (string-ref obj i)))))))

(define (to-string bv)
  ;;Convert  the bytevector OBJ  to a  string representation;  bytes are
  ;;taken from OBJ and inserted into the resulting string after encoding
  ;;them,  byte to  char, as  ASCII.  If  OBJ is  already a  string: OBJ
  ;;itself is returned.
  ;;
  (if (string? bv)
      bv
    (let* ((len (bytevector-length bv))
	   (str (make-string len)))
      (dotimes (i len str)
	(string-set! str i (integer->char (bytevector-u8-ref bv i)))))))

(define-maker (percent-encode obj)
  %percent-encode
  ((:char-selector	not-unreserved-char?)
   (:bytevector-result?	#f)))

(define (%percent-encode obj char-encode? bytevector-result?)
  ;;Return a percent-encoded string  representation of OBJ, which can be
  ;;a char or string or bytevector.
  ;;
  (let ((bv (cond ((string? obj)
		   (string->utf8 obj))
		  ((bytevector? obj)
		   obj)
		  ((char? obj)
		   (string->utf8 (string obj)))
		  (else
		   (assertion-violation 'percent-encode
		     "expected char, string or bytevector as input" obj)))))
    (let-values (((port getter)	(open-string-output-port))
		 ((len)		(bytevector-length bv)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (if bytevector-result?
	       (to-utf8 (getter))
	     (getter)))
	(let ((chi (bytevector-u8-ref bv i)))
	  (if (char-encode? chi)
	      (display (vector-ref $percent-encoder-table chi) port)
	    (put-char port (integer->char chi))))))))

(define-maker (percent-decode obj)
  %percent-decode
  ((:bytevector-result?	#f)))

(define (%percent-decode obj bytevector-result?)
  ;;Percent-decode the given string.  Return the decoded string.
  ;;
  (let ((bv	(cond ((string? obj)
		       (string->utf8 obj))
		      ((bytevector? obj)
		       obj)
		      (else
		       (assertion-violation 'percent-decode
			 "expected string or bytevector as input" obj))))
	(buf	(make-string 2)))
    (let-values (((port getter)	(open-string-output-port))
		 ((len)		(bytevector-length bv)))
      (let loop ((i 0))
	(if (= i len)
	    (if bytevector-result?
		(to-utf8 (getter))
	      (getter))
	  (let ((chi (bytevector-u8-ref bv i)))
	    (if (= chi $int-%)
		(begin
		  (incr! i)
		  (string-set! buf 0 (integer->char (bytevector-u8-ref bv i)))
		  (incr! i)
		  (string-set! buf 1 (integer->char (bytevector-u8-ref bv i)))
		  (put-char port (integer->char (string->number buf 16)))
		  (loop (+ 1 i)))
	      (begin
		(put-char port (integer->char chi))
		(loop (+ 1 i))))))))))

(define (normalise-percent-encoded-string in-str)
  ;;Normalise the  given percent-encoded string; chars  that are encoded
  ;;but should not are decoded.  Return the normalised string.
  ;;
  ;;We assume that  IN-STR is composed by characters  in the valid range
  ;;for URIs.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((len)		(string-length in-str)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (getter))
      (let ((ch (string-ref in-str i)))
	(if (char=? ch #\%)
	    (let* ((pc (substring in-str (+ 1 i) (+ 3 i)))
		   (ch (integer->char (string->number pc 16))))
	      (set! i (+ 2 i))
	      (if (unreserved-char? ch)
		  (put-char port ch)
		(begin
		  (put-char port #\%)
		  (display pc port))))
	  (put-char port ch))))))


;;;; done

)

;;; end of file
