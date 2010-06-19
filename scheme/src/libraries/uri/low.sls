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

    ;; plain string <-> bytevector conversion
    to-bytevector			to-string

    ;; percent character encoding/decoding
    unreserved-char?
    percent-encode			percent-decode
    normalise-percent-encoded-string	normalise-percent-encoded-bytevector

    ;; parser functions
    parse-scheme	parse-hier-part
    parse-query		parse-fragment
    valid-segment?
    )
  (import (nausicaa)
    (rnrs mutable-strings))


;;;; constants

(define-constant $int-a		(char->integer #\a))
(define-constant $int-f		(char->integer #\f))
(define-constant $int-z		(char->integer #\z))
(define-constant $int-A		(char->integer #\A))
(define-constant $int-F		(char->integer #\F))
(define-constant $int-Z		(char->integer #\Z))
(define-constant $int-0		(char->integer #\0))
(define-constant $int-9		(char->integer #\9))

(define-constant $int-percent		(char->integer #\%))
(define-constant $int-minus		(char->integer #\-))

;; gen-delims
(define-constant $int-colon		(char->integer #\:))
(define-constant $int-slash		(char->integer #\/))
(define-constant $int-question-mark	(char->integer #\?))
(define-constant $int-number-sign	(char->integer #\#))
(define-constant $int-open-bracket	(char->integer #\[))
(define-constant $int-close-bracket	(char->integer #\]))
(define-constant $int-at-sign		(char->integer #\@))

;; sub-delims
(define-constant $int-bang		(char->integer #\!))
(define-constant $int-dollar		(char->integer #\$))
(define-constant $int-ampersand		(char->integer #\&))
(define-constant $int-quote		(char->integer #\'))
(define-constant $int-open-paren	(char->integer #\())
(define-constant $int-close-paren	(char->integer #\)))
(define-constant $int-star		(char->integer #\*))
(define-constant $int-plus		(char->integer #\+))
(define-constant $int-comma		(char->integer #\,))
(define-constant $int-semicolon		(char->integer #\;))
(define-constant $int-equal		(char->integer #\=))

;; unreserved
(define-constant $int-dash		(char->integer #\-))
(define-constant $int-dot		(char->integer #\.))
(define-constant $int-underscore	(char->integer #\_))
(define-constant $int-tilde		(char->integer #\~))


;;;; char integer predicates

(define-inline (is-alpha? chi)
  (or (<= $int-A chi $int-Z)
      (<= $int-a chi $int-z)))

(define-inline (is-dec-digit? chi)
  (<= $int-0 chi $int-9))

(define-inline (is-hex-digit? chi)
  (or (is-dec-digit? chi)
      (<= $int-A chi $int-F)
      (<= $int-a chi $int-f)))

(define-inline (is-alpha-digit? chi)
  (or (is-alpha? chi)
      (is-dec-digit? chi)))

(define-inline (is-gen-delim? chi)
  (or (= chi $int-colon)
      (= chi $int-slash)
      (= chi $int-question-mark)
      (= chi $int-number-sign)
      (= chi $int-open-bracket)
      (= chi $int-close-bracket)
      (= chi $int-at-sign)))

(define-inline (is-sub-delim? chi)
  (or (= chi $int-bang)
      (= chi $int-dollar)
      (= chi $int-ampersand)
      (= chi $int-quote)
      (= chi $int-open-paren)
      (= chi $int-close-paren)
      (= chi $int-star)
      (= chi $int-plus)
      (= chi $int-comma)
      (= chi $int-semicolon)
      (= chi $int-equal)))

(define-inline (is-reserved? chi)
  (or (is-gen-delim? chi)
      (is-sub-delim? chi)))

(define-inline (is-unreserved? chi)
  (or (is-alpha-digit? chi)
      (= chi $int-dash)
      (= chi $int-dot)
      (= chi $int-underscore)
      (= chi $int-tilde)))


;;;; plain string <-> bytevector conversion

(define (to-bytevector obj)
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


;;;; percent encoding/decoding

(define-constant $percent-encoder-table
  (list->vector
   (map to-bytevector
     '("%00" "%01" "%02" "%03" "%04" "%05" "%06" "%07" "%08" "%09" "%0a" "%0b" "%0c" "%0d" "%0e" "%0f"
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
       "%f0" "%f1" "%f2" "%f3" "%f4" "%f5" "%f6" "%f7" "%f8" "%f9" "%fa" "%fb" "%fc" "%fd" "%fe" "%ff"))))

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
    (and chi (is-unreserved? chi))))

(define (not-unreserved-char? obj)
  (not (unreserved-char? obj)))

(define-maker (percent-encode obj)
  %percent-encode
  ((:char-selector	not-unreserved-char?)
   (:string-result?	#f)))

(define (%percent-encode obj char-encode? string-result?)
  ;;Return a percent-encoded bytevector or string representation of OBJ,
  ;;which can be a char or string or bytevector.
  ;;
  (let ((bv (cond ((bytevector? obj)
		   obj)
		  ((string? obj)
		   (string->utf8 obj))
		  ((char? obj)
		   (string->utf8 (string obj)))
		  (else
		   (assertion-violation 'percent-encode
		     "expected char, string or bytevector as input" obj)))))
    (let-values (((port getter)	(open-bytevector-output-port))
		 ((len)		(bytevector-length bv)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (if string-result?
	       (to-string (getter))
	     (getter)))
	(let ((chi (bytevector-u8-ref bv i)))
	  (if (char-encode? chi)
	      (put-bytevector port (vector-ref $percent-encoder-table chi))
	    (put-u8 port chi)))))))

(define-maker (percent-decode obj)
  %percent-decode
  ((:string-result?	#f)))

(define (%percent-decode obj string-result?)
  ;;Percent-decode the  given object;  return the decoded  bytevector or
  ;;string.
  ;;
  (let ((bv	(cond ((string? obj)
		       (string->utf8 obj))
		      ((bytevector? obj)
		       obj)
		      (else
		       (assertion-violation 'percent-decode
			 "expected string or bytevector as input" obj))))
	(buf	(make-string 2)))
    (let-values (((port getter)	(open-bytevector-output-port))
		 ((len)		(bytevector-length bv)))
      (let loop ((i 0))
	(if (= i len)
	    (if string-result?
		(to-string (getter))
	      (getter))
	  (let ((chi (bytevector-u8-ref bv i)))
	    (put-u8 port (if (= chi $int-percent)
			     (begin
			       (incr! i)
			       (string-set! buf 0 (integer->char (bytevector-u8-ref bv i)))
			       (incr! i)
			       (string-set! buf 1 (integer->char (bytevector-u8-ref bv i)))
			       (string->number buf 16))
			   chi))
	    (loop (+ 1 i))))))))


;;;; percent-encoding normalisation

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

(define (normalise-percent-encoded-bytevector in-bv)
  ;;Normalise the  given percent-encoded string; chars  that are encoded
  ;;but should not are decoded.  Return the normalised string.
  ;;
  ;;We assume that  IN-STR is composed by characters  in the valid range
  ;;for URIs.
  ;;
  (let-values (((port getter)	(open-bytevector-output-port))
	       ((len)		(bytevector-length in-bv))
	       ((buf)		(make-string 2)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (getter))
      (let ((chi (bytevector-u8-ref in-bv i)))
	(if (= chi $int-percent)
	    (begin
	      (incr! i)
	      (string-set! buf 0 (integer->char (bytevector-u8-ref in-bv i)))
	      (incr! i)
	      (string-set! buf 1 (integer->char (bytevector-u8-ref in-bv i)))
	      (let ((chi (string->number buf 16)))
		(if (unreserved-char? chi)
		    (put-u8 port chi)
		  (begin
		    (put-u8 port $int-percent)
		    (put-bytevector port in-bv (- i 1) 2)))))
	  (put-u8 port chi))))))


;;;; element parsers

(define (parse-scheme in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a scheme URI
  ;;element.   If a  colon is  found:  return a  bytevector holding  the
  ;;accumulated bytes, colon excluded; else return false.
  ;;
  ;;When successful: leave  the port position to the  byte after the one
  ;;representing the colon; if an error occurs: rewind the port position
  ;;to the one before this function call.
  ;;
  (let ((start-position (port-position in-port)))
    (define-inline (set-position-start!)
      (set-port-position! in-port start-position))
    (define-inline (return-failure)
      (begin
	(set-position-start!)
	#f))
    (receive (ou-port getter)
	(open-bytevector-output-port)
      (let ((chi (get-u8 in-port)))
	(cond ((eof-object? chi)
	       #f)
	      ((is-alpha-digit? chi)
	       (put-u8 ou-port chi)
	       (let loop ((chi (get-u8 in-port)))
		 (if (eof-object? chi)
		     (return-failure)
		   (cond ((or (is-alpha-digit? chi)
			      (= chi $int-plus)
			      (= chi $int-minus)
			      (= chi $int-dot))
			  (put-u8 ou-port chi)
			  (loop (get-u8 in-port)))
			 ((= chi $int-colon)
			  (getter))
			 (else
			  (return-failure))))))
	      (else
	       (return-failure)))))))

(define (parse-hier-part in-port)
  ;;Accumulate bytes from  IN-PORT while they are valid  for a hier-part
  ;;URI segment.  If a EOF or a question mark or a number sign is found:
  ;;return a bytevector holding  the accumulated bytes, question mark or
  ;;number sign excluded; else return false.  Leave the port position to
  ;;the byte after the last byte of the hier-part.
  ;;
  ;;An empty hier-part is not accepted: if the first value from the port
  ;;is EOF, the return value is false.
  ;;
  (define-inline (set-position-back-one!)
    (set-port-position! in-port (- (port-position in-port) 1)))
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (if (eof-object? chi)
	  #f ;forbid empty hier-part
	(begin
	  (put-u8 ou-port chi)
	  (let loop ((chi (get-u8 in-port)))
	    (cond ((eof-object? chi)
		   (getter))
		  ((or (= chi $int-question-mark)
		       (= chi $int-number-sign))
		   (set-position-back-one!)
		   (getter))
		  (else
		   (put-u8 ou-port chi)
		   (loop (get-u8 in-port))))))))))

(define (parse-query in-port)
  ;;Accumulate bytes from  IN-PORT while they are valid  for a query URI
  ;;segment; the first  byte read from IN-PORT must  be a question mark.
  ;;If an EOF or a number  sign is read: return a bytevector holding the
  ;;accumulated bytes, starting question mark excluded and ending number
  ;;sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte  of the  query segment;  if an  error occurs:  rewind  the port
  ;;position to the one before this function call.
  ;;
  ;;Notice  that  an empty  query  segment  is  valid (a  question  mark
  ;;followed by EOF).
  ;;
  (let ((start-position (port-position in-port)))
    (define-inline (set-position-start!)
      (set-port-position! in-port start-position))
    (define-inline (set-position-back-one!)
      (set-port-position! in-port (- (port-position in-port) 1)))
    (define-inline (return-failure)
      (begin
	(set-position-start!)
	#f))
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     #f)
	    ((= chi $int-question-mark)
	     (receive (ou-port getter)
		 (open-bytevector-output-port)
	       (let loop ((chi (get-u8 in-port)))
		 (cond ((eof-object? chi)
			(getter))
		       ((= chi $int-number-sign)
			(set-position-back-one!)
			(getter))
		       ((or (is-unreserved? chi)
			    (is-sub-delim? chi)
			    (= chi $int-slash)
			    (= chi $int-question-mark)
			    (= chi $int-colon)
			    (= chi $int-at-sign))
			(put-u8 ou-port chi)
			(loop (get-u8 in-port)))
		       ((= chi $int-percent)
			(let ((chi1 (get-u8 in-port)))
			  (cond ((eof-object? chi1)
				 (return-failure))
				((is-hex-digit? chi1)
				 (let ((chi2 (get-u8 in-port)))
				   (cond ((eof-object? chi2)
					  (return-failure))
					 ((is-hex-digit? chi2)
					  (put-u8 ou-port $int-percent)
					  (put-u8 ou-port chi1)
					  (put-u8 ou-port chi2)
					  (loop (get-u8 in-port)))
					 (else
					  (return-failure)))))
				(else
				 (return-failure)))))
		       (else
			(return-failure))))))
	     (else ;does not start with a question mark
	      (return-failure))))))

(define (parse-fragment in-port)
  ;;Accumulate bytes  from IN-PORT while  they are valid for  a fragment
  ;;URI segment; the first byte read from IN-PORT must be a number sign.
  ;;If  an EOF  is read:  return  a bytevector  holding the  accumulated
  ;;bytes, starting number sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte of  the fragment segment; if  an error occurs:  rewind the port
  ;;position to the one before this function call.
  ;;
  ;;Notice  that an  empty  fragment  segment is  valid  (a number  sign
  ;;followed by EOF).
  ;;
  (let ((start-position (port-position in-port)))
    (define-inline (set-position-start!)
      (set-port-position! in-port start-position))
    (define-inline (set-position-back-one!)
      (set-port-position! in-port (- (port-position in-port) 1)))
    (define-inline (return-failure)
      (begin
	(set-position-start!)
	#f))
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     #f)
	    ((= chi $int-number-sign)
	     (receive (ou-port getter)
		 (open-bytevector-output-port)
	       (let loop ((chi (get-u8 in-port)))
		 (cond ((eof-object? chi)
			(getter))
		       ((or (is-unreserved? chi)
			    (is-sub-delim? chi)
			    (= chi $int-slash)
			    (= chi $int-question-mark)
			    (= chi $int-colon)
			    (= chi $int-at-sign))
			(put-u8 ou-port chi)
			(loop (get-u8 in-port)))
		       ((= chi $int-percent)
			(let ((chi1 (get-u8 in-port)))
			  (cond ((eof-object? chi1)
				 (return-failure))
				((is-hex-digit? chi1)
				 (let ((chi2 (get-u8 in-port)))
				   (cond ((eof-object? chi2)
					  (return-failure))
					 ((is-hex-digit? chi2)
					  (put-u8 ou-port $int-percent)
					  (put-u8 ou-port chi1)
					  (put-u8 ou-port chi2)
					  (loop (get-u8 in-port)))
					 (else
					  (return-failure)))))
				(else
				 (return-failure)))))
		       (else
			(return-failure))))))
	     (else ;does not start with a number sign
	      (return-failure))))))

(define (valid-segment? port)
  ;;Scan bytes  from PORT until  EOF is found;  return true if  the read
  ;;bytes are valid for a segment, false otherwise.  Ensure that:
  ;;
  ;;*  A percent  character is  followed by  two bytes  representing hex
  ;;digits.
  ;;
  ;;*  All the  non  percent-encoded  bytes are  in  the unreserved  set
  ;;defined by RFC 3986.
  ;;
  (let ((start-position (port-position port)))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (define-inline (return bool)
	    (values bool (port-position port)))
	  (let loop ((chi (get-u8 port)))
	    (cond ((eof-object? chi)
		   (return #t))
		  ((= chi $int-percent)
		   (let ((chi1 (get-u8 port)))
		     (cond ((eof-object? chi1)
			    (return #f))
			   ((is-hex-digit? chi1)
			    (let ((chi2 (get-u8 port)))
			      (cond ((eof-object? chi2)
				     (return #f))
				    ((is-hex-digit? chi2)
				     (loop (get-u8 port)))
				    (else
				     (return #f)))))
			   (else
			    (return #f)))))
		  ((is-unreserved? chi)
		   (loop (get-u8 port)))
		  (else
		   (return #f)))))
	(lambda ()
	  (set-port-position! port start-position)))))


;;;; done

)

;;; end of file
