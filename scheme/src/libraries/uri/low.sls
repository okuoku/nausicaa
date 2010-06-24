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
    parse-scheme		parse-hier-part
    parse-query			parse-fragment
    parse-userinfo		parse-reg-name
    parse-authority		parse-ip-literal
    parse-ipvfuture

    parse-relative-part

    parse-segment		parse-segment-nz
    parse-segment-nz-nc		parse-slash-and-segment

    parse-path-empty		parse-path-abempty
    parse-path-absolute		parse-path-noscheme
    parse-path-rootless		parse-path

    ;; validation
    valid-component?
    )
  (import (nausicaa)
    (uri conditions)
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

(define-constant $int-v		(char->integer #\v))

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

(define-inline (is-pchar-not-percent-encoded? chi)
  ;;Evaluate  to true  if CHI  matches  the "pchar"  component with  the
  ;;exception of the percent-encoded sequence.
  ;;
  (or (is-unreserved? chi)
      (is-sub-delim? chi)
      (= chi $int-colon)
      (= chi $int-at-sign)))


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


;;;; parser helpers

(define-syntax define-parser-macros
  (lambda (stx)
    (syntax-case stx ()
      ((?k ?port)
       (with-syntax ((SET-POSITION-START!	(datum->syntax #'?k 'set-position-start!))
		     (SET-POSITION-BACK-ONE!	(datum->syntax #'?k 'set-position-back-one!))
		     (RETURN-FAILURE		(datum->syntax #'?k 'return-failure)))
	 #'(begin
	     (define start-position
	       (port-position ?port))
	     (define-inline (SET-POSITION-START!)
	       (set-port-position! ?port start-position))
	     (define-inline (SET-POSITION-BACK-ONE! last-read-byte)
	       (unless (eof-object? last-read-byte)
		 (set-port-position! ?port (- (port-position ?port) 1))))
	     (define-inline (RETURN-FAILURE)
	       (begin
		 (SET-POSITION-START!)
		 #f))))))))

(define (%port-lookahead-byte port)
  ;;Read one byte from the binary  PORT and return it.  If the read byte
  ;;does NOT represent  EOF: rewind the port position,  so that the same
  ;;byte is read again.
  ;;
  (begin0-let ((chi (get-u8 port)))
    (unless (eof-object? chi)
      (set-port-position! port (- (port-position port) 1)))))

(define (%parse-percent-encoded-sequence who in-port ou-port set-position-start!)
  ;;To be called after a  byte representing a percent character in ASCII
  ;;encoding as been read from  IN-PORT; parse the two HEXDIG bytes from
  ;;IN-PORT validating them as percent-encoded byte.
  ;;
  ;;If  successful: put  a  percent byte  and  the two  HEXDIG bytes  in
  ;;OU-PORT and return void.
  ;;
  ;;If EOF  or an invalid byte  is read: rewind the  input port position
  ;;calling  SET-POSITION-START!,  then  raise  an exception  with  type
  ;;"&parser-error",  using  WHO  as  value  for  the  "&who"  condition
  ;;component; in this case nothing is written to OU-PORT.
  ;;
  (define (%error)
    (set-position-start!)
    (raise
     (condition
      (make-who-condition who)
      (make-message-condition "end of input or invalid byte in percent-encoded sequence")
      (make-parser-error-condition (- (port-position in-port) 1)))))
  (let ((first-hexdig-byte (get-u8 in-port)))
    (cond ((eof-object? first-hexdig-byte)
	   (%error))
	  ((is-hex-digit? first-hexdig-byte)
	   (let ((second-hexdig-byte (get-u8 in-port)))
	     (cond ((eof-object? second-hexdig-byte)
		    (%error))
		   ((is-hex-digit? second-hexdig-byte)
		    (put-u8 ou-port $int-percent)
		    (put-u8 ou-port first-hexdig-byte)
		    (put-u8 ou-port second-hexdig-byte))
		   (else
		    (%error)))))
	  ;;Invalid byte in percent-encoded sequence.
	  (else
	   (%error)))))


;;;; URI component parsers

(define (parse-scheme in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a scheme URI
  ;;element.   If a  colon is  found:  return a  bytevector holding  the
  ;;accumulated bytes, colon excluded; else return false.
  ;;
  ;;When successful: leave  the port position to the  byte after the one
  ;;representing the colon; if an error occurs: rewind the port position
  ;;to the one before this function call.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     #f)
	    ;;A "scheme" component starts with an alpha and goes on with
	    ;;alpha, digit or "+", "-", ".".
	    ((is-alpha? chi)
	     (put-u8 ou-port chi)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (if (eof-object? chi)
		   (return-failure)
		 (cond ((or (is-alpha-digit? chi)
			    (= chi $int-plus)
			    (= chi $int-minus)
			    (= chi $int-dot))
			(put-u8 ou-port chi)
			(process-next-byte (get-u8 in-port)))
		       ((= chi $int-colon)
			(getter))
		       (else
			(return-failure))))))
	    (else
	     (return-failure))))))

(define (parse-hier-part in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for a "hier-part"
  ;;URI component.   If a  EOF or a  question mark  or a number  sign is
  ;;found: return  a bytevector holding the  accumulated bytes, question
  ;;mark or  number sign  excluded; else return  false.  Leave  the port
  ;;position to the byte after the last byte of the "hier-part".
  ;;
  ;;An empty hier-part is not accepted: if the first value from the port
  ;;is EOF, the return value is false.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (if (eof-object? chi)
	  #f ;forbid empty hier-part
	(begin
	  (put-u8 ou-port chi)
	  (let process-next-byte ((chi (get-u8 in-port)))
	    (cond ((eof-object? chi)
		   (getter))
		  ((or (= chi $int-question-mark)
		       (= chi $int-number-sign))
		   (set-position-back-one! chi)
		   (getter))
		  (else
		   (put-u8 ou-port chi)
		   (process-next-byte (get-u8 in-port))))))))))

(define (parse-query in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for a "query" URI
  ;;component; the first byte read from IN-PORT must be a question mark.
  ;;If an EOF or a number  sign is read: return a bytevector holding the
  ;;accumulated bytes, starting question mark excluded and ending number
  ;;sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte of the  "query" component; if an error  occurs: rewind the port
  ;;position to the one before this function call.
  ;;
  ;;Notice that  an empty  "query" component is  valid (a  question mark
  ;;followed by EOF).
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   #f)

	  ;;A "query" component must begin with a question mark.
	  ((= chi $int-question-mark)
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (getter))

		     ;;If  a number  sign  if found,  it terminates  the
		     ;;"query"   component  and   starts   a  "fragment"
		     ;;component.
		     ((= chi $int-number-sign)
		      (set-position-back-one! chi)
		      (getter))

		     ;;Characters     in     categories    "unreserved",
		     ;;"sub-delim" or "/", "?", ":", "@" are valid.
		     ((or (is-unreserved? chi)
			  (is-sub-delim? chi)
			  (= chi $int-slash)
			  (= chi $int-question-mark)
			  (= chi $int-colon)
			  (= chi $int-at-sign))
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))

		     ;;A percent-encoded sequence is valid.
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
					(process-next-byte (get-u8 in-port)))
				       (else
					(return-failure)))))
			      (else
			       (return-failure)))))
		     (else
		      (return-failure))))))

	  ;;Does not start with a question mark.
	  (else
	   (return-failure)))))

(define (parse-fragment in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "fragment"
  ;;URI component;  the first  byte read from  IN-PORT must be  a number
  ;;sign.   If  an  EOF  is   read:  return  a  bytevector  holding  the
  ;;accumulated bytes, starting number sign excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;byte of  the "fragment"  component; if an  error occurs:  rewind the
  ;;port position to the one before this function call.
  ;;
  ;;Notice that  an empty "fragment"  component is valid (a  number sign
  ;;followed by EOF).
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   #f)

	  ;;A "fragment" component starts with a number sign.
	  ((= chi $int-number-sign)
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (getter))

		     ;;Characters   in   the  categories   "unreserved",
		     ;;"sub-delim" or "/", "?", ":", "@" are valid.
		     ((or (is-unreserved? chi)
			  (is-sub-delim? chi)
			  (= chi $int-slash)
			  (= chi $int-question-mark)
			  (= chi $int-colon)
			  (= chi $int-at-sign))
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))

		     ;;A percent-encoded sequence is valid.
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
					(process-next-byte (get-u8 in-port)))
				       (else
					(return-failure)))))
			      (else
			       (return-failure)))))
		     (else
		      (return-failure))))))

	  ;;Does not start with a number sign.
	  (else
	   (return-failure)))))


;;;; relative-ref components

(define (parse-relative-part in-port)
  ;;Read bytes from IN-PORT expecting to get, from the first byte to the
  ;;EOF,  a   "relative-part"  component,   which  is  a   component  of
  ;;"relative-ref".  Return three values:
  ;;
  ;;(1) A bytevector holding the  bytes of the "authority" component, or
  ;;false if there is no "authority".
  ;;
  ;;(2) A  symbol representing the  kind of path;  it can be  one among:
  ;;"path-abempty", "path-absolute",  "path-noscheme", "path-empty".  It
  ;;is false if no path component is present.
  ;;
  ;;(3)  A list of  bytevectors holding  the path  segments, or  null if
  ;;there is no path component.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read one, which is the EOF  position; if an error occurs: rewind the
  ;;port position to the one before this function call.
  ;;
  ;;Notice that if  the first byte read from IN-PORT  is EOF: the return
  ;;values are #f, "path-empty" and null.
  ;;
  (define-parser-macros in-port)
  (let ((authority (parse-authority in-port)))
    (if authority
	(let ((bv (parse-path-abempty in-port)))
	  (if bv
	      (values authority 'path-abempty bv)
	    (values (return-failure) #f '())))
      (cond ((parse-path-absolute in-port)
	     => (lambda (segments)
		  (values #f 'path-absolute segments)))
	    ((parse-path-noscheme in-port)
	     => (lambda (segments)
		  (values #f 'path-noscheme segments)))
	    (else
	     (values #f 'path-empty '()))))))



;;;; hier-part component parsers

(define (parse-authority in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"authority" component in  the "hier-part" of an URI.   The first two
  ;;bytes read must represent,  in ASCII encoding, two slash characters;
  ;;after the  two slashes,  if EOF  or a byte  representing a  slash is
  ;;read:  return a  bytevector  holding the  accumulated bytes,  ending
  ;;slash excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;accumulated byte;  if an error  occurs: rewind the port  position to
  ;;the one before this function call.
  ;;
  ;;Notice that  an empty  authority (after the  two slashes)  is valid,
  ;;corresponding  to  the  case   of  "authority"  equal  to  a  "host"
  ;;component, equal to a "reg-name" component which can be empty.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (return-failure))
	  ((= chi $int-slash)
	   (let ((chi1 (get-u8 in-port)))
	     (cond ((eof-object? chi1)
		    (return-failure))
		   ((= chi1 $int-slash)
		    (receive (ou-port getter)
			(open-bytevector-output-port)
		      (let process-next-byte ((chi (get-u8 in-port)))
			(cond ((eof-object? chi)
			       (getter))
			      ((= chi $int-slash)
			       (set-position-back-one! chi)
			       (getter))
			      (else
			       (put-u8 ou-port chi)
			       (process-next-byte (get-u8 in-port)))))))
		   (else
		    (return-failure)))))
	  (else
	   (return-failure)))))

(define (parse-userinfo in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for an "userinfo"
  ;;component in the  "hier-part" of an URI.  If  a byte representing an
  ;;at sign, in ASCII encoding, is read: return a bytevector holding the
  ;;accumulated bytes, ending at sign excluded; else return false.
  ;;
  ;;If successful: leave the port  position to the byte after the ending
  ;;at sign;  if an error  occurs: rewind the  port position to  the one
  ;;before this function call.
  ;;
  ;;Notice  that an  empty "userinfo"  component  is valid  (an at  sign
  ;;preceded by nothing).
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (return-failure))

	    ;;An at sign terminates the "userinfo" component.
	    ((= chi $int-at-sign)
	     (getter))

	    ;;Characters   in    the   categories   "unreserved"   and
	    ;;"sub-delims" or ":" are valid.
	    ((or (is-unreserved? chi) (is-sub-delim? chi) (= chi $int-colon))
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
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
			       (process-next-byte (get-u8 in-port)))
			      (else
			       (return-failure)))))
		     ;;Invalid byte in percent-encoded sequence.
		     (else
		      (return-failure)))))

	    ;;Invalid byte.
	    (else
	     (return-failure))))))

(define (parse-ip-literal in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IP-literal" component in the "hier-part" of an URI.  The first byte
  ;;must represent  an open  bracket character in  ASCII encoding;  if a
  ;;byte  representing a  closed bracket  is read:  return  a bytevector
  ;;holding the accumulated bytes, brackets excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No validation is performed  on the returned bytevector contents; the
  ;;returned  bytevector  can  be  empty  even  though  an  "IP-literal"
  ;;component cannot be of zero length inside the brackets.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (return-failure))
	  ((= chi $int-open-bracket)
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (return-failure))
		     ((= chi $int-close-bracket)
		      (getter))
		     (else
		      (put-u8 ou-port chi)
		      (process-next-byte (get-u8 in-port)))))))
	  (else
	   (return-failure)))))

(define (parse-ipvfuture in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IPvFuture" component  in the "authority" component of  an URI.  The
  ;;first byte must represent "v"  in ASCII encoding and the second byte
  ;;must represent  a single hexadecimal digit in  ASCII encoding; after
  ;;the  prolog is  read,  bytes  are accumulated  until  EOF is  found.
  ;;
  ;;Return  two values:  an exact  integer representing  the hexadecimal
  ;;digit in ASCII encoding; a bytevector holding the accumulated bytes;
  ;;else return false and false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No  specific  validation is  performed  on  the returned  bytevector
  ;;contents, bytes are  only tested to be valid  for the component; the
  ;;returned  bytevector  can  be   empty  even  though  an  "IPvFuture"
  ;;component cannot be of zero length inside the brackets.
  ;;
  (define-parser-macros in-port)
  (define (%error)
    (values (return-failure) #f))
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (%error))
	  ((= chi $int-v)
	   (let ((version-chi (get-u8 in-port)))
	     (if (is-hex-digit? version-chi)
		 (receive (ou-port getter)
		     (open-bytevector-output-port)
		   (let process-next-byte ((chi (get-u8 in-port)))
		     (cond ((eof-object? chi)
			    (values version-chi (getter)))
			   ((or (is-unreserved? chi)
				(is-sub-delim? chi)
				(= chi $int-colon))
			    (put-u8 ou-port chi)
			    (process-next-byte (get-u8 in-port)))
			   (else
			    (%error)))))
	       (%error))))
	  (else
	   (%error)))))

(define (parse-reg-name in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "reg-name"
  ;;component  in  the  "hier-part"  of  an  URI.   If  EOF  or  a  byte
  ;;representing a colon or a  slash, in ASCII encoding, is read: return
  ;;a bytevector  holding the accumulated  bytes, ending colon  or slash
  ;;excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;Notice that an empty "reg-name" component is valid.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (getter))

	    ((or (eof-object? chi)
		 (= chi $int-colon)
		 (= chi $int-slash))
	     (set-position-back-one! chi)
	     (getter))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" are valid.
	    ((or (is-unreserved? chi) (is-sub-delim? chi))
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
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
			       (process-next-byte (get-u8 in-port)))
			      (else
			       (return-failure)))))
		     ;;Invalid byte in percent-encoded sequence.
		     (else
		      (return-failure)))))

	    ;;Invalid byte.
	    (else
	     (return-failure))))))


;;;; segment component parsers

(define (parse-segment in-port)
  ;;Accumulate bytes from  IN-PORT while they are valid  for a "segment"
  ;;URI component; notice that an empty "segment" is valid.
  ;;
  ;;If  EOF or  a  byte not  valid for  a  "segment" is  read: return  a
  ;;bytevector  holding  the  bytes  accumulated so  far,  invalid  byte
  ;;excluded; the port  position is left pointing to  the byte after the
  ;;last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (getter))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    ((is-pchar-not-percent-encoded? chi)
	     (put-u8 ou-port chi)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    ((= chi $int-percent)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (set-position-back-one! chi)
	     (getter))))))

(define (parse-segment-nz in-port)
  ;;Accumulate  bytes   from  IN-PORT  while   they  are  valid   for  a
  ;;"segment-nz" URI component; notice that an empty "segment-nz" is not
  ;;valid.
  ;;
  ;;If the first read operation returns EOF or an invalid byte: the port
  ;;position is  restored to the one  before this function  call and the
  ;;return value is false.
  ;;
  ;;If, after at least one valid byte is read, EOF or an invalid byte is
  ;;read:  return a  bytevector holding  the bytes  accumulated  so far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment-nz)
  (define at-least-one? #f)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (if at-least-one?
		 (getter)
	       (return-failure)))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    ((is-pchar-not-percent-encoded? chi)
	     (put-u8 ou-port chi)
	     (set! at-least-one? #t)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    ((= chi $int-percent)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (if at-least-one?
		 (begin
		   (set-position-back-one! chi)
		   (getter))
	       (return-failure)))))))

(define (parse-segment-nz-nc in-port)
  ;;Accumulate  bytes   from  IN-PORT  while   they  are  valid   for  a
  ;;"segment-nz-nc" URI component;  notice that an empty "segment-nz-nc"
  ;;is not valid.
  ;;
  ;;If the first read operation returns EOF or an invalid byte: the port
  ;;position is  restored to the one  before this function  call and the
  ;;return value is false.
  ;;
  ;;If, after at least one valid byte is read, EOF or an invalid byte is
  ;;read:  return a  bytevector holding  the bytes  accumulated  so far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (define who 'parse-segment-nz-nc)
  (define at-least-one? #f)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let process-next-byte ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     (if at-least-one?
		 (getter)
	       (return-failure)))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;or ":" or "@" are valid.
	    ((or (is-unreserved? chi)
		 (is-sub-delim? chi)
		 (= chi $int-at-sign))
	     (put-u8 ou-port chi)
	     (set! at-least-one? #t)
	     (process-next-byte (get-u8 in-port)))

	    ;;A percent-encoded sequence is valid.
	    ((= chi $int-percent)
	     (%parse-percent-encoded-sequence who in-port ou-port (lambda ()
								    (set-position-start!)))
	     (process-next-byte (get-u8 in-port)))

	    ;;Any other character terminates the segment.
	    (else
	     (if at-least-one?
		 (begin
		   (set-position-back-one! chi)
		   (getter))
	       (return-failure)))))))

(define (parse-slash-and-segment in-port)
  ;;Attempt  to read  from  IN-PORT the  sequence  slash character  plus
  ;;"segment" component; notice that an empty "segment" is valid.
  ;;
  ;;If  these  components are  successfully  read:  return a  bytevector
  ;;(possibly empty)  holding the accumulated "segment"  bytes; the port
  ;;position is  left pointing  to the byte  after the  last accumulated
  ;;byte from the "segment".  Otherwise:
  ;;
  ;;* If EOF is read as first byte: return EOF.
  ;;
  ;;*  If a  byte different  from slash  is read  as first  byte: return
  ;;false.
  ;;
  ;;* If  an invalid percent-encoded  sequence is read, an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (cond ((eof-object? chi)
	     chi)

	    ;;The first  byte must represent  a slash character,  then a
	    ;;"segment"  must  be  present.   In case  of  failure  from
	    ;;PARSE-SEGMENT:  we do  not  just return  its return  value
	    ;;because we have  to rewind the port position  to the slash
	    ;;byte.
	    ((= chi $int-slash)
	     (let ((bv (with-exception-handler
			   (lambda (E)
			     (set-position-start!)
			     (raise E))
			 (lambda ()
			   (parse-segment in-port)))))
	       (or bv (return-failure))))

	    ;;The first byte is not a slash nor EOF.
	    (else
	     (return-failure))))))


;;;; path components

(define (parse-path-empty in-port)
  ;;Parse a "path-empty"  URI component; read one byte  from IN-PORT: if
  ;;it is EOF return the  empty bytevector, else restore the position to
  ;;the initial value and return false.
  ;;
  (define-parser-macros in-port)
  (if (eof-object? (get-u8 in-port))
      '()
    (return-failure)))

(define (parse-path-abempty in-port)
  ;;Parse from IN-PORT a, possibly empty, sequence of sequences of bytes
  ;;representing slash  characters in  ASCII encoding and  "segment" URI
  ;;components; the sequence  must end with EOF.  Return  a list holding
  ;;bytevectors representing the segments, or false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  (define-parser-macros in-port)
  (let read-next-segment ((segments '()))
    (let ((bv (parse-slash-and-segment in-port)))
      (cond ((eof-object? bv)
	     (reverse segments))
	    (bv
	     (read-next-segment (cons bv segments)))
	    (else
	     (return-failure))))))

(define (parse-path-absolute in-port)
  ;;Parse from  IN-PORT a "path-absolute" URI component,  after this the
  ;;port   must  return   EOF.   Return   a  list   holding  bytevectors
  ;;representing the segments, or false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-absolute" can  be just a slash character with no
  ;;segments attached.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (return-failure))
	  ((= chi $int-slash)
	   (let ((bv (parse-segment-nz in-port)))
	     (if bv
		 (let ((segments (parse-path-abempty in-port)))
		   (if segments
		       (cons bv segments)
		     (return-failure)))
	       '())))
	  (else
	   (return-failure)))))

(define (parse-path-noscheme in-port)
  ;;Parse from  IN-PORT a "path-noscheme" URI component.   Return a list
  ;;holding bytevectors representing the segments, or false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-noscheme" must not start with a slash character,
  ;;and then it must have at least one non-empty "segment" component.
  ;;
  (let ((bv (parse-segment-nz-nc in-port)))
    (and bv
	 (let ((segments (parse-path-abempty in-port)))
	   (if segments
	       (cons bv segments)
	     (list bv))))))

(define (parse-path-rootless in-port)
  ;;Parse from  IN-PORT a "path-rootless" URI component.   Return a list
  ;;holding bytevectors representing the segments, or false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-rootless" must not start with a slash character,
  ;;and then it must have at least one non-empty "segment" component.
  ;;
  (let ((bv (parse-segment-nz in-port)))
    (and bv
	 (let ((segments (parse-path-abempty in-port)))
	   (if segments
	       (cons bv segments)
	     (list bv))))))

(define (parse-path in-port)
  ;;Parse from IN-PORT a "path" URI component.  Return two values: false
  ;;or one of the  symbols "path-absolute", "path-rootless"; the list of
  ;;bytevectors representing the segments, possibly null.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  (cond ((parse-path-absolute in-port)
	 => (lambda (segments)
	      (values 'path-absolute segments)))
	((parse-path-rootless in-port)
	 => (lambda (segments)
	      (values 'path-rootless segments)))
	(else (values #f '()))))


;;;; validation

(define (valid-component? port)
  ;;Scan bytes  from PORT until  EOF is found;  return true if  the read
  ;;bytes are valid for a component, false otherwise.  Ensure that:
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
	  (let process-next-byte ((chi (get-u8 port)))
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
				     (process-next-byte (get-u8 port)))
				    (else
				     (return #f)))))
			   (else
			    (return #f)))))
		  ((is-unreserved? chi)
		   (process-next-byte (get-u8 port)))
		  (else
		   (return #f)))))
	(lambda ()
	  (set-port-position! port start-position)))))


;;;; done

)

;;; end of file
