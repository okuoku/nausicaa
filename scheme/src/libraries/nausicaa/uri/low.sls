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
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa uri low)
  (export

    ;; plain string <-> bytevector conversion
    to-bytevector			to-string

    ;; percent character encoding/decoding
    unreserved-char?
    percent-encode			percent-decode
    normalise-percent-encoded-string	normalise-percent-encoded-bytevector

    ;; parser functions
    parse-scheme		collect-hier-part
    parse-query			parse-fragment
    parse-userinfo		parse-reg-name
    parse-authority		parse-ip-literal
    parse-ipvfuture		parse-port
    (rename (collect-hier-part	collect-relative-part))

    parse-relative-part

    parse-segment		parse-segment-nz
    parse-segment-nz-nc		parse-slash-and-segment

    parse-path-empty		parse-path-abempty
    parse-path-absolute		parse-path-noscheme
    parse-path-rootless		parse-path

    ;; validation
    valid-component?

    ;; auxiliary syntaxes
    char-selector		string-result?)
  (import (nausicaa)
    (nausicaa uri conditions)
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


;;;; helpers

(define-auxiliary-syntaxes
  char-selector
  string-result?)

(define (parser-error who message offset . irritants)
  (raise (condition (make-parser-error-condition offset)
		    (make-who-condition who)
		    (make-message-condition message)
		    (make-irritants-condition irritants))))


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
	(let ((chi (char->integer (string-ref obj i))))
	  (if (<= 0 chi 255)
	      (bytevector-u8-set! bv i chi)
	    (parser-error 'to-bytevector
	      "character from string out of range for one-to-one conversion to bytevector"
	      i obj)))))))

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
  ;;Section 2.1  Percent-Encoding of  RFC 3986 states  "For consistency,
  ;;URI  producers  and  normalizers  should use  uppercase  hexadecimal
  ;;digits for all percent-encodings."
  ;;
  (list->vector
   (map to-bytevector
     '("%00" "%01" "%02" "%03" "%04" "%05" "%06" "%07" "%08" "%09" "%0A" "%0B" "%0C" "%0D" "%0E" "%0F"
       "%10" "%11" "%12" "%13" "%14" "%15" "%16" "%17" "%18" "%19" "%1A" "%1B" "%1C" "%1D" "%1E" "%1F"
       "%20" "%21" "%22" "%23" "%24" "%25" "%26" "%27" "%28" "%29" "%2A" "%2B" "%2C" "%2D" "%2E" "%2F"
       "%30" "%31" "%32" "%33" "%34" "%35" "%36" "%37" "%38" "%39" "%3A" "%3B" "%3C" "%3D" "%3E" "%3F"
       "%40" "%41" "%42" "%43" "%44" "%45" "%46" "%47" "%48" "%49" "%4A" "%4B" "%4C" "%4D" "%4E" "%4F"
       "%50" "%51" "%52" "%53" "%54" "%55" "%56" "%57" "%58" "%59" "%5A" "%5B" "%5C" "%5D" "%5E" "%5F"
       "%60" "%61" "%62" "%63" "%64" "%65" "%66" "%67" "%68" "%69" "%6A" "%6B" "%6C" "%6D" "%6E" "%6F"
       "%70" "%71" "%72" "%73" "%74" "%75" "%76" "%77" "%78" "%79" "%7A" "%7B" "%7C" "%7D" "%7E" "%7F"
       "%80" "%81" "%82" "%83" "%84" "%85" "%86" "%87" "%88" "%89" "%8A" "%8B" "%8C" "%8D" "%8E" "%8F"
       "%90" "%91" "%92" "%93" "%94" "%95" "%96" "%97" "%98" "%99" "%9A" "%9B" "%9C" "%9D" "%9E" "%9F"
       "%A0" "%A1" "%A2" "%A3" "%A4" "%A5" "%A6" "%A7" "%A8" "%A9" "%AA" "%AB" "%AC" "%AD" "%AE" "%AF"
       "%B0" "%B1" "%B2" "%B3" "%B4" "%B5" "%B6" "%B7" "%B8" "%B9" "%BA" "%BB" "%BC" "%BD" "%BE" "%BF"
       "%C0" "%C1" "%C2" "%C3" "%C4" "%C5" "%C6" "%C7" "%C8" "%C9" "%CA" "%CB" "%CC" "%CD" "%CE" "%CF"
       "%D0" "%D1" "%D2" "%D3" "%D4" "%D5" "%D6" "%D7" "%D8" "%D9" "%DA" "%DB" "%DC" "%DD" "%DE" "%DF"
       "%E0" "%E1" "%E2" "%E3" "%E4" "%E5" "%E6" "%E7" "%E8" "%E9" "%EA" "%EB" "%EC" "%ED" "%EE" "%EF"
       "%F0" "%F1" "%F2" "%F3" "%F4" "%F5" "%F6" "%F7" "%F8" "%F9" "%FA" "%FB" "%FC" "%FD" "%FE" "%FF"))))

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
  ((char-selector	not-unreserved-char?)
   (string-result?	#f)))

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
  ((string-result?	#f)))

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
  ;;but should not are decoded.   Return the normalised string, in which
  ;;percent-encoded characters are displayed in upper case.
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
		  (display (string-upcase pc) port))))
	  (put-char port ch))))))

(define (normalise-percent-encoded-bytevector in-bv)
  ;;Normalise  the  given  percent-encoded  bytevector; chars  that  are
  ;;encoded  but   should  not  are  decoded.    Return  the  normalised
  ;;bytevector,  in which  percent-encoded characters  are  displayed in
  ;;upper case.
  ;;
  ;;We  assume  that  IN-BV  is  composed by  integer  corresponding  to
  ;;characters in the valid range for URIs.
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
	      (let ((chi (string->number (string-upcase buf) 16)))
		(if (unreserved-char? chi)
		    (put-u8 port chi)
		  (put-bytevector port (percent-encode (integer->char chi))))))
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

(define (collect-hier-part in-port)
  ;;Accumulate  bytes  from IN-PORT  while  they  are  acceptable for  a
  ;;"hier-part" URI  component.  If EOF or  a question mark  or a number
  ;;sign is  found: return a  bytevector holding the  accumulated bytes,
  ;;question mark or number sign excluded; else return false.  Leave the
  ;;port position to the byte after the last byte of the "hier-part".
  ;;
  ;;An empty hier-part is not accepted: if the first value from the port
  ;;is EOF, the return value is false.
  ;;
  ;;This function does no validation of the returned bytevector.
  ;;
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (lookahead-u8 in-port)))
      (if (eof-object? chi)
	  #f ;forbid empty hier-part
	(begin
	  (put-u8 ou-port chi)
	  (get-u8 in-port)
	  (let process-next-byte ((chi (lookahead-u8 in-port)))
	    (cond ((eof-object? chi)
		   (getter))
		  ((or (= chi $int-question-mark)
		       (= chi $int-number-sign))
		   (getter))
		  (else
		   (put-u8 ou-port chi)
		   (get-u8 in-port)
		   (process-next-byte (lookahead-u8 in-port))))))))))

(define (parse-query in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for a "query" URI
  ;;component; the first byte read from IN-PORT must be a question mark.
  ;;If EOF  or a number  sign is read:  return a bytevector  holding the
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
	  ((not (= chi $int-question-mark))
	   (return-failure))

	  (else
	   (receive (ou-port getter)
	       (open-bytevector-output-port)
	     (let process-next-byte ((chi (get-u8 in-port)))
	       (cond ((eof-object? chi)
		      (getter))

		     ;;A  number sign  terminates the  "query" component
		     ;;and starts a "fragment" component.
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
		      (return-failure)))))))))

(define (parse-fragment in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "fragment"
  ;;URI component;  the first  byte read from  IN-PORT must be  a number
  ;;sign.  If EOF  is read: return a bytevector  holding the accumulated
  ;;bytes, starting number sign excluded; else return false.
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
	  ((not (= chi $int-number-sign))
	   (return-failure))

	  (else
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
		      (return-failure)))))))))


;;;; hier-part/relative-part component parsers

(define (parse-authority in-port)
  ;;Accumulate  bytes from  IN-PORT  while they  are  acceptable for  an
  ;;"authority"  component   in  the  "hier-part"  of  an   URI  or  the
  ;;"relative-part" of a "relative-ref".   The first two bytes read must
  ;;represent, in  ASCII encoding, two  slash characters; after  the two
  ;;slashes, if EOF or a byte representing a slash, a question mark or a
  ;;number  sign is read:  return a  bytevector holding  the accumulated
  ;;bytes,  ending slash, question  mark or  number sign  excluded; else
  ;;return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;accumulated byte;  if an error  occurs: rewind the port  position to
  ;;the one before this function call.
  ;;
  ;;Notice that  an empty authority  (after the two leading  slashes) is
  ;;valid: it  is the case of  "authority" equal to  a "host" component,
  ;;equal to a "reg-name" component which can be empty.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (cond ((eof-object? chi)
	   (return-failure))
	  ((not (= chi $int-slash))
	   (return-failure))
	  (else
	   (let ((chi1 (get-u8 in-port)))
	     (cond ((eof-object? chi1)
		    (return-failure))
		   ((= chi1 $int-slash)
		    (receive (ou-port getter)
			(open-bytevector-output-port)
		      (let process-next-byte ((chi (get-u8 in-port)))
			(cond ((eof-object? chi)
			       (getter))
			      ((or (= chi $int-slash)
				   (= chi $int-question-mark)
				   (= chi $int-number-sign))
			       (set-position-back-one! chi)
			       (getter))
			      (else
			       (put-u8 ou-port chi)
			       (process-next-byte (get-u8 in-port)))))))
		   (else
		    (return-failure))))))))

(define (parse-userinfo in-port)
  ;;Accumulate bytes from IN-PORT while they are valid for an "userinfo"
  ;;component in  the "authority" component.  If a  byte representing an
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
  ;;"IP-literal" component  of a "host" component.  The  first byte must
  ;;represent an  open bracket  character in ASCII  encoding; if  a byte
  ;;representing a  closed bracket is read: return  a bytevector holding
  ;;the accumulated bytes, brackets excluded; else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;No validation is performed  on the returned bytevector contents; the
  ;;returned  bytevector  can  be  empty  even  though  an  "IP-literal"
  ;;component  cannot be  of  zero  length inside  the  brackets: it  is
  ;;responsibility of  the caller  to check the  length of  the returned
  ;;bytevector.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not (= chi $int-open-bracket)))
	(return-failure)
      (receive (ou-port getter)
	  (open-bytevector-output-port)
	(let process-next-byte ((chi (get-u8 in-port)))
	  (cond ((eof-object? chi)
		 (return-failure))
		((= chi $int-close-bracket)
		 (getter))
		(else
		 (put-u8 ou-port chi)
		 (process-next-byte (get-u8 in-port)))))))))

(define (parse-ipvfuture in-port)
  ;;Accumulate  bytes   from  IN-PORT  while  they  are   valid  for  an
  ;;"IPvFuture" component in the "IP-literal" component.  The first byte
  ;;must  represent "v"  in  ASCII  encoding and  the  second byte  must
  ;;represent a  single hexadecimal digit  in ASCII encoding;  after the
  ;;prolog is read, bytes are accumulated until EOF is found.
  ;;
  ;;Return  two values:  an exact  integer representing  the hexadecimal
  ;;digit in ASCII encoding; a bytevector holding the accumulated bytes;
  ;;else return false and false.
  ;;
  ;;If an error occurs: rewind the  port position to the one before this
  ;;function call.
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
    (if (or (eof-object? chi) (not (= chi $int-v)))
	(%error)
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
	  (%error))))))

(define (parse-reg-name in-port)
  ;;Accumulate bytes from IN-PORT while  they are valid for a "reg-name"
  ;;component in the "host" component.   If EOF or a byte representing a
  ;;colon, slash,  question mark  or number sign  in ASCII  encoding, is
  ;;read: return a bytevector holding the accumulated bytes, ending byte
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

	    ((or (= chi $int-colon)
		 (= chi $int-slash)
		 (= chi $int-question-mark)
		 (= chi $int-number-sign))
	     (set-position-back-one! chi)
	     (getter))

	    ;;Characters in the categories "unreserved" and "sub-delims"
	    ;;are valid.
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

(define (parse-port in-port)
  ;;Accumulate  bytes from  IN-PORT while  they are  valid for  a "port"
  ;;component  in  the  "authority"  component.   The  first  byte  must
  ;;represent a  colon in ASCII encoding;  after that: if EOF  or a byte
  ;;not representing a decimal digit, in ASCII encoding, is read: return
  ;;a bytevector holding the accumulated bytes, starting colon excluded;
  ;;else return false.
  ;;
  ;;If successful:  leave the port position  to the byte  after last one
  ;;read from the port; if an  error occurs: rewind the port position to
  ;;the one before this function call.
  ;;
  ;;Notice that an  empty "port" component after the  mandatory colon is
  ;;valid.
  ;;
  (define-parser-macros in-port)
  (let ((chi (get-u8 in-port)))
    (if (or (eof-object? chi) (not (= chi $int-colon)))
	(return-failure)
      (receive (ou-port getter)
	  (open-bytevector-output-port)
	(let process-next-byte ((chi (get-u8 in-port)))
	  (cond ((eof-object? chi)
		 (getter))

		((is-dec-digit? chi)
		 (put-u8 ou-port chi)
		 (process-next-byte (get-u8 in-port)))

		(else
		 (set-position-back-one! chi)
		 (getter))))))))


;;;; segment component parsers

(define (parse-segment in-port)
  ;;Accumulate bytes from  IN-PORT while they are valid  for a "segment"
  ;;component; notice that an empty "segment" is valid.
  ;;
  ;;If  EOF or  a  byte not  valid for  a  "segment" is  read: return  a
  ;;possibly  empty bytevector  holding  the bytes  accumulated so  far,
  ;;invalid byte  excluded; the  port position is  left pointing  to the
  ;;byte after the last accumulated one.
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
  ;;"segment-nz"  component; notice  that an  empty "segment-nz"  is not
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
  ;;"segment-nz-nc" component;  notice that an  empty "segment-nz-nc" is
  ;;not valid.
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
  ;;byte from the "segment".
  ;;
  ;;If EOF or a byte different  from slash is read as first byte: return
  ;;false; the port  position is rewind to the  one before this function
  ;;call.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (receive (ou-port getter)
      (open-bytevector-output-port)
    (let ((chi (get-u8 in-port)))
      (if (or (eof-object? chi) (not (= chi $int-slash)))
	  (return-failure)
	;;In case of  failure from PARSE-SEGMENT: we do  not just return
	;;its return value  because we have to rewind  the port position
	;;to the slash byte.
	(let ((bv (with-exception-handler
		      (lambda (E)
			(set-position-start!)
			(raise E))
		    (lambda ()
		      (parse-segment in-port)))))
	  (or bv (return-failure)))))))


;;;; path components

(define (parse-path-empty in-port)
  ;;Parse a "path-empty" component;  lookahead one byte from IN-PORT: if
  ;;it  is EOF  or a  question mark  or number  sign in  ASCII encoding:
  ;;return null; else return false.
  ;;
  ;;In any case leave the port position where it was before the function
  ;;call.
  ;;
  (define-parser-macros in-port)
  (let ((chi (lookahead-u8 in-port)))
    (if (or (eof-object? chi)
	    (= chi $int-question-mark)
	    (= chi $int-number-sign))
	'()
      #f)))

(define (parse-path-abempty in-port)
  ;;Parse from  IN-PORT a, possibly  empty, sequence of  sequences: byte
  ;;representing  the  slash  character  in  ASCII  encoding,  "segment"
  ;;component.   Return  a   possibly  empty  list  holding  bytevectors
  ;;representing the segments.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;If  an invalid  percent-encoded sequence  is read,  an  exception is
  ;;raised with type "&parser-error"; the port position is rewind to the
  ;;one before this function call.
  ;;
  (define-parser-macros in-port)
  (with-exception-handler
      (lambda (E)
	(set-position-start!)
	(raise E))
    (lambda ()
      (let read-next-segment ((segments '()))
	(let ((bv (parse-slash-and-segment in-port)))
	  (if (bytevector? bv)
	      (read-next-segment (cons bv segments))
	    (reverse segments)))))))

(define (parse-path-absolute in-port)
  ;;Parse  from   IN-PORT  a  "path-absolute"  component;   it  is  like
  ;;PARSE-PATH-ABEMPTY,  but  expects  a  slash  as  first  byte  and  a
  ;;non-slash  as second  byte.  Return  a possibly  empty  list holding
  ;;bytevectors representing the segments, or false.
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
    (if (or (eof-object? chi) (not (= chi $int-slash)))
	(return-failure)
      (let ((chi1 (lookahead-u8 in-port)))
	(if (and (not (eof-object? chi1)) (= chi1 $int-slash))
	    (return-failure)
	  (begin
	    (set-position-back-one! chi)
	    (parse-path-abempty in-port)))))))

(define (parse-path-noscheme in-port)
  ;;Parse  from  IN-PORT  a  "path-noscheme" URI  component.   Return  a
  ;;non-empty  list holding  bytevectors representing  the  segments, or
  ;;false.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice that a "path-noscheme" must not start with a slash character,
  ;;and then  it must have  at least one non-empty  "segment" component;
  ;;the first "segment" must not contain a colon caracter.
  ;;
  (define-parser-macros in-port)
  (let ((bv (parse-segment-nz-nc in-port)))
    (if (and bv (let ((chi (lookahead-u8 in-port)))
		  (or (eof-object? chi) (not (= chi $int-colon)))))
	(let ((segments (parse-path-abempty in-port)))
	  (if segments
	      (cons bv segments)
	    (list bv)))
      (return-failure))))

(define (parse-path-rootless in-port)
  ;;Parse  from  IN-PORT  a  "path-rootless" URI  component.   Return  a
  ;;non-empty  list holding  bytevectors representing  the  segments, or
  ;;false.
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
  ;;Parse from IN-PORT a "path"  component.  Return two values: false or
  ;;one    of     the    symbols    "path-absolute",    "path-noscheme",
  ;;"path-rootless", "path-empty"; the  list of bytevectors representing
  ;;the segments, possibly null.
  ;;
  ;;If successful:  leave the port position  to the byte  after the last
  ;;read byte; if  an error occurs: rewind the port  position to the one
  ;;before this function call.
  ;;
  ;;Notice  that the "path"  component can  be followed  only by  EOF, a
  ;;query or a fragment.
  ;;
  (let ((chi (lookahead-u8 in-port)))
    (cond ((or (eof-object? chi)
	       (= chi $int-question-mark)
	       (= chi $int-number-sign))
	   (values 'path-empty '()))
	  ((parse-path-absolute in-port)
	   => (lambda (segments)
		(values 'path-absolute segments)))
	  ((and (= chi $int-slash) (parse-path-abempty in-port))
	   => (lambda (segments)
		(values 'path-abempty segments)))
	  ((parse-path-noscheme in-port)
	   => (lambda (segments)
		(values 'path-noscheme segments)))
	  ((parse-path-rootless in-port)
	   => (lambda (segments)
		(values 'path-rootless segments)))
	  (else (values #f '())))))


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
  ;;is  false  if  no  path  component is  present.   "path-abempty"  is
  ;;possible only when the "authority" is present.
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


;;;; validation

(define (valid-component? port)
  ;;Scan bytes  from PORT  until EOF is  found; return two  values, when
  ;;success:  true and the  port position  of the  last byte  read; when
  ;;failure: false  and the port position  of the invalid  byte.  In any
  ;;case: the port  position is reverted to the state  it had before the
  ;;call to this function.
  ;;
  ;;Ensure that:
  ;;
  ;;*  A percent  character is  followed by  two bytes  representing hex
  ;;digits.
  ;;
  ;;*  All the  non  percent-encoded  bytes are  in  the unreserved  set
  ;;defined by RFC 3986.
  ;;
  (let ((start-position (port-position port)))
    (unwind-protect
	(let ()
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
      (set-port-position! port start-position))))


;;;; done

)

;;; end of file
