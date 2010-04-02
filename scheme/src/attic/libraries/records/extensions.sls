;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: builtin record types extensions
;;;Date: Mon Oct 12, 2009
;;;
;;;Abstract
;;;
;;;
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


(library (records extensions)
  (export
    define-record-extension
    <pair*> <list*>
    <vector*> <bytevector*> <hashtable*>
    <number*> <port*>
    <condition*> <string*> <char*>)
  (import (rnrs)
    (rnrs mutable-pairs)
    (records builtins)
    (records helpers))


(define-syntax define-record-extension
  (syntax-rules (record-type parent fields)
    ((_ ?extension-name
	(parent ?record-name)
	(fields (?field-name ?accessor ?mutator) ...))
     (define ?extension-name
       (make-record-extension (record-type-descriptor ?record-name)
			      (quote ?extension-name)
			      `((?field-name ,?accessor ,?mutator) ...))))))


(define-record-extension <pair*>
  (parent <pair>)
  (fields (car car #f)
	  (cdr cdr #f)))

(define-record-extension <list*>
  (parent <list>)
  (fields (car car #f)
	  (cdr cdr #f)
	  (length length #f)))


(define-record-extension <vector*>
  (parent <vector>)
  (fields (length vector-length #f)))

(define-record-extension <bytevector*>
  (parent <bytevector>)
  (fields (length bytevector-length #f)))

(define-record-extension <hashtable*>
  (parent <hashtable>)
  (fields (size hashtable-size #f)
	  (keys hashtable-keys #f)
	  (entries hashtable-entries #f)))


(define-record-extension <number*>
  (parent <number>)
  (fields (exact	exact		#f)
	  (inexact	inexact		#f)

	  (exact?	exact?		#f)
	  (inexact?	inexact?	#f)

	  (zero?	zero?		#f)
	  (positive?	positive?	#f)
	  (negative?	negative?	#f)

	  (odd?		odd?		#f)
	  (even?	even?		#f)

	  (finite?	finite?		#f)
	  (infinite?	infinite?	#f)
	  (nan?		nan?		#f)

	  (real-part	real-part	#f)
	  (imag-part	imag-part	#f)
	  (magnitude	magnitude	#f)
	  (angle	angle		#f)

	  (numerator	numerator	#f)
	  (denominator	denominator	#f)

	  (floor	floor		#f)
	  (ceiling	ceiling		#f)
	  (truncate	truncate	#f)
	  (round	round		#f)))


(define-record-extension <port*>
  (parent <port>)
  (fields (transcoder			port-transcoder			#f)
	  (textual?			textual-port?			#f)
	  (binary?			binary-port?			#f)
	  (has-port-position?		port-has-port-position?		#f)
	  (has-set-port-position?	port-has-set-port-position!?	#f)
	  (port-position		port-position			set-port-position!)
	  (eof?				port-eof?			#f)
	  (input?			input-port?			#f)
	  (output?			output-port?			#f)))


(define-record-extension <condition*>
  (parent <condition>)
  (fields (message	condition-message	#f)
	  (who		condition-who		#f)
	  (irritants	condition-irritants	#f)))

(define-record-extension <string*>
  (parent <string>)
  (fields (length	string-length		#f)
	  (upcase	string-upcase		#f)
	  (downcase	string-downcase		#f)
	  (titlecase	string-titlecase	#f)
	  (foldcase	string-foldcase		#f)))

(define-record-extension <char*>
  (parent <char>)
  (fields (upcase	char-upcase		#f)
	  (downcase	char-downcase		#f)
	  (titlecase	char-titlecase		#f)
	  (foldcase	char-foldcase		#f)))


;;;; done

)

;;; end of file
