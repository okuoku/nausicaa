;;;
;;;Part of: Nausicaa/Expat
;;;Contents: expat platform functions tests
;;;Date: Sun Jan  4, 2009
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



;;;; setup

(import (r6rs)
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (expat platform)
  (expat sizeof))

(check-set-mode! 'report-failed)


;;;; XML strings

(define xml-1 "
<stuff>
 <thing>
  <alpha>one</alpha>
  <beta>two<beta>
 </thing>
 <thing>
  <alpha>123</alpha>
  <beta>456<beta>
 </thing>
</stuff>")



;;;; code

(check
    (with-compensations
      (letrec ((parser
		(compensate
		    (let ((p (XML_ParserCreate pointer-null)))
		      (if (pointer-null? p)
			  (raise-out-of-memory 'XML_ParserCreate)
			p))
		  (with
		   (XML_ParserFree parser)))))

	(let ((start	(make-c-callback
			 'void '(pointer pointer pointer)
			 (lambda (data elements attributes)
			   #f)))
	      (end	(make-c-callback
			 'void '(pointer pointer)
			 (lambda (data element)
			   #f))))
	  (XML_SetElementHandler parser start end)
	  (let* ((buflen	(string-length xml-1))
		 (bufptr	(string->cstring/c xml))
		 (finished	1)
		 (result	(XML_Parse parser bufptr buflen finished)))
	    (when (= XML_STATUS_ERROR)
	      (error 'XML_Parse
		(cstring->string (XML_ErrorString (XML_GetErrorCode parser))))))
	  #f)))
  => #f)


;;;; done

(check-report)

;;; end of file
