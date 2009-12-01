;;;
;;;Part of: Nausicaa/Expat
;;;Contents: expat platform functions tests
;;;Date: Sun Jan  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (only (foreign ffi)
	make-c-callback*)
  (foreign xml expat platform)
  (foreign xml expat sizeof)
  (checks)
  (debugging)
  (compensations)
  (foreign memory)
  (foreign cstrings))

(check-set-mode! 'report-failed)
(display "*** testing platform\n")


;;;; XML strings

(define xml-1 "
<stuff>
 <thing>
  <alpha>one</alpha>
  <beta>two</beta>
 </thing>
 <thing>
  <alpha>123</alpha>
  <beta>456</beta>
 </thing>
</stuff>")



(parametrise ((check-test-name 'simple)
	      (debugging	#t))

  (check
      (with-compensations
	(letrec ((parser (compensate
			     (begin0-let ((p (XML_ParserCreate pointer-null)))
			       (when (pointer-null? p)
				 (raise-out-of-memory 'XML_ParserCreate #f)))
			   (with
			    (XML_ParserFree parser)))))

	  (define (start-callback data element attributes)
	    (let ((element    (cstring->string element))
		  (attributes (argv->strings attributes)))
	      (debug "start ~s ~s () - ~s" element attributes data)))

	  (define (end-callback data element)
	    (let ((element	(cstring->string element)))
	      (debug "end ~s - ~s" element data)))

	  (let ((start	(make-c-callback* void start-callback (pointer pointer pointer)))
		(end	(make-c-callback* void end-callback   (pointer pointer))))
	    (XML_SetElementHandler parser start end)
	    (let* ((buflen	(string-length xml-1))
		   (bufptr	(string->cstring/c xml-1))
		   (finished	1)
		   (result	(begin
				  (write (list start end XML_Parse bufptr parser))(newline)
				  (XML_Parse parser bufptr buflen finished))))
	      (debug "here")
	      (when (= result XML_STATUS_ERROR)
		(error 'XML_Parse
		  (cstring->string (XML_ErrorString (XML_GetErrorCode parser))))))
	    #f)))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
