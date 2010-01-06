;;;
;;;Part of: Nausicaa/Expat
;;;Contents: expat high-level API tests
;;;Date: Wed Jan  6, 2010
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


(import (nausicaa)
  (checks)
  (debugging)
  (compensations)
  (foreign memory)
  (foreign cstrings)
  (foreign xml expat)
  (only (foreign ffi) make-c-callback*))

(check-set-mode! 'report-failed)
(display "*** testing Expat high-level library\n")


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
	      (debugging	#f))

  (check
      (with-compensations
	(letrec ((parser (compensate
			     (begin0-let ((p (xml-parser-create pointer-null)))
			       (when (pointer-null? p)
				 (raise-out-of-memory 'xml-parser-create #f)))
			   (with
			    (xml-parser-free parser)))))

	  (define (start-callback data element attributes)
	    (let ((element    (cstring->string element))
		  (attributes (argv->strings attributes)))
	      (debug "start ~s ~s () - ~s" element attributes data)))

	  (define (end-callback data element)
	    (let ((element	(cstring->string element)))
	      (debug "end ~s - ~s" element data)))

	  (let ((start	(make-c-callback* void start-callback (pointer pointer pointer)))
		(end	(make-c-callback* void end-callback   (pointer pointer))))
	    (xml-set-element-handler parser start end)
	    (let* ((buflen	(string-length xml-1))
		   (bufptr	(string->cstring/c xml-1))
		   (finished	1)
		   (result	(xml-parse parser bufptr buflen finished)))
	      (debug "here")
	      (when (= result XML_STATUS_ERROR)
		(error 'xml-parse
		  (cstring->string (xml-error-string (xml-get-error-code parser))))))
	    #f)))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
