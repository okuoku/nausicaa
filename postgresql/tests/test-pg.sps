;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: high-level API tests
;;;Date: Mon Feb 15, 2010
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
  (compensations)
  (prefix (foreign databases postgresql) pg:)
  (prefix (foreign databases postgresql compensated) pg:)
  (pretty-print)
  (debugging)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing PostgreSQL library\n")


(parametrise ((check-test-name	'connecting))

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (pg:connect-db "dbname=nausicaa-test")
			 (with
			  (pg:finish conn)))))
	  (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
	      #f
	    (begin
	      #t))))
    => #t)

  (check		;compensated
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:status/ok? conn)))
    => #t)

  (check		;reset
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (if (not (pg:status/ok? conn))
	      #f
	    (begin
	      (pg:reset conn)
	      (pg:status/ok? conn)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (pg:set-db-login #f #f #f #f "nausicaa-test" #f #f)
			 (with
			  (pg:finish conn)))))
	  (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
	      #f
	    (begin
	      #t))))
    => #t)

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (pg:set-db #f #f #f #f "nausicaa-test")
			 (with
			  (pg:finish conn)))))
	  (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
	      #f
	    (begin
	      #t))))
    => #t)

  #t)


(parametrise ((check-test-name	'misc)
	      (debugging	#t))

  (check
      (let ((ops (pg:connection-defaults)))
	;;(pretty-print ops)
	(for-all pg:<connect-option>? ops))
    => #t)

  (check
      (let ((ops (pg:connection-info-parse "dbname=nausicaa-test")))
	;;(pretty-print ops)
	(for-all pg:<connect-option>? ops))
    => #t)

  (check
      (guard (E (else
      		 ;;(debug-print-condition "connect-parse" E)
      		 #t))
	(pg:connection-info-parse "ciao"))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
