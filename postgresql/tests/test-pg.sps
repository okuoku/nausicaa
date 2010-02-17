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


(parametrise ((check-test-name	'inspection))

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (list (pg:connection-password conn)
		(pg:connection-database conn)
		)))
    => '("" "nausicaa-test"))

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:connection-transaction-status conn)))
    (=> enum-set=?)
    (pg:transaction-status idle))

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:connection-parameter-status conn 'server_version)))
    => "8.4.2")

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:connection-protocol-version conn)))
    => 3)

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:connection-server-version conn)))
    => 80402)

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:pid? (pg:connection-backend-pid conn))))
    => #t)

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (list (pg:connection-needs-password conn)
		(pg:connection-used-password conn))))
    => '(#f #f))

  (check
      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	  (pg:connection-get-ssl conn)))
    => #f)



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
