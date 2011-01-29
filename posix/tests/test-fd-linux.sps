;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors Linux library
;;;Date: Fri Jan 22, 2010
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


(import (nausicaa)
  (nausicaa checks)
  (nausicaa strings)
  (nausicaa ffi)
  (nausicaa ffi memory)
  (nausicaa ffi cstrings)
  (nausicaa ffi errno)
  (for (nausicaa posix typedefs) expand run)
  (for (nausicaa posix extensions) expand)
  (prefix (nausicaa posix fd) posix:)
  (prefix (nausicaa linux fd) linux:)
  (nausicaa posix sizeof))

(check-set-mode! 'report-failed)
(display "*** testing Linux fd\n")

(define TMPDIR (get-environment-variable "TMPDIR"))

(define the-pathname (string-join (list TMPDIR "name.ext") "/"))

(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")


(parametrise ((check-test-name	'pipe)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in pipe" E))
    (lambda ()

      (check	;raw fd
	  (with-compensations
	    (let-values (((in ou) (linux:pipe2 (pipe2-flags cloexec))))
	      (push-compensation (posix:close in))
	      (push-compensation (posix:close ou))
	      (let ((s (string->cstring/c "ciao\n")))
		(posix:write ou s (strlen s)))
	      (let* ((p	(malloc 10))
		     (len	(posix:read in p 10)))
		(cstring->string p len))))
	=> "ciao\n")

      (check	;binary port
	  (with-compensations
	    (let-values (((in ou) (linux:pipe2 O_CLOEXEC)))
	      (letrec ((inp	(compensate
				    (posix:fd->binary-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (posix:fd->binary-output-port ou)
				  (with
				   (close-port oup)))))
		(put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
		(flush-output-port oup)
		(bytevector->string (get-bytevector-n inp 5) (native-transcoder)))))
	=> "ciao\n")

      #f)))


;;;; done

(check-report)

;;; end of file
