;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for streams
;;;Date: Thu Dec  4, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (strings)
  (receive)
  (compensations)
  (foreign ffi)
  (foreign memory)
  (foreign cstrings)
  (foreign errno)
  (posix sizeof)
  (glibc streams)
  (prefix (glibc streams platform) platform:))

(check-set-mode! 'report-failed)

(define TMPDIR (get-environment-variable "TMPDIR"))
(display "*** testing Glibc streams\n")


(define the-pathname (string-join (list TMPDIR "name.ext") "/"))

(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")

(define the-string-newline
  "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.
")



(parametrise ((check-test-name	'basic))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(string->cstring/c the-string))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fread p len 1 S)
	    (fclose S)
	    (cstring->string p))))
    => the-string)

  #t)


(parametrise ((check-test-name	'getline))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(string->cstring/c the-string))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 SEEK_SET)
	    (begin0
		(list (getline S) (getline S)
		      (getline S) (getline S))
	      (fclose S)))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher."))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(string->cstring/c the-string-newline))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 SEEK_SET)
	    (begin0
		(list (getline S) (getline S)
		      (getline S) (getline S))
	      (fclose S)))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  (check
      (let* ((pathname the-pathname))
	(with-compensations
	  (letrec ((S (compensate
			  (fopen pathname "w+")
			(with (fclose S))))
		   (*pointer	(malloc-small/c))
		   (*count	(malloc-small/c))
		   (lines	'()))

	    (define-syntax getp
	      (syntax-rules ()
		((_)
		 (pointer-ref-c-pointer *pointer 0))))

	    (define (free)
	      (let ((p (getp)))
		(unless (pointer-null? p)
		  (primitive-free p))))

	    (fwrite (string->cstring/c the-string) 1
		    (string-length the-string) S)
	    (fseek S 0 SEEK_SET)
	    (let loop ()
	      (receive (result errno)
		  (platform:getline *pointer *count (FILE*->pointer S))
		(cond ((ferror S)
		       (free)
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (free)
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher."))

  (check
      (let* ((pathname the-pathname))
	(with-compensations
	  (letrec ((S (compensate
			  (fopen pathname "w+")
			(with (fclose S))))
		   (*pointer	(malloc-small/c))
		   (*count	(malloc-small/c))
		   (lines	'()))

	    (define-syntax getp
	      (syntax-rules ()
		((_)
		 (pointer-ref-c-pointer *pointer 0))))

	    (fwrite (string->cstring/c the-string-newline) 1
		    (string-length the-string-newline)
		    S)
	    (fseek S 0 SEEK_SET)
	    (let loop ()
	      (receive (result errno)
		  (platform:getline *pointer *count (FILE*->pointer S))
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  #t)


(parametrise ((check-test-name	'getdelim))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(string->cstring/c the-string))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 SEEK_SET)
	    (begin0
		(list (getdelim S #\newline) (getdelim S #\newline)
		      (getdelim S #\newline) (getdelim S #\newline))
	      (fclose S)))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher."))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(string->cstring/c the-string-newline))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 SEEK_SET)
	    (begin0
		(list (getdelim S #\newline) (getdelim S #\newline)
		      (getdelim S #\newline) (getdelim S #\newline))
	      (fclose S)))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  (check
      (let* ((pathname the-pathname))
	(with-compensations
	  (letrec ((S (compensate
			  (fopen pathname "w+")
			(with (fclose S))))
		   (*pointer	(malloc-small/c))
		   (*count	(malloc-small/c))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (string->cstring/c the-string) 1
		    (string-length the-string) S)
	    (fseek S 0 SEEK_SET)
	    (let loop ()
	      (receive (result errno)
		  (platform:getdelim *pointer *count (char->integer #\newline) (FILE*->pointer S))
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher."))


  (check
      (let* ((pathname the-pathname))
	(with-compensations
	  (letrec ((S (compensate
			  (fopen pathname "w+")
			(with (fclose S))))
		   (*pointer	(malloc-small/c))
		   (*count	(malloc-small/c))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (string->cstring/c the-string-newline) 1
		    (string-length the-string-newline) S)
	    (fseek S 0 SEEK_SET)
	    (let loop ()
	      (receive (result errno)
		  (platform:getdelim *pointer *count (char->integer #\newline) (FILE*->pointer S))
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  #t)


;;;; done

(check-report)

;;; end of file
