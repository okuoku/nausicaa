;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: test for streams
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-04 18:31:03 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(import (rnrs)
  (uriel lang)
  (uriel printing)
  (uriel test)
  (uriel getenv)
  (rename (uriel ffi)
	  (string-or-symbol->cstring/compensated s->c))
  (srfi receive)
  (srfi parameters)
  (only (string-lib) string-join)
  (glibc streams)
  (glibc streams-extended)
  (glibc streams-unlocked))

(check-set-mode! 'report-failed)

(define TMPDIR (getenv "TMPDIR"))

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



;;;; basic stream operations

(parameterize ((testname 'basic))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S (fopen pathname "w+"))
		 (p (s->c the-string))
		 (len (strlen p)))
	    (fwrite p 1 len S)
	    (fread p len 1 S)
	    (fclose S)
	    (cstring->string p))))
    => the-string)
  )


;;;; getting lines

(write the-pathname)(newline)

(parameterize ((testname 'getline))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(s->c the-string))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 valueof-seek-set)
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
		 (p	(s->c the-string-newline))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 valueof-seek-set)
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
		   (*pointer	(compensate-malloc/small))
		   (*count	(compensate-malloc/small))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (s->c the-string) 1
		    (string-length the-string-newline) S)
	    (fseek S 0 valueof-seek-set)
	    (let loop ()
	      (receive (result errno)
		  (primitive-getline *pointer *count S)
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string/len (getp) result)
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
		   (*pointer	(compensate-malloc/small))
		   (*count	(compensate-malloc/small))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (s->c the-string-newline) 1
		    (string-length the-string-newline) S)
	    (fseek S 0 valueof-seek-set)
	    (let loop ()
	      (receive (result errno)
		  (primitive-getline *pointer *count S)
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string/len (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  )


;;;; getting delimited sequences

(parameterize ((testname 'getdelim))

  (check
      (let ((pathname the-pathname))
	(with-compensations
	  (let* ((S	(fopen pathname "w+"))
		 (p	(s->c the-string))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 valueof-seek-set)
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
		 (p	(s->c the-string-newline))
		 (len	(strlen p)))
	    (fwrite p 1 len S)
	    (fseek S 0 valueof-seek-set)
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
		   (*pointer	(compensate-malloc/small))
		   (*count	(compensate-malloc/small))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (s->c the-string) 1
		    (string-length the-string-newline) S)
	    (fseek S 0 valueof-seek-set)
	    (let loop ()
	      (receive (result errno)
		  (primitive-getdelim *pointer *count (char->integer #\newline) S)
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string/len (getp) result)
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
		   (*pointer	(compensate-malloc/small))
		   (*count	(compensate-malloc/small))
		   (getp	(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
		   (lines	'()))

	    (fwrite (s->c the-string-newline) 1
		    (string-length the-string-newline) S)
	    (fseek S 0 valueof-seek-set)
	    (let loop ()
	      (receive (result errno)
		  (primitive-getdelim *pointer *count (char->integer #\newline) S)
		(cond ((ferror S)
		       (primitive-free (getp))
		       (raise-errno-error 'reading-line errno S))
		      ((= -1 result)
		       (primitive-free (getp))
		       (reverse lines))
		      (else
		       (set! lines
			     (cons (cstring->string/len (getp) result)
				   lines))
		       (loop))))))))
    => '("Le Poete est semblable au prince des nuees\n"
	 "Qui hante la tempete e se rit de l'archer;\n"
	 "Exile sul le sol au milieu des huees,\n"
	 "Ses ailes de geant l'empechent de marcher.\n"))

  )



;;;; done

(check-report)

;;; end of file
