; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;
; Fonction pour reconstituer le module output2.scm a partir du fichier
; multilex.scm
;

(define update
  (let ((entete
	 '("; SILex - Scheme Implementation of Lex"
	   "; Copyright (C) 2001  Danny Dube'"
	   "; "
	   "; This program is free software; you can redistribute it and/or"
	   "; modify it under the terms of the GNU General Public License"
	   "; as published by the Free Software Foundation; either version 2"
	   "; of the License, or (at your option) any later version."
	   "; "
	   "; This program is distributed in the hope that it will be useful,"
	   "; but WITHOUT ANY WARRANTY; without even the implied warranty of"
	   "; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
	   "; GNU General Public License for more details."
	   "; "
	   "; You should have received a copy of the GNU General Public License"
	   "; along with this program; if not, write to the Free Software"
	   "; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA."
	   ""
	   ";"
	   "; Fonction de copiage du fichier run-time"
	   ";"
	   ""
	   "(define out-print-run-time-lib"
	   "  (lambda (port)"
	   "    (display \"; *** This file start\" port)"
	   "    (display \"s with a copy of the \" port)"
	   "    (display \"file multilex.scm ***\" port)"
	   "    (newline port)")))
    (lambda ()
      (let ((in-port (open-input-file "multilex.scm"))
	    (out-port (open-output-file "output2.scm")))
	(for-each (lambda (str)
		    (display str out-port)
		    (newline out-port))
		  entete)
	(display "    (display \"" out-port)
	(let loop ((c (read-char in-port)))
	  (if (eof-object? c)
	      (begin
		(display "\" port)))" out-port)
		(newline out-port)
		(close-input-port in-port)
		(close-output-port out-port))
	      (begin
		(cond ((char=? c #\")
		       (write-char #\\ out-port)
		       (write-char #\" out-port))
		      ((char=? c #\\)
		       (write-char #\\ out-port)
		       (write-char #\\ out-port))
		      (else
		       (write-char c out-port)))
		(loop (read-char in-port)))))))))
