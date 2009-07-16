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
; Fonction pour reconstituer le fichier silex.scm a partir des
; differents modules
;

(define update
  (lambda ()
    (let ((entete
	   '("; SILex - Scheme Implementation of Lex"
	     "; SILex 1.0"
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
	     ""))
	  (out-port (open-output-file "silex.scm")))
      (for-each (lambda (str)
		  (display str out-port)
		  (newline out-port))
		entete)
      (for-each
       (lambda (in-file)
	 (display "; Module " out-port)
	 (display in-file out-port)
	 (display "." out-port)
	 (newline out-port)
	 (let ((in-port (open-input-file in-file)))
	   (let loop ((c (read-char in-port))
		      (skip? #t)
		      (bol? #t))
	     (if (eof-object? c)
		 (begin
		   (newline out-port)
		   (close-input-port in-port))
		 (if skip?
		     (if bol?
			 (if (char=? c #\;)
			     (loop (read-char in-port) #t #f)
			     (begin
			       (write-char c out-port)
			       (loop (read-char in-port) #f 'dontcare)))
			 (if (char=? c #\newline)
			     (loop (read-char in-port) #t #t)
			     (loop (read-char in-port) #t #f)))
		     (begin
		       (write-char c out-port)
		       (loop (read-char in-port) #f 'dontcare)))))))
       '("util.scm"
	 "action.l.scm"
	 "class.l.scm"
	 "macro.l.scm"
	 "regexp.l.scm"
	 "string.l.scm"
	 "multilex.scm"
	 "lexparser.scm"
	 "re2nfa.scm"
	 "noeps.scm"
	 "sweep.scm"
	 "nfa2dfa.scm"
	 "prep.scm"
	 "output.scm"
	 "output2.scm"
	 "main.scm"))
      (close-output-port out-port))))
