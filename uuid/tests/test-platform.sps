;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/UUID
;;;Contents: tests for platform API
;;;Date: Tue Oct 27, 2009
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


(import (nausicaa)
  (foreign uuid platform)
  (foreign uuid sizeof)
  (foreign memory)
  (foreign cstrings)
  (foreign ffi sizeof)
  (compensations)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing UUID platform\n")

(define (true? obj)
  (not (zero? obj)))


(parametrise ((check-test-name	'version))

  (check
      (integer? (uuid_version))
    => #t)

  #t)


(parametrise ((check-test-name	'cloning))

  (check
      (with-compensations
	(let ((*one	(malloc-block/c sizeof-pointer))
	      (*two	(malloc-block/c sizeof-pointer))
	      (*bool1	(malloc-block/c sizeof-int))
	      (*bool2	(malloc-block/c sizeof-int))
	      (*bool3	(malloc-block/c sizeof-int)))
	  (letrec* ((one	(compensate
				    (assert (= UUID_RC_OK (uuid_create *one)))
				    (pointer-ref-c-pointer *one 0)
				  (with
				   (uuid_destroy one))))
		    (two	(compensate
				    (assert (= UUID_RC_OK (uuid_clone one *two)))
				    (pointer-ref-c-pointer *two 0)
				  (with
				   (uuid_destroy two)))))
	    (assert (= UUID_RC_OK (uuid_isnil one *bool1)))
	    (assert (= UUID_RC_OK (uuid_isnil two *bool2)))
	    (assert (= UUID_RC_OK (uuid_compare one two *bool3)))
	    (list (true? (pointer-ref-c-signed-int *bool1 0))
		  (true? (pointer-ref-c-signed-int *bool2 0))
		  (= 0 (pointer-ref-c-signed-int *bool3 0))))))
    => '(#t #t #t))

  #t)


(parametrise ((check-test-name	'import-export))

  (check	;export to STR
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr)))
	  (letrec* ((uuid (compensate
			      (let ((*uuid (malloc-block/c sizeof-pointer)))
				(assert (= UUID_RC_OK (uuid_create *uuid)))
				(pointer-ref-c-pointer *uuid 0))
			    (with
			     (uuid_destroy uuid)))))
	    (assert (= UUID_RC_OK (uuid_import uuid UUID_FMT_STR cstr clen)))
	    (let ((*str (malloc-block/c sizeof-pointer)))
	      (pointer-set-c-pointer! *str 0 pointer-null)
	      (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	      (letrec ((cstr (compensate
	    			 (pointer-ref-c-pointer *str 0)
	    		       (with
	    			(primitive-free cstr)))))
	    	(cstring->string cstr))))))
    => "4e8e1494-c318-11de-83fe-001e68fdaf8a")

  (check	;export to BIN
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr)))
	  (letrec* ((uuid (compensate
			      (let ((*uuid (malloc-block/c sizeof-pointer)))
				(assert (= UUID_RC_OK (uuid_create *uuid)))
				(pointer-ref-c-pointer *uuid 0))
			    (with
			     (uuid_destroy uuid)))))
	    (assert (= UUID_RC_OK (uuid_import uuid UUID_FMT_STR cstr clen)))
	    (let ((*ptr (malloc-block/c sizeof-pointer))
		  (*len (malloc-block/c sizeof-int)))
	      (pointer-set-c-pointer! *ptr 0 pointer-null)
	      (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_BIN *ptr *len)))
	      (pointer->bytevector (pointer-ref-c-pointer    *ptr 0)
				   (pointer-ref-c-signed-int *len 0))))))
    => '#vu8(78 142 20 148 195 24 17 222 131 254 0 30 104 253 175 138))

  (check 'this	;export to SIV
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr)))
	  (letrec* ((uuid (compensate
			      (let ((*uuid (malloc-block/c sizeof-pointer)))
				(assert (= UUID_RC_OK (uuid_create *uuid)))
				(pointer-ref-c-pointer *uuid 0))
			    (with
			     (uuid_destroy uuid)))))
	    (assert (= UUID_RC_OK (uuid_import uuid UUID_FMT_STR cstr clen)))
	    (let ((*ptr (malloc-block/c sizeof-pointer))
		  (*len (malloc-block/c sizeof-int)))
	      (pointer-set-c-pointer! *ptr 0 pointer-null)
	      (compensate
		  (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_SIV *ptr *len)))
		(with
		 (primitive-free (pointer-ref-c-pointer *ptr 0))))
	      (cstring->string (pointer-ref-c-pointer *ptr 0)
			       (- (pointer-ref-c-signed-int *len 0) 1))))))
    => "104417507259474184274030196212824387466")
;;;     01234567890123456789012345678901234567890
;;;     0         1         2         3         4

  (check	;export to TXT
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr)))
	  (letrec* ((uuid (compensate
			      (let ((*uuid (malloc-block/c sizeof-pointer)))
				(assert (= UUID_RC_OK (uuid_create *uuid)))
				(pointer-ref-c-pointer *uuid 0))
			    (with
			     (uuid_destroy uuid)))))
	    (assert (= UUID_RC_OK (uuid_import uuid UUID_FMT_STR cstr clen)))
	    (let ((*ptr (malloc-block/c sizeof-pointer))
		  (*len (malloc-block/c sizeof-int)))
	      (pointer-set-c-pointer! *ptr 0 pointer-null)
	      (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_TXT *ptr *len)))
	      (cstring->string (pointer-ref-c-pointer    *ptr 0)
			       (- (pointer-ref-c-signed-int *len 0) 1))))))
    => "encode: STR:     4e8e1494-c318-11de-83fe-001e68fdaf8a
        SIV:     104417507259474184274030196212824387466
decode: variant: DCE 1.1, ISO/IEC 11578:1996
        version: 1 (time and node based)
        content: time:  2009-10-27 16:46:43.225922.0 UTC
                 clock: 1022 (usually random)
                 node:  00:1e:68:fd:af:8a (global unicast)
")

  #t)


(parametrise ((check-test-name	'loading))

  (check	;load nil
      (with-compensations
	(letrec* ((uuid (compensate
			    (let ((*uuid (malloc-block/c sizeof-pointer)))
			      (assert (= UUID_RC_OK (uuid_create *uuid)))
			      (pointer-ref-c-pointer *uuid 0))
			  (with
			   (uuid_destroy uuid)))))
	  (assert (= UUID_RC_OK (uuid_load uuid (string->cstring/c "nil"))))
	  (let ((*str (malloc-block/c sizeof-pointer)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (cstring->string cstr)))))
    => "00000000-0000-0000-0000-000000000000")

  (check	;load ns:DNS
      (with-compensations
	(letrec* ((uuid (compensate
			    (let ((*uuid (malloc-block/c sizeof-pointer)))
			      (assert (= UUID_RC_OK (uuid_create *uuid)))
			      (pointer-ref-c-pointer *uuid 0))
			  (with
			   (uuid_destroy uuid)))))
	  (assert (= UUID_RC_OK (uuid_load uuid (string->cstring/c "ns:DNS"))))
	  (let ((*str (malloc-block/c sizeof-pointer)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (cstring->string cstr)))))
    => "6ba7b810-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:URL
      (with-compensations
	(letrec* ((uuid (compensate
			    (let ((*uuid (malloc-block/c sizeof-pointer)))
			      (assert (= UUID_RC_OK (uuid_create *uuid)))
			      (pointer-ref-c-pointer *uuid 0))
			  (with
			   (uuid_destroy uuid)))))
	  (assert (= UUID_RC_OK (uuid_load uuid (string->cstring/c "ns:URL"))))
	  (let ((*str (malloc-block/c sizeof-pointer)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (cstring->string cstr)))))
    => "6ba7b811-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:OID
      (with-compensations
	(letrec* ((uuid (compensate
			    (let ((*uuid (malloc-block/c sizeof-pointer)))
			      (assert (= UUID_RC_OK (uuid_create *uuid)))
			      (pointer-ref-c-pointer *uuid 0))
			  (with
			   (uuid_destroy uuid)))))
	  (assert (= UUID_RC_OK (uuid_load uuid (string->cstring/c "ns:OID"))))
	  (let ((*str (malloc-block/c sizeof-pointer)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (cstring->string cstr)))))
    => "6ba7b812-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:X500
      (with-compensations
	(letrec* ((uuid (compensate
			    (let ((*uuid (malloc-block/c sizeof-pointer)))
			      (assert (= UUID_RC_OK (uuid_create *uuid)))
			      (pointer-ref-c-pointer *uuid 0))
			  (with
			   (uuid_destroy uuid)))))
	  (assert (= UUID_RC_OK (uuid_load uuid (string->cstring/c "ns:X500"))))
	  (let ((*str (malloc-block/c sizeof-pointer)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (cstring->string cstr)))))
    => "6ba7b814-9dad-11d1-80b4-00c04fd430c8")

  #t)


(parametrise ((check-test-name	'generation))

  (check
      (with-compensations
	(let ((*uuid	(malloc-block/c sizeof-pointer))
	      (*str	(malloc-block/c sizeof-pointer)))
	  (assert (= UUID_RC_OK (uuid_create *uuid)))
	  (letrec ((uuid (compensate
			     (pointer-ref-c-pointer *uuid 0)
			   (with
			    (uuid_destroy uuid)))))
  	    (assert (= UUID_RC_OK (uuid_make uuid UUID_MAKE_V1)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (let ((str (cstring->string cstr)))
		;;;(write str)(newline)
		(string? str))))))
    => #t)

  (check
      (with-compensations
	(let ((*uuid	(malloc-block/c sizeof-pointer))
	      (*str	(malloc-block/c sizeof-pointer)))
	  (assert (= UUID_RC_OK (uuid_create *uuid)))
	  (letrec ((uuid (compensate
			     (pointer-ref-c-pointer *uuid 0)
			   (with
			    (uuid_destroy uuid)))))
	    (assert (= UUID_RC_OK (uuid_make uuid UUID_MAKE_V4)))
	    (pointer-set-c-pointer! *str 0 pointer-null)
	    (assert (= UUID_RC_OK (uuid_export uuid UUID_FMT_STR *str pointer-null)))
	    (letrec ((cstr (compensate
			       (pointer-ref-c-pointer *str 0)
			     (with
			      (primitive-free cstr)))))
	      (let ((str (cstring->string cstr)))
		;;;(write str)(newline)
		(string? str))))))
    => #t)

  (check		;make V3
      (with-compensations
	(let ((*uuid1 (malloc-block/c sizeof-pointer))
	      (*uuid2 (malloc-block/c sizeof-pointer)))
	  (letrec ((uuid1 (compensate
			      (assert (= UUID_RC_OK (uuid_create *uuid1)))
			      (pointer-ref-c-pointer *uuid1 0)
			    (with
			     (uuid_destroy uuid1))))
		   (uuid2 (compensate
			      (assert (= UUID_RC_OK (uuid_create *uuid2)))
			      (pointer-ref-c-pointer *uuid2 0)
			    (with
			     (uuid_destroy uuid2)))))

	    (assert (= UUID_RC_OK (uuid_load uuid1 (string->cstring/c "ns:DNS"))))
	    (assert (= UUID_RC_OK (uuid_make uuid2 UUID_MAKE_V3
					     uuid1 (string->cstring/c "ciao mamma"))))

	    (let ((*str (malloc-block/c sizeof-pointer)))
	      (pointer-set-c-pointer! *str 0 pointer-null)
	      (assert (= UUID_RC_OK (uuid_export uuid2 UUID_FMT_STR *str pointer-null)))
	      (letrec ((cstr (compensate
				 (pointer-ref-c-pointer *str 0)
			       (with
				(primitive-free cstr)))))
		#t)))))
    => #t)

  (check		;make V5
      (with-compensations
	(let ((*uuid1 (malloc-block/c sizeof-pointer))
	      (*uuid2 (malloc-block/c sizeof-pointer)))
	  (letrec ((uuid1 (compensate
			      (assert (= UUID_RC_OK (uuid_create *uuid1)))
			      (pointer-ref-c-pointer *uuid1 0)
			    (with
			     (uuid_destroy uuid1))))
		   (uuid2 (compensate
			      (assert (= UUID_RC_OK (uuid_create *uuid2)))
			      (pointer-ref-c-pointer *uuid2 0)
			    (with
			     (uuid_destroy uuid2)))))

	    (assert (= UUID_RC_OK (uuid_load uuid1 (string->cstring/c "ns:DNS"))))
	    (assert (= UUID_RC_OK (uuid_make uuid2 UUID_MAKE_V5
					     uuid1 (string->cstring/c "ciao mamma"))))

	    (let ((*str (malloc-block/c sizeof-pointer)))
	      (pointer-set-c-pointer! *str 0 pointer-null)
	      (assert (= UUID_RC_OK (uuid_export uuid2 UUID_FMT_STR *str pointer-null)))
	      (letrec ((cstr (compensate
				 (pointer-ref-c-pointer *str 0)
			       (with
				(primitive-free cstr)))))
		#t)))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
