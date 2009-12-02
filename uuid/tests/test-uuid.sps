;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/UUID
;;;Contents: tests for high level API
;;;Date: Wed Oct 28, 2009
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
  (foreign uuid)
  (foreign uuid compensated)
  (foreign memory)
  (foreign cstrings)
  (compensations)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing UUID primitives\n")


(parametrise ((check-test-name	'version))

  (check
      (integer? (uuid-version))
    => #t)

  #t)


(parametrise ((check-test-name	'cloning))

  (check
      (with-compensations
	(let* ((one (uuid-create/c))
	       (two (uuid-clone/c one)))
	  (list (uuid-isnil? one)
		(uuid-isnil? two)
		(= 0 (uuid-compare one two)))))
    => '(#t #t #t))

  #t)


(parametrise ((check-test-name	'import-export))

  (check	;export to STR
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr))
	       (uuid	(uuid-import/c UUID_FMT_STR cstr clen)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "4e8e1494-c318-11de-83fe-001e68fdaf8a")

  (check	;export to BIN
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr))
	       (uuid	(uuid-import/c UUID_FMT_STR cstr clen)))
	  (uuid-export uuid UUID_FMT_BIN)))
    => '#vu8(78 142 20 148 195 24 17 222 131 254 0 30 104 253 175 138))

  (check	;export to SIV
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr))
	       (uuid	(uuid-import/c UUID_FMT_STR cstr clen)))
	  (uuid-export uuid UUID_FMT_SIV)))
    => "104417507259474184274030196212824387466")
;;;     01234567890123456789012345678901234567890
;;;     0         1         2         3         4

  (check	;export to TXT
      (with-compensations
	(let* ((str	"4e8e1494-c318-11de-83fe-001e68fdaf8a")
	       (cstr	(string->cstring/c str))
	       (clen	(strlen cstr))
	       (uuid	(uuid-import/c UUID_FMT_STR cstr clen)))
	  (uuid-export uuid UUID_FMT_TXT)))
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
	(let ((uuid (uuid-load/c 'nil)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "00000000-0000-0000-0000-000000000000")

  (check	;load ns:DNS
      (with-compensations
	(let ((uuid (uuid-load/c 'ns:DNS)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "6ba7b810-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:URL
      (with-compensations
	(let ((uuid (uuid-load/c 'ns:URL)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "6ba7b811-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:OID
      (with-compensations
	(let ((uuid (uuid-load/c 'ns:OID)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "6ba7b812-9dad-11d1-80b4-00c04fd430c8")

  (check	;load ns:X500
      (with-compensations
	(let ((uuid (uuid-load/c 'ns:X500)))
	  (uuid-export uuid UUID_FMT_STR)))
    => "6ba7b814-9dad-11d1-80b4-00c04fd430c8")

  #t)


(parametrise ((check-test-name	'generation))

  (check
      (with-compensations
	(let ((uuid (uuid-make/c UUID_MAKE_V1)))
	  (string? (uuid-export uuid UUID_FMT_STR))))
    => #t)

  (check
      (with-compensations
	(let ((uuid (uuid-make/c UUID_MAKE_V4)))
	  (string? (uuid-export uuid UUID_FMT_STR))))
    => #t)

  (check
      (with-compensations
	(let* ((uuid-1 (uuid-create/c))
	       (uuid-2 (uuid-make/c UUID_MAKE_V3 uuid-1 "ciao mamma")))
	  (string? (uuid-export uuid-2 UUID_FMT_STR))))
    => #t)

  (check
      (with-compensations
	(let* ((uuid-1 (uuid-create/c))
	       (uuid-2 (uuid-make/c UUID_MAKE_V5 uuid-1 "ciao mamma")))
	  (string? (uuid-export uuid-2 UUID_FMT_STR))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
