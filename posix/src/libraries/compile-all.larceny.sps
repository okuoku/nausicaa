;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: compile script
;;;Date: Fri Dec 26, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

(import (rnrs)
  (larceny compiler))

(compile-library "foreign/posix/sizeof.sls"
		 "foreign/posix/sizeof.larceny.slfasl")

(compile-library "foreign/posix/time/stub.sls"
		 "foreign/posix/time/stub.larceny.slfasl")

(compile-library "foreign/posix/time.sls"
		 "foreign/posix/time.larceny.slfasl")

(compile-library "foreign/posix/process/stub.sls"
		 "foreign/posix/process/stub.larceny.slfasl")

(compile-library "foreign/posix/process/platform.sls"
		 "foreign/posix/process/platform.larceny.slfasl")

(compile-library "foreign/posix/process.sls"
		 "foreign/posix/process.larceny.slfasl")

(compile-library "foreign/posix/job/platform.sls"
		 "foreign/posix/job/platform.larceny.slfasl")

(compile-library "foreign/posix/job.sls"
		 "foreign/posix/job.larceny.slfasl")

(compile-library "foreign/posix/fd/platform.sls"
		 "foreign/posix/fd/platform.larceny.slfasl")

(compile-library "foreign/posix/fd.sls"
		 "foreign/posix/fd.larceny.slfasl")

(compile-library "foreign/posix/file/platform.sls"
		 "foreign/posix/file/platform.larceny.slfasl")

(compile-library "foreign/posix/file/stat.sls"
		 "foreign/posix/file/stat.larceny.slfasl")

(compile-library "foreign/posix/file.sls"
		 "foreign/posix/file.larceny.slfasl")


(compile-library "foreign/posix/environment/platform.sls"
		 "foreign/posix/environment/platform.larceny.slfasl")

(compile-library "foreign/posix/environment.sls"
		 "foreign/posix/environment.larceny.slfasl")

;;; end of file
