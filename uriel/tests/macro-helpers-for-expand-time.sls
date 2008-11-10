;;; 
;;; Part of: Uriel libraries for Ikarus Scheme
;;; Contents: helper functions for expand time
;;; Date: Mon Nov 10, 2008
;;; 
;;; Abstract
;;; 
;;;	The  functions  in  this  library  have  the  purpose  of
;;;	demonstrating that the same  helper functions can be used
;;;	in more  than one  macro body if  they are leaded  from a
;;;	separate  library  using  the  (for ----  expand)  import
;;;	specification.
;;; 
;;; Copyright (c) 2008 Marco Maggi
;;; 
;;; This program is free software: you can redistribute it and/or
;;; modify it under  the terms of the GNU  General Public License
;;; as published by the  Free Software Foundation, either version
;;; 3 of the License, or (at your option) any later version.
;;; 
;;; This  program is  distributed in  the  hope that  it will  be
;;; useful, but  WITHOUT ANY  WARRANTY; without even  the implied
;;; warranty  of  MERCHANTABILITY  or  FITNESS FOR  A  PARTICULAR
;;; PURPOSE.   See  the  GNU  General  Public  License  for  more
;;; details.
;;; 
;;; You should  have received  a copy of  the GNU  General Public
;;; License   along    with   this   program.     If   not,   see
;;; <http://www.gnu.org/licenses/>.
;;; 

(library (macro-helpers-for-expand-time)
	 (export gasp gulp)
	 (import (rnrs))
	 (define (gasp arg)
	   arg)
	 (define (gulp arg)
	   arg))


;;; end of file
