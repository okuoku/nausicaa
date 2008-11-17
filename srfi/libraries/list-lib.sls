;;; list-lib.sls --
;;;
;;; Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;; Permission is  hereby granted, free of charge,  to any person
;;; obtaining   a   copy   of   this  software   and   associated
;;; documentation files (the "Software"), to deal in the Software
;;; without restriction, including  without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the  Software, and to permit persons to
;;; whom  the Software  is furnished  to  do so,  subject to  the
;;; following conditions:
;;;
;;; The above  copyright notice and this  permission notice shall
;;; be  included in  all copies  or substantial  portions  of the
;;; Software.
;;;
;;; Except as contained in this  notice, the name(s) of the above
;;; copyright  holders  shall  not  be  used  in  advertising  or
;;; otherwise to promote the sale,  use or other dealings in this
;;; Software without prior written authorization.
;;;
;;; THE  SOFTWARE IS PROVIDED  "AS IS",  WITHOUT WARRANTY  OF ANY
;;; KIND, EXPRESS  OR IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE
;;; WARRANTIES  OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR
;;; PURPOSE AND  NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS
;;; OR  COPYRIGHT HOLDERS  BE LIABLE  FOR ANY  CLAIM,  DAMAGES OR
;;; OTHER LIABILITY,  WHETHER IN AN  ACTION OF CONTRACT,  TORT OR
;;; OTHERWISE,  ARISING FROM, OUT  OF OR  IN CONNECTION  WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(library (list-lib)
   ;;These should be all the exports from (srfi lists).
   (export xcons tree-copy make-list list-tabulate cons* list-copy 
	   proper-list? circular-list? dotted-list? not-pair? null-list? list=
	   circular-list length+
	   iota
	   first second third fourth fifth sixth seventh eighth ninth tenth
	   car+cdr
	   take       drop       
	   take-right drop-right 
	   take!      drop-right!
	   split-at   split-at!
	   last last-pair
	   zip unzip1 unzip2 unzip3 unzip4 unzip5
	   count
	   append! append-reverse append-reverse! concatenate concatenate! 
	   unfold       fold       pair-fold       reduce
	   unfold-right fold-right pair-fold-right reduce-right
	   append-map append-map! map! pair-for-each filter-map map-in-order
	   filter  partition  remove
	   filter! partition! remove! 
	   find find-tail any every list-index
	   take-while drop-while take-while!
	   span break span! break!
	   delete delete!
	   alist-cons alist-copy
	   delete-duplicates delete-duplicates!
	   alist-delete alist-delete!
	   reverse! 
	   lset<= lset= lset-adjoin  
	   lset-union  lset-intersection  lset-difference  lset-xor  
	   lset-diff+intersection
	   lset-union! lset-intersection! lset-difference! lset-xor!
	   lset-diff+intersection!)
   (import (srfi lists)))

;;; end of file
