;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Libxml2
;;;Contents: mostly loading test for the high-level library
;;;Date: Fri Mar 19, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (foreign ffi)
  (foreign cstrings)
  (foreign memory)
  (xml libxml2)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing high-level API\n")


(parametrise ((check-test-name	'version))

  (check
      (begin
	(xml-check-version LIBXML_VERSION)
	(xml-cleanup-parser)
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'file-reading))

  (define pathname "proof.xml")
  (define xml-1
    "<?xml version=\"1.0\"?><alpha><beta>ciao</beta></alpha>")

  (when (file-exists? pathname)
    (delete-file pathname))

  (check
      (with-compensations
	  (compensate
	      (xml-check-version LIBXML_VERSION)
	    (with
	     (xml-cleanup-parser)))
	  (compensate
	    (with-output-to-file pathname
	      (lambda ()
		(display xml-1)))
	    (with
	     (delete-file pathname)))
	(letrec ((doc (compensate
			  (xml-parse-file (string->cstring/c pathname))
			(with
			 (xml-free-doc doc)))))
	  (assert (not (pointer-null? doc)))

	  ((recursion (walk-tree root-node)
	    (let loop ((tree '())
		       (node root-node))
              (with-struct-xmlNode node
		 (if (pointer-null? node)
		     (reverse tree)
		   (loop (cons (cons (cstring->string node.name)
				     (walk-tree node.children))
			       tree)
			 node.next)))))
	   (xml-doc-get-root-element doc))))
    => '(("alpha" ("beta" ("text")))))

  #t)


(parametrise ((check-test-name  'html-parser))

  (define html-page "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">
<html>
  <head>
    <title>Marco Maggi Vanity Pages</title>
    <link href=\"webpage.css\" rel=\"stylesheet\" type=\"text/css\">
   </head>
  <body>
  <p>This is the body</p>
  </body>
</html>
<!-- end of page -->")

  (check
      (with-compensations
	  (compensate
	      (xml-check-version LIBXML_VERSION)
	    (with
	     (xml-cleanup-parser)))
	(letrec ((parser (compensate
			     (html-create-push-parser-ctxt pointer-null pointer-null
							   pointer-null 0
							   pointer-null 0)
			   (with
			    (xml-free-doc (struct-xmlParserCtxt-myDoc-ref parser))
			    (html-free-parser-ctxt parser)))))
	  (define-xmlParserCtxt parser)

	  (html-ctxt-use-options parser
				 (bitwise-ior HTML_PARSE_NOBLANKS
					      HTML_PARSE_NOERROR
					      HTML_PARSE_NOWARNING
					      HTML_PARSE_NONET))

	  (let* ((page.ptr (string->cstring/c html-page))
		 (page.len (strlen page.ptr)))
	    (html-parse-chunk parser page.ptr page.len 1))

	  ((recursion (walk-tree root-node)
	    (let loop ((tree '())
		       (node root-node))
              (with-struct-xmlNode node
		 (if (pointer-null? node)
		     (reverse tree)
		   (loop (cons (cons (cstring->string node.name)
				     (walk-tree node.children))
			       tree)
			 node.next)))))
	   (xml-doc-get-root-element parser.myDoc))))
    => '(("html"
	  ("head"
	   ("title" ("text"))
	   ("link"))
	  ("body"
	   ("p" ("text"))))))

  #t)


(parametrise ((check-test-name	'xpath))

  (define pathname "proof.xml")
  (define xml-1
    "<?xml version=\"1.0\"?><alpha><beta>ciao</beta></alpha>")

  (when (file-exists? pathname)
    (delete-file pathname))

  (check
      (with-compensations
	  (compensate
	      (xml-check-version LIBXML_VERSION)
	    (with
	     (xml-cleanup-parser)))

	  (compensate
	    (with-output-to-file pathname
	      (lambda ()
		(display xml-1)))
	    (with
	     (delete-file pathname)))

	(letrec ((doc (compensate
			  (xml-parse-file (string->cstring/c pathname))
			(with
			 (xml-free-doc doc)))))
	  (assert (not (pointer-null? doc)))

	  (let* ((ctx	 (begin0-let ((ctx (xml-xpath-new-context doc)))
			   (if (pointer-null? ctx)
			       (raise-out-of-memory #f #f)
			     (push-compensation
			      (xml-xpath-free-context ctx)))))
		 (result (begin0-let
			     ((result (xml-xpath-eval-expression
				       (string->cstring/c "//alpha/beta") ctx)))
			   (if (pointer-null? result)
			       (raise-out-of-memory #f #f)
			     (push-compensation
			      (xml-xpath-free-object result))))))
	    (define-xmlXPathObject result)
	    (define nodeset result.nodesetval)
	    (define-xmlNodeSet nodeset)
	    (define nodetab nodeset.nodeTab)
	    ;; (let loop ((i    0)
	    ;; 	       (keys '()))
	    ;;   (if (= i nodeset.nodeNr)
	    ;; 	  keys
	    ;; 	)
	    ;; (define-xmlNode nodetab)
	    ;; (display nodeset.nodeNr)(newline)


	    #f)))

    => #f)

  #t)




;;;; done

(check-report)

;;; end of file
