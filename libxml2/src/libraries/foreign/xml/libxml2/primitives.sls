;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Libxml2
;;;Contents: primitive functions
;;;Date: Thu Dec 10, 2009
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


(library (foreign xml libxml2 primitives)
  (export)
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (foreign cstrings)
    (foreign xml libxml2 platform)
    (foreign xml libxml2 sizeof))


;;;; callback makers

(define (make-xml-c14n-is-visible-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* xmlNodePtr xmlNodePtr)))


(define (make-xml-shell-readline-func scheme-function)
  (make-c-callback* char*
		    scheme-function
		    (char*)))

(define (make-xml-shell-cmd scheme-function)
  (make-c-callback* int
		    scheme-function
		    (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))

(define (make-xml-char-encoding-input-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* void* void* void*)))

(define (make-xml-char-encoding-output-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* void* void* void*)))

(define (make-xml-parser-input-buffer-create-filename-func scheme-function)
  (make-c-callback* xmlParserInputBufferPtr
		    scheme-function
		    (char* xmlCharEncoding)))

(define (make-xml-output-buffer-create-filename-func scheme-function)
  (make-c-callback* xmlOutputBufferPtr
		    scheme-function
		    (char* xmlCharEncodingHandlerPtr int)))

(define (make-xml-register-node-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlNodePtr)))

(define (make-xml-deregister-node-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlNodePtr)))

(define (make-xml-hash-deallocator scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar*)))

(define (make-xml-hash-copier scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (void* xmlChar*)))

(define (make-xml-hash-scanner scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* void* xmlChar*)))

(define (make-xml-hash-scanner-full scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* void* xmlChar* xmlChar* xmlChar*)))

(define (make-xml-list-deallocator scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlLinkPtr)))

(define (make-xml-list-data-compare scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* void*)))

(define (make-xml-list-walker scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* void*)))

(define (make-ftp-list-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* char* char* char* char* unsigned-long int int char* int int int)))

(define (make-ftp-data-callback scheme-function)
  (make-c-callback* void
		    ftpDataCallback
		    (void* char* int)))

(define (make-xml-parser-input-deallocate scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlChar*)))

(define (make-resolve-entity-sax-func scheme-function)
  (make-c-callback* xmlParserInputPtr
		    scheme-function
		    (void* xmlChar* xmlChar*)))

(define (make-internal-subset-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar*)))

(define (make-external-subset-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar*)))

(define (make-get-entity-sax-func scheme-function)
  (make-c-callback* xmlEntityPtr
		    scheme-function
		    (void* xmlChar*)))

(define (make-get-parameter-entity-sax-func scheme-function)
  (make-c-callback* xmlEntityPtr
		    scheme-function
		    (void* xmlChar*)))

(define (make-entity-decl-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* int xmlChar* xmlChar* xmlChar*)))

(define (make-notation-decl-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar*)))

(define (make-attribute-decl-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* int int xmlChar* xmlEnumerationPtr)))

(define (make-element-decl-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* int xmlElementContentPtr)))

(define (make-unparsed-entity-decl-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar* xmlChar*)))

(define (make-set-document-locator-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlSAXLocatorPtr)))

(define (make-start-document-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void*)))

(define (make-end-document-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void*)))

(define (make-start-element-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar**)))

(define (make-end-element-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar*)))

(define (make-attribute-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar*)))

(define (make-reference-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar*)))

(define (make-characters-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* int)))

(define (make-ignorable-whitespace-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* int)))

(define (make- scheme-function)
  (make-c-callback* void
		    processingInstructionSAXFunc
		    (void* xmlChar* xmlChar*)))

(define (make-comment-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar*)))

(define (make-cdata-block-sax-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* int)))

;;Variadic!!!
;;
;; (define (make-warning-sax-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-error-sax-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-fatal-error-sax-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

(define (make-is-standalone-sax-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-has-internal-subset-sax-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-has-external-subset-sax-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-start-element-ns-sax2-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar* int xmlChar** int int xmlChar**)))

(define (make-endElementNsSAX2Func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlChar* xmlChar* xmlChar*)))

(define (make-xml-external-entity-loader scheme-function)
  (make-c-callback* xmlParserInputPtr
		    scheme-function
		    (char* char* xmlParserCtxtPtr)))


;;Variadic!!!
;;
;; (define (make-xml-relax-ng-validity-error-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-relax-ng-validity-warning-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-schematron-validity-error-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-schematron-validity-warning-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

(define (make-xml-dom-wrap-acquire-ns-function scheme-function)
  (make-c-callback* xmlNsPtr
		    scheme-function
		    (xmlDOMWrapCtxtPtr xmlNodePtr xmlChar* xmlChar*)))

;;Variadic!!!
;;
;; (define (make-xml-validity-error-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-validity-warning-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-generic-error-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

(define (make-xml-structured-error-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* xmlErrorPtr)))

(define (make-xml-input-match-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (char*)))

(define (make-xml-input-open-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (char*)))

(define (make-xml-input-read-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* char* int)))

(define (make-xml-input-close-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-xml-output-match-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (char*)))

(define (make-xml-output-open-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (char*)))

(define (make-xml-output-write-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* char* int)))

(define (make-xml-output-close-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-xml-free-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void*)))

(define (make-xml-malloc-func scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (size_t)))

(define (make-xml-realloc-func scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (void* size_t)))

(define (make-xml-strdup-func scheme-function)
  (make-c-callback* char*
		    scheme-function
		    (char*)))

(define (make-xml-text-reader-error-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* char* xmlParserSeverities xmlTextReaderLocatorPtr)))

(define (make-xml-reg-exec-callbacks scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlRegExecCtxtPtr xmlChar* void* void*)))

;;Variadic!!!
;;
;; (define (make-xml-schema-validity-error-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

;;Variadic!!!
;;
;; (define (make-xml-schema-validity-warning-func scheme-function)
;;   (make-c-callback* void
;; 		    scheme-function
;; 		    (void* char* ...)))

(define (make-xml-xpath-convert-func scheme-function)
  (make-c-callback* int
		    scheme-function
		    (xmlXPathObjectPtr int)))

(define (make-xml-xpath-eval-func scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlXPathParserContextPtr int)))

(define (make-xml-xpath-axis-func scheme-function)
  (make-c-callback* xmlXPathObjectPtr
		    scheme-function
		    (xmlXPathParserContextPtr xmlXPathObjectPtr)))


(define (make-xml-xpath-function scheme-function)
  (make-c-callback* void
		    scheme-function
		    (xmlXPathParserContextPtr int)))

(define (make-xml-xpath-variable-lookup-func scheme-function)
  (make-c-callback* xmlXPathObjectPtr
		    scheme-function
		    (void* xmlChar* xmlChar*)))

(define (make-xml-xpath-func-lookup-func scheme-function)
  (make-c-callback* xmlXPathFunction
		    scheme-function
		    (void* xmlChar* xmlChar*)))



;;;; done

)

;;; end of file
