;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Expat
;;;Contents: Expat primitives
;;;Date: Tue Dec  1, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign xml expat primitives)
  (export

    xml-parse/string
    xml-attributes->alist

    make-xml-start-callback
    make-xml-end-callback
    make-xml-data-callback
    make-xml-comment-callback
    make-xml-start-cdata-callback
    make-xml-end-cdata-callback

    make-xml-processing-instruction-callback
    make-xml-default-callback
    make-xml-external-entity-ref-callback
    make-xml-skipped-entity-callback
    make-xml-unknown-encoding-callback
    make-xml-start-namespace-decl-callback
    make-xml-end-namespace-decl-callback
    make-xml-xml-decl-callback
    make-xml-start-doctype-decl-callback
    make-xml-end-doctype-decl-callback
    make-xml-element-decl-callback
    make-xml-attlist-decl-callback
    make-xml-entity-decl-callback
    make-xml-unparsed-entity-decl-callback
    make-xml-notation-decl-callback
    make-xml-not-standalone-callback

    (rename
     (XML_SetElementDeclHandler			xml-set-element-decl-handler)
     (XML_SetAttlistDeclHandler			xml-set-attlist-decl-handler)
     (XML_SetXmlDeclHandler			xml-set-xml-decl-handler)
     (XML_ParserCreate				xml-parser-create)
     (XML_ParserCreateNS			xml-parser-create-ns)
     (XML_ParserCreate_MM			xml-parser-create-mm)
     (XML_ParserReset				xml-parser-reset)
     (XML_SetEntityDeclHandler			xml-set-entity-decl-handler)
     (XML_SetElementHandler			xml-set-element-handler)
     (XML_SetStartElementHandler		xml-set-start-element-handler)
     (XML_SetEndElementHandler			xml-set-end-element-handler)
     (XML_SetCharacterDataHandler		xml-set-character-data-handler)
     (XML_SetProcessingInstructionHandler	xml-set-processing-instruction-handler)
     (XML_SetCommentHandler			xml-set-comment-handler)
     (XML_SetCdataSectionHandler		xml-set-cdata-section-handler)
     (XML_SetStartCdataSectionHandler		xml-set-start-cdata-section-handler)
     (XML_SetEndCdataSectionHandler		xml-set-end-cdata-section-handler)
     (XML_SetDefaultHandler			xml-set-default-handler)
     (XML_SetDefaultHandlerExpand		xml-set-default-handler-expand)
     (XML_SetDoctypeDeclHandler			xml-set-doctype-decl-handler)
     (XML_SetStartDoctypeDeclHandler		xml-set-start-doctype-decl-handler)
     (XML_SetEndDoctypeDeclHandler		xml-set-end-doctype-decl-handler)
     (XML_SetUnparsedEntityDeclHandler		xml-set-unparsed-entity-decl-handler)
     (XML_SetNotationDeclHandler		xml-set-notation-decl-handler)
     (XML_SetNamespaceDeclHandler		xml-set-namespace-decl-handler)
     (XML_SetStartNamespaceDeclHandler		xml-set-start-namespace-decl-handler)
     (XML_SetEndNamespaceDeclHandler		xml-set-end-namespace-decl-handler)
     (XML_SetNotStandaloneHandler		xml-set-not-standalone-handler)
     (XML_SetExternalEntityRefHandler		xml-set-external-entity-ref-handler)
     (XML_SetExternalEntityRefHandlerArg	xml-set-external-entity-ref-handler-arg)
     (XML_SetSkippedEntityHandler		xml-set-skipped-entity-handler)
     (XML_SetUnknownEncodingHandler		xml-set-unknown-encoding-handler)
     (XML_DefaultCurrent			xml-default-current)
     (XML_SetReturnNSTriplet			xml-set-return-ns-triplet)
     (XML_SetUserData				xml-set-user-data)
     (XML_SetEncoding				xml-set-encoding)
     (XML_UseParserAsHandlerArg			xml-use-parser-as-handler-arg)
     (XML_UseForeignDTD				xml-use-foreign-dtd)
     (XML_SetBase				xml-set-base)
     (XML_GetBase				xml-get-base)
     (XML_GetSpecifiedAttributeCount		xml-get-specified-attribute-count)
     (XML_GetIdAttributeIndex			xml-get-id-attribute-index)
     (XML_Parse					xml-parse)
     (XML_GetBuffer				xml-get-buffer)
     (XML_ParseBuffer				xml-parse-buffer)
     (XML_StopParser				xml-stop-parser)
     (XML_ResumeParser				xml-resume-parser)
     (XML_GetParsingStatus			xml-get-parsing-status)
     (XML_ExternalEntityParserCreate		xml-external-entity-parser-create)
     (XML_SetParamEntityParsing			xml-set-param-entity-parsing)
     (XML_GetErrorCode				xml-get-error-code)
     (XML_GetCurrentLineNumber			xml-get-current-line-number)
     (XML_GetCurrentColumnNumber		xml-get-current-column-number)
     (XML_GetCurrentByteIndex			xml-get-current-byte-index)
     (XML_GetCurrentByteCount			xml-get-current-byte-count)
     (XML_GetInputContext			xml-get-input-context)
     (XML_FreeContentModel			xml-free-content-model)
     (XML_MemMalloc				xml-mem-malloc)
     (XML_MemRealloc				xml-mem-realloc)
     (XML_MemFree				xml-mem-free)
     (XML_ParserFree				xml-parser-free)
     (XML_ErrorString				xml-error-string)
     (XML_ExpatVersion				xml-expat-version)
     (XML_GetFeatureList    			xml-get-feature-list))
    )
  (import (rnrs)
    (compensations)
    (only (foreign ffi) make-c-callback*)
    (only (foreign ffi pointers) pointer-null pointer-null?)
    (only (foreign cstrings) cstring->string argv->strings string->cstring/c)
    (foreign xml expat conditions)
    (foreign xml expat platform)
    (foreign xml expat sizeof))


;;;; helpers

(define (xml-attributes->alist attrs)
  (define (doit result tag val . attrs)
    (let ((result `((,(string->symbol tag) . ,val) . ,result)))
      (if (< 0 (length attrs))
	  (apply doit result attrs)
	(reverse result))))
  (if (< 0 (length attrs))
      (apply doit '() attrs)
    '()))



;;;; basic callbacks

(define (make-xml-start-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data element attributes)
     (scheme-function (string->symbol (cstring->string element))
		      (xml-attributes->alist (argv->strings attributes))))
   (pointer pointer pointer)))

(define (make-xml-end-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data element)
     (scheme-function (string->symbol (cstring->string element))))
   (pointer pointer)))

(define (make-xml-data-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data buf.ptr buf.len)
     (scheme-function (cstring->string buf.ptr buf.len)))
   (pointer pointer int)))

(define (make-xml-comment-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data data)
     (scheme-function (cstring->string data)))
   (pointer pointer)))

(define (make-xml-start-cdata-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data)
     (scheme-function))
   (pointer)))

(define (make-xml-end-cdata-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data)
     (scheme-function))
   (pointer)))


;;;; advanced callbacks

(define (make-xml-processing-instruction-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data target data)
     (scheme-function (cstring->string target)
		      (cstring->string data)))
   (pointer pointer pointer)))

(define (make-xml-default-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data buf.ptr buf.len)
     (scheme-function (cstring->string buf.ptr buf.len)))
   (pointer pointer signed-int)))

(define (make-xml-external-entity-ref-callback scheme-function)
  (make-c-callback*
   int
   (lambda (parser context base system-id public-id)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (if (scheme-function parser (or-false context)
			    (or-false base) (or-false public-id)
			    (cstring->string system-id))
	   XML_STATUS_OK
	 XML_STATUS_ERROR)))
   (pointer pointer pointer pointer pointer)))

(define (make-xml-skipped-entity-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data entity-name is-parameter-entity)
     (scheme-function (cstring->string entity-name)
		      (not (= 0 is-parameter-entity))))
   (pointer pointer int)))

(define (make-xml-unknown-encoding-callback scheme-function)
  (make-c-callback*
   int
   (lambda (unused-encoding-handler-data name info)
     (if (scheme-function (cstring->string name) info)
	 XML_STATUS_OK
       XML_STATUS_ERROR))
   (pointer pointer pointer)))

(define (make-xml-start-namespace-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data prefix uri)
     (scheme-function (cstring->string prefix)
		      (cstring->string uri)))
   (pointer pointer pointer)))

(define (make-xml-end-namespace-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data prefix)
     (scheme-function (cstring->string prefix)))
   (pointer pointer)))

(define (make-xml-xml-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data version encoding standalone)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (scheme-function (or-false version)
			(or-false encoding)
			(case standalone
			  ((-1)		'no-standalone)
			  ((0)		'no)
			  ((+1)		'yes)))))
   (pointer pointer pointer signed-int)))

(define (make-xml-start-doctype-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data doctype-name sysid pubid has-internal-subset)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (scheme-function (cstring->string doctype-name)
			(or-false sysid)
			(or-false pubid)
			(not (= 0 has-internal-subset)))))
   (pointer pointer pointer pointer signed-int)))

(define (make-xml-end-doctype-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data)
     (scheme-function))
   (pointer)))

(define (make-xml-element-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data name model)
     (scheme-function (cstring->string name) model))
   (pointer pointer pointer)))

(define (make-xml-attlist-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data elname attname att-type default is-required)
     (scheme-function (cstring->string elname)
		      (cstring->string attname)
		      (cstring->string att-type)
		      (if (pointer-null? default)
			  #f
			(cstring->string default))
		      (not (= 0 is-required))))
   (pointer pointer pointer pointer pointer signed-int)))

(define (make-xml-entity-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data entity-name is-parameter-entity
            value.ptr value.len base system-id public-id notation-name)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (scheme-function (cstring->string entity-name)
			(not (= 0 is-parameter-entity))
			(if (pointer-null? value.ptr)
			    #f
			  (cstring->string value.ptr value.len))
			(or-false base)
			(or-false system-id)
			(or-false public-id)
			(or-false notation-name))))
   (pointer pointer signed-int pointer signed-int pointer pointer pointer pointer)))

(define (make-xml-unparsed-entity-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data entity-name
            base system-id public-id notation-name)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (scheme-function (cstring->string entity-name)
			(or-false base)
			(or-false system-id)
			(or-false public-id)
			(or-false notation-name))))
   (pointer pointer pointer pointer pointer pointer)))

(define (make-xml-notation-decl-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data notation-name base system-id public-id)
     (let-syntax ((or-false (syntax-rules ()
			      ((_ ?arg)
			       (if (pointer-null? ?arg)
				   #f
				 (cstring->string ?arg))))))
       (scheme-function (cstring->string notation-name)
			(or-false base)
			(or-false system-id)
			(or-false public-id))))
   (pointer pointer pointer pointer pointer)))

(define (make-xml-not-standalone-callback scheme-function)
  (make-c-callback*
   void
   (lambda (unused-client-data)
     (scheme-function))
   (pointer)))


;;;; parsing

(define (xml-parse/string parser string finished?)
  (with-compensations
    (let* ((buf.len	(string-length string))
	   (buf.ptr	(string->cstring/c string))
	   (result	(XML_Parse parser buf.ptr buf.len (if finished? 1 0))))
      (when (= result XML_STATUS_ERROR)
	(raise (condition (make-expat-error-condition)
			  (make-expat-parser-condition parser)
			  (make-who-condition 'xml-parse)
			  (make-message-condition
			   (cstring->string (XML_ErrorString (XML_GetErrorCode parser))))))))))




;;;; done

)

;;; end of file
