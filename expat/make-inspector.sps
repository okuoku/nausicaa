;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Expat
;;;Contents: library inspection creator
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


(import (nausicaa)
  (foreign ffi inspector-maker))

(define expat-library-spec
  '(foreign xml expat sizeof))

(define-shared-object expat libexpat.so)


;;;; C types

(define-c-type-alias XML_Parser		pointer)

(define-c-type XML_Bool			signed-int)
(define-c-type XML_Char			signed-int)
(define-c-type XML_LChar		signed-int)
(define-c-type XML_Index		signed-int)
(define-c-type XML_Size			unsigned-int)


;;;; data structures

(define-c-struct XML_Content "XML_Content"
  (signed-int		type)
  (signed-int		quant)
  (pointer		name)
  (unsigned-int		numchildren)
  (pointer		children))

(define-c-struct XML_Encoding "XML_Encoding"
  (embedded		map)
  (pointer		data)
  (pointer		convert)
  (pointer		release))

(define-c-struct XML_ParsingStatus "XML_ParsingStatus"
  (pointer		parsing)
  (signed-int		finalBuffer))

(define-c-struct XML_Expat_Version "XML_Expat_Version"
  (signed-int		major)
  (signed-int		minor)
  (signed-int		micro))

(define-c-struct XML_Feature "XML_Feature"
  (signed-int		feature)
  (pointer		name)
  (signed-int		value))


;;;; constants

(define-c-defines "XML_Bool"
  XML_TRUE
  XML_FALSE)

(define-c-enumeration XML_Status
  "enum XML_Status"
  XML_STATUS_ERROR
  XML_STATUS_OK
  XML_STATUS_SUSPENDED)

(define-c-enumeration XML_Error
  "enum XML_Error"
  XML_ERROR_NONE
  XML_ERROR_NO_MEMORY
  XML_ERROR_SYNTAX
  XML_ERROR_NO_ELEMENTS
  XML_ERROR_INVALID_TOKEN
  XML_ERROR_UNCLOSED_TOKEN
  XML_ERROR_PARTIAL_CHAR
  XML_ERROR_TAG_MISMATCH
  XML_ERROR_DUPLICATE_ATTRIBUTE
  XML_ERROR_JUNK_AFTER_DOC_ELEMENT
  XML_ERROR_PARAM_ENTITY_REF
  XML_ERROR_UNDEFINED_ENTITY
  XML_ERROR_RECURSIVE_ENTITY_REF
  XML_ERROR_ASYNC_ENTITY
  XML_ERROR_BAD_CHAR_REF
  XML_ERROR_BINARY_ENTITY_REF
  XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF
  XML_ERROR_MISPLACED_XML_PI
  XML_ERROR_UNKNOWN_ENCODING
  XML_ERROR_INCORRECT_ENCODING
  XML_ERROR_UNCLOSED_CDATA_SECTION
  XML_ERROR_EXTERNAL_ENTITY_HANDLING
  XML_ERROR_NOT_STANDALONE
  XML_ERROR_UNEXPECTED_STATE
  XML_ERROR_ENTITY_DECLARED_IN_PE
  XML_ERROR_FEATURE_REQUIRES_XML_DTD
  XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING
  XML_ERROR_UNBOUND_PREFIX
  XML_ERROR_UNDECLARING_PREFIX
  XML_ERROR_INCOMPLETE_PE
  XML_ERROR_XML_DECL
  XML_ERROR_TEXT_DECL
  XML_ERROR_PUBLICID
  XML_ERROR_SUSPENDED
  XML_ERROR_NOT_SUSPENDED
  XML_ERROR_ABORTED
  XML_ERROR_FINISHED
  XML_ERROR_SUSPEND_PE
  XML_ERROR_RESERVED_PREFIX_XML
  XML_ERROR_RESERVED_PREFIX_XMLNS
  XML_ERROR_RESERVED_NAMESPACE_URI)

(define-c-enumeration XML_Content_Type
  "enum XML_Content_Type"
  XML_CTYPE_EMPTY
  XML_CTYPE_ANY
  XML_CTYPE_MIXED
  XML_CTYPE_NAME
  XML_CTYPE_CHOICE
  XML_CTYPE_SEQ)

(define-c-enumeration XML_Content_Quant
  "enum XML_Content_Quant"
  XML_CQUANT_NONE
  XML_CQUANT_OPT
  XML_CQUANT_REP
  XML_CQUANT_PLUS)

(define-c-enumeration XML_Parsing
  "enum XML_Parsing"
  XML_INITIALIZED
  XML_PARSING
  XML_FINISHED
  XML_SUSPENDED)

(define-c-enumeration XML_ParamEntityParsing
  "enum XML_ParamEntityParsing"
  XML_PARAM_ENTITY_PARSING_NEVER
  XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE
  XML_PARAM_ENTITY_PARSING_ALWAYS)

(define-c-defines "version numbers"
  XML_MAJOR_VERSION
  XML_MINOR_VERSION
  XML_MICRO_VERSION)

(define-c-enumeration XML_FeatureEnum
  "enum XML_FeatureEnum"
  XML_FEATURE_END
  XML_FEATURE_UNICODE
  XML_FEATURE_UNICODE_WCHAR_T
  XML_FEATURE_DTD
  XML_FEATURE_CONTEXT_BYTES
  XML_FEATURE_MIN_SIZE
  XML_FEATURE_SIZEOF_XML_CHAR
  XML_FEATURE_SIZEOF_XML_LCHAR
  XML_FEATURE_NS
  XML_FEATURE_LARGE_SIZE)


;;;; done

(autoconf-lib-write "configuration/expat-inspector.m4" expat-library-spec
		    "NAUSICAA_EXPAT")
(sizeof-lib-write   "src/libraries/foreign/xml/expat/sizeof.sls.in" expat-library-spec)

;;; end of file
