;;;
;;;Part of: Nausicaa/Expat
;;;Contents: interface to Expat for R6RS Scheme
;;;Date: Sat Jan  3, 2009
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


(library (foreign xml expat)
  (export
    xml-set-element-decl-handler			xml-set-attlist-decl-handler
    xml-set-xml-decl-handler				xml-parser-create
    xml-parser-create-ns				xml-parser-create-mm
    xml-parser-reset					xml-set-entity-decl-handler
    xml-set-element-handler				xml-set-start-element-handler
    xml-set-end-element-handler				xml-set-character-data-handler
    xml-set-processing-instruction-handler		xml-set-comment-handler
    xml-set-cdata-section-handler			xml-set-start-cdata-section-handler
    xml-set-end-cdata-section-handler			xml-set-default-handler
    xml-set-default-handler-expand			xml-set-doctype-decl-handler
    xml-set-start-doctype-decl-handler			xml-set-end-doctype-decl-handler
    xml-set-unparsed-entity-decl-handler		xml-set-notation-decl-handler
    xml-set-namespace-decl-handler			xml-set-start-namespace-decl-handler
    xml-set-end-namespace-decl-handler			xml-set-not-standalone-handler
    xml-set-external-entity-ref-handler			xml-set-external-entity-ref-handler-arg
    xml-set-skipped-entity-handler			xml-set-unknown-encoding-handler
    xml-default-current					xml-set-return-ns-triplet
    xml-set-user-data					xml-set-encoding
    xml-use-parser-as-handler-arg			xml-use-foreign-dtd
    xml-set-base					xml-get-base
    xml-get-specified-attribute-count			xml-get-id-attribute-index
    xml-parse						xml-get-buffer
    xml-parse-buffer					xml-stop-parser
    xml-resume-parser					xml-get-parsing-status
    xml-external-entity-parser-create			xml-set-param-entity-parsing
    xml-get-error-code					xml-get-current-line-number
    xml-get-current-column-number			xml-get-current-byte-index
    xml-get-current-byte-count				xml-get-input-context
    xml-free-content-model				xml-mem-malloc
    xml-mem-realloc					xml-mem-free
    xml-parser-free					xml-error-string
    xml-expat-version					xml-get-feature-list

    XML_TRUE						XML_FALSE
    XML_STATUS_ERROR					XML_STATUS_OK
    XML_STATUS_SUSPENDED
    XML_ERROR_NONE				XML_ERROR_NO_MEMORY
    XML_ERROR_SYNTAX				XML_ERROR_NO_ELEMENTS
    XML_ERROR_INVALID_TOKEN			XML_ERROR_UNCLOSED_TOKEN
    XML_ERROR_PARTIAL_CHAR			XML_ERROR_TAG_MISMATCH
    XML_ERROR_DUPLICATE_ATTRIBUTE		XML_ERROR_JUNK_AFTER_DOC_ELEMENT
    XML_ERROR_PARAM_ENTITY_REF			XML_ERROR_UNDEFINED_ENTITY
    XML_ERROR_RECURSIVE_ENTITY_REF		XML_ERROR_ASYNC_ENTITY
    XML_ERROR_BAD_CHAR_REF			XML_ERROR_BINARY_ENTITY_REF
    XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF	XML_ERROR_MISPLACED_XML_PI
    XML_ERROR_UNKNOWN_ENCODING			XML_ERROR_INCORRECT_ENCODING
    XML_ERROR_UNCLOSED_CDATA_SECTION		XML_ERROR_EXTERNAL_ENTITY_HANDLING
    XML_ERROR_NOT_STANDALONE			XML_ERROR_UNEXPECTED_STATE
    XML_ERROR_ENTITY_DECLARED_IN_PE		XML_ERROR_FEATURE_REQUIRES_XML_DTD
    XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING	XML_ERROR_UNBOUND_PREFIX
    XML_ERROR_UNDECLARING_PREFIX		XML_ERROR_INCOMPLETE_PE
    XML_ERROR_XML_DECL				XML_ERROR_TEXT_DECL
    XML_ERROR_PUBLICID				XML_ERROR_SUSPENDED
    XML_ERROR_NOT_SUSPENDED			XML_ERROR_ABORTED
    XML_ERROR_FINISHED				XML_ERROR_SUSPEND_PE
    XML_ERROR_RESERVED_PREFIX_XML		XML_ERROR_RESERVED_PREFIX_XMLNS
    XML_ERROR_RESERVED_NAMESPACE_URI		XML_CTYPE_EMPTY
    XML_CTYPE_ANY				XML_CTYPE_MIXED
    XML_CTYPE_NAME				XML_CTYPE_CHOICE
    XML_CTYPE_SEQ				XML_CQUANT_NONE
    XML_CQUANT_OPT				XML_CQUANT_REP
    XML_CQUANT_PLUS				XML_INITIALIZED
    XML_PARSING					XML_FINISHED
    XML_SUSPENDED				XML_PARAM_ENTITY_PARSING_NEVER
    XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE	XML_PARAM_ENTITY_PARSING_ALWAYS
    XML_MAJOR_VERSION				XML_MINOR_VERSION
    XML_MICRO_VERSION				XML_FEATURE_END
    XML_FEATURE_UNICODE				XML_FEATURE_UNICODE_WCHAR_T
    XML_FEATURE_DTD				XML_FEATURE_CONTEXT_BYTES
    XML_FEATURE_MIN_SIZE			XML_FEATURE_SIZEOF_XML_CHAR
    XML_FEATURE_SIZEOF_XML_LCHAR		XML_FEATURE_NS
    XML_FEATURE_LARGE_SIZE)
  (import (rnrs)
    (foreign ffi)
    (foreign xml expat primitives)
    (foreign xml expat sizeof)))

;;; end of file
