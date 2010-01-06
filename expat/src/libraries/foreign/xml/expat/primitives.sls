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

    (rename
     (platform:XML_SetElementDeclHandler		xml-set-element-decl-handler)
     (platform:XML_SetAttlistDeclHandler		xml-set-attlist-decl-handler)
     (platform:XML_SetXmlDeclHandler			xml-set-xml-decl-handler)
     (platform:XML_ParserCreate				xml-parser-create)
     (platform:XML_ParserCreateNS			xml-parser-create-ns)
     (platform:XML_ParserCreate_MM			xml-parser-create-mm)
     (platform:XML_ParserReset				xml-parser-reset)
     (platform:XML_SetEntityDeclHandler			xml-set-entity-decl-handler)
     (platform:XML_SetElementHandler			xml-set-element-handler)
     (platform:XML_SetStartElementHandler		xml-set-start-element-handler)
     (platform:XML_SetEndElementHandler			xml-set-end-element-handler)
     (platform:XML_SetCharacterDataHandler		xml-set-character-data-handler)
     (platform:XML_SetProcessingInstructionHandler	xml-set-processing-instruction-handler)
     (platform:XML_SetCommentHandler			xml-set-comment-handler)
     (platform:XML_SetCdataSectionHandler		xml-set-cdata-section-handler)
     (platform:XML_SetStartCdataSectionHandler		xml-set-start-cdata-section-handler)
     (platform:XML_SetEndCdataSectionHandler		xml-set-end-cdata-section-handler)
     (platform:XML_SetDefaultHandler			xml-set-default-handler)
     (platform:XML_SetDefaultHandlerExpand		xml-set-default-handler-expand)
     (platform:XML_SetDoctypeDeclHandler		xml-set-doctype-decl-handler)
     (platform:XML_SetStartDoctypeDeclHandler		xml-set-start-doctype-decl-handler)
     (platform:XML_SetEndDoctypeDeclHandler		xml-set-end-doctype-decl-handler)
     (platform:XML_SetUnparsedEntityDeclHandler		xml-set-unparsed-entity-decl-handler)
     (platform:XML_SetNotationDeclHandler		xml-set-notation-decl-handler)
     (platform:XML_SetNamespaceDeclHandler		xml-set-namespace-decl-handler)
     (platform:XML_SetStartNamespaceDeclHandler		xml-set-start-namespace-decl-handler)
     (platform:XML_SetEndNamespaceDeclHandler		xml-set-end-namespace-decl-handler)
     (platform:XML_SetNotStandaloneHandler		xml-set-not-standalone-handler)
     (platform:XML_SetExternalEntityRefHandler		xml-set-external-entity-ref-handler)
     (platform:XML_SetExternalEntityRefHandlerArg	xml-set-external-entity-ref-handler-arg)
     (platform:XML_SetSkippedEntityHandler		xml-set-skipped-entity-handler)
     (platform:XML_SetUnknownEncodingHandler		xml-set-unknown-encoding-handler)
     (platform:XML_DefaultCurrent			xml-default-current)
     (platform:XML_SetReturnNSTriplet			xml-set-return-ns-triplet)
     (platform:XML_SetUserData				xml-set-user-data)
     (platform:XML_SetEncoding				xml-set-encoding)
     (platform:XML_UseParserAsHandlerArg		xml-use-parser-as-handler-arg)
     (platform:XML_UseForeignDTD			xml-use-foreign-dtd)
     (platform:XML_SetBase				xml-set-base)
     (platform:XML_GetBase				xml-get-base)
     (platform:XML_GetSpecifiedAttributeCount		xml-get-specified-attribute-count)
     (platform:XML_GetIdAttributeIndex			xml-get-id-attribute-index)
     (platform:XML_Parse				xml-parse)
     (platform:XML_GetBuffer				xml-get-buffer)
     (platform:XML_ParseBuffer				xml-parse-buffer)
     (platform:XML_StopParser				xml-stop-parser)
     (platform:XML_ResumeParser				xml-resume-parser)
     (platform:XML_GetParsingStatus			xml-get-parsing-status)
     (platform:XML_ExternalEntityParserCreate		xml-external-entity-parser-create)
     (platform:XML_SetParamEntityParsing		xml-set-param-entity-parsing)
     (platform:XML_GetErrorCode				xml-get-error-code)
     (platform:XML_GetCurrentLineNumber			xml-get-current-line-number)
     (platform:XML_GetCurrentColumnNumber		xml-get-current-column-number)
     (platform:XML_GetCurrentByteIndex			xml-get-current-byte-index)
     (platform:XML_GetCurrentByteCount			xml-get-current-byte-count)
     (platform:XML_GetInputContext			xml-get-input-context)
     (platform:XML_FreeContentModel			xml-free-content-model)
     (platform:XML_MemMalloc				xml-mem-malloc)
     (platform:XML_MemRealloc				xml-mem-realloc)
     (platform:XML_MemFree				xml-mem-free)
     (platform:XML_ParserFree				xml-parser-free)
     (platform:XML_ErrorString				xml-error-string)
     (platform:XML_ExpatVersion				xml-expat-version)
     (platform:XML_GetFeatureList    			xml-get-feature-list))
    )
  (import (rnrs)
    (prefix (foreign xml expat platform) platform:)
    (foreign xml expat sizeof))


;;;; code



;;;; done

)

;;; end of file
