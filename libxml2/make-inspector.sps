;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Libxml2
;;;Contents: foreign library inspection generator
;;;Date: Thu Dec 10, 2009
;;;
;;;Abstract
;;;
;;;	Some header  files from Libxml2 were not  processed because they
;;;	appeared  to export  unfinished or  private APIs.   If  you find
;;;	something missing, email the Nausicaa/Libxml2 maintainer.
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
  (foreign ffi inspector-maker))

(define-syntax define-c-callback-pointer-type
  (syntax-rules ()
    ((_ ?name)
     (define-c-type-alias ?name pointer))))


;;;; basic

(define-c-type-alias xmlChar*			pointer)
(define-c-type-alias xmlChar**			pointer)


;;;; interface for all global variables of the library
;;
;;Header file "globals.h".
;;

(define-c-type-alias xmlGlobalState*		pointer)
(define-c-type-alias xmlGlobalStatePtr		pointer)

(define-c-type-alias xmlParserInputBufferCreateFilenameFunc	pointer)
(define-c-type-alias xmlOutputBufferCreateFilenameFunc		pointer)
(define-c-type-alias xmlRegisterNodeFunc			pointer)
(define-c-type-alias xmlDeregisterNodeFunc			pointer)

;;This structure is excluded  because "globals.h" defines C preprocessor
;;macros with names  matching the names of the  fields, so inspection of
;;the fields always fails.
;;
;; (define-c-struct xmlGlobalState
;;   "struct _xmlGlobalState"
;;   (pointer		xmlParserVersion)
;;   (embedded		xmlDefaultSAXLocator)
;;   (embedded		xmlDefaultSAXHandler)
;;   (embedded		docbDefaultSAXHandler)
;;   (embedded		htmlDefaultSAXHandler)
;;   (pointer		xmlFree)
;;   (pointer		xmlMalloc)
;;   (pointer		xmlMemStrdup)
;;   (pointer		xmlRealloc)
;;   (pointer		xmlGenericError)
;;   (pointer		xmlStructuredError)
;;   (pointer		xmlGenericErrorContext)
;;   (signed-int		oldXMLWDcompatibility)
;;   (signed-int		xmlBufferAllocScheme)
;;   (signed-int		xmlDefaultBufferSize)
;;   (singed-int		xmlSubstituteEntitiesDefaultValue)
;;   (signed-int		xmlDoValidityCheckingDefaultValue)
;;   (signed-int		xmlGetWarningsDefaultValue)
;;   (signed-int		xmlKeepBlanksDefaultValue)
;;   (signed-int		xmlLineNumbersDefaultValue)
;;   (signed-int		xmlLoadExtDtdDefaultValue)
;;   (signed-int		xmlParserDebugEntities)
;;   (signed-int		xmlPedanticParserDefaultValue)
;;   (signed-int		xmlSaveNoEmptyTags)
;;   (signed-int		xmlIndentTreeOutput)
;;   (pointer		xmlTreeIndentString)
;;   (pointer		xmlRegisterNodeDefaultValue)
;;   (pointer		xmlDeregisterNodeDefaultValue)
;;   (pointer		xmlMallocAtomic)
;;   (embedded		xmlLastError)
;;   (pointer		xmlParserInputBufferCreateFilenameValue)
;;   (pointer		xmlOutputBufferCreateFilenameValue)
;;   (pointer		xmlStructuredErrorContext))


;;;; Canonical XML and Exclusive XML Canonicalization
;;
;; Header file "c14.h".
;;

(define-c-enumeration xmlC14NMode
  "xmlC14NMode"
  XML_C14N_1_0
  XML_C14N_EXCLUSIVE_1_0
  XML_C14N_1_1)

(define-c-type-alias xmlC14NIsVisibleCallback		callback)


;;;; Interfaces to the Catalog handling system
;;
;;Header file "catalog.h".
;;

(define-c-string-defines "catalog strings"
  XML_CATALOGS_NAMESPACE
  XML_CATALOG_PI)

(define-c-enumeration xmlCatalogPrefer
  "xmlCatalogPrefer"
  XML_CATA_PREFER_NONE
  XML_CATA_PREFER_PUBLIC
  XML_CATA_PREFER_SYSTEM)

(define-c-enumeration xmlCatalogAllow
  "xmlCatalogAllow"
  XML_CATA_ALLOW_NONE
  XML_CATA_ALLOW_GLOBAL
  XML_CATA_ALLOW_DOCUMENT
  XML_CATA_ALLOW_ALL)

(define-c-type-alias xmlCatalogPtr		pointer)
(define-c-type-alias xmlCatalog*		pointer)


;;;; Unicode character range checking
;;
;;Header file "chvalid.h".
;;

(define-c-type-alias xmlChSRange*		pointer)
(define-c-type-alias xmlChSRangePtr		pointer)

(define-c-struct xmlChSRange
  "xmlChSRange"
  (unsigned-int		low)
  (unsigned-int		high))

(define-c-type-alias xmlChLRange*		pointer)
(define-c-type-alias xmlChLRangePtr		pointer)

(define-c-struct xmlChLRange
  "xmlChLRange"
  (unsigned-int			low)
  (unsigned-int			high))

(define-c-type-alias xmlChRangeGroupPtr		pointer)
(define-c-type-alias xmlChRangeGroup*		pointer)

(define-c-struct xmlChRangeGroup
  "xmlChRangeGroup"
  (signed-int			nbShortRange)
  (signed-int			nbLongRange)
  (pointer			shortRange)
  (pointer			longRange))



;;;; Tree debugging APIs
;;
;;Header file "debugXML.h".
;;


(define-c-type-alias xmlShellReadlineFunc	pointer)

(define-c-type-alias xmlShellCtxt*		pointer)
(define-c-type-alias xmlShellCtxtPtr		pointer)

(define-c-struct xmlShellCtxt
  "xmlShellCtxt"
  (pointer			filename)
  (pointer			doc)
  (pointer			node)
  (pointer			pctxt)
  (signed-int			loaded)
  (pointer			output)
  (pointer			input))

(define-c-type-alias xmlShellCmd		pointer)


;;;; string dictionnary
;;
;;Header file "dict.h".
;;

(define-c-type-alias xmlDict*		pointer)
(define-c-type-alias xmlDictPtr		pointer)



;;;; interface for the encoding conversion functions
;;
;;Header file "encoding.h"
;;

(define-c-type-alias xmlCharEncodingInputFunc		pointer)
(define-c-type-alias xmlCharEncodingOutputFunc		pointer)

(define-c-enumeration xmlCharEncoding
  "xmlCharEncoding"
  XML_CHAR_ENCODING_ERROR
  XML_CHAR_ENCODING_NONE
  XML_CHAR_ENCODING_UTF8
  XML_CHAR_ENCODING_UTF16LE
  XML_CHAR_ENCODING_UTF16BE
  XML_CHAR_ENCODING_UCS4LE
  XML_CHAR_ENCODING_UCS4BE
  XML_CHAR_ENCODING_EBCDIC
  XML_CHAR_ENCODING_UCS4_2143
  XML_CHAR_ENCODING_UCS4_3412
  XML_CHAR_ENCODING_UCS2
  XML_CHAR_ENCODING_8859_1
  XML_CHAR_ENCODING_8859_2
  XML_CHAR_ENCODING_8859_3
  XML_CHAR_ENCODING_8859_4
  XML_CHAR_ENCODING_8859_5
  XML_CHAR_ENCODING_8859_6
  XML_CHAR_ENCODING_8859_7
  XML_CHAR_ENCODING_8859_8
  XML_CHAR_ENCODING_8859_9
  XML_CHAR_ENCODING_2022_JP
  XML_CHAR_ENCODING_SHIFT_JIS
  XML_CHAR_ENCODING_EUC_JP
  XML_CHAR_ENCODING_ASCII)

(define-c-type-alias xmlCharEncodingHandler*		pointer)
(define-c-type-alias xmlCharEncodingHandlerPtr		pointer)

(define-c-struct xmlCharEncodingHandler
  "xmlCharEncodingHandler"
  (pointer			name)
  (pointer			input)
  (pointer			output)
  (pointer			iconv_in)
  (pointer			iconv_out))



;;;; interface for the XML entities handling
;;
;;Header file "entities.h".
;;

(define-c-enumeration xmlEntityType
  "xmlEntityType"
  XML_INTERNAL_GENERAL_ENTITY
  XML_EXTERNAL_GENERAL_PARSED_ENTITY
  XML_EXTERNAL_GENERAL_UNPARSED_ENTITY
  XML_INTERNAL_PARAMETER_ENTITY
  XML_EXTERNAL_PARAMETER_ENTITY
  XML_INTERNAL_PREDEFINED_ENTITY)

(define-c-struct _xmlEntity
  "struct _xmlEntity"
  (pointer		_private)
  (signed-int		type)
  (pointer		name)
  (pointer		children)
  (pointer		last)
  (pointer		parent)
  (pointer		next)
  (pointer		prev)
  (pointer		doc)

  (pointer		orig)
  (pointer		content)
  (signed-int		length)
  (signed-int		etype)
  (pointer		ExternalID)
  (pointer		SystemID)

  (pointer		nexte)
  (pointer		URI)
  (signed-int		owner)
  (signed-int		checked))

(define-c-type-alias xmlEntitiesTable*		pointer)
(define-c-type-alias xmlEntitiesTablePtr	pointer)


;;;; Chained hash tables
;;
;;Header file "hash.c"
;;

(define-c-type-alias xmlHashTable*		pointer)
(define-c-type-alias xmlHashTablePtr		pointer)

(define-c-type-alias xmlHashDeallocator		pointer)
(define-c-type-alias xmlHashCopier		pointer)
(define-c-type-alias xmlHashScanner		pointer)
(define-c-type-alias xmlHashScannerFull		pointer)


;;;; specific APIs to process HTML tree, especially serialization
;;
;;Header file "HTMLtree.h".
;;

(define-c-defines "HTML tree symbols"
  HTML_TEXT_NODE
  HTML_ENTITY_REF_NODE
  HTML_COMMENT_NODE
  HTML_PRESERVE_NODE
  HTML_PI_NODE)


;;;; HTML parser
;;
;;Non-validating real-world HTML parser.  Header file "HTMLparser.h".
;;

(define-c-type-alias htmlDocPtr			pointer)
(define-c-type-alias htmlNodePtr		pointer)
(define-c-type-alias htmlParserCtxtPtr		pointer)
(define-c-type-alias htmlSAXHandlerPtr		pointer)
(define-c-type-alias htmlParserInputPtr		pointer)

(define-c-type-alias htmlElemDescPtr		pointer)
(define-c-type-alias htmlElemDesc*		pointer)

(define-c-type-alias htmlEntityDescPtr		pointer)
(define-c-type-alias htmlEntityDesc*		pointer)

;;Internal description  of an HTML  element, representing HTML  4.01 and
;;XHTML 1.0 (which share the same structure).
;;
(define-c-struct htmlElemDesc
  "struct _htmlElemDesc"
  (pointer		name)
  (signed-int		startTag)
  (signed-int		endTag)
  (signed-int		saveEndTag)
  (signed-int		empty)
  (signed-int		depr)
  (signed-int		dtd)
  (signed-int		isinline)
  (pointer		desc)
  (pointer		subelts)
  (pointer		defaultsubelt)
  (pointer		attrs_opt)
  (pointer		attrs_depr)
  (pointer		attrs_req))

;;Internal description of an HTML entity.
;;
(define-c-struct htmlEntityDesc
  "htmlEntityDesc"
  (unsigned-int		value)
  (pointer		name)
  (pointer		desc))

(define-c-enumeration htmlParserOption
  "htmlParserOption"
  HTML_PARSE_RECOVER
  HTML_PARSE_NOERROR
  HTML_PARSE_NOWARNING
  HTML_PARSE_PEDANTIC
  HTML_PARSE_NOBLANKS
  HTML_PARSE_NONET
  HTML_PARSE_COMPACT)

(define-c-enumeration htmlStatus
  "htmlStatus"
  HTML_NA
  HTML_INVALID
  HTML_DEPRECATED
  HTML_VALID
  HTML_REQUIRED)


;;;; lists interfaces
;;
;;Header file "list.h".
;;

(define-c-type-alias xmlLink*			pointer)
(define-c-type-alias xmlLinkPtr			pointer)

(define-c-type-alias xmlList*			pointer)
(define-c-type-alias xmlListPtr			pointer)

(define-c-type-alias xmlListDeallocator		pointer)
(define-c-type-alias xmlListDataCompare		pointer)
(define-c-type-alias xmlListWalker		pointer)


;;;; minimal FTP implementation
;;
;;Header file "nanoftp.h".
;;

(define-c-type-alias ftpListCallback		pointer)
(define-c-type-alias ftpDataCallback		pointer)



;;;; minimal HTTP implementation
;;
;;Header file "nanohttp.h".
;;


;;;; the core parser module
;;
;;Header file "parser.h".
;;

(define-c-string-defines "XML version"
  XML_DEFAULT_VERSION)

(define-c-struct _xmlParserInput
  "struct _xmlParserInput"
  (pointer		buf)
  (pointer		filename)
  (pointer		directory)
  (pointer		base)
  (pointer		cur)
  (pointer		end)
  (signed-int		length)
  (signed-int		line)
  (signed-int		col)
  (unsigned-int	consumed)
  (pointer		free)
  (pointer		encoding)
  (pointer		version)
  (signed-int		standalone)
  (signed-int		id))

(define-c-type-alias xmlParserNodeInfo*		pointer)
(define-c-type-alias xmlParserNodeInfoPtr	pointer)

(define-c-struct xmlParserNodeInfo
  "xmlParserNodeInfo"
  (pointer		node)
  (unsigned-int	begin_pos)
  (unsigned-int	begin_line)
  (unsigned-int	end_pos)
  (unsigned-int	end_line))

(define-c-type-alias xmlParserNodeInfoSeq*	pointer)
(define-c-type-alias xmlParserNodeInfoSeqPtr	pointer)

(define-c-struct xmlParserNodeInfoSeq
  "xmlParserNodeInfoSeq"
  (unsigned-int	maximum)
  (unsigned-int	length)
  (pointer		buffer))

(define-c-enumeration xmlParserInputState
  "xmlParserInputState"
  XML_PARSER_EOF
  XML_PARSER_START
  XML_PARSER_MISC
  XML_PARSER_PI
  XML_PARSER_DTD
  XML_PARSER_PROLOG
  XML_PARSER_COMMENT
  XML_PARSER_START_TAG
  XML_PARSER_CONTENT
  XML_PARSER_CDATA_SECTION
  XML_PARSER_END_TAG
  XML_PARSER_ENTITY_DECL
  XML_PARSER_ENTITY_VALUE
  XML_PARSER_ATTRIBUTE_VALUE
  XML_PARSER_SYSTEM_LITERAL
  XML_PARSER_EPILOG
  XML_PARSER_IGNORE
  XML_PARSER_PUBLIC_LITERAL)

(define-c-defines "core parser constants"
  XML_DETECT_IDS
  XML_COMPLETE_ATTRS
  XML_SKIP_IDS)

(define-c-enumeration xmlParserMode
  "xmlParserMode"
  XML_PARSE_UNKNOWN
  XML_PARSE_DOM
  XML_PARSE_SAX
  XML_PARSE_PUSH_DOM
  XML_PARSE_PUSH_SAX
  XML_PARSE_READER)

(define-c-struct _xmlParserCtxt
  "struct _xmlParserCtxt"
  (pointer		sax)
  (pointer		userData)
  (pointer		myDoc)
  (signed-int		wellFormed)
  (signed-int		replaceEntities)
  (pointer		version)
  (pointer		encoding)
  (signed-int		standalone)
  (signed-int		html)
  (pointer		input)
  (signed-int		inputNr)
  (signed-int		inputMax)
  (pointer		inputTab)
  (pointer		node)
  (signed-int		nodeNr)
  (signed-int		nodeMax)
  (pointer		nodeTab)
  (signed-int		record_info)
  (embedded		node_seq)
  (signed-int		errNo)
  (signed-int		hasExternalSubset)
  (signed-int		hasPErefs)
  (signed-int		external)
  (signed-int		valid)
  (signed-int		validate)
  (embedded		vctxt)
  (embedded		instate)
  (signed-int		token)
  (pointer		directory)
  (pointer		name)
  (signed-int		nameNr)
  (signed-int		nameMax)
  (pointer		nameTab)
  (signed-int		nbChars)
  (signed-int		checkIndex)
  (signed-int		keepBlanks)
  (signed-int		disableSAX)
  (signed-int		inSubset)
  (pointer		intSubName)
  (pointer		extSubURI)
  (pointer		extSubSystem)
  (pointer		space)
  (signed-int		spaceNr)
  (signed-int		spaceMax)
  (pointer		spaceTab)
  (signed-int		depth)
  (pointer		entity)
  (signed-int		charset)
  (signed-int		nodelen)
  (signed-int		nodemem)
  (signed-int		pedantic)
  (pointer		_private)
  (signed-int		loadsubset)
  (signed-int		linenumbers)
  (pointer		catalogs)
  (signed-int		recovery)
  (signed-int		progressive)
  (pointer		dict)
  (pointer		atts)
  (signed-int		maxatts)
  (signed-int		docdict)
  (pointer		str_xml)
  (pointer		str_xmlns)
  (pointer		str_xml_ns)
  (signed-int           sax2)
  (signed-int		nsNr)
  (signed-int		nsMax)
  (pointer		nsTab)
  (pointer		attallocs)
  (pointer		pushTab)
  (pointer		attsDefault)
  (pointer		attsSpecial)
  (signed-int		nsWellFormed)
  (signed-int		options)
  (signed-int		dictNames)
  (signed-int		freeElemsNr)
  (pointer		freeElems)
  (signed-int		freeAttrsNr)
  (pointer		freeAttrs)
  (embedded		lastError)
  (embedded		xmlParserMode)
  (unsigned-int		nbentities)
  (unsigned-int		sizeentities))

(define-c-struct _xmlSAXLocator
  "struct _xmlSAXLocator"
  (pointer			getPublicId)
  (pointer			getSystemId)
  (pointer			getLineNumber)
  (pointer			getColumnNumber))


(define-c-defines "Special constant found in SAX2 blocks initialized fields"
  XML_SAX2_MAGIC)

;; struct _xmlSAXHandler {
;;     internalSubsetSAXFunc internalSubset;
;;     isStandaloneSAXFunc isStandalone;
;;     hasInternalSubsetSAXFunc hasInternalSubset;
;;     hasExternalSubsetSAXFunc hasExternalSubset;
;;     resolveEntitySAXFunc resolveEntity;
;;     getEntitySAXFunc getEntity;
;;     entityDeclSAXFunc entityDecl;
;;     notationDeclSAXFunc notationDecl;
;;     attributeDeclSAXFunc attributeDecl;
;;     elementDeclSAXFunc elementDecl;
;;     unparsedEntityDeclSAXFunc unparsedEntityDecl;
;;     setDocumentLocatorSAXFunc setDocumentLocator;
;;     startDocumentSAXFunc startDocument;
;;     endDocumentSAXFunc endDocument;
;;     startElementSAXFunc startElement;
;;     endElementSAXFunc endElement;
;;     referenceSAXFunc reference;
;;     charactersSAXFunc characters;
;;     ignorableWhitespaceSAXFunc ignorableWhitespace;
;;     processingInstructionSAXFunc processingInstruction;
;;     commentSAXFunc comment;
;;     warningSAXFunc warning;
;;     errorSAXFunc error;
;;     fatalErrorSAXFunc fatalError; /* unused error() get all the errors */
;;     getParameterEntitySAXFunc getParameterEntity;
;;     cdataBlockSAXFunc cdataBlock;
;;     externalSubsetSAXFunc externalSubset;
;;     unsigned int initialized;
;;     /* The following fields are extensions available only on version 2 */
;;     void *_private;
;;     startElementNsSAX2Func startElementNs;
;;     endElementNsSAX2Func endElementNs;
;;     xmlStructuredErrorFunc serror;
;; };

(define-c-type-alias xmlSAXHandlerV1*		pointer)
(define-c-type-alias xmlSAXHandlerV1Ptr		pointer)

;; struct _xmlSAXHandlerV1 {
;;     internalSubsetSAXFunc internalSubset;
;;     isStandaloneSAXFunc isStandalone;
;;     hasInternalSubsetSAXFunc hasInternalSubset;
;;     hasExternalSubsetSAXFunc hasExternalSubset;
;;     resolveEntitySAXFunc resolveEntity;
;;     getEntitySAXFunc getEntity;
;;     entityDeclSAXFunc entityDecl;
;;     notationDeclSAXFunc notationDecl;
;;     attributeDeclSAXFunc attributeDecl;
;;     elementDeclSAXFunc elementDecl;
;;     unparsedEntityDeclSAXFunc unparsedEntityDecl;
;;     setDocumentLocatorSAXFunc setDocumentLocator;
;;     startDocumentSAXFunc startDocument;
;;     endDocumentSAXFunc endDocument;
;;     startElementSAXFunc startElement;
;;     endElementSAXFunc endElement;
;;     referenceSAXFunc reference;
;;     charactersSAXFunc characters;
;;     ignorableWhitespaceSAXFunc ignorableWhitespace;
;;     processingInstructionSAXFunc processingInstruction;
;;     commentSAXFunc comment;
;;     warningSAXFunc warning;
;;     errorSAXFunc error;
;;     fatalErrorSAXFunc fatalError; /* unused error() get all the errors */
;;     getParameterEntitySAXFunc getParameterEntity;
;;     cdataBlockSAXFunc cdataBlock;
;;     externalSubsetSAXFunc externalSubset;
;;     unsigned int initialized;
;; };


(define-c-enumeration xmlParserOption
  "xmlParserOption"
  XML_PARSE_RECOVER
  XML_PARSE_NOENT
  XML_PARSE_DTDLOAD
  XML_PARSE_DTDATTR
  XML_PARSE_DTDVALID
  XML_PARSE_NOERROR
  XML_PARSE_NOWARNING
  XML_PARSE_PEDANTIC
  XML_PARSE_NOBLANKS
  XML_PARSE_SAX1
  XML_PARSE_XINCLUDE
  XML_PARSE_NONET
  XML_PARSE_NODICT
  XML_PARSE_NSCLEAN
  XML_PARSE_NOCDATA
  XML_PARSE_NOXINCNODE
  XML_PARSE_COMPACT
  XML_PARSE_OLD10
  XML_PARSE_NOBASEFIX
  XML_PARSE_HUGE
  XML_PARSE_OLDSAX)

(define-c-enumeration xmlFeature
  "xmlFeature"
  XML_WITH_THREAD
  XML_WITH_TREE
  XML_WITH_OUTPUT
  XML_WITH_PUSH
  XML_WITH_READER
  XML_WITH_PATTERN
  XML_WITH_WRITER
  XML_WITH_SAX1
  XML_WITH_FTP
  XML_WITH_HTTP
  XML_WITH_VALID
  XML_WITH_HTML
  XML_WITH_LEGACY
  XML_WITH_C14N
  XML_WITH_CATALOG
  XML_WITH_XPATH
  XML_WITH_XPTR
  XML_WITH_XINCLUDE
  XML_WITH_ICONV
  XML_WITH_ISO8859X
  XML_WITH_UNICODE
  XML_WITH_REGEXP
  XML_WITH_AUTOMATA
  XML_WITH_EXPR
  XML_WITH_SCHEMAS
  XML_WITH_SCHEMATRON
  XML_WITH_MODULES
  XML_WITH_DEBUG
  XML_WITH_DEBUG_MEM
  XML_WITH_DEBUG_RUN
  XML_WITH_ZLIB
  XML_WITH_NONE)

(define-c-callback-pointer-type xmlParserInputDeallocate)
(define-c-callback-pointer-type resolveEntitySAXFunc)
(define-c-callback-pointer-type internalSubsetSAXFunc)
(define-c-callback-pointer-type externalSubsetSAXFunc)
(define-c-callback-pointer-type getEntitySAXFunc)
(define-c-callback-pointer-type getParameterEntitySAXFunc)
(define-c-callback-pointer-type entityDeclSAXFunc)
(define-c-callback-pointer-type notationDeclSAXFunc)
(define-c-callback-pointer-type attributeDeclSAXFunc)
(define-c-callback-pointer-type elementDeclSAXFunc)
(define-c-callback-pointer-type unparsedEntityDeclSAXFunc)
(define-c-callback-pointer-type setDocumentLocatorSAXFunc)
(define-c-callback-pointer-type startDocumentSAXFunc)
(define-c-callback-pointer-type endDocumentSAXFunc)
(define-c-callback-pointer-type startElementSAXFunc)
(define-c-callback-pointer-type endElementSAXFunc)
(define-c-callback-pointer-type attributeSAXFunc)
(define-c-callback-pointer-type referenceSAXFunc)
(define-c-callback-pointer-type charactersSAXFunc)
(define-c-callback-pointer-type ignorableWhitespaceSAXFunc)
(define-c-callback-pointer-type processingInstructionSAXFunc)
(define-c-callback-pointer-type commentSAXFunc)
(define-c-callback-pointer-type cdataBlockSAXFunc)
(define-c-callback-pointer-type warningSAXFunc)
(define-c-callback-pointer-type errorSAXFunc)
(define-c-callback-pointer-type fatalErrorSAXFunc)
(define-c-callback-pointer-type isStandaloneSAXFunc)
(define-c-callback-pointer-type hasInternalSubsetSAXFunc)
(define-c-callback-pointer-type hasExternalSubsetSAXFunc)
(define-c-callback-pointer-type startElementNsSAX2Func)
(define-c-callback-pointer-type endElementNsSAX2Func)
(define-c-callback-pointer-type xmlExternalEntityLoader)

(define-c-callback-pointer-type xmlEntityReferenceFunc)


;;;; pattern expression handling
;;
;;Header file "pattern.h"
;;

(define-c-type-alias xmlPattern*		pointer)
(define-c-type-alias xmlPatternPtr		pointer)

(define-c-enumeration xmlPatternFlags
  "xmlPatternFlags"
  XML_PATTERN_DEFAULT
  XML_PATTERN_XPATH
  XML_PATTERN_XSSEL
  XML_PATTERN_XSFIELD)

(define-c-type-alias xmlStreamCtxt*		pointer)
(define-c-type-alias xmlStreamCtxtPtr		pointer)


;;;; implementation of the Relax-NG validation
;;
;;Header file "relaxng.h"
;;

(define-c-type-alias xmlRelaxNG*		pointer)
(define-c-type-alias xmlRelaxNGPtr		pointer)

(define-c-type-alias xmlRelaxNGParserCtxt*	pointer)
(define-c-type-alias xmlRelaxNGParserCtxtPtr	pointer)

(define-c-type-alias xmlRelaxNGValidCtxt*	pointer)
(define-c-type-alias xmlRelaxNGValidCtxtPtr	pointer)

(define-c-callback-pointer-type xmlRelaxNGValidityErrorFunc)
(define-c-callback-pointer-type xmlRelaxNGValidityWarningFunc)

(define-c-enumeration xmlRelaxNGValidErr
  "xmlRelaxNGValidErr"
  XML_RELAXNG_OK
  XML_RELAXNG_ERR_MEMORY
  XML_RELAXNG_ERR_TYPE
  XML_RELAXNG_ERR_TYPEVAL
  XML_RELAXNG_ERR_DUPID
  XML_RELAXNG_ERR_TYPECMP
  XML_RELAXNG_ERR_NOSTATE
  XML_RELAXNG_ERR_NODEFINE
  XML_RELAXNG_ERR_LISTEXTRA
  XML_RELAXNG_ERR_LISTEMPTY
  XML_RELAXNG_ERR_INTERNODATA
  XML_RELAXNG_ERR_INTERSEQ
  XML_RELAXNG_ERR_INTEREXTRA
  XML_RELAXNG_ERR_ELEMNAME
  XML_RELAXNG_ERR_ATTRNAME
  XML_RELAXNG_ERR_ELEMNONS
  XML_RELAXNG_ERR_ATTRNONS
  XML_RELAXNG_ERR_ELEMWRONGNS
  XML_RELAXNG_ERR_ATTRWRONGNS
  XML_RELAXNG_ERR_ELEMEXTRANS
  XML_RELAXNG_ERR_ATTREXTRANS
  XML_RELAXNG_ERR_ELEMNOTEMPTY
  XML_RELAXNG_ERR_NOELEM
  XML_RELAXNG_ERR_NOTELEM
  XML_RELAXNG_ERR_ATTRVALID
  XML_RELAXNG_ERR_CONTENTVALID
  XML_RELAXNG_ERR_EXTRACONTENT
  XML_RELAXNG_ERR_INVALIDATTR
  XML_RELAXNG_ERR_DATAELEM
  XML_RELAXNG_ERR_VALELEM
  XML_RELAXNG_ERR_LISTELEM
  XML_RELAXNG_ERR_DATATYPE
  XML_RELAXNG_ERR_VALUE
  XML_RELAXNG_ERR_LIST
  XML_RELAXNG_ERR_NOGRAMMAR
  XML_RELAXNG_ERR_EXTRADATA
  XML_RELAXNG_ERR_LACKDATA
  XML_RELAXNG_ERR_INTERNAL
  XML_RELAXNG_ERR_ELEMWRONG
  XML_RELAXNG_ERR_TEXTWRONG)

(define-c-enumeration xmlRelaxNGParserFlag
  "xmlRelaxNGParserFlag"
  XML_RELAXNGP_NONE
  XML_RELAXNGP_FREE_DOC
  XML_RELAXNGP_CRNG)


;;;; SAX2 parser interface used to build the DOM tree
;;
;;Header file "SAX2.h".
;;


;;;; XML Schematron implementation
;;
;;Header file "schematron.h".
;;

(define-c-enumeration xmlSchematronValidOptions
  "xmlSchematronValidOptions"
  XML_SCHEMATRON_OUT_QUIET
  XML_SCHEMATRON_OUT_TEXT
  XML_SCHEMATRON_OUT_XML
  XML_SCHEMATRON_OUT_ERROR
  XML_SCHEMATRON_OUT_FILE
  XML_SCHEMATRON_OUT_BUFFER
  XML_SCHEMATRON_OUT_IO)

(define-c-type-alias xmlSchematron*		pointer)
(define-c-type-alias xmlSchematronPtr		pointer)

(define-c-type-alias xmlSchematronParserCtxt*	pointer)
(define-c-type-alias xmlSchematronParserCtxtPtr	pointer)

(define-c-type-alias xmlSchematronValidCtxt*	pointer)
(define-c-type-alias xmlSchematronValidCtxtPtr	pointer)


(define-c-callback-pointer-type xmlSchematronValidityErrorFunc)
(define-c-callback-pointer-type xmlSchematronValidityWarningFunc)


;;;; interfaces for thread handling
;;
;;Header file "thread.h".
;;

(define-c-type-alias xmlMutex*			pointer)
(define-c-type-alias xmlMutexPtr		pointer)

(define-c-type-alias xmlRMutex*			pointer)
(define-c-type-alias xmlRMutexPtr		pointer)


;;;; interfaces for tree manipulation
;;
;;Header file "tree.h".
;;

(define-c-type-alias xmlParserInputBuffer*		pointer)
(define-c-type-alias xmlParserInputBufferPtr		pointer)

(define-c-type-alias xmlOutputBuffer*			pointer)
(define-c-type-alias xmlOutputBufferPtr			pointer)

(define-c-type-alias xmlParserInput*			pointer)
(define-c-type-alias xmlParserInputPtr			pointer)

(define-c-type-alias xmlParserCtxt*			pointer)
(define-c-type-alias xmlParserCtxtPtr			pointer)

(define-c-type-alias xmlSAXLocator*			pointer)
(define-c-type-alias xmlSAXLocatorPtr			pointer)

(define-c-type-alias xmlSAXHandler*			pointer)
(define-c-type-alias xmlSAXHandlerPtr			pointer)

(define-c-type-alias xmlEntity*				pointer)
(define-c-type-alias xmlEntityPtr			pointer)

(define-c-callback-pointer-type xmlDOMWrapAcquireNsFunction)

(define-c-defines "tree constants"
  BASE_BUFFER_SIZE)

(define-c-enumeration xmlBufferAllocationScheme
  "xmlBufferAllocationScheme"
  XML_BUFFER_ALLOC_DOUBLEIT
  XML_BUFFER_ALLOC_EXACT
  XML_BUFFER_ALLOC_IMMUTABLE
  XML_BUFFER_ALLOC_IO)

(define-c-type-alias xmlBuffer*				pointer)
(define-c-type-alias xmlBufferPtr			pointer)

(define-c-struct xmlBuffer
  "xmlBuffer"
  (pointer			content)
  (unsigned-int			use)
  (unsigned-int			size)
  (embedded			alloc)
  (pointer			contentIO))

(define-c-string-defines "tree strings"
  XML_XML_NAMESPACE
  XML_XML_ID)

(define-c-enumeration xmlElementType
  "xmlElementType"
  XML_ELEMENT_NODE
  XML_ATTRIBUTE_NODE
  XML_TEXT_NODE
  XML_CDATA_SECTION_NODE
  XML_ENTITY_REF_NODE
  XML_ENTITY_NODE
  XML_PI_NODE
  XML_COMMENT_NODE
  XML_DOCUMENT_NODE
  XML_DOCUMENT_TYPE_NODE
  XML_DOCUMENT_FRAG_NODE
  XML_NOTATION_NODE
  XML_HTML_DOCUMENT_NODE
  XML_DTD_NODE
  XML_ELEMENT_DECL
  XML_ATTRIBUTE_DECL
  XML_ENTITY_DECL
  XML_NAMESPACE_DECL
  XML_XINCLUDE_START
  XML_XINCLUDE_END
  XML_DOCB_DOCUMENT_NODE)

(define-c-type-alias xmlNotation*		pointer)
(define-c-type-alias xmlNotationPtr		pointer)

(define-c-struct xmlNotation
  "xmlNotation"
  (pointer			name)
  (pointer			PublicID)
  (pointer			SystemID))

(define-c-enumeration xmlAttributeType
  "xmlAttributeType"
  XML_ATTRIBUTE_CDATA
  XML_ATTRIBUTE_ID
  XML_ATTRIBUTE_IDREF
  XML_ATTRIBUTE_IDREFS
  XML_ATTRIBUTE_ENTITY
  XML_ATTRIBUTE_ENTITIES
  XML_ATTRIBUTE_NMTOKEN
  XML_ATTRIBUTE_NMTOKENS
  XML_ATTRIBUTE_ENUMERATION
  XML_ATTRIBUTE_NOTATION)

(define-c-enumeration xmlAttributeDefault
  "xmlAttributeDefault"
  XML_ATTRIBUTE_NONE
  XML_ATTRIBUTE_REQUIRED
  XML_ATTRIBUTE_IMPLIED
  XML_ATTRIBUTE_FIXED)

(define-c-type-alias xmlEnumeration*		pointer)
(define-c-type-alias xmlEnumerationPtr		pointer)

(define-c-struct xmlEnumeration
  "xmlEnumeration"
  (pointer			next)
  (pointer			name))

(define-c-type-alias xmlAttribute*		pointer)
(define-c-type-alias xmlAttributePtr		pointer)

(define-c-struct xmlAttribute
  "xmlAttribute"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (pointer			nexth)
  (embedded			atype)
  (embedded			def)
  (pointer			defaultValue)
  (pointer			tree)
  (pointer			prefix)
  (pointer			elem))

(define-c-enumeration xmlElementContentType
  "xmlElementContentType"
  XML_ELEMENT_CONTENT_PCDATA
  XML_ELEMENT_CONTENT_ELEMENT
  XML_ELEMENT_CONTENT_SEQ
  XML_ELEMENT_CONTENT_OR)

(define-c-enumeration xmlElementContentOccur
  "xmlElementContentOccur"
  XML_ELEMENT_CONTENT_ONCE
  XML_ELEMENT_CONTENT_OP
  XML_ELEMENT_CONTENT_MULT
  XML_ELEMENT_CONTENT_PLUS)

(define-c-type-alias xmlElementContent*			pointer)
(define-c-type-alias xmlElementContentPtr		pointer)

(define-c-struct xmlElementContent
  "xmlElementContent"
  (embedded			type)
  (embedded			ocur)
  (pointer			name)
  (pointer			c1)
  (pointer			c2)
  (pointer			parent)
  (pointer			prefix))

(define-c-enumeration xmlElementTypeVal
  "xmlElementTypeVal"
  XML_ELEMENT_TYPE_UNDEFINED
  XML_ELEMENT_TYPE_EMPTY
  XML_ELEMENT_TYPE_ANY
  XML_ELEMENT_TYPE_MIXED
  XML_ELEMENT_TYPE_ELEMENT)

(define-c-type-alias xmlElement*			pointer)
(define-c-type-alias xmlElementPtr			pointer)

(define-c-struct xmlElement
  "xmlElement"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (embedded			etype)
  (pointer			content)
  (pointer			attributes)
  (pointer			prefix)
  (pointer			contModel))

(define-c-type-alias xmlNsType*		pointer)

(define-c-defines "a namespace declaration node"
  XML_LOCAL_NAMESPACE)

(define-c-type-alias xmlNs*		pointer)
(define-c-type-alias xmlNsPtr		pointer)

(define-c-struct xmlNs
  "xmlNs"
  (pointer			next)
  (embedded			type)
  (pointer			href)
  (pointer			prefix)
  (pointer			_private)
  (pointer			context))

(define-c-type-alias xmlDtd*		pointer)
(define-c-type-alias xmlDtdPtr		pointer)

(define-c-struct xmlDtd
  "xmlDtd"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (pointer			notations)
  (pointer			elements)
  (pointer			attributes)
  (pointer			entities)
  (pointer			ExternalID)
  (pointer			SystemID)
  (pointer			pentities))

(define-c-type-alias xmlAttr*		pointer)
(define-c-type-alias xmlAttrPtr		pointer)

(define-c-struct xmlAttr
  "xmlAttr"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (pointer			ns)
  (embedded			atype)
  (pointer			psvi))

(define-c-type-alias xmlID*		pointer)
(define-c-type-alias xmlIDPtr		pointer)

(define-c-struct xmlID
  "xmlID"
  (pointer			next)
  (pointer			value)
  (embedded			attr)
  (pointer			name)
  (signed-int			lineno)
  (pointer			doc))

(define-c-type-alias xmlRef*		pointer)
(define-c-type-alias xmlRefPtr		pointer)

(define-c-struct xmlRef
  "xmlRef"
  (pointer			next)
  (pointer			value)
  (embedded			attr)
  (pointer			name)
  (signed-int			lineno))

(define-c-type-alias xmlNode*		pointer)
(define-c-type-alias xmlNodePtr		pointer)
(define-c-type-alias xmlNodePtr*	pointer)

(define-c-struct xmlNode
  "xmlNode"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (pointer			ns)
  (pointer			content)
  (pointer			properties)
  (pointer			nsDef)
  (pointer			psvi)
  (unsigned-int			line)
  (unsigned-int			extra))

(define-c-enumeration xmlDocProperties
  "xmlDocProperties"
  XML_DOC_WELLFORMED
  XML_DOC_NSVALID
  XML_DOC_OLD10
  XML_DOC_DTDVALID
  XML_DOC_XINCLUDE
  XML_DOC_USERBUILT
  XML_DOC_INTERNAL
  XML_DOC_HTML)

(define-c-type-alias xmlDoc*		pointer)
(define-c-type-alias xmlDocPtr		pointer)
(define-c-type-alias xmlDocPtr*		pointer)

(define-c-struct xmlDoc
  "xmlDoc"
  (pointer			_private)
  (embedded			type)
  (pointer			name)
  (pointer			children)
  (pointer			last)
  (pointer			parent)
  (pointer			next)
  (pointer			prev)
  (pointer			doc)
  (signed-int			compression)
  (signed-int			standalone)
  (pointer			intSubset)
  (pointer			extSubset)
  (pointer			oldNs)
  (pointer			version)
  (pointer			encoding)
  (pointer			ids)
  (pointer			refs)
  (pointer			URL)
  (signed-int			charset)
  (pointer			dict)
  (pointer			psvi)
  (signed-int			parseFlags)
  (signed-int			properties))

(define-c-type-alias xmlDOMWrapCtxt*		pointer)
(define-c-type-alias xmlDOMWrapCtxtPtr		pointer)

(define-c-struct xmlDOMWrapCtxt
  "xmlDOMWrapCtxt"
  (pointer			_private)
  (signed-int			type)
  (pointer			namespaceMap)
  (pointer			getNsForNodeFunc))



;;;; library of generic URI related routines
;;
;;Header file "uri.h".
;;

(define-c-type-alias xmlURI*		pointer)
(define-c-type-alias xmlURIPtr		pointer)

(define-c-struct xmlURI
  "xmlURI"
  (pointer		scheme)
  (pointer		opaque)
  (pointer		authority)
  (pointer		server)
  (pointer		user)
  (signed-int		port)
  (pointer		path)
  (pointer		query)
  (pointer		fragment)
  (signed-int		cleanup)
  (pointer		query_raw))


;;;; The DTD validation
;;
;;Header file "valid.h".
;;

(define-c-callback-pointer-type xmlValidityErrorFunc)
(define-c-callback-pointer-type xmlValidityWarningFunc)

(define-c-type-alias xmlValidState*		pointer)
(define-c-type-alias xmlValidStatePtr		pointer)

(define-c-defines "DTD validation constants"
  XML_CTXT_FINISH_DTD_0
  XML_CTXT_FINISH_DTD_1)

(define-c-type-alias xmlValidCtxt*		pointer)
(define-c-type-alias xmlValidCtxtPtr		pointer)

(define-c-struct xmlValidCtxt
  "xmlValidCtxt"
  (pointer			userData)
  (pointer			error)
  (pointer			warning)
  (pointer			node)
  (signed-int			nodeNr)
  (signed-int			nodeMax)
  (pointer			nodeTab)
  (unsigned-int			finishDtd)
  (pointer			doc)
  (signed-int			valid)
  (pointer			vstate)
  (signed-int			vstateNr)
  (signed-int			vstateMax)
  (pointer			vstateTab)
  (pointer			am)
  (pointer			state))

(define-c-type-alias xmlNotationTable*		pointer)
(define-c-type-alias xmlNotationTablePtr	pointer)

(define-c-type-alias xmlElementTable*		pointer)
(define-c-type-alias xmlElementTablePtr		pointer)

(define-c-type-alias xmlAttributeTable*		pointer)
(define-c-type-alias xmlAttributeTablePtr	pointer)

(define-c-type-alias xmlIDTable*		pointer)
(define-c-type-alias xmlIDTablePtr		pointer)

(define-c-type-alias xmlRefTable*		pointer)
(define-c-type-alias xmlRefTablePtr		pointer)


;;;; implementation of XInclude
;;
;;Header file "xinclude.h".
;;

(define-c-string-defines  "XInclude strings"
  XINCLUDE_NS
  XINCLUDE_OLD_NS
  XINCLUDE_NODE
  XINCLUDE_FALLBACK
  XINCLUDE_HREF
  XINCLUDE_PARSE
  XINCLUDE_PARSE_XML
  XINCLUDE_PARSE_TEXT
  XINCLUDE_PARSE_ENCODING
  XINCLUDE_PARSE_XPOINTER)

(define-c-type-alias xmlXIncludeCtxt*		pointer)
(define-c-type-alias xmlXIncludeCtxtPtr		pointer)


;;;; API to build regexp automata
;;
;;Header file "xmlautomata.h".
;;

(define-c-type-alias xmlAutomata*		pointer)
(define-c-type-alias xmlAutomataPtr		pointer)

(define-c-type-alias xmlAutomataState*		pointer)
(define-c-type-alias xmlAutomataStatePtr	pointer)


;;;; error handling
;;
;;Header file "xmlerror.h".
;;

(define-c-enumeration xmlErrorLevel
  "xmlErrorLevel"
  XML_ERR_NONE
  XML_ERR_WARNING
  XML_ERR_ERROR
  XML_ERR_FATAL)

(define-c-enumeration xmlErrorDomain
  "xmlErrorDomain"
  XML_FROM_NONE
  XML_FROM_PARSER
  XML_FROM_TREE
  XML_FROM_NAMESPACE
  XML_FROM_DTD
  XML_FROM_HTML
  XML_FROM_MEMORY
  XML_FROM_OUTPUT
  XML_FROM_IO
  XML_FROM_FTP
  XML_FROM_HTTP
  XML_FROM_XINCLUDE
  XML_FROM_XPATH
  XML_FROM_XPOINTER
  XML_FROM_REGEXP
  XML_FROM_DATATYPE
  XML_FROM_SCHEMASP
  XML_FROM_SCHEMASV
  XML_FROM_RELAXNGP
  XML_FROM_RELAXNGV
  XML_FROM_CATALOG
  XML_FROM_C14N
  XML_FROM_XSLT
  XML_FROM_VALID
  XML_FROM_CHECK
  XML_FROM_WRITER
  XML_FROM_MODULE
  XML_FROM_I18N
  XML_FROM_SCHEMATRONV)

(define-c-type-alias xmlError*		pointer)
(define-c-type-alias xmlErrorPtr	pointer)

(define-c-struct xmlError
  "xmlError"
  (signed-int		domain)
  (signed-int		code)
  (pointer		message)
  (signed-int		level)
  (pointer		file)
  (signed-int		line)
  (pointer		str1)
  (pointer		str2)
  (pointer		str3)
  (signed-int		int1)
  (signed-int		int2)
  (pointer		ctxt)
  (pointer		node))

(define-c-enumeration xmlParserErrors
  "xmlParserErrors"
  XML_ERR_OK
  XML_ERR_INTERNAL_ERROR
  XML_ERR_NO_MEMORY
  XML_ERR_DOCUMENT_START
  XML_ERR_DOCUMENT_EMPTY
  XML_ERR_DOCUMENT_END
  XML_ERR_INVALID_HEX_CHARREF
  XML_ERR_INVALID_DEC_CHARREF
  XML_ERR_INVALID_CHARREF
  XML_ERR_INVALID_CHAR
  XML_ERR_CHARREF_AT_EOF
  XML_ERR_CHARREF_IN_PROLOG
  XML_ERR_CHARREF_IN_EPILOG
  XML_ERR_CHARREF_IN_DTD
  XML_ERR_ENTITYREF_AT_EOF
  XML_ERR_ENTITYREF_IN_PROLOG
  XML_ERR_ENTITYREF_IN_EPILOG
  XML_ERR_ENTITYREF_IN_DTD
  XML_ERR_PEREF_AT_EOF
  XML_ERR_PEREF_IN_PROLOG
  XML_ERR_PEREF_IN_EPILOG
  XML_ERR_PEREF_IN_INT_SUBSET
  XML_ERR_ENTITYREF_NO_NAME
  XML_ERR_ENTITYREF_SEMICOL_MISSING
  XML_ERR_PEREF_NO_NAME
  XML_ERR_PEREF_SEMICOL_MISSING
  XML_ERR_UNDECLARED_ENTITY
  XML_WAR_UNDECLARED_ENTITY
  XML_ERR_UNPARSED_ENTITY
  XML_ERR_ENTITY_IS_EXTERNAL
  XML_ERR_ENTITY_IS_PARAMETER
  XML_ERR_UNKNOWN_ENCODING
  XML_ERR_UNSUPPORTED_ENCODING
  XML_ERR_STRING_NOT_STARTED
  XML_ERR_STRING_NOT_CLOSED
  XML_ERR_NS_DECL_ERROR
  XML_ERR_ENTITY_NOT_STARTED
  XML_ERR_ENTITY_NOT_FINISHED
  XML_ERR_LT_IN_ATTRIBUTE
  XML_ERR_ATTRIBUTE_NOT_STARTED
  XML_ERR_ATTRIBUTE_NOT_FINISHED
  XML_ERR_ATTRIBUTE_WITHOUT_VALUE
  XML_ERR_ATTRIBUTE_REDEFINED
  XML_ERR_LITERAL_NOT_STARTED
  XML_ERR_LITERAL_NOT_FINISHED
  XML_ERR_COMMENT_NOT_FINISHED
  XML_ERR_PI_NOT_STARTED
  XML_ERR_PI_NOT_FINISHED
  XML_ERR_NOTATION_NOT_STARTED
  XML_ERR_NOTATION_NOT_FINISHED
  XML_ERR_ATTLIST_NOT_STARTED
  XML_ERR_ATTLIST_NOT_FINISHED
  XML_ERR_MIXED_NOT_STARTED
  XML_ERR_MIXED_NOT_FINISHED
  XML_ERR_ELEMCONTENT_NOT_STARTED
  XML_ERR_ELEMCONTENT_NOT_FINISHED
  XML_ERR_XMLDECL_NOT_STARTED
  XML_ERR_XMLDECL_NOT_FINISHED
  XML_ERR_CONDSEC_NOT_STARTED
  XML_ERR_CONDSEC_NOT_FINISHED
  XML_ERR_EXT_SUBSET_NOT_FINISHED
  XML_ERR_DOCTYPE_NOT_FINISHED
  XML_ERR_MISPLACED_CDATA_END
  XML_ERR_CDATA_NOT_FINISHED
  XML_ERR_RESERVED_XML_NAME
  XML_ERR_SPACE_REQUIRED
  XML_ERR_SEPARATOR_REQUIRED
  XML_ERR_NMTOKEN_REQUIRED
  XML_ERR_NAME_REQUIRED
  XML_ERR_PCDATA_REQUIRED
  XML_ERR_URI_REQUIRED
  XML_ERR_PUBID_REQUIRED
  XML_ERR_LT_REQUIRED
  XML_ERR_GT_REQUIRED
  XML_ERR_LTSLASH_REQUIRED
  XML_ERR_EQUAL_REQUIRED
  XML_ERR_TAG_NAME_MISMATCH
  XML_ERR_TAG_NOT_FINISHED
  XML_ERR_STANDALONE_VALUE
  XML_ERR_ENCODING_NAME
  XML_ERR_HYPHEN_IN_COMMENT
  XML_ERR_INVALID_ENCODING
  XML_ERR_EXT_ENTITY_STANDALONE
  XML_ERR_CONDSEC_INVALID
  XML_ERR_VALUE_REQUIRED
  XML_ERR_NOT_WELL_BALANCED
  XML_ERR_EXTRA_CONTENT
  XML_ERR_ENTITY_CHAR_ERROR
  XML_ERR_ENTITY_PE_INTERNAL
  XML_ERR_ENTITY_LOOP
  XML_ERR_ENTITY_BOUNDARY
  XML_ERR_INVALID_URI
  XML_ERR_URI_FRAGMENT
  XML_WAR_CATALOG_PI
  XML_ERR_NO_DTD
  XML_ERR_CONDSEC_INVALID_KEYWORD
  XML_ERR_VERSION_MISSING
  XML_WAR_UNKNOWN_VERSION
  XML_WAR_LANG_VALUE
  XML_WAR_NS_URI
  XML_WAR_NS_URI_RELATIVE
  XML_ERR_MISSING_ENCODING
  XML_WAR_SPACE_VALUE
  XML_ERR_NOT_STANDALONE
  XML_ERR_ENTITY_PROCESSING
  XML_ERR_NOTATION_PROCESSING
  XML_WAR_NS_COLUMN
  XML_WAR_ENTITY_REDEFINED
  XML_ERR_UNKNOWN_VERSION
  XML_ERR_VERSION_MISMATCH
  XML_NS_ERR_XML_NAMESPACE
  XML_NS_ERR_UNDEFINED_NAMESPACE
  XML_NS_ERR_QNAME
  XML_NS_ERR_ATTRIBUTE_REDEFINED
  XML_NS_ERR_EMPTY
  XML_NS_ERR_COLON
  XML_DTD_ATTRIBUTE_DEFAULT
  XML_DTD_ATTRIBUTE_REDEFINED
  XML_DTD_ATTRIBUTE_VALUE
  XML_DTD_CONTENT_ERROR
  XML_DTD_CONTENT_MODEL
  XML_DTD_CONTENT_NOT_DETERMINIST
  XML_DTD_DIFFERENT_PREFIX
  XML_DTD_ELEM_DEFAULT_NAMESPACE
  XML_DTD_ELEM_NAMESPACE
  XML_DTD_ELEM_REDEFINED
  XML_DTD_EMPTY_NOTATION
  XML_DTD_ENTITY_TYPE
  XML_DTD_ID_FIXED
  XML_DTD_ID_REDEFINED
  XML_DTD_ID_SUBSET
  XML_DTD_INVALID_CHILD
  XML_DTD_INVALID_DEFAULT
  XML_DTD_LOAD_ERROR
  XML_DTD_MISSING_ATTRIBUTE
  XML_DTD_MIXED_CORRUPT
  XML_DTD_MULTIPLE_ID
  XML_DTD_NO_DOC
  XML_DTD_NO_DTD
  XML_DTD_NO_ELEM_NAME
  XML_DTD_NO_PREFIX
  XML_DTD_NO_ROOT
  XML_DTD_NOTATION_REDEFINED
  XML_DTD_NOTATION_VALUE
  XML_DTD_NOT_EMPTY
  XML_DTD_NOT_PCDATA
  XML_DTD_NOT_STANDALONE
  XML_DTD_ROOT_NAME
  XML_DTD_STANDALONE_WHITE_SPACE
  XML_DTD_UNKNOWN_ATTRIBUTE
  XML_DTD_UNKNOWN_ELEM
  XML_DTD_UNKNOWN_ENTITY
  XML_DTD_UNKNOWN_ID
  XML_DTD_UNKNOWN_NOTATION
  XML_DTD_STANDALONE_DEFAULTED
  XML_DTD_XMLID_VALUE
  XML_DTD_XMLID_TYPE
  XML_DTD_DUP_TOKEN
  XML_HTML_STRUCURE_ERROR
  XML_HTML_UNKNOWN_TAG
  XML_RNGP_ANYNAME_ATTR_ANCESTOR
  XML_RNGP_ATTR_CONFLICT
  XML_RNGP_ATTRIBUTE_CHILDREN
  XML_RNGP_ATTRIBUTE_CONTENT
  XML_RNGP_ATTRIBUTE_EMPTY
  XML_RNGP_ATTRIBUTE_NOOP
  XML_RNGP_CHOICE_CONTENT
  XML_RNGP_CHOICE_EMPTY
  XML_RNGP_CREATE_FAILURE
  XML_RNGP_DATA_CONTENT
  XML_RNGP_DEF_CHOICE_AND_INTERLEAVE
  XML_RNGP_DEFINE_CREATE_FAILED
  XML_RNGP_DEFINE_EMPTY
  XML_RNGP_DEFINE_MISSING
  XML_RNGP_DEFINE_NAME_MISSING
  XML_RNGP_ELEM_CONTENT_EMPTY
  XML_RNGP_ELEM_CONTENT_ERROR
  XML_RNGP_ELEMENT_EMPTY
  XML_RNGP_ELEMENT_CONTENT
  XML_RNGP_ELEMENT_NAME
  XML_RNGP_ELEMENT_NO_CONTENT
  XML_RNGP_ELEM_TEXT_CONFLICT
  XML_RNGP_EMPTY
  XML_RNGP_EMPTY_CONSTRUCT
  XML_RNGP_EMPTY_CONTENT
  XML_RNGP_EMPTY_NOT_EMPTY
  XML_RNGP_ERROR_TYPE_LIB
  XML_RNGP_EXCEPT_EMPTY
  XML_RNGP_EXCEPT_MISSING
  XML_RNGP_EXCEPT_MULTIPLE
  XML_RNGP_EXCEPT_NO_CONTENT
  XML_RNGP_EXTERNALREF_EMTPY
  XML_RNGP_EXTERNAL_REF_FAILURE
  XML_RNGP_EXTERNALREF_RECURSE
  XML_RNGP_FORBIDDEN_ATTRIBUTE
  XML_RNGP_FOREIGN_ELEMENT
  XML_RNGP_GRAMMAR_CONTENT
  XML_RNGP_GRAMMAR_EMPTY
  XML_RNGP_GRAMMAR_MISSING
  XML_RNGP_GRAMMAR_NO_START
  XML_RNGP_GROUP_ATTR_CONFLICT
  XML_RNGP_HREF_ERROR
  XML_RNGP_INCLUDE_EMPTY
  XML_RNGP_INCLUDE_FAILURE
  XML_RNGP_INCLUDE_RECURSE
  XML_RNGP_INTERLEAVE_ADD
  XML_RNGP_INTERLEAVE_CREATE_FAILED
  XML_RNGP_INTERLEAVE_EMPTY
  XML_RNGP_INTERLEAVE_NO_CONTENT
  XML_RNGP_INVALID_DEFINE_NAME
  XML_RNGP_INVALID_URI
  XML_RNGP_INVALID_VALUE
  XML_RNGP_MISSING_HREF
  XML_RNGP_NAME_MISSING
  XML_RNGP_NEED_COMBINE
  XML_RNGP_NOTALLOWED_NOT_EMPTY
  XML_RNGP_NSNAME_ATTR_ANCESTOR
  XML_RNGP_NSNAME_NO_NS
  XML_RNGP_PARAM_FORBIDDEN
  XML_RNGP_PARAM_NAME_MISSING
  XML_RNGP_PARENTREF_CREATE_FAILED
  XML_RNGP_PARENTREF_NAME_INVALID
  XML_RNGP_PARENTREF_NO_NAME
  XML_RNGP_PARENTREF_NO_PARENT
  XML_RNGP_PARENTREF_NOT_EMPTY
  XML_RNGP_PARSE_ERROR
  XML_RNGP_PAT_ANYNAME_EXCEPT_ANYNAME
  XML_RNGP_PAT_ATTR_ATTR
  XML_RNGP_PAT_ATTR_ELEM
  XML_RNGP_PAT_DATA_EXCEPT_ATTR
  XML_RNGP_PAT_DATA_EXCEPT_ELEM
  XML_RNGP_PAT_DATA_EXCEPT_EMPTY
  XML_RNGP_PAT_DATA_EXCEPT_GROUP
  XML_RNGP_PAT_DATA_EXCEPT_INTERLEAVE
  XML_RNGP_PAT_DATA_EXCEPT_LIST
  XML_RNGP_PAT_DATA_EXCEPT_ONEMORE
  XML_RNGP_PAT_DATA_EXCEPT_REF
  XML_RNGP_PAT_DATA_EXCEPT_TEXT
  XML_RNGP_PAT_LIST_ATTR
  XML_RNGP_PAT_LIST_ELEM
  XML_RNGP_PAT_LIST_INTERLEAVE
  XML_RNGP_PAT_LIST_LIST
  XML_RNGP_PAT_LIST_REF
  XML_RNGP_PAT_LIST_TEXT
  XML_RNGP_PAT_NSNAME_EXCEPT_ANYNAME
  XML_RNGP_PAT_NSNAME_EXCEPT_NSNAME
  XML_RNGP_PAT_ONEMORE_GROUP_ATTR
  XML_RNGP_PAT_ONEMORE_INTERLEAVE_ATTR
  XML_RNGP_PAT_START_ATTR
  XML_RNGP_PAT_START_DATA
  XML_RNGP_PAT_START_EMPTY
  XML_RNGP_PAT_START_GROUP
  XML_RNGP_PAT_START_INTERLEAVE
  XML_RNGP_PAT_START_LIST
  XML_RNGP_PAT_START_ONEMORE
  XML_RNGP_PAT_START_TEXT
  XML_RNGP_PAT_START_VALUE
  XML_RNGP_PREFIX_UNDEFINED
  XML_RNGP_REF_CREATE_FAILED
  XML_RNGP_REF_CYCLE
  XML_RNGP_REF_NAME_INVALID
  XML_RNGP_REF_NO_DEF
  XML_RNGP_REF_NO_NAME
  XML_RNGP_REF_NOT_EMPTY
  XML_RNGP_START_CHOICE_AND_INTERLEAVE
  XML_RNGP_START_CONTENT
  XML_RNGP_START_EMPTY
  XML_RNGP_START_MISSING
  XML_RNGP_TEXT_EXPECTED
  XML_RNGP_TEXT_HAS_CHILD
  XML_RNGP_TYPE_MISSING
  XML_RNGP_TYPE_NOT_FOUND
  XML_RNGP_TYPE_VALUE
  XML_RNGP_UNKNOWN_ATTRIBUTE
  XML_RNGP_UNKNOWN_COMBINE
  XML_RNGP_UNKNOWN_CONSTRUCT
  XML_RNGP_UNKNOWN_TYPE_LIB
  XML_RNGP_URI_FRAGMENT
  XML_RNGP_URI_NOT_ABSOLUTE
  XML_RNGP_VALUE_EMPTY
  XML_RNGP_VALUE_NO_CONTENT
  XML_RNGP_XMLNS_NAME
  XML_RNGP_XML_NS
  XML_XPATH_EXPRESSION_OK
  XML_XPATH_NUMBER_ERROR
  XML_XPATH_UNFINISHED_LITERAL_ERROR
  XML_XPATH_START_LITERAL_ERROR
  XML_XPATH_VARIABLE_REF_ERROR
  XML_XPATH_UNDEF_VARIABLE_ERROR
  XML_XPATH_INVALID_PREDICATE_ERROR
  XML_XPATH_EXPR_ERROR
  XML_XPATH_UNCLOSED_ERROR
  XML_XPATH_UNKNOWN_FUNC_ERROR
  XML_XPATH_INVALID_OPERAND
  XML_XPATH_INVALID_TYPE
  XML_XPATH_INVALID_ARITY
  XML_XPATH_INVALID_CTXT_SIZE
  XML_XPATH_INVALID_CTXT_POSITION
  XML_XPATH_MEMORY_ERROR
  XML_XPTR_SYNTAX_ERROR
  XML_XPTR_RESOURCE_ERROR
  XML_XPTR_SUB_RESOURCE_ERROR
  XML_XPATH_UNDEF_PREFIX_ERROR
  XML_XPATH_ENCODING_ERROR
  XML_XPATH_INVALID_CHAR_ERROR
  XML_TREE_INVALID_HEX
  XML_TREE_INVALID_DEC
  XML_TREE_UNTERMINATED_ENTITY
  XML_TREE_NOT_UTF8
  XML_SAVE_NOT_UTF8
  XML_SAVE_CHAR_INVALID
  XML_SAVE_NO_DOCTYPE
  XML_SAVE_UNKNOWN_ENCODING
  XML_REGEXP_COMPILE_ERROR
  XML_IO_UNKNOWN
  XML_IO_EACCES
  XML_IO_EAGAIN
  XML_IO_EBADF
  XML_IO_EBADMSG
  XML_IO_EBUSY
  XML_IO_ECANCELED
  XML_IO_ECHILD
  XML_IO_EDEADLK
  XML_IO_EDOM
  XML_IO_EEXIST
  XML_IO_EFAULT
  XML_IO_EFBIG
  XML_IO_EINPROGRESS
  XML_IO_EINTR
  XML_IO_EINVAL
  XML_IO_EIO
  XML_IO_EISDIR
  XML_IO_EMFILE
  XML_IO_EMLINK
  XML_IO_EMSGSIZE
  XML_IO_ENAMETOOLONG
  XML_IO_ENFILE
  XML_IO_ENODEV
  XML_IO_ENOENT
  XML_IO_ENOEXEC
  XML_IO_ENOLCK
  XML_IO_ENOMEM
  XML_IO_ENOSPC
  XML_IO_ENOSYS
  XML_IO_ENOTDIR
  XML_IO_ENOTEMPTY
  XML_IO_ENOTSUP
  XML_IO_ENOTTY
  XML_IO_ENXIO
  XML_IO_EPERM
  XML_IO_EPIPE
  XML_IO_ERANGE
  XML_IO_EROFS
  XML_IO_ESPIPE
  XML_IO_ESRCH
  XML_IO_ETIMEDOUT
  XML_IO_EXDEV
  XML_IO_NETWORK_ATTEMPT
  XML_IO_ENCODER
  XML_IO_FLUSH
  XML_IO_WRITE
  XML_IO_NO_INPUT
  XML_IO_BUFFER_FULL
  XML_IO_LOAD_ERROR
  XML_IO_ENOTSOCK
  XML_IO_EISCONN
  XML_IO_ECONNREFUSED
  XML_IO_ENETUNREACH
  XML_IO_EADDRINUSE
  XML_IO_EALREADY
  XML_IO_EAFNOSUPPORT
  XML_XINCLUDE_RECURSION
  XML_XINCLUDE_PARSE_VALUE
  XML_XINCLUDE_ENTITY_DEF_MISMATCH
  XML_XINCLUDE_NO_HREF
  XML_XINCLUDE_NO_FALLBACK
  XML_XINCLUDE_HREF_URI
  XML_XINCLUDE_TEXT_FRAGMENT
  XML_XINCLUDE_TEXT_DOCUMENT
  XML_XINCLUDE_INVALID_CHAR
  XML_XINCLUDE_BUILD_FAILED
  XML_XINCLUDE_UNKNOWN_ENCODING
  XML_XINCLUDE_MULTIPLE_ROOT
  XML_XINCLUDE_XPTR_FAILED
  XML_XINCLUDE_XPTR_RESULT
  XML_XINCLUDE_INCLUDE_IN_INCLUDE
  XML_XINCLUDE_FALLBACKS_IN_INCLUDE
  XML_XINCLUDE_FALLBACK_NOT_IN_INCLUDE
  XML_XINCLUDE_DEPRECATED_NS
  XML_XINCLUDE_FRAGMENT_ID
  XML_CATALOG_MISSING_ATTR
  XML_CATALOG_ENTRY_BROKEN
  XML_CATALOG_PREFER_VALUE
  XML_CATALOG_NOT_CATALOG
  XML_CATALOG_RECURSION
  XML_SCHEMAP_PREFIX_UNDEFINED
  XML_SCHEMAP_ATTRFORMDEFAULT_VALUE
  XML_SCHEMAP_ATTRGRP_NONAME_NOREF
  XML_SCHEMAP_ATTR_NONAME_NOREF
  XML_SCHEMAP_COMPLEXTYPE_NONAME_NOREF
  XML_SCHEMAP_ELEMFORMDEFAULT_VALUE
  XML_SCHEMAP_ELEM_NONAME_NOREF
  XML_SCHEMAP_EXTENSION_NO_BASE
  XML_SCHEMAP_FACET_NO_VALUE
  XML_SCHEMAP_FAILED_BUILD_IMPORT
  XML_SCHEMAP_GROUP_NONAME_NOREF
  XML_SCHEMAP_IMPORT_NAMESPACE_NOT_URI
  XML_SCHEMAP_IMPORT_REDEFINE_NSNAME
  XML_SCHEMAP_IMPORT_SCHEMA_NOT_URI
  XML_SCHEMAP_INVALID_BOOLEAN
  XML_SCHEMAP_INVALID_ENUM
  XML_SCHEMAP_INVALID_FACET
  XML_SCHEMAP_INVALID_FACET_VALUE
  XML_SCHEMAP_INVALID_MAXOCCURS
  XML_SCHEMAP_INVALID_MINOCCURS
  XML_SCHEMAP_INVALID_REF_AND_SUBTYPE
  XML_SCHEMAP_INVALID_WHITE_SPACE
  XML_SCHEMAP_NOATTR_NOREF
  XML_SCHEMAP_NOTATION_NO_NAME
  XML_SCHEMAP_NOTYPE_NOREF
  XML_SCHEMAP_REF_AND_SUBTYPE
  XML_SCHEMAP_RESTRICTION_NONAME_NOREF
  XML_SCHEMAP_SIMPLETYPE_NONAME
  XML_SCHEMAP_TYPE_AND_SUBTYPE
  XML_SCHEMAP_UNKNOWN_ALL_CHILD
  XML_SCHEMAP_UNKNOWN_ANYATTRIBUTE_CHILD
  XML_SCHEMAP_UNKNOWN_ATTR_CHILD
  XML_SCHEMAP_UNKNOWN_ATTRGRP_CHILD
  XML_SCHEMAP_UNKNOWN_ATTRIBUTE_GROUP
  XML_SCHEMAP_UNKNOWN_BASE_TYPE
  XML_SCHEMAP_UNKNOWN_CHOICE_CHILD
  XML_SCHEMAP_UNKNOWN_COMPLEXCONTENT_CHILD
  XML_SCHEMAP_UNKNOWN_COMPLEXTYPE_CHILD
  XML_SCHEMAP_UNKNOWN_ELEM_CHILD
  XML_SCHEMAP_UNKNOWN_EXTENSION_CHILD
  XML_SCHEMAP_UNKNOWN_FACET_CHILD
  XML_SCHEMAP_UNKNOWN_FACET_TYPE
  XML_SCHEMAP_UNKNOWN_GROUP_CHILD
  XML_SCHEMAP_UNKNOWN_IMPORT_CHILD
  XML_SCHEMAP_UNKNOWN_LIST_CHILD
  XML_SCHEMAP_UNKNOWN_NOTATION_CHILD
  XML_SCHEMAP_UNKNOWN_PROCESSCONTENT_CHILD
  XML_SCHEMAP_UNKNOWN_REF
  XML_SCHEMAP_UNKNOWN_RESTRICTION_CHILD
  XML_SCHEMAP_UNKNOWN_SCHEMAS_CHILD
  XML_SCHEMAP_UNKNOWN_SEQUENCE_CHILD
  XML_SCHEMAP_UNKNOWN_SIMPLECONTENT_CHILD
  XML_SCHEMAP_UNKNOWN_SIMPLETYPE_CHILD
  XML_SCHEMAP_UNKNOWN_TYPE
  XML_SCHEMAP_UNKNOWN_UNION_CHILD
  XML_SCHEMAP_ELEM_DEFAULT_FIXED
  XML_SCHEMAP_REGEXP_INVALID
  XML_SCHEMAP_FAILED_LOAD
  XML_SCHEMAP_NOTHING_TO_PARSE
  XML_SCHEMAP_NOROOT
  XML_SCHEMAP_REDEFINED_GROUP
  XML_SCHEMAP_REDEFINED_TYPE
  XML_SCHEMAP_REDEFINED_ELEMENT
  XML_SCHEMAP_REDEFINED_ATTRGROUP
  XML_SCHEMAP_REDEFINED_ATTR
  XML_SCHEMAP_REDEFINED_NOTATION
  XML_SCHEMAP_FAILED_PARSE
  XML_SCHEMAP_UNKNOWN_PREFIX
  XML_SCHEMAP_DEF_AND_PREFIX
  XML_SCHEMAP_UNKNOWN_INCLUDE_CHILD
  XML_SCHEMAP_INCLUDE_SCHEMA_NOT_URI
  XML_SCHEMAP_INCLUDE_SCHEMA_NO_URI
  XML_SCHEMAP_NOT_SCHEMA
  XML_SCHEMAP_UNKNOWN_MEMBER_TYPE
  XML_SCHEMAP_INVALID_ATTR_USE
  XML_SCHEMAP_RECURSIVE
  XML_SCHEMAP_SUPERNUMEROUS_LIST_ITEM_TYPE
  XML_SCHEMAP_INVALID_ATTR_COMBINATION
  XML_SCHEMAP_INVALID_ATTR_INLINE_COMBINATION
  XML_SCHEMAP_MISSING_SIMPLETYPE_CHILD
  XML_SCHEMAP_INVALID_ATTR_NAME
  XML_SCHEMAP_REF_AND_CONTENT
  XML_SCHEMAP_CT_PROPS_CORRECT_1
  XML_SCHEMAP_CT_PROPS_CORRECT_2
  XML_SCHEMAP_CT_PROPS_CORRECT_3
  XML_SCHEMAP_CT_PROPS_CORRECT_4
  XML_SCHEMAP_CT_PROPS_CORRECT_5
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_1
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_1
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_2
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_2
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_3
  XML_SCHEMAP_WILDCARD_INVALID_NS_MEMBER
  XML_SCHEMAP_INTERSECTION_NOT_EXPRESSIBLE
  XML_SCHEMAP_UNION_NOT_EXPRESSIBLE
  XML_SCHEMAP_SRC_IMPORT_3_1
  XML_SCHEMAP_SRC_IMPORT_3_2
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_1
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_2
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_3
  XML_SCHEMAP_COS_CT_EXTENDS_1_3
  XML_SCHEMAV_NOROOT
  XML_SCHEMAV_UNDECLAREDELEM
  XML_SCHEMAV_NOTTOPLEVEL
  XML_SCHEMAV_MISSING
  XML_SCHEMAV_WRONGELEM
  XML_SCHEMAV_NOTYPE
  XML_SCHEMAV_NOROLLBACK
  XML_SCHEMAV_ISABSTRACT
  XML_SCHEMAV_NOTEMPTY
  XML_SCHEMAV_ELEMCONT
  XML_SCHEMAV_HAVEDEFAULT
  XML_SCHEMAV_NOTNILLABLE
  XML_SCHEMAV_EXTRACONTENT
  XML_SCHEMAV_INVALIDATTR
  XML_SCHEMAV_INVALIDELEM
  XML_SCHEMAV_NOTDETERMINIST
  XML_SCHEMAV_CONSTRUCT
  XML_SCHEMAV_INTERNAL
  XML_SCHEMAV_NOTSIMPLE
  XML_SCHEMAV_ATTRUNKNOWN
  XML_SCHEMAV_ATTRINVALID
  XML_SCHEMAV_VALUE
  XML_SCHEMAV_FACET
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_1
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_2
  XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_3
  XML_SCHEMAV_CVC_TYPE_3_1_1
  XML_SCHEMAV_CVC_TYPE_3_1_2
  XML_SCHEMAV_CVC_FACET_VALID
  XML_SCHEMAV_CVC_LENGTH_VALID
  XML_SCHEMAV_CVC_MINLENGTH_VALID
  XML_SCHEMAV_CVC_MAXLENGTH_VALID
  XML_SCHEMAV_CVC_MININCLUSIVE_VALID
  XML_SCHEMAV_CVC_MAXINCLUSIVE_VALID
  XML_SCHEMAV_CVC_MINEXCLUSIVE_VALID
  XML_SCHEMAV_CVC_MAXEXCLUSIVE_VALID
  XML_SCHEMAV_CVC_TOTALDIGITS_VALID
  XML_SCHEMAV_CVC_FRACTIONDIGITS_VALID
  XML_SCHEMAV_CVC_PATTERN_VALID
  XML_SCHEMAV_CVC_ENUMERATION_VALID
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_1
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_2
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_3
  XML_SCHEMAV_CVC_COMPLEX_TYPE_2_4
  XML_SCHEMAV_CVC_ELT_1
  XML_SCHEMAV_CVC_ELT_2
  XML_SCHEMAV_CVC_ELT_3_1
  XML_SCHEMAV_CVC_ELT_3_2_1
  XML_SCHEMAV_CVC_ELT_3_2_2
  XML_SCHEMAV_CVC_ELT_4_1
  XML_SCHEMAV_CVC_ELT_4_2
  XML_SCHEMAV_CVC_ELT_4_3
  XML_SCHEMAV_CVC_ELT_5_1_1
  XML_SCHEMAV_CVC_ELT_5_1_2
  XML_SCHEMAV_CVC_ELT_5_2_1
  XML_SCHEMAV_CVC_ELT_5_2_2_1
  XML_SCHEMAV_CVC_ELT_5_2_2_2_1
  XML_SCHEMAV_CVC_ELT_5_2_2_2_2
  XML_SCHEMAV_CVC_ELT_6
  XML_SCHEMAV_CVC_ELT_7
  XML_SCHEMAV_CVC_ATTRIBUTE_1
  XML_SCHEMAV_CVC_ATTRIBUTE_2
  XML_SCHEMAV_CVC_ATTRIBUTE_3
  XML_SCHEMAV_CVC_ATTRIBUTE_4
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_1
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_1
  XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_2
  XML_SCHEMAV_CVC_COMPLEX_TYPE_4
  XML_SCHEMAV_CVC_COMPLEX_TYPE_5_1
  XML_SCHEMAV_CVC_COMPLEX_TYPE_5_2
  XML_SCHEMAV_ELEMENT_CONTENT
  XML_SCHEMAV_DOCUMENT_ELEMENT_MISSING
  XML_SCHEMAV_CVC_COMPLEX_TYPE_1
  XML_SCHEMAV_CVC_AU
  XML_SCHEMAV_CVC_TYPE_1
  XML_SCHEMAV_CVC_TYPE_2
  XML_SCHEMAV_CVC_IDC
  XML_SCHEMAV_CVC_WILDCARD
  XML_SCHEMAV_MISC
  XML_XPTR_UNKNOWN_SCHEME
  XML_XPTR_CHILDSEQ_START
  XML_XPTR_EVAL_FAILED
  XML_XPTR_EXTRA_OBJECTS
  XML_C14N_CREATE_CTXT
  XML_C14N_REQUIRES_UTF8
  XML_C14N_CREATE_STACK
  XML_C14N_INVALID_NODE
  XML_C14N_UNKNOW_NODE
  XML_C14N_RELATIVE_NAMESPACE
  XML_FTP_PASV_ANSWER
  XML_FTP_EPSV_ANSWER
  XML_FTP_ACCNT
  XML_FTP_URL_SYNTAX
  XML_HTTP_URL_SYNTAX
  XML_HTTP_USE_IP
  XML_HTTP_UNKNOWN_HOST
  XML_SCHEMAP_SRC_SIMPLE_TYPE_1
  XML_SCHEMAP_SRC_SIMPLE_TYPE_2
  XML_SCHEMAP_SRC_SIMPLE_TYPE_3
  XML_SCHEMAP_SRC_SIMPLE_TYPE_4
  XML_SCHEMAP_SRC_RESOLVE
  XML_SCHEMAP_SRC_RESTRICTION_BASE_OR_SIMPLETYPE
  XML_SCHEMAP_SRC_LIST_ITEMTYPE_OR_SIMPLETYPE
  XML_SCHEMAP_SRC_UNION_MEMBERTYPES_OR_SIMPLETYPES
  XML_SCHEMAP_ST_PROPS_CORRECT_1
  XML_SCHEMAP_ST_PROPS_CORRECT_2
  XML_SCHEMAP_ST_PROPS_CORRECT_3
  XML_SCHEMAP_COS_ST_RESTRICTS_1_1
  XML_SCHEMAP_COS_ST_RESTRICTS_1_2
  XML_SCHEMAP_COS_ST_RESTRICTS_1_3_1
  XML_SCHEMAP_COS_ST_RESTRICTS_1_3_2
  XML_SCHEMAP_COS_ST_RESTRICTS_2_1
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_1
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_2
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_1
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_2
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_3
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_4
  XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_5
  XML_SCHEMAP_COS_ST_RESTRICTS_3_1
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1_2
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_2
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_1
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_3
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_4
  XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_5
  XML_SCHEMAP_COS_ST_DERIVED_OK_2_1
  XML_SCHEMAP_COS_ST_DERIVED_OK_2_2
  XML_SCHEMAP_S4S_ELEM_NOT_ALLOWED
  XML_SCHEMAP_S4S_ELEM_MISSING
  XML_SCHEMAP_S4S_ATTR_NOT_ALLOWED
  XML_SCHEMAP_S4S_ATTR_MISSING
  XML_SCHEMAP_S4S_ATTR_INVALID_VALUE
  XML_SCHEMAP_SRC_ELEMENT_1
  XML_SCHEMAP_SRC_ELEMENT_2_1
  XML_SCHEMAP_SRC_ELEMENT_2_2
  XML_SCHEMAP_SRC_ELEMENT_3
  XML_SCHEMAP_P_PROPS_CORRECT_1
  XML_SCHEMAP_P_PROPS_CORRECT_2_1
  XML_SCHEMAP_P_PROPS_CORRECT_2_2
  XML_SCHEMAP_E_PROPS_CORRECT_2
  XML_SCHEMAP_E_PROPS_CORRECT_3
  XML_SCHEMAP_E_PROPS_CORRECT_4
  XML_SCHEMAP_E_PROPS_CORRECT_5
  XML_SCHEMAP_E_PROPS_CORRECT_6
  XML_SCHEMAP_SRC_INCLUDE
  XML_SCHEMAP_SRC_ATTRIBUTE_1
  XML_SCHEMAP_SRC_ATTRIBUTE_2
  XML_SCHEMAP_SRC_ATTRIBUTE_3_1
  XML_SCHEMAP_SRC_ATTRIBUTE_3_2
  XML_SCHEMAP_SRC_ATTRIBUTE_4
  XML_SCHEMAP_NO_XMLNS
  XML_SCHEMAP_NO_XSI
  XML_SCHEMAP_COS_VALID_DEFAULT_1
  XML_SCHEMAP_COS_VALID_DEFAULT_2_1
  XML_SCHEMAP_COS_VALID_DEFAULT_2_2_1
  XML_SCHEMAP_COS_VALID_DEFAULT_2_2_2
  XML_SCHEMAP_CVC_SIMPLE_TYPE
  XML_SCHEMAP_COS_CT_EXTENDS_1_1
  XML_SCHEMAP_SRC_IMPORT_1_1
  XML_SCHEMAP_SRC_IMPORT_1_2
  XML_SCHEMAP_SRC_IMPORT_2
  XML_SCHEMAP_SRC_IMPORT_2_1
  XML_SCHEMAP_SRC_IMPORT_2_2
  XML_SCHEMAP_INTERNAL
  XML_SCHEMAP_NOT_DETERMINISTIC
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_1
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_2
  XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_3
  XML_SCHEMAP_MG_PROPS_CORRECT_1
  XML_SCHEMAP_MG_PROPS_CORRECT_2
  XML_SCHEMAP_SRC_CT_1
  XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_3
  XML_SCHEMAP_AU_PROPS_CORRECT_2
  XML_SCHEMAP_A_PROPS_CORRECT_2
  XML_SCHEMAP_C_PROPS_CORRECT
  XML_SCHEMAP_SRC_REDEFINE
  XML_SCHEMAP_SRC_IMPORT
  XML_SCHEMAP_WARN_SKIP_SCHEMA
  XML_SCHEMAP_WARN_UNLOCATED_SCHEMA
  XML_SCHEMAP_WARN_ATTR_REDECL_PROH
  XML_SCHEMAP_WARN_ATTR_POINTLESS_PROH
  XML_SCHEMAP_AG_PROPS_CORRECT
  XML_SCHEMAP_COS_CT_EXTENDS_1_2
  XML_SCHEMAP_AU_PROPS_CORRECT
  XML_SCHEMAP_A_PROPS_CORRECT_3
  XML_SCHEMAP_COS_ALL_LIMITED
  XML_SCHEMATRONV_ASSERT
  XML_SCHEMATRONV_REPORT
  XML_MODULE_OPEN
  XML_MODULE_CLOSE
  XML_CHECK_FOUND_ELEMENT
  XML_CHECK_FOUND_ATTRIBUTE
  XML_CHECK_FOUND_TEXT
  XML_CHECK_FOUND_CDATA
  XML_CHECK_FOUND_ENTITYREF
  XML_CHECK_FOUND_ENTITY
  XML_CHECK_FOUND_PI
  XML_CHECK_FOUND_COMMENT
  XML_CHECK_FOUND_DOCTYPE
  XML_CHECK_FOUND_FRAGMENT
  XML_CHECK_FOUND_NOTATION
  XML_CHECK_UNKNOWN_NODE
  XML_CHECK_ENTITY_TYPE
  XML_CHECK_NO_PARENT
  XML_CHECK_NO_DOC
  XML_CHECK_NO_NAME
  XML_CHECK_NO_ELEM
  XML_CHECK_WRONG_DOC
  XML_CHECK_NO_PREV
  XML_CHECK_WRONG_PREV
  XML_CHECK_NO_NEXT
  XML_CHECK_WRONG_NEXT
  XML_CHECK_NOT_DTD
  XML_CHECK_NOT_ATTR
  XML_CHECK_NOT_ATTR_DECL
  XML_CHECK_NOT_ELEM_DECL
  XML_CHECK_NOT_ENTITY_DECL
  XML_CHECK_NOT_NS_DECL
  XML_CHECK_NO_HREF
  XML_CHECK_WRONG_PARENT
  XML_CHECK_NS_SCOPE
  XML_CHECK_NS_ANCESTOR
  XML_CHECK_NOT_UTF8
  XML_CHECK_NO_DICT
  XML_CHECK_NOT_NCNAME
  XML_CHECK_OUTSIDE_DICT
  XML_CHECK_WRONG_NAME
  XML_CHECK_NAME_NOT_NULL
  XML_I18N_NO_NAME
  XML_I18N_NO_HANDLER
  XML_I18N_EXCESS_HANDLER
  XML_I18N_CONV_FAILED
  XML_I18N_NO_OUTPUT
  XML_CHECK_
  XML_CHECK_X)

(define-c-callback-pointer-type xmlGenericErrorFunc)
(define-c-callback-pointer-type xmlStructuredErrorFunc)



;;;; interface for the I/O interfaces used by the parser
;;
;;Header file "XMLio.h".
;;

(define-c-struct _xmlParserInputBuffer
  "struct _xmlParserInputBuffer"
  (pointer			context)
  (pointer			readcallback)
  (pointer			closecallback)
  (pointer			encoder)
  (pointer			buffer)
  (pointer			raw)
  (signed-int			compressed)
  (signed-int			error)
  (unsigned-int		rawconsumed))

(define-c-struct _xmlOutputBuffer
  "struct _xmlOutputBuffer"
  (pointer			context)
  (pointer			writecallback)
  (pointer			closecallback)
  (pointer			encoder)
  (pointer			buffer)
  (pointer			conv)
  (signed-int			written)
  (signed-int			error))

(define-c-callback-pointer-type xmlInputMatchCallback)
(define-c-callback-pointer-type xmlInputOpenCallback)
(define-c-callback-pointer-type xmlInputReadCallback)
(define-c-callback-pointer-type xmlInputCloseCallback)
(define-c-callback-pointer-type xmlOutputMatchCallback)
(define-c-callback-pointer-type xmlOutputOpenCallback)
(define-c-callback-pointer-type xmlOutputWriteCallback)
(define-c-callback-pointer-type xmlOutputCloseCallback)




;;;; interface for the memory allocator
;;
;;Header file "xmlmemory.h".
;;

(define-c-callback-pointer-type xmlFreeFunc)
(define-c-callback-pointer-type xmlMallocFunc)
(define-c-callback-pointer-type xmlReallocFunc)
(define-c-callback-pointer-type xmlStrdupFunc)


;;;; dynamic module loading
;;
;;Header file "xmlmodule.h".
;;

(define-c-type-alias xmlModule*		pointer)
(define-c-type-alias xmlModulePtr	pointer)

(define-c-enumeration xmlModuleOption
  "xmlModuleOption"
  XML_MODULE_LAZY
  XML_MODULE_LOCAL)


;;;; the XMLReader implementation
;;
;;Header file "xmlreader.h".
;;

(define-c-enumeration xmlParserSeverities
  "xmlParserSeverities"
  XML_PARSER_SEVERITY_VALIDITY_WARNING
  XML_PARSER_SEVERITY_VALIDITY_ERROR
  XML_PARSER_SEVERITY_WARNING
  XML_PARSER_SEVERITY_ERROR)

(define-c-enumeration xmlTextReaderMode
  "xmlTextReaderMode"
  XML_TEXTREADER_MODE_INITIAL
  XML_TEXTREADER_MODE_INTERACTIVE
  XML_TEXTREADER_MODE_ERROR
  XML_TEXTREADER_MODE_EOF
  XML_TEXTREADER_MODE_CLOSED
  XML_TEXTREADER_MODE_READING)

(define-c-enumeration xmlParserProperties
  "xmlParserProperties"
  XML_PARSER_LOADDTD
  XML_PARSER_DEFAULTATTRS
  XML_PARSER_VALIDATE
  XML_PARSER_SUBST_ENTITIES)

(define-c-enumeration xmlReaderTypes
  "xmlReaderTypes"
  XML_READER_TYPE_NONE
  XML_READER_TYPE_ELEMENT
  XML_READER_TYPE_ATTRIBUTE
  XML_READER_TYPE_TEXT
  XML_READER_TYPE_CDATA
  XML_READER_TYPE_ENTITY_REFERENCE
  XML_READER_TYPE_ENTITY
  XML_READER_TYPE_PROCESSING_INSTRUCTION
  XML_READER_TYPE_COMMENT
  XML_READER_TYPE_DOCUMENT
  XML_READER_TYPE_DOCUMENT_TYPE
  XML_READER_TYPE_DOCUMENT_FRAGMENT
  XML_READER_TYPE_NOTATION
  XML_READER_TYPE_WHITESPACE
  XML_READER_TYPE_SIGNIFICANT_WHITESPACE
  XML_READER_TYPE_END_ELEMENT
  XML_READER_TYPE_END_ENTITY
  XML_READER_TYPE_XML_DECLARATION)

(define-c-type-alias xmlTextReader*		pointer)
(define-c-type-alias xmlTextReaderPtr		pointer)

(define-c-type-alias xmlTextReaderLocatorPtr	pointer)

(define-c-callback-pointer-type xmlTextReaderErrorFunc)


;;;; regular expressions handling
;;
;;Header file "xmlregexp.h"
;;

(define-c-type-alias xmlRegexp*		pointer)
(define-c-type-alias xmlRegexpPtr	pointer)

(define-c-type-alias xmlRegExecCtxt*	pointer)
(define-c-type-alias xmlRegExecCtxtPtr	pointer)

(define-c-type-alias xmlExpCtxt*	pointer)
(define-c-type-alias xmlExpCtxtPtr	pointer)

(define-c-type-alias xmlExpNode*	pointer)
(define-c-type-alias xmlExpNodePtr	pointer)

(define-c-enumeration xmlExpNodeType
  "xmlExpNodeType"
  XML_EXP_EMPTY
  XML_EXP_FORBID
  XML_EXP_ATOM
  XML_EXP_SEQ
  XML_EXP_OR
  XML_EXP_COUNT)

(define-c-callback-pointer-type xmlRegExecCallbacks)



;;;; the XML document serializer
;;
;;Header file "xmlsave.h".
;;

(define-c-enumeration xmlSaveOption
  "xmlSaveOption"
  XML_SAVE_FORMAT
  XML_SAVE_NO_DECL
  XML_SAVE_NO_EMPTY
  XML_SAVE_NO_XHTML
  XML_SAVE_XHTML
  XML_SAVE_AS_XML
  XML_SAVE_AS_HTML)

(define-c-type-alias xmlSaveCtxt*		pointer)
(define-c-type-alias xmlSaveCtxtPtr		pointer)


;;;; internal interfaces for XML Schemas
;;
;;Header file "schemasInternals.h"
;;

(define-c-enumeration xmlSchemaValType
  "xmlSchemaValType"
  XML_SCHEMAS_UNKNOWN
  XML_SCHEMAS_STRING
  XML_SCHEMAS_NORMSTRING
  XML_SCHEMAS_DECIMAL
  XML_SCHEMAS_TIME
  XML_SCHEMAS_GDAY
  XML_SCHEMAS_GMONTH
  XML_SCHEMAS_GMONTHDAY
  XML_SCHEMAS_GYEAR
  XML_SCHEMAS_GYEARMONTH
  XML_SCHEMAS_DATE
  XML_SCHEMAS_DATETIME
  XML_SCHEMAS_DURATION
  XML_SCHEMAS_FLOAT
  XML_SCHEMAS_DOUBLE
  XML_SCHEMAS_BOOLEAN
  XML_SCHEMAS_TOKEN
  XML_SCHEMAS_LANGUAGE
  XML_SCHEMAS_NMTOKEN
  XML_SCHEMAS_NMTOKENS
  XML_SCHEMAS_NAME
  XML_SCHEMAS_QNAME
  XML_SCHEMAS_NCNAME
  XML_SCHEMAS_ID
  XML_SCHEMAS_IDREF
  XML_SCHEMAS_IDREFS
  XML_SCHEMAS_ENTITY
  XML_SCHEMAS_ENTITIES
  XML_SCHEMAS_NOTATION
  XML_SCHEMAS_ANYURI
  XML_SCHEMAS_INTEGER
  XML_SCHEMAS_NPINTEGER
  XML_SCHEMAS_NINTEGER
  XML_SCHEMAS_NNINTEGER
  XML_SCHEMAS_PINTEGER
  XML_SCHEMAS_INT
  XML_SCHEMAS_UINT
  XML_SCHEMAS_LONG
  XML_SCHEMAS_ULONG
  XML_SCHEMAS_SHORT
  XML_SCHEMAS_USHORT
  XML_SCHEMAS_BYTE
  XML_SCHEMAS_UBYTE
  XML_SCHEMAS_HEXBINARY
  XML_SCHEMAS_BASE64BINARY
  XML_SCHEMAS_ANYTYPE
  XML_SCHEMAS_ANYSIMPLETYPE)

(define-c-enumeration xmlSchemaTypeType
  "xmlSchemaTypeType"
  XML_SCHEMA_TYPE_BASIC
  XML_SCHEMA_TYPE_ANY
  XML_SCHEMA_TYPE_FACET
  XML_SCHEMA_TYPE_SIMPLE
  XML_SCHEMA_TYPE_COMPLEX
  XML_SCHEMA_TYPE_SEQUENCE
  XML_SCHEMA_TYPE_CHOICE
  XML_SCHEMA_TYPE_ALL
  XML_SCHEMA_TYPE_SIMPLE_CONTENT
  XML_SCHEMA_TYPE_COMPLEX_CONTENT
  XML_SCHEMA_TYPE_UR
  XML_SCHEMA_TYPE_RESTRICTION
  XML_SCHEMA_TYPE_EXTENSION
  XML_SCHEMA_TYPE_ELEMENT
  XML_SCHEMA_TYPE_ATTRIBUTE
  XML_SCHEMA_TYPE_ATTRIBUTEGROUP
  XML_SCHEMA_TYPE_GROUP
  XML_SCHEMA_TYPE_NOTATION
  XML_SCHEMA_TYPE_LIST
  XML_SCHEMA_TYPE_UNION
  XML_SCHEMA_TYPE_ANY_ATTRIBUTE
  XML_SCHEMA_TYPE_IDC_UNIQUE
  XML_SCHEMA_TYPE_IDC_KEY
  XML_SCHEMA_TYPE_IDC_KEYREF
  XML_SCHEMA_TYPE_PARTICLE
  XML_SCHEMA_TYPE_ATTRIBUTE_USE
  XML_SCHEMA_FACET_MININCLUSIVE
  XML_SCHEMA_FACET_MINEXCLUSIVE
  XML_SCHEMA_FACET_MAXINCLUSIVE
  XML_SCHEMA_FACET_MAXEXCLUSIVE
  XML_SCHEMA_FACET_TOTALDIGITS
  XML_SCHEMA_FACET_FRACTIONDIGITS
  XML_SCHEMA_FACET_PATTERN
  XML_SCHEMA_FACET_ENUMERATION
  XML_SCHEMA_FACET_WHITESPACE
  XML_SCHEMA_FACET_LENGTH
  XML_SCHEMA_FACET_MAXLENGTH
  XML_SCHEMA_FACET_MINLENGTH
  XML_SCHEMA_EXTRA_QNAMEREF
  XML_SCHEMA_EXTRA_ATTR_USE_PROHIB)

(define-c-enumeration xmlSchemaContentType
  "xmlSchemaContentType"
  XML_SCHEMA_CONTENT_UNKNOWN
  XML_SCHEMA_CONTENT_EMPTY
  XML_SCHEMA_CONTENT_ELEMENTS
  XML_SCHEMA_CONTENT_MIXED
  XML_SCHEMA_CONTENT_SIMPLE
  XML_SCHEMA_CONTENT_MIXED_OR_ELEMENTS
  XML_SCHEMA_CONTENT_BASIC
  XML_SCHEMA_CONTENT_ANY)

(define-c-type-alias xmlSchemaVal*		pointer)
(define-c-type-alias xmlSchemaValPtr		pointer)
(define-c-type-alias xmlSchemaValPtr*		pointer)

(define-c-type-alias xmlSchemaType*		pointer)
(define-c-type-alias xmlSchemaTypePtr		pointer)
(define-c-type-alias xmlSchemaTypePtr*		pointer)

(define-c-type-alias xmlSchemaFacet*		pointer)
(define-c-type-alias xmlSchemaFacetPtr		pointer)
(define-c-type-alias xmlSchemaFacetPtr*		pointer)

(define-c-type-alias xmlSchemaAnnot*		pointer)
(define-c-type-alias xmlSchemaAnnotPtr		pointer)
(define-c-type-alias xmlSchemaAnnotPtr*		pointer)

(define-c-struct xmlSchemaAnnot
  "struct _xmlSchemaAnnot"
  (pointer			next)
  (pointer			content))

(define-c-defines "schemas constants"
  XML_SCHEMAS_ANYATTR_SKIP
  XML_SCHEMAS_ANYATTR_LAX
  XML_SCHEMAS_ANYATTR_STRICT
  XML_SCHEMAS_ANY_SKIP
  XML_SCHEMAS_ANY_LAX
  XML_SCHEMAS_ANY_STRICT
  XML_SCHEMAS_ATTR_USE_PROHIBITED
  XML_SCHEMAS_ATTR_USE_REQUIRED
  XML_SCHEMAS_ATTR_USE_OPTIONAL
  XML_SCHEMAS_ATTR_GLOBAL
  XML_SCHEMAS_ATTR_NSDEFAULT
  XML_SCHEMAS_ATTR_INTERNAL_RESOLVED
  XML_SCHEMAS_ATTR_FIXED)

(define-c-type-alias xmlSchemaAttribute*	pointer)
(define-c-type-alias xmlSchemaAttributePtr	pointer)

(define-c-struct xmlSchemaAttribute
  "struct _xmlSchemaAttribute"
  (signed-int			type)
  (pointer			next)
  (pointer			name)
  (pointer			id)
  (pointer			ref)
  (pointer			refNs)
  (pointer			typeName)
  (pointer			typeNs)
  (pointer			annot)
  (pointer			base)
  (signed-int			occurs)
  (pointer			defValue)
  (pointer			subtypes)
  (pointer			node)
  (pointer			targetNamespace)
  (signed-int			flags)
  (pointer			refPrefix)
  (pointer			defVal)
  (pointer			refDecl))

(define-c-type-alias xmlSchemaAttributeLink*		pointer)
(define-c-type-alias xmlSchemaAttributeLinkPtr		pointer)
(define-c-type-alias xmlSchemaAttributeLinkPtr*		pointer)

(define-c-struct xmlSchemaAttributeLink
  "struct _xmlSchemaAttributeLink"
  (pointer			next)
  (pointer			attr))

(define-c-type-alias xmlSchemaWildcardNs*		pointer)
(define-c-type-alias xmlSchemaWildcardNsPtr		pointer)

(define-c-struct xmlSchemaWildcardNs
  "struct _xmlSchemaWildcardNs"
  (pointer			next)
  (pointer			value))

(define-c-type-alias xmlSchemaWildcard*			pointer)
(define-c-type-alias xmlSchemaWildcardPtr		pointer)
(define-c-type-alias xmlSchemaWildcardPtr*		pointer)

(define-c-struct xmlSchemaWildcard
  "struct _xmlSchemaWildcard"
  (signed-int			type)
  (pointer			id)
  (pointer			annot)
  (pointer			xmlNodePtr)
  (signed-int			minOccurs)
  (signed-int			maxOccurs)
  (signed-int			processContents)
  (signed-int			any)
  (pointer			nsSet)
  (pointer			negNsSet)
  (signed-int			flags))

(define-c-defines "schemas constants"
  XML_SCHEMAS_WILDCARD_COMPLETE

  XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED
  XML_SCHEMAS_ATTRGROUP_GLOBAL
  XML_SCHEMAS_ATTRGROUP_MARKED
  XML_SCHEMAS_ATTRGROUP_REDEFINED
  XML_SCHEMAS_ATTRGROUP_HAS_REFS

  XML_SCHEMAS_TYPE_MIXED
  XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION
  XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION
  XML_SCHEMAS_TYPE_GLOBAL
  XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD
  XML_SCHEMAS_TYPE_VARIETY_ABSENT
  XML_SCHEMAS_TYPE_VARIETY_LIST
  XML_SCHEMAS_TYPE_VARIETY_UNION
  XML_SCHEMAS_TYPE_VARIETY_ATOMIC
  XML_SCHEMAS_TYPE_FINAL_EXTENSION
  XML_SCHEMAS_TYPE_FINAL_RESTRICTION
  XML_SCHEMAS_TYPE_FINAL_LIST
  XML_SCHEMAS_TYPE_FINAL_UNION
  XML_SCHEMAS_TYPE_FINAL_DEFAULT
  XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE
  XML_SCHEMAS_TYPE_MARKED
  XML_SCHEMAS_TYPE_BLOCK_DEFAULT
  XML_SCHEMAS_TYPE_BLOCK_EXTENSION
  XML_SCHEMAS_TYPE_BLOCK_RESTRICTION
  XML_SCHEMAS_TYPE_ABSTRACT
  XML_SCHEMAS_TYPE_FACETSNEEDVALUE
  XML_SCHEMAS_TYPE_INTERNAL_RESOLVED
  XML_SCHEMAS_TYPE_INTERNAL_INVALID
  XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE
  XML_SCHEMAS_TYPE_WHITESPACE_REPLACE
  XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE
  XML_SCHEMAS_TYPE_HAS_FACETS
  XML_SCHEMAS_TYPE_NORMVALUENEEDED
  XML_SCHEMAS_TYPE_FIXUP_1
  XML_SCHEMAS_TYPE_REDEFINED
;;; XML_SCHEMAS_TYPE_REDEFINING

  XML_SCHEMAS_ELEM_NILLABLE
  XML_SCHEMAS_ELEM_GLOBAL
  XML_SCHEMAS_ELEM_DEFAULT
  XML_SCHEMAS_ELEM_FIXED
  XML_SCHEMAS_ELEM_ABSTRACT
  XML_SCHEMAS_ELEM_TOPLEVEL
  XML_SCHEMAS_ELEM_REF
  XML_SCHEMAS_ELEM_NSDEFAULT
  XML_SCHEMAS_ELEM_INTERNAL_RESOLVED
  XML_SCHEMAS_ELEM_CIRCULAR
  XML_SCHEMAS_ELEM_BLOCK_ABSENT
  XML_SCHEMAS_ELEM_BLOCK_EXTENSION
  XML_SCHEMAS_ELEM_BLOCK_RESTRICTION
  XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION
  XML_SCHEMAS_ELEM_FINAL_ABSENT
  XML_SCHEMAS_ELEM_FINAL_EXTENSION
  XML_SCHEMAS_ELEM_FINAL_RESTRICTION
  XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD
  XML_SCHEMAS_ELEM_INTERNAL_CHECKED

  XML_SCHEMAS_FACET_UNKNOWN
  XML_SCHEMAS_FACET_PRESERVE
  XML_SCHEMAS_FACET_REPLACE
  XML_SCHEMAS_FACET_COLLAPSE

  XML_SCHEMAS_QUALIF_ELEM
  XML_SCHEMAS_QUALIF_ATTR
  XML_SCHEMAS_FINAL_DEFAULT_EXTENSION
  XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION
  XML_SCHEMAS_FINAL_DEFAULT_LIST
  XML_SCHEMAS_FINAL_DEFAULT_UNION
  XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION
  XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION
  XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION
  XML_SCHEMAS_INCLUDING_CONVERT_NS)

(define-c-type-alias xmlSchemaAttributeGroup*		pointer)
(define-c-type-alias xmlSchemaAttributeGroupPtr		pointer)
(define-c-type-alias xmlSchemaAttributeGroupPtr*	pointer)

(define-c-struct xmlSchemaAttributeGroup
  "struct _xmlSchemaAttributeGroup"
  (signed-int			type)
  (pointer			next)
  (pointer			name)
  (pointer			id)
  (pointer			ref)
  (pointer			refNs)
  (pointer			annot)
  (pointer			attributes)
  (pointer			node)
  (signed-int			flags)
  (pointer			attributeWildcard)
  (pointer			refPrefix)
  (pointer			refItem)
  (pointer			targetNamespace)
  (pointer			attrUses))

(define-c-type-alias xmlSchemaTypeLink*		pointer)
(define-c-type-alias xmlSchemaTypeLinkPtr	pointer)
(define-c-type-alias xmlSchemaTypeLinkPtr*	pointer)

(define-c-struct xmlSchemaTypeLink
  "struct _xmlSchemaTypeLink"
  (pointer			next)
  (pointer			type))

(define-c-type-alias xmlSchemaFacetLink*	pointer)
(define-c-type-alias xmlSchemaFacetLinkPtr	pointer)
(define-c-type-alias xmlSchemaFacetLinkPtr*	pointer)

(define-c-struct xmlSchemaFacetLink
  "struct _xmlSchemaFacetLink"
  (pointer			next)
  (pointer			facet))

;; struct _xmlSchemaType {
;;     xmlSchemaTypeType type; /* The kind of type */
;;     struct _xmlSchemaType *next; /* the next type if in a sequence ... */
;;     const xmlChar *name;
;;     const xmlChar *id ; /* Deprecated; not used */
;;     const xmlChar *ref; /* Deprecated; not used */
;;     const xmlChar *refNs; /* Deprecated; not used */
;;     xmlSchemaAnnotPtr annot;
;;     xmlSchemaTypePtr subtypes;
;;     xmlSchemaAttributePtr attributes; /* Deprecated; not used */
;;     xmlNodePtr node;
;;     int minOccurs; /* Deprecated; not used */
;;     int maxOccurs; /* Deprecated; not used */

;;     int flags;
;;     xmlSchemaContentType contentType;
;;     const xmlChar *base; /* Base type's local name */
;;     const xmlChar *baseNs; /* Base type's target namespace */
;;     xmlSchemaTypePtr baseType; /* The base type component */
;;     xmlSchemaFacetPtr facets; /* Local facets */
;;     struct _xmlSchemaType *redef; /* Deprecated; not used */
;;     int recurse; /* Obsolete */
;;     xmlSchemaAttributeLinkPtr *attributeUses; /* Deprecated; not used */
;;     xmlSchemaWildcardPtr attributeWildcard;
;;     int builtInType; /* Type of built-in types. */
;;     xmlSchemaTypeLinkPtr memberTypes; /* member-types if a union type. */
;;     xmlSchemaFacetLinkPtr facetSet; /* All facets (incl. inherited) */
;;     const xmlChar *refPrefix; /* Deprecated; not used */
;;     xmlSchemaTypePtr contentTypeDef; /* Used for the simple content of complex types.
;;                                         Could we use @subtypes for this? */
;;     xmlRegexpPtr contModel; /* Holds the automaton of the content model */
;;     const xmlChar *targetNamespace;
;;     void *attrUses;
;; };

(define-c-type-alias xmlSchemaElement*		pointer)
(define-c-type-alias xmlSchemaElementPtr	pointer)
(define-c-type-alias xmlSchemaElementPtr*	pointer)

(define-c-struct xmlSchemaElement
  "struct _xmlSchemaElement"
  (signed-int			type)
  (pointer			next)
  (pointer			name)
  (pointer			id)
  (pointer			ref)
  (pointer			refNs)
  (pointer			annot)
  (pointer			subtypes)
  (pointer			attributes)
  (pointer			node)
  (signed-int			minOccurs)
  (signed-int			maxOccurs)
  (signed-int			flags)
  (pointer			targetNamespace)
  (pointer			namedType)
  (pointer			namedTypeNs)
  (pointer			substGroup)
  (pointer			substGroupNs)
  (pointer			scope)
  (pointer			value)
  (pointer			refDecl)
  (pointer			contModel)
  (signed-int			contentType)
  (pointer			refPrefix)
  (pointer			defVal)
  (pointer			idcs))

;; struct _xmlSchemaFacet {
;;     xmlSchemaTypeType type;        /* The kind of type */
;;     struct _xmlSchemaFacet *next;/* the next type if in a sequence ... */
;;     const xmlChar *value; /* The original value */
;;     const xmlChar *id; /* Obsolete */
;;     xmlSchemaAnnotPtr annot;
;;     xmlNodePtr node;
;;     int fixed; /* XML_SCHEMAS_FACET_PRESERVE, etc. */
;;     int whitespace;
;;     xmlSchemaValPtr val; /* The compiled value */
;;     xmlRegexpPtr    regexp; /* The regex for patterns */
;; };

(define-c-type-alias xmlSchemaNotation*		pointer)
(define-c-type-alias xmlSchemaNotationPtr	pointer)
(define-c-type-alias xmlSchemaNotationPtr*	pointer)

(define-c-struct xmlSchemaNotation
  "struct _xmlSchemaNotation"
  (signed-int			type)
  (pointer			name)
  (pointer			annot)
  (pointer			identifier)
  (pointer			targetNamespace))

;; struct _xmlSchema {
;;     const xmlChar *name; /* schema name */
;;     const xmlChar *targetNamespace; /* the target namespace */
;;     const xmlChar *version;
;;     const xmlChar *id; /* Obsolete */
;;     xmlDocPtr doc;
;;     xmlSchemaAnnotPtr annot;
;;     int flags;

;;     xmlHashTablePtr typeDecl;
;;     xmlHashTablePtr attrDecl;
;;     xmlHashTablePtr attrgrpDecl;
;;     xmlHashTablePtr elemDecl;
;;     xmlHashTablePtr notaDecl;

;;     xmlHashTablePtr schemasImports;

;;     void *_private;        /* unused by the library for users or bindings */
;;     xmlHashTablePtr groupDecl;
;;     xmlDictPtr      dict;
;;     void *includes;     /* the includes, this is opaque for now */
;;     int preserve;        /* whether to free the document */
;;     int counter; /* used to give ononymous components unique names */
;;     xmlHashTablePtr idcDef; /* All identity-constraint defs. */
;;     void *volatiles; /* Obsolete */
;; };



;;;; incomplete XML Schemas structure implementation
;;
;;Header file "xmlschemas.h".
;;

(define-c-enumeration xmlSchemaValidError
  "xmlSchemaValidError"
  XML_SCHEMAS_ERR_OK
  XML_SCHEMAS_ERR_NOROOT
  XML_SCHEMAS_ERR_UNDECLAREDELEM
  XML_SCHEMAS_ERR_NOTTOPLEVEL
  XML_SCHEMAS_ERR_MISSING
  XML_SCHEMAS_ERR_WRONGELEM
  XML_SCHEMAS_ERR_NOTYPE
  XML_SCHEMAS_ERR_NOROLLBACK
  XML_SCHEMAS_ERR_ISABSTRACT
  XML_SCHEMAS_ERR_NOTEMPTY
  XML_SCHEMAS_ERR_ELEMCONT
  XML_SCHEMAS_ERR_HAVEDEFAULT
  XML_SCHEMAS_ERR_NOTNILLABLE
  XML_SCHEMAS_ERR_EXTRACONTENT
  XML_SCHEMAS_ERR_INVALIDATTR
  XML_SCHEMAS_ERR_INVALIDELEM
  XML_SCHEMAS_ERR_NOTDETERMINIST
  XML_SCHEMAS_ERR_CONSTRUCT
  XML_SCHEMAS_ERR_INTERNAL
  XML_SCHEMAS_ERR_NOTSIMPLE
  XML_SCHEMAS_ERR_ATTRUNKNOWN
  XML_SCHEMAS_ERR_ATTRINVALID
  XML_SCHEMAS_ERR_VALUE
  XML_SCHEMAS_ERR_FACET
  XML_SCHEMAS_ERR_
  XML_SCHEMAS_ERR_XXX)

(define-c-enumeration xmlSchemaValidOption
  "xmlSchemaValidOption"
  XML_SCHEMA_VAL_VC_I_CREATE)

(define-c-type-alias xmlSchema*			pointer)
(define-c-type-alias xmlSchemaPtr		pointer)

(define-c-type-alias xmlSchemaParserCtxt*	pointer)
(define-c-type-alias xmlSchemaParserCtxtPtr	pointer)

(define-c-type-alias xmlSchemaValidCtxt*	pointer)
(define-c-type-alias xmlSchemaValidCtxtPtr	pointer)

(define-c-type-alias xmlSchemaSAXPlugStruct*	pointer)
(define-c-type-alias xmlSchemaSAXPlugPtr	pointer)

(define-c-callback-pointer-type xmlSchemaValidityErrorFunc)
(define-c-callback-pointer-type xmlSchemaValidityWarningFunc)


;;;; implementation of XML Schema Datatypes
;;
;;Header file "xmlschemestypes.h".
;;

(define-c-enumeration xmlSchemaWhitespaceValueType
  "xmlSchemaWhitespaceValueType"
  XML_SCHEMA_WHITESPACE_UNKNOWN
  XML_SCHEMA_WHITESPACE_PRESERVE
  XML_SCHEMA_WHITESPACE_REPLACE
  XML_SCHEMA_WHITESPACE_COLLAPSE)



;;;; set of routines to process strings
;;
;;Header file "xmlstring.h".
;;

(define-c-type-alias xmlChar		unsigned-char)


;;;; Unicode character APIs
;;
;;Header file "xmlunicode.h".
;;


;;;; compile-time version informations
;;
;;Header file "xmlversion.h".
;;

(define-c-string-defines "string version symbols"
  LIBXML_DOTTED_VERSION
  LIBXML_VERSION_STRING
  LIBXML_VERSION_EXTRA
  LIBXML_MODULE_EXTENSION
  )

(define-c-defines "version symbols"
  LIBXML_VERSION
  WITH_TRIO
  WITHOUT_TRIO
  LIBXML_THREAD_ENABLED
  LIBXML_TREE_ENABLED
  LIBXML_OUTPUT_ENABLED
  LIBXML_PUSH_ENABLED
  LIBXML_READER_ENABLED
  LIBXML_PATTERN_ENABLED
  LIBXML_WRITER_ENABLED
  LIBXML_SAX1_ENABLED
  LIBXML_FTP_ENABLED
  LIBXML_HTTP_ENABLED
  LIBXML_VALID_ENABLED
  LIBXML_HTML_ENABLED
  LIBXML_LEGACY_ENABLED
  LIBXML_C14N_ENABLED
  LIBXML_CATALOG_ENABLED
  LIBXML_DOCB_ENABLED
  LIBXML_XPATH_ENABLED
  LIBXML_XPTR_ENABLED
  LIBXML_XINCLUDE_ENABLED
  LIBXML_ICONV_ENABLED
  LIBXML_ISO8859X_ENABLED
  LIBXML_DEBUG_ENABLED
  DEBUG_MEMORY_LOCATION
  LIBXML_DEBUG_RUNTIME
  LIBXML_UNICODE_ENABLED
  LIBXML_REGEXP_ENABLED
  LIBXML_AUTOMATA_ENABLED
  LIBXML_EXPR_ENABLED
  LIBXML_SCHEMAS_ENABLED
  LIBXML_SCHEMATRON_ENABLED
  LIBXML_MODULES_ENABLED
  LIBXML_ZLIB_ENABLED)


;;;; text writing API for XML
;;
;;Header file "xmlwriter.h".
;;

(define-c-type-alias xmlTextWriter*		pointer)
(define-c-type-alias xmlTextWriterPtr		pointer)


;;;; XML Path Language implementation
;;
;;Header file "xpath.h".
;;

(define-c-type-alias xmlXPathContext*		pointer)
(define-c-type-alias xmlXPathContextPtr		pointer)

(define-c-type-alias xmlXPathParserContext*	pointer)
(define-c-type-alias xmlXPathParserContextPtr	pointer)

(define-c-enumeration xmlXPathError
  "xmlXPathError"
  XPATH_EXPRESSION_OK
  XPATH_NUMBER_ERROR
  XPATH_UNFINISHED_LITERAL_ERROR
  XPATH_START_LITERAL_ERROR
  XPATH_VARIABLE_REF_ERROR
  XPATH_UNDEF_VARIABLE_ERROR
  XPATH_INVALID_PREDICATE_ERROR
  XPATH_EXPR_ERROR
  XPATH_UNCLOSED_ERROR
  XPATH_UNKNOWN_FUNC_ERROR
  XPATH_INVALID_OPERAND
  XPATH_INVALID_TYPE
  XPATH_INVALID_ARITY
  XPATH_INVALID_CTXT_SIZE
  XPATH_INVALID_CTXT_POSITION
  XPATH_MEMORY_ERROR
  XPTR_SYNTAX_ERROR
  XPTR_RESOURCE_ERROR
  XPTR_SUB_RESOURCE_ERROR
  XPATH_UNDEF_PREFIX_ERROR
  XPATH_ENCODING_ERROR
  XPATH_INVALID_CHAR_ERROR
  XPATH_INVALID_CTXT)

(define-c-type-alias xmlNodeSet*		pointer)
(define-c-type-alias xmlNodeSetPtr		pointer)

(define-c-struct xmlNodeSet
  "xmlNodeSet"
  (signed-int		nodeNr)
  (signed-int		nodeMax)
  (pointer		nodeTab))

(define-c-enumeration xmlXPathObjectType
  "xmlXPathObjectType"
  XPATH_UNDEFINED
  XPATH_NODESET
  XPATH_BOOLEAN
  XPATH_NUMBER
  XPATH_STRING
  XPATH_POINT
  XPATH_RANGE
  XPATH_LOCATIONSET
  XPATH_USERS
  XPATH_XSLT_TREE)

(define-c-type-alias xmlXPathObject*		pointer)
(define-c-type-alias xmlXPathObjectPtr		pointer)

(define-c-struct xmlXPathObject
  "xmlXPathObject"
  (signed-int		type)
  (pointer		nodesetval)
  (signed-int		boolval)
  (float		floatval)
  (pointer		stringval)
  (pointer		user)
  (signed-int		index)
  (pointer		user2)
  (signed-int		index2))

(define-c-type-alias xmlXPathType*		pointer)
(define-c-type-alias xmlXPathTypePtr		pointer)

(define-c-struct xmlXPathType
  "xmlXPathType"
  (pointer		name)
  (pointer		func))

(define-c-type-alias xmlXPathVariable*		pointer)
(define-c-type-alias xmlXPathVariablePtr	pointer)

(define-c-struct xmlXPathVariable
  "xmlXPathVariable"
  (pointer		name)
  (pointer		value))

(define-c-type-alias xmlXPathFunct*		pointer)
(define-c-type-alias xmlXPathFuncPtr		pointer)

(define-c-struct xmlXPathFunct
  "xmlXPathFunct"
  (pointer		name)
  (pointer		func))

(define-c-type-alias xmlXPathAxis*		pointer)
(define-c-type-alias xmlXPathAxisPtr		pointer)

(define-c-struct xmlXPathAxis
  "xmlXPathAxis"
  (pointer		name)
  (pointer		func))

(define-c-defines "xpath constants"
  XML_XPATH_CHECKNS
  XML_XPATH_NOVAR)

(define-c-struct xmlXPathContext
  "xmlXPathContext"
  (pointer		doc)
  (pointer		node)
  (signed-int		nb_variables_unused)
  (signed-int		max_variables_unused)
  (pointer		varHash)
  (signed-int		nb_types)
  (signed-int		max_types)
  (pointer		types)
  (signed-int		nb_funcs_unused)
  (signed-int		max_funcs_unused)
  (pointer		funcHash)
  (signed-int		nb_axis)
  (signed-int		max_axis)
  (pointer		axis)
  (pointer		namespaces)
  (signed-int		nsNr)
  (pointer		user)
  (signed-int		contextSize)
  (signed-int		proximityPosition)
  (signed-int		xptr)
  (pointer		here)
  (pointer		origin)
  (pointer		nsHash)
  (pointer		varLookupFunc)
  (pointer		varLookupData)
  (pointer		extra)
  (pointer		function)
  (pointer		functionURI)
  (pointer		funcLookupFunc)
  (pointer		funcLookupData)
  (pointer		tmpNsList)
  (signed-int		tmpNsNr)
  (pointer		userData)
  (pointer		error)
  (embedded		lastError)
  (pointer		debugNode)
  (pointer		dict)
  (signed-int		flags)
  (pointer		cache))

(define-c-type-alias xmlXPathCompExpr*		pointer)
(define-c-type-alias xmlXPathCompExprPtr	pointer)

(define-c-struct xmlXPathParserContext
  "xmlXPathParserContext"
  (pointer		cur)
  (pointer		base)
  (signed-int		error)
  (pointer		context)
  (pointer		value)
  (signed-int		valueNr)
  (signed-int		valueMax)
  (pointer		valueTab)
  (pointer		comp)
  (signed-int		xptr)
  (pointer		ancestor))

;; XMLPUBVAR double xmlXPathNAN;
;; XMLPUBVAR double xmlXPathPINF;
;; XMLPUBVAR double xmlXPathNINF;

;; #define xmlXPathNodeSetGetLength(ns) ((ns) ? (ns)->nodeNr : 0)
;; #define xmlXPathNodeSetItem(ns, index)				\
;; 		((((ns) != NULL) && 				\
;; 		  ((index) >= 0) && ((index) < (ns)->nodeNr)) ?	\
;; 		 (ns)->nodeTab[(index)]				\
;; 		 : NULL)
;; #define xmlXPathNodeSetIsEmpty(ns)                                      \
;;     (((ns) == NULL) || ((ns)->nodeNr == 0) || ((ns)->nodeTab == NULL))

(define-c-callback-pointer-type xmlXPathConvertFunc)
(define-c-callback-pointer-type xmlXPathEvalFunc)
(define-c-callback-pointer-type xmlXPathAxisFunc)
(define-c-callback-pointer-type xmlXPathFunction)
(define-c-callback-pointer-type xmlXPathVariableLookupFunc)
(define-c-callback-pointer-type xmlXPathFuncLookupFunc)



;;;; API to handle XML Pointers
;;
;;Header file "xpointer.h".
;;

(define-c-type-alias xmlLocationSet*		pointer)
(define-c-type-alias xmlLocationSetPtr		pointer)

(define-c-struct xmlLocationSet
  "xmlLocationSet"
  (signed-int		locNr)
  (signed-int		locMax)
  (pointer		locTab))


;;;; done

(define libxml2-library-spec
  '(foreign xml libxml2 sizeof))

(define-shared-object libxml2 libxml2.so)

(autoconf-lib-write "configuration/libxml2-inspector.m4" libxml2-library-spec)
(sizeof-lib-write   "src/libraries/foreign/xml/libxml2/sizeof.sls.in" libxml2-library-spec)

;;; end of file
