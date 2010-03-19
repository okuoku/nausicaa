;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Libxml2
;;;Contents: bindings to foreign functions
;;;Date: Thu Dec 10, 2009
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


(library (xml libxml2 platform)
  (export

    ;; Canonical XML and Exclusive XML Canonicalization
    xmlC14NDocSaveTo
    xmlC14NDocDumpMemory
    xmlC14NDocSave
    xmlC14NExecute

    ;; Unicode character range checking
    xmlCharInRange
    xmlIsBaseChar
    xmlIsBlank
    xmlIsChar
    xmlIsCombining
    xmlIsDigit
    xmlIsExtender
    xmlIsIdeographic
    xmlIsPubidChar

    xmlIsBaseChar_ch
    xmlIsBaseCharQ
    xmlIsBaseCharGroup
    xmlIsBlank_ch
    xmlIsBlankQ
    xmlIsChar_ch
    xmlIsCharQ
    xmlIsCharGroup
    xmlIsCombiningQ
    xmlIsCombiningGroup
    xmlIsDigit_ch
    xmlIsDigitQ
    xmlIsDigitGroup
    xmlIsExtender_ch
    xmlIsExtenderQ
    xmlIsExtenderGroup
    xmlIsIdeographicQ
    xmlIsIdeographicGroup
    xmlIsPubidChar_tab
    xmlIsPubidChar_ch
    xmlIsPubidCharQ

    ;; Interfaces to the Catalog handling system
    xmlNewCatalog
    xmlLoadACatalog
    xmlLoadSGMLSuperCatalog
    xmlConvertSGMLCatalog
    xmlACatalogAdd
    xmlACatalogRemove
    xmlACatalogResolve
    xmlACatalogResolveSystem
    xmlACatalogResolvePublic
    xmlACatalogResolveURI
    xmlACatalogDump
    xmlFreeCatalog
    xmlCatalogIsEmpty
    xmlInitializeCatalog
    xmlLoadCatalog
    xmlLoadCatalogs
    xmlCatalogCleanup
    xmlCatalogDump
    xmlCatalogResolve
    xmlCatalogResolveSystem
    xmlCatalogResolvePublic
    xmlCatalogResolveURI
    xmlCatalogAdd
    xmlCatalogRemove
    xmlParseCatalogFile
    xmlCatalogConvert
    xmlCatalogFreeLocal
    xmlCatalogAddLocal
    xmlCatalogLocalResolve
    xmlCatalogLocalResolveURI
    xmlCatalogSetDebug
    xmlCatalogSetDefaultPrefer
    xmlCatalogSetDefaults
    xmlCatalogGetDefaults
    xmlCatalogGetSystem
    xmlCatalogGetPublic

    ;; Tree debugging APIs
    xmlDebugDumpString
    xmlDebugDumpAttr
    xmlDebugDumpAttrList
    xmlDebugDumpOneNode
    xmlDebugDumpNode
    xmlDebugDumpNodeList
    xmlDebugDumpDocumentHead
    xmlDebugDumpDocument
    xmlDebugDumpDTD
    xmlDebugDumpEntities
    xmlDebugCheckDocument
    xmlLsOneNode
    xmlLsCountNode
    xmlBoolToText
    xmlShellPrintXPathError
    xmlShellPrintXPathResult
    xmlShellList
    xmlShellBase
    xmlShellDir
    xmlShellLoad
    xmlShellPrintNode
    xmlShellCat
    xmlShellWrite
    xmlShellSave
    xmlShellValidate
    xmlShellDu
    xmlShellPwd
    xmlShell

    ;; string dictionary
    xmlDictCreate
    xmlDictCreateSub
    xmlDictReference
    xmlDictFree
    xmlDictLookup
    xmlDictExists
    xmlDictQLookup
    xmlDictOwns
    xmlDictSize
    xmlDictCleanup

    ;; interface for the encoding conversion functions
    xmlInitCharEncodingHandlers
    xmlCleanupCharEncodingHandlers
    xmlRegisterCharEncodingHandler
    xmlGetCharEncodingHandler
    xmlFindCharEncodingHandler
    xmlNewCharEncodingHandler
    xmlAddEncodingAlias
    xmlDelEncodingAlias
    xmlGetEncodingAlias
    xmlCleanupEncodingAliases
    xmlParseCharEncoding
    xmlGetCharEncodingName
    xmlDetectCharEncoding
    xmlCharEncOutFunc
    xmlCharEncInFunc
    xmlCharEncFirstLine
    xmlCharEncCloseFunc
    UTF8Toisolat1
    isolat1ToUTF8

    ;; interface for the XML entities handling
    xmlInitializePredefinedEntities
    xmlNewEntity
    xmlAddDocEntity
    xmlAddDtdEntity
    xmlGetPredefinedEntity
    xmlGetDocEntity
    xmlGetDtdEntity
    xmlGetParameterEntity
    xmlEncodeEntities
    xmlEncodeEntitiesReentrant
    xmlEncodeSpecialChars
    xmlCreateEntitiesTable
    xmlCopyEntitiesTable
    xmlFreeEntitiesTable
    xmlDumpEntitiesTable
    xmlDumpEntityDecl
    xmlCleanupPredefinedEntities

    ;; interface for all global variables of the library
    xmlInitGlobals
    xmlCleanupGlobals
    xmlParserInputBufferCreateFilenameDefault
    xmlOutputBufferCreateFilenameDefault
    xmlInitializeGlobalState
    xmlThrDefSetGenericErrorFunc
    xmlThrDefSetStructuredErrorFunc
    xmlRegisterNodeDefault
    xmlThrDefRegisterNodeDefault
    xmlDeregisterNodeDefault
    xmlThrDefDeregisterNodeDefault
    xmlThrDefOutputBufferCreateFilenameDefault
    xmlThrDefParserInputBufferCreateFilenameDefault
    xmlMalloc
    xmlMallocAtomic
    xmlRealloc
    xmlFree
    xmlMemStrdup

    ;; chained hash tables
    xmlHashCreate
    xmlHashCreateDict
    xmlHashFree
    xmlHashAddEntry
    xmlHashUpdateEntry
    xmlHashAddEntry2
    xmlHashUpdateEntry2
    xmlHashAddEntry3
    xmlHashUpdateEntry3
    xmlHashRemoveEntry
    xmlHashRemoveEntry2
    xmlHashRemoveEntry3
    xmlHashLookup
    xmlHashLookup2
    xmlHashLookup3
    xmlHashQLookup
    xmlHashQLookup2
    xmlHashQLookup3
    xmlHashCopy
    xmlHashSize
    xmlHashScan
    xmlHashScan3
    xmlHashScanFull
    xmlHashScanFull3

    ;; HTML parser
    htmlTagLookup
    htmlEntityLookup
    htmlEntityValueLookup
    htmlIsAutoClosed
    htmlAutoCloseTag
    htmlParseEntityRef
    htmlParseCharRef
    htmlParseElement
    htmlNewParserCtxt
    htmlCreateMemoryParserCtxt
    htmlParseDocument
    htmlSAXParseDoc
    htmlParseDoc
    htmlSAXParseFile
    htmlParseFile
    UTF8ToHtml
    htmlEncodeEntities
    htmlIsScriptAttribute
    htmlHandleOmittedElem
    htmlCreatePushParserCtxt
    htmlParseChunk
    htmlFreeParserCtxt
    htmlCtxtReset
    htmlCtxtUseOptions
    htmlReadDoc
    htmlReadFile
    htmlReadMemory
    htmlReadFd
    htmlReadIO
    htmlCtxtReadDoc
    htmlCtxtReadFile
    htmlCtxtReadMemory
    htmlCtxtReadFd
    htmlCtxtReadIO
    htmlAttrAllowed
    htmlElementAllowedHere
    htmlElementStatusHere
    htmlNodeStatus

    ;; specific APIs to process HTML tree, especially serialization
    htmlNewDoc
    htmlNewDocNoDtD
    htmlGetMetaEncoding
    htmlSetMetaEncoding
    htmlDocDumpMemory
    htmlDocDumpMemoryFormat
    htmlDocDump
    htmlSaveFile
    htmlNodeDump
    htmlNodeDumpFile
    htmlNodeDumpFileFormat
    htmlSaveFileEnc
    htmlSaveFileFormat
    htmlNodeDumpFormatOutput
    htmlDocContentDumpOutput
    htmlDocContentDumpFormatOutput
    htmlNodeDumpOutput
    htmlIsBooleanAttr

    ;; lists interfaces
    xmlListCreate
    xmlListDelete
    xmlListSearch
    xmlListReverseSearch
    xmlListInsert
    xmlListAppend
    xmlListRemoveFirst
    xmlListRemoveLast
    xmlListRemoveAll
    xmlListClear
    xmlListEmpty
    xmlListFront
    xmlListEnd
    xmlListSize
    xmlListPopFront
    xmlListPopBack
    xmlListPushFront
    xmlListPushBack
    xmlListReverse
    xmlListSort
    xmlListWalk
    xmlListReverseWalk
    xmlListMerge
    xmlListDup
    xmlListCopy
    xmlLinkGetData

    ;; minimal FTP implementation
    xmlNanoFTPInit
    xmlNanoFTPCleanup
    xmlNanoFTPNewCtxt
    xmlNanoFTPFreeCtxt
    xmlNanoFTPConnectTo
    xmlNanoFTPOpen
    xmlNanoFTPConnect
    xmlNanoFTPClose
    xmlNanoFTPQuit
    xmlNanoFTPScanProxy
    xmlNanoFTPProxy
    xmlNanoFTPUpdateURL
    xmlNanoFTPGetResponse
    xmlNanoFTPCheckResponse
    xmlNanoFTPCwd
    xmlNanoFTPDele
    xmlNanoFTPGetConnection
    xmlNanoFTPCloseConnection
    xmlNanoFTPList
    xmlNanoFTPGetSocket
    xmlNanoFTPGet
    xmlNanoFTPRead

    ;; minimal HTTP implementation
    xmlNanoHTTPInit
    xmlNanoHTTPCleanup
    xmlNanoHTTPScanProxy
    xmlNanoHTTPFetch
    xmlNanoHTTPMethod
    xmlNanoHTTPMethodRedir
    xmlNanoHTTPOpen
    xmlNanoHTTPOpenRedir
    xmlNanoHTTPReturnCode
    xmlNanoHTTPAuthHeader
    xmlNanoHTTPRedir
    xmlNanoHTTPContentLength
    xmlNanoHTTPEncoding
    xmlNanoHTTPMimeType
    xmlNanoHTTPRead
    xmlNanoHTTPSave
    xmlNanoHTTPClose

    ;; the core parser module
    xmlInitParser
    xmlCleanupParser
    xmlParserInputRead
    xmlParserInputGrow
    xmlParseDoc
    xmlParseFile
    xmlParseMemory
    xmlSubstituteEntitiesDefault
    xmlKeepBlanksDefault
    xmlStopParser
    xmlPedanticParserDefault
    xmlLineNumbersDefault
    xmlRecoverDoc
    xmlRecoverMemory
    xmlRecoverFile
    xmlParseDocument
    xmlParseExtParsedEnt
    xmlSAXUserParseFile
    xmlSAXUserParseMemory
    xmlSAXParseDoc
    xmlSAXParseMemory
    xmlSAXParseMemoryWithData
    xmlSAXParseFile
    xmlSAXParseFileWithData
    xmlSAXParseEntity
    xmlParseEntity
    xmlSAXParseDTD
    xmlParseDTD
    xmlIOParseDTD
    xmlParseBalancedChunkMemory
    xmlParseInNodeContext
    xmlParseBalancedChunkMemoryRecover
    xmlParseExternalEntity
    xmlParseCtxtExternalEntity
    xmlNewParserCtxt
    xmlInitParserCtxt
    xmlClearParserCtxt
    xmlFreeParserCtxt
    xmlSetupParserForBuffer
    xmlCreateDocParserCtxt
    xmlGetFeaturesList
    xmlGetFeature
    xmlSetFeature
    xmlCreatePushParserCtxt
    xmlParseChunk
    xmlCreateIOParserCtxt
    xmlNewIOInputStream
    xmlParserFindNodeInfo
    xmlInitNodeInfoSeq
    xmlClearNodeInfoSeq
    xmlParserFindNodeInfoIndex
    xmlParserAddNodeInfo
    xmlSetExternalEntityLoader
    xmlGetExternalEntityLoader
    xmlLoadExternalEntity
    xmlByteConsumed
    xmlCtxtReset
    xmlCtxtResetPush
    xmlCtxtUseOptions
    xmlReadDoc
    xmlReadFile
    xmlReadMemory
    xmlReadFd
    xmlReadIO
    xmlCtxtReadDoc
    xmlCtxtReadFile
    xmlCtxtReadMemory
    xmlCtxtReadFd
    xmlCtxtReadIO
    xmlHasFeature

    ;; pattern expression handling
    xmlFreePattern
    xmlFreePatternList
    xmlPatterncompile
    xmlPatternMatch
    xmlPatternStreamable
    xmlPatternMaxDepth
    xmlPatternMinDepth
    xmlPatternFromRoot
    xmlPatternGetStreamCtxt
    xmlFreeStreamCtxt
    xmlStreamPushNode
    xmlStreamPush
    xmlStreamPushAttr
    xmlStreamPop
    xmlStreamWantsAnyNode

    ;; implementation of the Relax-NG validation
    xmlRelaxNGInitTypes
    xmlRelaxNGCleanupTypes
    xmlRelaxNGNewParserCtxt
    xmlRelaxNGNewMemParserCtxt
    xmlRelaxNGNewDocParserCtxt
    xmlRelaxParserSetFlag
    xmlRelaxNGFreeParserCtxt
    xmlRelaxNGSetParserErrors
    xmlRelaxNGGetParserErrors
    xmlRelaxNGSetParserStructuredErrors
    xmlRelaxNGParse
    xmlRelaxNGFree
    xmlRelaxNGDump
    xmlRelaxNGDumpTree
    xmlRelaxNGSetValidErrors
    xmlRelaxNGGetValidErrors
    xmlRelaxNGSetValidStructuredErrors
    xmlRelaxNGNewValidCtxt
    xmlRelaxNGFreeValidCtxt
    xmlRelaxNGValidateDoc
    xmlRelaxNGValidatePushElement
    xmlRelaxNGValidatePushCData
    xmlRelaxNGValidatePopElement
    xmlRelaxNGValidateFullElement

    ;; SAX2 parser interface used to build the DOM tree
    xmlSAX2GetPublicId
    xmlSAX2GetSystemId
    xmlSAX2SetDocumentLocator
    xmlSAX2GetLineNumber
    xmlSAX2GetColumnNumber
    xmlSAX2IsStandalone
    xmlSAX2HasInternalSubset
    xmlSAX2HasExternalSubset
    xmlSAX2InternalSubset
    xmlSAX2ExternalSubset
    xmlSAX2GetEntity
    xmlSAX2GetParameterEntity
    xmlSAX2ResolveEntity
    xmlSAX2EntityDecl
    xmlSAX2AttributeDecl
    xmlSAX2ElementDecl
    xmlSAX2NotationDecl
    xmlSAX2UnparsedEntityDecl
    xmlSAX2StartDocument
    xmlSAX2EndDocument
    xmlSAX2StartElement
    xmlSAX2EndElement
    xmlSAX2StartElementNs
    xmlSAX2EndElementNs
    xmlSAX2Reference
    xmlSAX2Characters
    xmlSAX2IgnorableWhitespace
    xmlSAX2ProcessingInstruction
    xmlSAX2Comment
    xmlSAX2CDataBlock
    xmlSAXDefaultVersion
    xmlSAXVersion
    xmlSAX2InitDefaultSAXHandler
    xmlSAX2InitHtmlDefaultSAXHandler
    htmlDefaultSAXHandlerInit
    xmlSAX2InitDocbDefaultSAXHandler
    docbDefaultSAXHandlerInit
    xmlDefaultSAXHandlerInit

    ;; XML Schematron implementation
    xmlSchematronNewParserCtxt
    xmlSchematronNewMemParserCtxt
    xmlSchematronNewDocParserCtxt
    xmlSchematronFreeParserCtxt
;;; xmlSchematronSetParserErrors
;;; xmlSchematronGetParserErrors
;;; xmlSchematronIsValid
    xmlSchematronParse
    xmlSchematronFree
    xmlSchematronSetValidStructuredErrors
;;;    xmlSchematronSetValidErrors
;;;    xmlSchematronGetValidErrors
;;;    xmlSchematronSetValidOptions
;;;    xmlSchematronValidCtxtGetOptions
;;;    xmlSchematronValidateOneElement
    xmlSchematronNewValidCtxt
    xmlSchematronFreeValidCtxt
    xmlSchematronValidateDoc

    ;; interfaces for thread handling
    xmlNewMutex
    xmlMutexLock
    xmlMutexUnlock
    xmlFreeMutex
    xmlNewRMutex
    xmlRMutexLock
    xmlRMutexUnlock
    xmlFreeRMutex
    xmlInitThreads
    xmlLockLibrary
    xmlUnlockLibrary
    xmlGetThreadId
    xmlIsMainThread
    xmlCleanupThreads
    xmlGetGlobalState
;;;    xmlDllMain

    ;; interfaces for tree manipulation
    xmlValidateNCName
    xmlValidateQName
    xmlValidateName
    xmlValidateNMToken
    xmlBuildQName
    xmlSplitQName2
    xmlSplitQName3
    xmlSetBufferAllocationScheme
    xmlGetBufferAllocationScheme
    xmlBufferCreate
    xmlBufferCreateSize
    xmlBufferCreateStatic
    xmlBufferResize
    xmlBufferFree
    xmlBufferDump
    xmlBufferAdd
    xmlBufferAddHead
    xmlBufferCat
    xmlBufferCCat
    xmlBufferShrink
    xmlBufferGrow
    xmlBufferEmpty
    xmlBufferContent
    xmlBufferSetAllocationScheme
    xmlBufferLength
    xmlCreateIntSubset
    xmlNewDtd
    xmlGetIntSubset
    xmlFreeDtd
    xmlNewGlobalNs
    xmlNewNs
    xmlFreeNs
    xmlFreeNsList
    xmlNewDoc
    xmlFreeDoc
    xmlNewDocProp
    xmlNewProp
    xmlNewNsProp
    xmlNewNsPropEatName
    xmlFreePropList
    xmlFreeProp
    xmlCopyProp
    xmlCopyPropList
    xmlCopyDtd
    xmlCopyDoc
    xmlNewDocNode
    xmlNewDocNodeEatName
    xmlNewNode
    xmlNewNodeEatName
    xmlNewChild
    xmlNewDocText
    xmlNewText
    xmlNewDocPI
    xmlNewPI
    xmlNewDocTextLen
    xmlNewTextLen
    xmlNewDocComment
    xmlNewComment
    xmlNewCDataBlock
    xmlNewCharRef
    xmlNewReference
    xmlCopyNode
    xmlDocCopyNode
    xmlDocCopyNodeList
    xmlCopyNodeList
    xmlNewTextChild
    xmlNewDocRawNode
    xmlNewDocFragment
    xmlGetLineNo
    xmlGetNodePath
    xmlDocGetRootElement
    xmlGetLastChild
    xmlNodeIsText
    xmlIsBlankNode
    xmlDocSetRootElement
    xmlNodeSetName
    xmlAddChild
    xmlAddChildList
    xmlReplaceNode
    xmlAddPrevSibling
    xmlAddSibling
    xmlAddNextSibling
    xmlUnlinkNode
    xmlTextMerge
    xmlTextConcat
    xmlFreeNodeList
    xmlFreeNode
    xmlSetTreeDoc
    xmlSetListDoc
    xmlSearchNs
    xmlSearchNsByHref
    xmlGetNsList
    xmlSetNs
    xmlCopyNamespace
    xmlCopyNamespaceList
    xmlSetProp
    xmlSetNsProp
    xmlGetNoNsProp
    xmlGetProp
    xmlHasProp
    xmlHasNsProp
    xmlGetNsProp
    xmlStringGetNodeList
    xmlStringLenGetNodeList
    xmlNodeListGetString
    xmlNodeListGetRawString
    xmlNodeSetContent
    xmlNodeSetContentLen
    xmlNodeAddContent
    xmlNodeAddContentLen
    xmlNodeGetContent
    xmlNodeBufGetContent
    xmlNodeGetLang
    xmlNodeGetSpacePreserve
    xmlNodeSetLang
    xmlNodeSetSpacePreserve
    xmlNodeGetBase
    xmlNodeSetBase
    xmlRemoveProp
    xmlUnsetNsProp
    xmlUnsetProp
    xmlBufferWriteCHAR
    xmlBufferWriteChar
    xmlBufferWriteQuotedString
    xmlDocPtr
    xmlReconciliateNs
    xmlDocDumpFormatMemory
    xmlDocDumpMemory
    xmlDocDumpMemoryEnc
    xmlDocDumpFormatMemoryEnc
    xmlDocFormatDump
    xmlDocDump
    xmlElemDump
    xmlSaveFile
    xmlSaveFormatFile
    xmlNodeDump
    xmlSaveFileTo
    xmlSaveFormatFileTo
    xmlNodeDumpOutput
    xmlSaveFormatFileEnc
    xmlSaveFileEnc
    xmlIsXHTML
    xmlGetDocCompressMode
    xmlSetDocCompressMode
    xmlGetCompressMode
    xmlSetCompressMode
    xmlDOMWrapNewCtxt
    xmlDOMWrapFreeCtxt
    xmlDOMWrapReconcileNamespaces
    xmlDOMWrapAdoptNode
    xmlDOMWrapRemoveNode
    xmlDOMWrapCloneNode
    xmlChildElementCount
    xmlNextElementSibling
    xmlFirstElementChild
    xmlLastElementChild
    xmlPreviousElementSibling

    ;; library of generic URI related routines
    xmlCreateURI
    xmlBuildURI
    xmlBuildRelativeURI
    xmlParseURI
    xmlParseURIRaw
    xmlParseURIReference
    xmlSaveUri
    xmlPrintURI
    xmlURIEscapeStr
    xmlURIUnescapeString
    xmlNormalizeURIPath
    xmlURIEscape
    xmlFreeURI
    xmlCanonicPath
    xmlPathToURI

    ;; The DTD validation
    xmlAddNotationDecl
    xmlCopyNotationTable
    xmlFreeNotationTable
    xmlDumpNotationDecl
    xmlDumpNotationTable
    xmlNewElementContent
    xmlCopyElementContent
    xmlFreeElementContent
    xmlNewDocElementContent
    xmlCopyDocElementContent
    xmlFreeDocElementContent
    xmlSnprintfElementContent
    xmlSprintfElementContent
    xmlAddElementDecl
    xmlCopyElementTable
    xmlFreeElementTable
    xmlDumpElementTable
    xmlDumpElementDecl
    xmlCreateEnumeration
    xmlFreeEnumeration
    xmlCopyEnumeration
    xmlAddAttributeDecl
    xmlCopyAttributeTable
    xmlFreeAttributeTable
    xmlDumpAttributeTable
    xmlDumpAttributeDecl
    xmlAddID
    xmlFreeIDTable
    xmlGetID
    xmlIsID
    xmlRemoveID
    xmlAddRef
    xmlFreeRefTable
    xmlIsRef
    xmlRemoveRef
    xmlGetRefs
    xmlNewValidCtxt
    xmlFreeValidCtxt
    xmlValidateRoot
    xmlValidateElementDecl
    xmlValidNormalizeAttributeValue
    xmlValidCtxtNormalizeAttributeValue
    xmlValidateAttributeDecl
    xmlValidateAttributeValue
    xmlValidateNotationDecl
    xmlValidateDtd
    xmlValidateDtdFinal
    xmlValidateDocument
    xmlValidateElement
    xmlValidateOneElement
    xmlValidateOneAttribute
    xmlValidateOneNamespace
    xmlValidateDocumentFinal
    xmlValidateNotationUse
    xmlIsMixedElement
    xmlGetDtdAttrDesc
    xmlGetDtdQAttrDesc
    xmlGetDtdNotationDesc
    xmlGetDtdQElementDesc
    xmlGetDtdElementDesc
    xmlValidGetPotentialChildren
    xmlValidGetValidElements
    xmlValidateNameValue
    xmlValidateNamesValue
    xmlValidateNmtokenValue
    xmlValidateNmtokensValue
    xmlValidBuildContentModel
    xmlValidatePushElement
    xmlValidatePushCData
    xmlValidatePopElement

    ;; implementation of XInclude
    xmlXIncludeProcess
    xmlXIncludeProcessFlags
    xmlXIncludeProcessFlagsData
    xmlXIncludeProcessTreeFlagsData
    xmlXIncludeProcessTree
    xmlXIncludeProcessTreeFlags
    xmlXIncludeNewContext
    xmlXIncludeSetFlags
    xmlXIncludeFreeContext
    xmlXIncludeProcessNode

    ;; API to build regexp automata
    xmlNewAutomata
    xmlFreeAutomata
    xmlAutomataGetInitState
    xmlAutomataSetFinalState
    xmlAutomataNewState
    xmlAutomataNewTransition
    xmlAutomataNewTransition2
    xmlAutomataNewNegTrans
    xmlAutomataNewCountTrans
    xmlAutomataNewCountTrans2
    xmlAutomataNewOnceTrans
    xmlAutomataNewOnceTrans2
    xmlAutomataNewAllTrans
    xmlAutomataNewEpsilon
    xmlAutomataNewCountedTrans
    xmlAutomataNewCounterTrans
    xmlAutomataNewCounter
    xmlAutomataCompile
    xmlAutomataIsDeterminist

    ;; error handling
    xmlSetGenericErrorFunc
    initGenericErrorDefaultFunc
    xmlSetStructuredErrorFunc
    xmlParserPrintFileInfo
    xmlParserPrintFileContext
    xmlGetLastError
    xmlResetLastError
    xmlCtxtGetLastError
    xmlCtxtResetLastError
    xmlResetError
    xmlCopyError

    ;; interface for the I/O interfaces used by the parser
    xmlCleanupInputCallbacks
    xmlPopInputCallbacks
    xmlRegisterDefaultInputCallbacks
    xmlAllocParserInputBuffer
    xmlParserInputBufferCreateFilename
    xmlParserInputBufferCreateFile
    xmlParserInputBufferCreateFd
    xmlParserInputBufferCreateMem
    xmlParserInputBufferCreateStatic
    xmlParserInputBufferCreateIO
    xmlParserInputBufferRead
    xmlParserInputBufferGrow
    xmlParserInputBufferPush
    xmlFreeParserInputBuffer
    xmlParserGetDirectory
    xmlRegisterInputCallbacks
    __xmlParserInputBufferCreateFilename
    xmlCleanupOutputCallbacks
    xmlRegisterDefaultOutputCallbacks
    xmlAllocOutputBuffer
    xmlOutputBufferCreateFilename
    xmlOutputBufferCreateFile
    xmlOutputBufferCreateBuffer
    xmlOutputBufferCreateFd
    xmlOutputBufferCreateIO
    xmlOutputBufferWrite
    xmlOutputBufferWriteString
    xmlOutputBufferWriteEscape
    xmlOutputBufferFlush
    xmlOutputBufferClose
    xmlRegisterOutputCallbacks
    __xmlOutputBufferCreateFilename
    xmlRegisterHTTPPostCallbacks
    xmlCheckHTTPInput
    xmlNoNetExternalEntityLoader
    xmlNormalizeWindowsPath
    xmlCheckFilename
    xmlFileMatch
    xmlFileOpen
    xmlFileRead
    xmlFileClose
    xmlIOHTTPMatch
    xmlIOHTTPOpen
    xmlIOHTTPOpenW
    xmlIOHTTPRead
    xmlIOHTTPClose
    xmlIOFTPMatch
    xmlIOFTPOpen
    xmlIOFTPRead
    xmlIOFTPClose

    ;; interface for the memory allocator
    xmlMemSetup
    xmlMemGet
    xmlGcMemSetup
    xmlGcMemGet
    xmlInitMemory
    xmlCleanupMemory
    xmlMemUsed
    xmlMemBlocks
    xmlMemDisplay
    xmlMemDisplayLast
    xmlMemShow
    xmlMemoryDump
    xmlMemMalloc
    xmlMemRealloc
    xmlMemFree
    xmlMemoryStrdup
    xmlMallocLoc
    xmlReallocLoc
    xmlMallocAtomicLoc
    xmlMemStrdupLoc

    ;; dynamic module loading
    xmlModuleOpen
    xmlModuleSymbol
    xmlModuleClose
    xmlModuleFree

    ;; the XMLReader implementation
    xmlNewTextReader
    xmlNewTextReaderFilename
    xmlFreeTextReader
    xmlTextReaderSetup
    xmlTextReaderRead
    xmlTextReaderReadInnerXml
    xmlTextReaderReadOuterXml
    xmlTextReaderReadString
    xmlTextReaderReadAttributeValue
    xmlTextReaderAttributeCount
    xmlTextReaderDepth
    xmlTextReaderHasAttributes
    xmlTextReaderHasValue
    xmlTextReaderIsDefault
    xmlTextReaderIsEmptyElement
    xmlTextReaderNodeType
    xmlTextReaderQuoteChar
    xmlTextReaderReadState
    xmlTextReaderIsNamespaceDecl
    xmlTextReaderConstBaseUri
    xmlTextReaderConstLocalName
    xmlTextReaderConstName
    xmlTextReaderConstNamespaceUri
    xmlTextReaderConstPrefix
    xmlTextReaderConstXmlLang
    xmlTextReaderConstString
    xmlTextReaderConstValue
    xmlTextReaderBaseUri
    xmlTextReaderLocalName
    xmlTextReaderName
    xmlTextReaderNamespaceUri
    xmlTextReaderPrefix
    xmlTextReaderXmlLang
    xmlTextReaderValue
    xmlTextReaderClose
    xmlTextReaderGetAttributeNo
    xmlTextReaderGetAttribute
    xmlTextReaderGetAttributeNs
    xmlTextReaderGetRemainder
    xmlTextReaderLookupNamespace
    xmlTextReaderMoveToAttributeNo
    xmlTextReaderMoveToAttribute
    xmlTextReaderMoveToAttributeNs
    xmlTextReaderMoveToFirstAttribute
    xmlTextReaderMoveToNextAttribute
    xmlTextReaderMoveToElement
    xmlTextReaderNormalization
    xmlTextReaderConstEncoding
    xmlTextReaderSetParserProp
    xmlTextReaderGetParserProp
    xmlTextReaderCurrentNode
    xmlTextReaderGetParserLineNumber
    xmlTextReaderGetParserColumnNumber
    xmlTextReaderPreserve
    xmlTextReaderPreservePattern
    xmlTextReaderCurrentDoc
    xmlTextReaderExpand
    xmlTextReaderNext
    xmlTextReaderNextSibling
    xmlTextReaderIsValid
    xmlTextReaderRelaxNGValidate
    xmlTextReaderRelaxNGSetSchema
    xmlTextReaderSchemaValidate
    xmlTextReaderSchemaValidateCtxt
    xmlTextReaderSetSchema
    xmlTextReaderConstXmlVersion
    xmlTextReaderStandalone
    xmlTextReaderByteConsumed
    xmlReaderWalker
    xmlReaderForDoc
    xmlReaderForFile
    xmlReaderForMemory
    xmlReaderForFd
    xmlReaderForIO
    xmlReaderNewWalker
    xmlReaderNewDoc
    xmlReaderNewFile
    xmlReaderNewMemory
    xmlReaderNewFd
    xmlReaderNewIO
    xmlTextReaderLocatorLineNumber
    xmlTextReaderLocatorBaseURI
    xmlTextReaderSetErrorHandler
    xmlTextReaderSetStructuredErrorHandler
    xmlTextReaderGetErrorHandler

    ;; regular expressions handling
    xmlRegexpCompile
    xmlRegFreeRegexp
    xmlRegexpExec
    xmlRegexpPrint
    xmlRegexpIsDeterminist
    xmlRegNewExecCtxt
    xmlRegFreeExecCtxt
    xmlRegExecPushString
    xmlRegExecPushString2
    xmlRegExecNextValues
    xmlRegExecErrInfo
    xmlExpFreeCtxt
    xmlExpNewCtxt
    xmlExpCtxtNbNodes
    xmlExpCtxtNbCons
    xmlExpFree
    xmlExpRef
    xmlExpParse
    xmlExpNewAtom
    xmlExpNewOr
    xmlExpNewSeq
    xmlExpNewRange
    xmlExpIsNillable
    xmlExpMaxToken
    xmlExpGetLanguage
    xmlExpGetStart
    xmlExpStringDerive
    xmlExpExpDerive
    xmlExpSubsume
    xmlExpDump

    ;; the XML document serializer
    xmlSaveToFd
    xmlSaveToFilename
    xmlSaveToBuffer
    xmlSaveToIO
    xmlSaveDoc
    xmlSaveTree
    xmlSaveFlush
    xmlSaveClose
    xmlSaveSetEscape
    xmlSaveSetAttrEscape

    ;; internal interfaces for XML Schemas
    xmlSchemaFreeType
    xmlSchemaFreeWildcard

    ;; incomplete XML Schemas structure implementation
    xmlSchemaNewParserCtxt
    xmlSchemaNewMemParserCtxt
    xmlSchemaNewDocParserCtxt
    xmlSchemaFreeParserCtxt
    xmlSchemaSetParserErrors
    xmlSchemaSetParserStructuredErrors
    xmlSchemaGetParserErrors
    xmlSchemaIsValid
    xmlSchemaParse
    xmlSchemaFree
    xmlSchemaDump
    xmlSchemaSetValidErrors
    xmlSchemaSetValidStructuredErrors
    xmlSchemaGetValidErrors
    xmlSchemaSetValidOptions
    xmlSchemaValidCtxtGetOptions
    xmlSchemaNewValidCtxt
    xmlSchemaFreeValidCtxt
    xmlSchemaValidateDoc
    xmlSchemaValidateOneElement
    xmlSchemaValidateStream
    xmlSchemaValidateFile
    xmlSchemaValidCtxtGetParserCtxt
    xmlSchemaSAXPlug
    xmlSchemaSAXUnplug

    ;; implementation of XML Schema Datatypes
    xmlSchemaInitTypes
    xmlSchemaCleanupTypes
    xmlSchemaGetPredefinedType
    xmlSchemaValidatePredefinedType
    xmlSchemaValPredefTypeNode
    xmlSchemaValidateFacet
    xmlSchemaValidateFacetWhtsp
    xmlSchemaFreeValue
    xmlSchemaNewFacet
    xmlSchemaCheckFacet
    xmlSchemaFreeFacet
    xmlSchemaCompareValues
    xmlSchemaGetBuiltInListSimpleTypeItemType
    xmlSchemaValidateListSimpleTypeFacet
    xmlSchemaGetBuiltInType
    xmlSchemaIsBuiltInTypeFacet
    xmlSchemaCollapseString
    xmlSchemaWhiteSpaceReplace
    xmlSchemaGetFacetValueAsULong
    xmlSchemaValidateLengthFacet
    xmlSchemaValidateLengthFacetWhtsp
    xmlSchemaValPredefTypeNodeNoNorm
    xmlSchemaGetCanonValue
    xmlSchemaGetCanonValueWhtsp
    xmlSchemaValueAppend
    xmlSchemaValueGetNext
    xmlSchemaValueGetAsString
    xmlSchemaValueGetAsBoolean
    xmlSchemaNewStringValue
    xmlSchemaNewNOTATIONValue
    xmlSchemaNewQNameValue
    xmlSchemaCompareValuesWhtsp
    xmlSchemaCopyValue
    xmlSchemaGetValType

    ;; set of routines to process strings
    xmlStrdup
    xmlStrndup
    xmlCharStrndup
    xmlCharStrdup
    xmlStrsub
    xmlStrchr
    xmlStrstr
    xmlStrcasestr
    xmlStrcmp
    xmlStrncmp
    xmlStrcasecmp
    xmlStrncasecmp
    xmlStrEqual
    xmlStrQEqual
    xmlStrlen
    xmlStrcat
    xmlStrncat
    xmlStrncatNew
    xmlGetUTF8Char
    xmlCheckUTF8
    xmlUTF8Strsize
    xmlUTF8Strndup
    xmlUTF8Strpos
    xmlUTF8Strloc
    xmlUTF8Strsub
    xmlUTF8Strlen
    xmlUTF8Size
    xmlUTF8Charcmp

    ;; Unicode character APIs
    xmlUCSIsAegeanNumbers
    xmlUCSIsAlphabeticPresentationForms
    xmlUCSIsArabic
    xmlUCSIsArabicPresentationFormsA
    xmlUCSIsArabicPresentationFormsB
    xmlUCSIsArmenian
    xmlUCSIsArrows
    xmlUCSIsBasicLatin
    xmlUCSIsBengali
    xmlUCSIsBlockElements
    xmlUCSIsBopomofo
    xmlUCSIsBopomofoExtended
    xmlUCSIsBoxDrawing
    xmlUCSIsBraillePatterns
    xmlUCSIsBuhid
    xmlUCSIsByzantineMusicalSymbols
    xmlUCSIsCJKCompatibility
    xmlUCSIsCJKCompatibilityForms
    xmlUCSIsCJKCompatibilityIdeographs
    xmlUCSIsCJKCompatibilityIdeographsSupplement
    xmlUCSIsCJKRadicalsSupplement
    xmlUCSIsCJKSymbolsandPunctuation
    xmlUCSIsCJKUnifiedIdeographs
    xmlUCSIsCJKUnifiedIdeographsExtensionA
    xmlUCSIsCJKUnifiedIdeographsExtensionB
    xmlUCSIsCherokee
    xmlUCSIsCombiningDiacriticalMarks
    xmlUCSIsCombiningDiacriticalMarksforSymbols
    xmlUCSIsCombiningHalfMarks
    xmlUCSIsCombiningMarksforSymbols
    xmlUCSIsControlPictures
    xmlUCSIsCurrencySymbols
    xmlUCSIsCypriotSyllabary
    xmlUCSIsCyrillic
    xmlUCSIsCyrillicSupplement
    xmlUCSIsDeseret
    xmlUCSIsDevanagari
    xmlUCSIsDingbats
    xmlUCSIsEnclosedAlphanumerics
    xmlUCSIsEnclosedCJKLettersandMonths
    xmlUCSIsEthiopic
    xmlUCSIsGeneralPunctuation
    xmlUCSIsGeometricShapes
    xmlUCSIsGeorgian
    xmlUCSIsGothic
    xmlUCSIsGreek
    xmlUCSIsGreekExtended
    xmlUCSIsGreekandCoptic
    xmlUCSIsGujarati
    xmlUCSIsGurmukhi
    xmlUCSIsHalfwidthandFullwidthForms
    xmlUCSIsHangulCompatibilityJamo
    xmlUCSIsHangulJamo
    xmlUCSIsHangulSyllables
    xmlUCSIsHanunoo
    xmlUCSIsHebrew
    xmlUCSIsHighPrivateUseSurrogates
    xmlUCSIsHighSurrogates
    xmlUCSIsHiragana
    xmlUCSIsIPAExtensions
    xmlUCSIsIdeographicDescriptionCharacters
    xmlUCSIsKanbun
    xmlUCSIsKangxiRadicals
    xmlUCSIsKannada
    xmlUCSIsKatakana
    xmlUCSIsKatakanaPhoneticExtensions
    xmlUCSIsKhmer
    xmlUCSIsKhmerSymbols
    xmlUCSIsLao
    xmlUCSIsLatin1Supplement
    xmlUCSIsLatinExtendedA
    xmlUCSIsLatinExtendedB
    xmlUCSIsLatinExtendedAdditional
    xmlUCSIsLetterlikeSymbols
    xmlUCSIsLimbu
    xmlUCSIsLinearBIdeograms
    xmlUCSIsLinearBSyllabary
    xmlUCSIsLowSurrogates
    xmlUCSIsMalayalam
    xmlUCSIsMathematicalAlphanumericSymbols
    xmlUCSIsMathematicalOperators
    xmlUCSIsMiscellaneousMathematicalSymbolsA
    xmlUCSIsMiscellaneousMathematicalSymbolsB
    xmlUCSIsMiscellaneousSymbols
    xmlUCSIsMiscellaneousSymbolsandArrows
    xmlUCSIsMiscellaneousTechnical
    xmlUCSIsMongolian
    xmlUCSIsMusicalSymbols
    xmlUCSIsMyanmar
    xmlUCSIsNumberForms
    xmlUCSIsOgham
    xmlUCSIsOldItalic
    xmlUCSIsOpticalCharacterRecognition
    xmlUCSIsOriya
    xmlUCSIsOsmanya
    xmlUCSIsPhoneticExtensions
    xmlUCSIsPrivateUse
    xmlUCSIsPrivateUseArea
    xmlUCSIsRunic
    xmlUCSIsShavian
    xmlUCSIsSinhala
    xmlUCSIsSmallFormVariants
    xmlUCSIsSpacingModifierLetters
    xmlUCSIsSpecials
    xmlUCSIsSuperscriptsandSubscripts
    xmlUCSIsSupplementalArrowsA
    xmlUCSIsSupplementalArrowsB
    xmlUCSIsSupplementalMathematicalOperators
    xmlUCSIsSupplementaryPrivateUseAreaA
    xmlUCSIsSupplementaryPrivateUseAreaB
    xmlUCSIsSyriac
    xmlUCSIsTagalog
    xmlUCSIsTagbanwa
    xmlUCSIsTags
    xmlUCSIsTaiLe
    xmlUCSIsTaiXuanJingSymbols
    xmlUCSIsTamil
    xmlUCSIsTelugu
    xmlUCSIsThaana
    xmlUCSIsThai
    xmlUCSIsTibetan
    xmlUCSIsUgaritic
    xmlUCSIsUnifiedCanadianAboriginalSyllabics
    xmlUCSIsVariationSelectors
    xmlUCSIsVariationSelectorsSupplement
    xmlUCSIsYiRadicals
    xmlUCSIsYiSyllables
    xmlUCSIsYijingHexagramSymbols
    xmlUCSIsBlock
    xmlUCSIsCatC
    xmlUCSIsCatCc
    xmlUCSIsCatCf
    xmlUCSIsCatCo
    xmlUCSIsCatCs
    xmlUCSIsCatL
    xmlUCSIsCatLl
    xmlUCSIsCatLm
    xmlUCSIsCatLo
    xmlUCSIsCatLt
    xmlUCSIsCatLu
    xmlUCSIsCatM
    xmlUCSIsCatMc
    xmlUCSIsCatMe
    xmlUCSIsCatMn
    xmlUCSIsCatN
    xmlUCSIsCatNd
    xmlUCSIsCatNl
    xmlUCSIsCatNo
    xmlUCSIsCatP
    xmlUCSIsCatPc
    xmlUCSIsCatPd
    xmlUCSIsCatPe
    xmlUCSIsCatPf
    xmlUCSIsCatPi
    xmlUCSIsCatPo
    xmlUCSIsCatPs
    xmlUCSIsCatS
    xmlUCSIsCatSc
    xmlUCSIsCatSk
    xmlUCSIsCatSm
    xmlUCSIsCatSo
    xmlUCSIsCatZ
    xmlUCSIsCatZl
    xmlUCSIsCatZp
    xmlUCSIsCatZs
    xmlUCSIsCat

    ;; compile-time version informations
    xmlCheckVersion

    ;; text writing API for XML
    xmlNewTextWriter
    xmlNewTextWriterFilename
    xmlNewTextWriterMemory
    xmlNewTextWriterPushParser
    xmlNewTextWriterDoc
    xmlNewTextWriterTree
    xmlFreeTextWriter
    xmlTextWriterStartDocument
    xmlTextWriterEndDocument
    xmlTextWriterStartComment
    xmlTextWriterEndComment
    xmlTextWriterWriteComment
    xmlTextWriterStartElement
    xmlTextWriterStartElementNS
    xmlTextWriterEndElement
    xmlTextWriterFullEndElement
    xmlTextWriterWriteElement
    xmlTextWriterWriteElementNS
    xmlTextWriterWriteRawLen
    xmlTextWriterWriteRaw
    xmlTextWriterWriteString
    xmlTextWriterWriteBase64
    xmlTextWriterWriteBinHex
    xmlTextWriterStartAttribute
    xmlTextWriterStartAttributeNS
    xmlTextWriterEndAttribute
    xmlTextWriterWriteAttribute
    xmlTextWriterWriteAttributeNS
    xmlTextWriterStartPI
    xmlTextWriterEndPI
    xmlTextWriterWritePI
    xmlTextWriterStartCDATA
    xmlTextWriterEndCDATA
    xmlTextWriterWriteCDATA
    xmlTextWriterStartDTD
    xmlTextWriterEndDTD
    xmlTextWriterWriteDTD
    xmlTextWriterStartDTDElement
    xmlTextWriterEndDTDElement
    xmlTextWriterWriteDTDElement
    xmlTextWriterStartDTDAttlist
    xmlTextWriterEndDTDAttlist
    xmlTextWriterWriteDTDAttlist
    xmlTextWriterStartDTDEntity
    xmlTextWriterEndDTDEntity
    xmlTextWriterWriteDTDInternalEntity
    xmlTextWriterWriteDTDExternalEntity
    xmlTextWriterWriteDTDExternalEntityContents
    xmlTextWriterWriteDTDEntity
    xmlTextWriterWriteDTDNotation
    xmlTextWriterSetIndent
    xmlTextWriterSetIndentString
    xmlTextWriterFlush

    ;; XML Path Language implementation
    xmlXPathFreeObject
    xmlXPathNodeSetCreate
    xmlXPathFreeNodeSetList
    xmlXPathFreeNodeSet
    xmlXPathObjectCopy
    xmlXPathCmpNodes
    xmlXPathCastNumberToBoolean
    xmlXPathCastStringToBoolean
    xmlXPathCastNodeSetToBoolean
    xmlXPathCastToBoolean
    xmlXPathCastBooleanToNumber
    xmlXPathCastStringToNumber
    xmlXPathCastNodeToNumber
    xmlXPathCastNodeSetToNumber
    xmlXPathCastToNumber
    xmlXPathCastBooleanToString
    xmlXPathCastNumberToString
    xmlXPathCastNodeToString
    xmlXPathCastNodeSetToString
    xmlXPathCastToString
    xmlXPathConvertBoolean
    xmlXPathConvertNumber
    xmlXPathConvertString
    xmlXPathNewContext
    xmlXPathFreeContext
    xmlXPathContextSetCache
    xmlXPathOrderDocElems
    xmlXPathEval
    xmlXPathEvalExpression
    xmlXPathEvalPredicate
    xmlXPathCompile
    xmlXPathCtxtCompile
    xmlXPathCompiledEval
    xmlXPathCompiledEvalToBoolean
    xmlXPathFreeCompExpr
    xmlXPathInit
    xmlXPathIsNaN
    xmlXPathIsInf

    ;; API to handle XML Pointers
    xmlXPtrLocationSetCreate
    xmlXPtrFreeLocationSet
    xmlXPtrLocationSetMerge
    xmlXPtrNewRange
    xmlXPtrNewRangePoints
    xmlXPtrNewRangeNodePoint
    xmlXPtrNewRangePointNode
    xmlXPtrNewRangeNodes
    xmlXPtrNewLocationSetNodes
    xmlXPtrNewLocationSetNodeSet
    xmlXPtrNewRangeNodeObject
    xmlXPtrNewCollapsedRange
    xmlXPtrLocationSetAdd
    xmlXPtrWrapLocationSet
    xmlXPtrLocationSetDel
    xmlXPtrLocationSetRemove
    xmlXPtrNewContext
    xmlXPtrEval
    xmlXPtrRangeToFunction
    xmlXPtrBuildNodeList
    xmlXPtrEvalRangePredicate
    )
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (xml libxml2 shared-object)
    (xml libxml2 sizeof))


(define char**		'pointer)
(define int*		'pointer)
(define int**		'pointer)
(define void**		'pointer)
(define unsigned-long*	'pointer)
(define unsigned-char*	'pointer)

(define xmlRelaxNGValidityErrorFunc*		'pointer)
(define xmlRelaxNGValidityWarningFunc*		'pointer)
(define xmlGenericErrorFunc*			'pointer)
(define xmlTextReaderErrorFunc*			'pointer)
(define xmlSchematronValidityErrorFunc*		'pointer)
(define xmlSchematronValidityWarningFunc*	'pointer)

(define xmlSchemaValidityErrorFunc*		'pointer)
(define xmlSchemaValidityWarningFunc*		'pointer)
(define xmlSAXHandlerPtr*			'pointer)

(define xmlNsPtr*				'pointer)
(define xmlFreeFunc*				'pointer)
(define xmlMallocFunc*				'pointer)
(define xmlReallocFunc*				'pointer)
(define xmlStrdupFunc*				'pointer)


;;;; Canonical XML and Exclusive XML Canonicalization
;;
;; Header file "c14.h".
;;

(define-c-functions libxml2-shared-object
  (xmlC14NDocSaveTo
   (int xmlC14NDocSaveTo (xmlDocPtr xmlNodeSetPtr int xmlChar** int xmlOutputBufferPtr)))
  (xmlC14NDocDumpMemory
   (int xmlC14NDocDumpMemory (xmlDocPtr xmlNodeSetPtr int xmlChar** int xmlChar**)))
  (xmlC14NDocSave
   (int xmlC14NDocSave (xmlDocPtr xmlNodeSetPtr int xmlChar** int char* int)))
  (xmlC14NExecute
   (int xmlC14NExecute (xmlDocPtr xmlC14NIsVisibleCallback void* int xmlChar** int xmlOutputBufferPtr))))


;;;; Unicode character range checking
;;
;;Header file "chvalid.h".
;;

(define-c-functions libxml2-shared-object
  (xmlCharInRange
   (int xmlCharInRange (unsigned-int xmlChRangeGroup*)))
  (xmlIsBaseChar
   (int xmlIsBaseChar (unsigned-int)))
  (xmlIsBlank
   (int xmlIsBlank (unsigned-int)))
  (xmlIsChar
   (int xmlIsChar (unsigned-int)))
  (xmlIsCombining
   (int xmlIsCombining (unsigned-int)))
  (xmlIsDigit
   (int xmlIsDigit (unsigned-int)))
  (xmlIsExtender
   (int xmlIsExtender (unsigned-int)))
  (xmlIsIdeographic
   (int xmlIsIdeographic (unsigned-int)))
  (xmlIsPubidChar
   (int xmlIsPubidChar (unsigned-int))))

(define (xmlIsBaseChar_ch c)
  (or (<= #x41 c #x5a)
      (<= #x61 c #x7a)
      (<= #xc0 c #xd6)
      (<= #xd8 c #xf6)
      (<= #xf8 c)))

(define xmlIsBaseCharGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsBaseCharGroup))

(define (xmlIsBaseCharQ c)
  (if (< c #x100)
      (xmlIsBaseChar_ch c)
    (xmlCharInRange c xmlIsBaseCharGroup)))

(define (xmlIsBlank_ch c)
  (or (= c #x20)
      (<= #x9 c #xa)
      (= c #xd)))

(define (xmlIsBlankQ c)
  (if (< c #x100)
      (xmlIsBlank_ch c)
    0))

(define (xmlIsChar_ch c)
  (or (<= #x9 c #xa)
      (= c #xd)
      (<= #x20 c)))

(define (xmlIsCharQ c)
  (if (< c #x100)
      (xmlIsChar_ch c)
    (or (<= #x100 c #xd7ff)
	(<= #xe000 c #xfffd)
	(<= #x10000 c #x10ffff))))

(define xmlIsCharGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsCharGroup))

(define (xmlIsCombiningQ c)
  (if (< c #x100)
      0
    (xmlCharInRange c xmlIsCombiningGroup)))

(define xmlIsCombiningGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsCombiningGroup))

(define (xmlIsDigit_ch c)
  (<= #x30 c #x39))

(define (xmlIsDigitQ c)
  (if (< c #x100)
      (xmlIsDigit_ch c)
    (xmlCharInRange c xmlIsDigitGroup)))

(define xmlIsDigitGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsDigitGroup))

(define (xmlIsExtender_ch c)
  (= c #xb7))

(define (xmlIsExtenderQ c)
  (if (< c #x100)
      (xmlIsExtender_ch c)
    (xmlCharInRange c xmlIsExtenderGroup)))

(define xmlIsExtenderGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsExtenderGroup))

(define (xmlIsIdeographicQ c)
  (if (< c #x100)
      0
    (or (<= #x4e00 c #x9fa5)
	(= c #x3007)
	(<= #x3021 c #x3029))))

(define xmlIsIdeographicGroup
  (lookup-shared-object libxml2-shared-object 'xmlIsIdeographicGroup))

(define xmlIsPubidChar_tab
  (lookup-shared-object libxml2-shared-object 'xmlIsPubidChar_tab))

(define (xmlIsPubidChar_ch c)
  (pointer-ref-c-unsigned-char xmlIsPubidChar_tab c))

(define (xmlIsPubidCharQ c)
  (if (< c #x100)
      (xmlIsPubidChar_ch c)
    0))


;;;; Tree debugging APIs
;;
;;Header file "debugXML.h".
;;

(define-c-functions libxml2-shared-object
  (xmlDebugDumpString
   (void xmlDebugDumpString (FILE* xmlChar*)))
  (xmlDebugDumpAttr
   (void xmlDebugDumpAttr (FILE* xmlAttrPtr int)))
  (xmlDebugDumpAttrList
   (void xmlDebugDumpAttrList (FILE* xmlAttrPtr int)))
  (xmlDebugDumpOneNode
   (void xmlDebugDumpOneNode (FILE* xmlNodePtr int)))
  (xmlDebugDumpNode
   (void xmlDebugDumpNode (FILE* xmlNodePtr int)))
  (xmlDebugDumpNodeList
   (void xmlDebugDumpNodeList (FILE* xmlNodePtr int)))
  (xmlDebugDumpDocumentHead
   (void xmlDebugDumpDocumentHead (FILE* xmlDocPtr)))
  (xmlDebugDumpDocument
   (void xmlDebugDumpDocument (FILE* xmlDocPtr)))
  (xmlDebugDumpDTD
   (void xmlDebugDumpDTD (FILE* xmlDtdPtr)))
  (xmlDebugDumpEntities
   (void xmlDebugDumpEntities (FILE* xmlDocPtr)))
  (xmlDebugCheckDocument
   (int xmlDebugCheckDocument (FILE* xmlDocPtr)))
  (xmlLsOneNode
   (void xmlLsOneNode (FILE* xmlNodePtr)))
  (xmlLsCountNode
   (int xmlLsCountNode (xmlNodePtr)))
  (xmlBoolToText
   (char* xmlBoolToText (int)))
  (xmlShellPrintXPathError
   (void xmlShellPrintXPathError (int char*)))
  (xmlShellPrintXPathResult
   (void xmlShellPrintXPathResult (xmlXPathObjectPtr)))
  (xmlShellList
   (int xmlShellList (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellBase
   (int xmlShellBase (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellDir
   (int xmlShellDir (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellLoad
   (int xmlShellLoad (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellPrintNode
   (void xmlShellPrintNode (xmlNodePtr)))
  (xmlShellCat
   (int xmlShellCat (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellWrite
   (int xmlShellWrite (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellSave
   (int xmlShellSave (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellValidate
   (int xmlShellValidate (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellDu
   (int xmlShellDu (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShellPwd
   (int xmlShellPwd (xmlShellCtxtPtr char* xmlNodePtr xmlNodePtr)))
  (xmlShell
   (void xmlShell (xmlDocPtr char* xmlShellReadlineFunc FILE*))))


;;;; string dictionnary
;;
;;Header file "dict.h".
;;

(define-c-functions libxml2-shared-object
  (xmlDictCreate
   (xmlDictPtr xmlDictCreate (void)))
  (xmlDictCreateSub
   (xmlDictPtr xmlDictCreateSub (xmlDictPtr)))
  (xmlDictReference
   (int xmlDictReference (xmlDictPtr)))
  (xmlDictFree
   (void xmlDictFree (xmlDictPtr)))
  (xmlDictLookup
   (xmlChar* xmlDictLookup (xmlDictPtr xmlChar* int)))
  (xmlDictExists
   (xmlChar* xmlDictExists (xmlDictPtr xmlChar* int)))
  (xmlDictQLookup
   (xmlChar* xmlDictQLookup (xmlDictPtr xmlChar* xmlChar*)))
  (xmlDictOwns
   (int xmlDictOwns (xmlDictPtr xmlChar*)))
  (xmlDictSize
   (int xmlDictSize (xmlDictPtr)))
  (xmlDictCleanup
   (void xmlDictCleanup (void))))


;;;; Interfaces to the Catalog handling system
;;
;;Header file "catalog.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNewCatalog
   (xmlCatalogPtr xmlNewCatalog (int)))
  (xmlLoadACatalog
   (xmlCatalogPtr xmlLoadACatalog (char*)))
  (xmlLoadSGMLSuperCatalog
   (xmlCatalogPtr xmlLoadSGMLSuperCatalog (char*)))
  (xmlConvertSGMLCatalog
   (int xmlConvertSGMLCatalog (xmlCatalogPtr)))
  (xmlACatalogAdd
   (int xmlACatalogAdd (xmlCatalogPtr xmlChar* xmlChar* xmlChar*)))
  (xmlACatalogRemove
   (int xmlACatalogRemove (xmlCatalogPtr xmlChar*)))
  (xmlACatalogResolve
   (xmlChar* xmlACatalogResolve	(xmlCatalogPtr xmlChar* xmlChar*)))
  (xmlACatalogResolveSystem
   (xmlChar* xmlACatalogResolveSystem (xmlCatalogPtr xmlChar*)))
  (xmlACatalogResolvePublic
   (xmlChar* xmlACatalogResolvePublic (xmlCatalogPtr xmlChar*)))
  (xmlACatalogResolveURI
   (xmlChar* xmlACatalogResolveURI (xmlCatalogPtr xmlChar*)))
  (xmlACatalogDump
   (void xmlACatalogDump (xmlCatalogPtr FILE*)))
  (xmlFreeCatalog
   (void xmlFreeCatalog (xmlCatalogPtr)))
  (xmlCatalogIsEmpty
   (int xmlCatalogIsEmpty (xmlCatalogPtr)))
  (xmlInitializeCatalog
   (void xmlInitializeCatalog (void)))
  (xmlLoadCatalog
   (int xmlLoadCatalog (char*)))
  (xmlLoadCatalogs
   (void xmlLoadCatalogs (char*)))
  (xmlCatalogCleanup
   (void xmlCatalogCleanup (void)))
  (xmlCatalogDump
   (void xmlCatalogDump (FILE*)))
  (xmlCatalogResolve
   (xmlChar* xmlCatalogResolve (xmlChar* xmlChar*)))
  (xmlCatalogResolveSystem
   (xmlChar* xmlCatalogResolveSystem (xmlChar*)))
  (xmlCatalogResolvePublic
   (xmlChar* xmlCatalogResolvePublic (xmlChar*)))
  (xmlCatalogResolveURI
   (xmlChar* xmlCatalogResolveURI (xmlChar*)))
  (xmlCatalogAdd
   (int xmlCatalogAdd (xmlChar* xmlChar* xmlChar*)))
  (xmlCatalogRemove
   (int xmlCatalogRemove (xmlChar*)))
  (xmlParseCatalogFile
   (xmlDocPtr xmlParseCatalogFile (char*)))
  (xmlCatalogConvert
   (int xmlCatalogConvert (void)))
  (xmlCatalogFreeLocal
   (void xmlCatalogFreeLocal (void*)))
  (xmlCatalogAddLocal
   (void* xmlCatalogAddLocal (void* xmlChar*)))
  (xmlCatalogLocalResolve
   (xmlChar* xmlCatalogLocalResolve (void* xmlChar* xmlChar*)))
  (xmlCatalogLocalResolveURI
   (xmlChar* xmlCatalogLocalResolveURI (void* xmlChar*)))
  (xmlCatalogSetDebug
   (int xmlCatalogSetDebug (int)))
  (xmlCatalogSetDefaultPrefer
   (xmlCatalogPrefer xmlCatalogSetDefaultPrefer (xmlCatalogPrefer)))
  (xmlCatalogSetDefaults
   (void xmlCatalogSetDefaults (xmlCatalogAllow)))
  (xmlCatalogGetDefaults
   (xmlCatalogAllow xmlCatalogGetDefaults (void)))
  (xmlCatalogGetSystem
   (xmlChar* xmlCatalogGetSystem (xmlChar*)))
  (xmlCatalogGetPublic
   (xmlChar* xmlCatalogGetPublic (xmlChar*))))


;;;; interface for the encoding conversion functions
;;
;;Header file "encoding.h"
;;

(define-c-functions libxml2-shared-object
  (xmlInitCharEncodingHandlers
   (void xmlInitCharEncodingHandlers (void)))
  (xmlCleanupCharEncodingHandlers
   (void xmlCleanupCharEncodingHandlers	(void)))
  (xmlRegisterCharEncodingHandler
   (void xmlRegisterCharEncodingHandler	(xmlCharEncodingHandlerPtr)))
  (xmlGetCharEncodingHandler
   (xmlCharEncodingHandlerPtr xmlGetCharEncodingHandler (xmlCharEncoding)))
  (xmlFindCharEncodingHandler
   (xmlCharEncodingHandlerPtr xmlFindCharEncodingHandler (char*)))
  (xmlNewCharEncodingHandler
   (xmlCharEncodingHandlerPtr xmlNewCharEncodingHandler
			      (char* xmlCharEncodingInputFunc xmlCharEncodingOutputFunc)))
  (xmlAddEncodingAlias
   (int xmlAddEncodingAlias (char* char*)))
  (xmlDelEncodingAlias
   (int xmlDelEncodingAlias (char*)))
  (xmlGetEncodingAlias
   (char* xmlGetEncodingAlias (char*)))
  (xmlCleanupEncodingAliases
   (void xmlCleanupEncodingAliases (void)))
  (xmlParseCharEncoding
   (xmlCharEncoding xmlParseCharEncoding (char*)))
  (xmlGetCharEncodingName
   (char* xmlGetCharEncodingName (xmlCharEncoding)))
  (xmlDetectCharEncoding
   (xmlCharEncoding xmlDetectCharEncoding (void* int)))
  (xmlCharEncOutFunc
   (int xmlCharEncOutFunc (xmlCharEncodingHandler* xmlBufferPtr xmlBufferPtr)))
  (xmlCharEncInFunc
   (int xmlCharEncInFunc (xmlCharEncodingHandler* xmlBufferPtr xmlBufferPtr)))
  (xmlCharEncFirstLine
   (int xmlCharEncFirstLine (xmlCharEncodingHandler* xmlBufferPtr xmlBufferPtr)))
  (xmlCharEncCloseFunc
   (int xmlCharEncCloseFunc (xmlCharEncodingHandler*)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (UTF8Toisolat1
   (int UTF8Toisolat1 (void* void* void* void*)))
  ;;#endif /* LIBXML_OUTPUT_ENABLED */
  (isolat1ToUTF8
   (int isolat1ToUTF8 (void* void* void* void*))))


;;;; interface for the XML entities handling
;;
;;Header file "entities.h".
;;

(define-c-functions libxml2-shared-object

  ;; #ifdef LIBXML_LEGACY_ENABLED
  (xmlInitializePredefinedEntities
   (void xmlInitializePredefinedEntities (void)))
  ;; #endif /* LIBXML_LEGACY_ENABLED */
  (xmlNewEntity
   (xmlEntityPtr xmlNewEntity (xmlDocPtr xmlChar* int xmlChar* xmlChar* xmlChar*)))
  (xmlAddDocEntity
   (xmlEntityPtr xmlAddDocEntity (xmlDocPtr xmlChar* int xmlChar* xmlChar* xmlChar*)))
  (xmlAddDtdEntity
   (xmlEntityPtr xmlAddDtdEntity (xmlDocPtr xmlChar* int xmlChar* xmlChar* xmlChar*)))
  (xmlGetPredefinedEntity
   (xmlEntityPtr xmlGetPredefinedEntity	(xmlChar*)))
  (xmlGetDocEntity
   (xmlEntityPtr xmlGetDocEntity (xmlDocPtr xmlChar*)))
  (xmlGetDtdEntity
   (xmlEntityPtr xmlGetDtdEntity (xmlDocPtr xmlChar*)))
  (xmlGetParameterEntity
   (xmlEntityPtr xmlGetParameterEntity (xmlDocPtr xmlChar*)))
  ;; #ifdef LIBXML_LEGACY_ENABLED
  (xmlEncodeEntities
   (xmlChar* xmlEncodeEntities (xmlDocPtr xmlChar*)))
  ;; #endif /* LIBXML_LEGACY_ENABLED */
  (xmlEncodeEntitiesReentrant
   (xmlChar* xmlEncodeEntitiesReentrant (xmlDocPtr xmlChar*)))
  (xmlEncodeSpecialChars
   (xmlChar* xmlEncodeSpecialChars (xmlDocPtr xmlChar*)))
  (xmlCreateEntitiesTable
   (xmlEntitiesTablePtr xmlCreateEntitiesTable (void)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyEntitiesTable
   (xmlEntitiesTablePtr xmlCopyEntitiesTable (xmlEntitiesTablePtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlFreeEntitiesTable
   (void xmlFreeEntitiesTable (xmlEntitiesTablePtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlDumpEntitiesTable
   (void xmlDumpEntitiesTable (xmlBufferPtr xmlEntitiesTablePtr)))
  (xmlDumpEntityDecl
   (void xmlDumpEntityDecl (xmlBufferPtr xmlEntityPtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  ;; #ifdef LIBXML_LEGACY_ENABLED
  (xmlCleanupPredefinedEntities
   (void xmlCleanupPredefinedEntities(void)))
  ;; #endif /* LIBXML_LEGACY_ENABLED */
  )


;;;; interface for all global variables of the library
;;
;;Header file "globals.h".
;;

(define-c-functions libxml2-shared-object
  (xmlInitGlobals
   (void xmlInitGlobals (void)))
  (xmlCleanupGlobals
   (void xmlCleanupGlobals (void)))
  (xmlParserInputBufferCreateFilenameDefault
   (xmlParserInputBufferCreateFilenameFunc xmlParserInputBufferCreateFilenameDefault
					   (xmlParserInputBufferCreateFilenameFunc)))
  (xmlOutputBufferCreateFilenameDefault
   (xmlOutputBufferCreateFilenameFunc xmlOutputBufferCreateFilenameDefault
				      (xmlOutputBufferCreateFilenameFunc)))
  (xmlInitializeGlobalState
   (void xmlInitializeGlobalState (xmlGlobalStatePtr)))
  (xmlThrDefSetGenericErrorFunc
   (void xmlThrDefSetGenericErrorFunc (void* xmlGenericErrorFunc)))
  (xmlThrDefSetStructuredErrorFunc
   (void xmlThrDefSetStructuredErrorFunc (void* xmlStructuredErrorFunc)))
  (xmlRegisterNodeDefault
   (xmlRegisterNodeFunc xmlRegisterNodeDefault (xmlRegisterNodeFunc)))
  (xmlThrDefRegisterNodeDefault
   (xmlRegisterNodeFunc xmlThrDefRegisterNodeDefault (xmlRegisterNodeFunc)))
  (xmlDeregisterNodeDefault
   (xmlDeregisterNodeFunc xmlDeregisterNodeDefault (xmlDeregisterNodeFunc)))
  (xmlThrDefDeregisterNodeDefault
   (xmlDeregisterNodeFunc xmlThrDefDeregisterNodeDefault (xmlDeregisterNodeFunc)))
  (xmlThrDefOutputBufferCreateFilenameDefault
   (xmlOutputBufferCreateFilenameFunc xmlThrDefOutputBufferCreateFilenameDefault
				      (xmlOutputBufferCreateFilenameFunc)))
  (xmlThrDefParserInputBufferCreateFilenameDefault
   (xmlParserInputBufferCreateFilenameFunc xmlThrDefParserInputBufferCreateFilenameDefault
					   (xmlParserInputBufferCreateFilenameFunc)))
  (xmlMalloc
   (void* xmlMalloc (size_t)))
  (xmlMallocAtomic
   (void* xmlMallocAtomic (size_t)))
  (xmlRealloc
   (void* xmlRealloc (void* size_t)))
  (xmlFree
   (void xmlFree (void*)))
  (xmlMemStrdup
   (char* xmlMemStrdup (char*))))


;;;; Chained hash tables
;;
;;Header file "hash.c"
;;

(define-c-functions libxml2-shared-object
  (xmlHashCreate
   (xmlHashTablePtr xmlHashCreate (int)))
  (xmlHashCreateDict
   (xmlHashTablePtr xmlHashCreateDict (int xmlDictPtr)))
  (xmlHashFree
   (void xmlHashFree (xmlHashTablePtr xmlHashDeallocator)))
  (xmlHashAddEntry
   (int xmlHashAddEntry	(xmlHashTablePtr xmlChar* void*)))
  (xmlHashUpdateEntry
   (int xmlHashUpdateEntry (xmlHashTablePtr xmlChar* void* xmlHashDeallocator)))
  (xmlHashAddEntry2
   (int xmlHashAddEntry2 (xmlHashTablePtr xmlChar* xmlChar* void*)))
  (xmlHashUpdateEntry2
   (int xmlHashUpdateEntry2 (xmlHashTablePtr xmlChar* xmlChar* void* xmlHashDeallocator)))
  (xmlHashAddEntry3
   (int xmlHashAddEntry3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* void*)))
  (xmlHashUpdateEntry3
   (int xmlHashUpdateEntry3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* void* xmlHashDeallocator)))
  (xmlHashRemoveEntry
   (int xmlHashRemoveEntry (xmlHashTablePtr xmlChar* xmlHashDeallocator)))
  (xmlHashRemoveEntry2
   (int xmlHashRemoveEntry2 (xmlHashTablePtr xmlChar* xmlChar* xmlHashDeallocator)))
  (xmlHashRemoveEntry3
   (int xmlHashRemoveEntry3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* xmlHashDeallocator)))
  (xmlHashLookup
   (void* xmlHashLookup (xmlHashTablePtr xmlChar*)))
  (xmlHashLookup2
   (void* xmlHashLookup2 (xmlHashTablePtr xmlChar* xmlChar*)))
  (xmlHashLookup3
   (void* xmlHashLookup3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar*)))
  (xmlHashQLookup
   (void* xmlHashQLookup (xmlHashTablePtr xmlChar* xmlChar*)))
  (xmlHashQLookup2
   (void* xmlHashQLookup2 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlHashQLookup3
   (void* xmlHashQLookup3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlHashCopy
   (xmlHashTablePtr xmlHashCopy	(xmlHashTablePtr xmlHashCopier)))
  (xmlHashSize
   (int xmlHashSize (xmlHashTablePtr)))
  (xmlHashScan
   (void xmlHashScan (xmlHashTablePtr xmlHashScanner void*)))
  (xmlHashScan3
   (void xmlHashScan3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* xmlHashScanner void*)))
  (xmlHashScanFull
   (void xmlHashScanFull (xmlHashTablePtr xmlHashScannerFull void*)))
  (xmlHashScanFull3
   (void xmlHashScanFull3 (xmlHashTablePtr xmlChar* xmlChar* xmlChar* xmlHashScannerFull void*))))


;;;; HTML parser
;;
;;Header file "HTMLparser.h".
;;

(define-c-functions libxml2-shared-object
  (htmlTagLookup		(htmlElemDescPtr htmlTagLookup (xmlChar*)))
  (htmlEntityLookup		(htmlEntityDescPtr htmlEntityLookup (xmlChar*)))
  (htmlEntityValueLookup	(htmlEntityDescPtr htmlEntityValueLookup (unsigned-int)))
  (htmlIsAutoClosed		(int htmlIsAutoClosed (htmlDocPtr htmlNodePtr)))
  (htmlAutoCloseTag		(int htmlAutoCloseTag (htmlDocPtr xmlChar* htmlNodePtr)))
  (htmlParseEntityRef		(htmlEntityDesc* htmlParseEntityRef (htmlParserCtxtPtr xmlChar**)))
  (htmlParseCharRef		(int htmlParseCharRef (htmlParserCtxtPtr)))
  (htmlParseElement		(void htmlParseElement (htmlParserCtxtPtr)))
  (htmlNewParserCtxt		(htmlParserCtxtPtr htmlNewParserCtxt (void)))
  (htmlCreateMemoryParserCtxt	(htmlParserCtxtPtr htmlCreateMemoryParserCtxt (char* int)))
  (htmlParseDocument		(int htmlParseDocument (htmlParserCtxtPtr)))
  (htmlSAXParseDoc		(htmlDocPtr htmlSAXParseDoc (xmlChar* char* htmlSAXHandlerPtr void*)))
  (htmlParseDoc			(htmlDocPtr htmlParseDoc (xmlChar* char*)))
  (htmlSAXParseFile		(htmlDocPtr htmlSAXParseFile (char* char* htmlSAXHandlerPtr void*)))
  (htmlParseFile		(htmlDocPtr htmlParseFile (char* char*)))
  (UTF8ToHtml			(int UTF8ToHtml (void* void* void* void*)))
  (htmlEncodeEntities		(int htmlEncodeEntities (void* void* void* void* int)))
  (htmlIsScriptAttribute	(int htmlIsScriptAttribute (xmlChar*)))
  (htmlHandleOmittedElem	(int htmlHandleOmittedElem (int)))
  (htmlCreatePushParserCtxt	(htmlParserCtxtPtr htmlCreatePushParserCtxt
					   (htmlSAXHandlerPtr void* char* int char* xmlCharEncoding)))
  (htmlParseChunk		(int htmlParseChunk (htmlParserCtxtPtr char* int int)))
  (htmlFreeParserCtxt		(void htmlFreeParserCtxt (htmlParserCtxtPtr)))

  (htmlCtxtReset		(void htmlCtxtReset (htmlParserCtxtPtr)))
  (htmlCtxtUseOptions		(int htmlCtxtUseOptions (htmlParserCtxtPtr int)))
  (htmlReadDoc			(htmlDocPtr htmlReadDoc (xmlChar* char* char* int)))
  (htmlReadFile			(htmlDocPtr htmlReadFile (char* char* int)))
  (htmlReadMemory		(htmlDocPtr htmlReadMemory (char* int char* char* int)))
  (htmlReadFd			(htmlDocPtr htmlReadFd (int char* char* int)))
  (htmlReadIO			(htmlDocPtr htmlReadIO (xmlInputReadCallback xmlInputCloseCallback
					void* char* char* int)))
  (htmlCtxtReadDoc
   (htmlDocPtr htmlCtxtReadDoc (xmlParserCtxtPtr xmlChar* char* char* int)))
  (htmlCtxtReadFile
   (htmlDocPtr htmlCtxtReadFile (xmlParserCtxtPtr char* char* int)))
  (htmlCtxtReadMemory
   (htmlDocPtr htmlCtxtReadMemory (xmlParserCtxtPtr char* int char* char* int)))
  (htmlCtxtReadFd
   (htmlDocPtr htmlCtxtReadFd (xmlParserCtxtPtr int char* char* int)))
  (htmlCtxtReadIO
   (htmlDocPtr htmlCtxtReadIO (xmlParserCtxtPtr xmlInputReadCallback xmlInputCloseCallback
						void* char* char* int)))

  (htmlAttrAllowed
   (htmlStatus htmlAttrAllowed (htmlElemDesc* xmlChar* int)))
  (htmlElementAllowedHere
   (int htmlElementAllowedHere (htmlElemDesc* xmlChar*)))
  (htmlElementStatusHere
   (htmlStatus htmlElementStatusHere (htmlElemDesc* htmlElemDesc*)))
  (htmlNodeStatus
   (htmlStatus htmlNodeStatus (htmlNodePtr int))))

;;;; specific APIs to process HTML tree, especially serialization
;;
;;Header file "HTMLTree.h".
;;

(define-c-functions libxml2-shared-object
  (htmlNewDoc
   (htmlDocPtr htmlNewDoc (xmlChar* xmlChar*)))
  (htmlNewDocNoDtD
   (htmlDocPtr htmlNewDocNoDtD (xmlChar* xmlChar*)))
  (htmlGetMetaEncoding
   (xmlChar* htmlGetMetaEncoding (htmlDocPtr)))
  (htmlSetMetaEncoding
   (int htmlSetMetaEncoding (htmlDocPtr xmlChar*)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (htmlDocDumpMemory
   (void htmlDocDumpMemory (xmlDocPtr xmlChar** void*)))
  (htmlDocDumpMemoryFormat
   (void htmlDocDumpMemoryFormat (xmlDocPtr xmlChar** void* int)))
  (htmlDocDump
   (int htmlDocDump (FILE* xmlDocPtr)))
  (htmlSaveFile
   (int htmlSaveFile (char* xmlDocPtr)))
  (htmlNodeDump
   (int htmlNodeDump (xmlBufferPtr xmlDocPtr xmlNodePtr)))
  (htmlNodeDumpFile
   (void htmlNodeDumpFile (FILE* xmlDocPtr xmlNodePtr)))
  (htmlNodeDumpFileFormat
   (int htmlNodeDumpFileFormat (FILE* xmlDocPtr xmlNodePtr char* int)))
  (htmlSaveFileEnc
   (int htmlSaveFileEnc (char* xmlDocPtr char*)))
  (htmlSaveFileFormat
   (int htmlSaveFileFormat (char* xmlDocPtr char* int)))
  (htmlNodeDumpFormatOutput
   (void htmlNodeDumpFormatOutput (xmlOutputBufferPtr xmlDocPtr xmlNodePtr char* int)))
  (htmlDocContentDumpOutput
   (void htmlDocContentDumpOutput (xmlOutputBufferPtr xmlDocPtr char*)))
  (htmlDocContentDumpFormatOutput
   (void htmlDocContentDumpFormatOutput (xmlOutputBufferPtr xmlDocPtr char* int)))
  (htmlNodeDumpOutput
   (void htmlNodeDumpOutput (xmlOutputBufferPtr xmlDocPtr xmlNodePtr char*)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (htmlIsBooleanAttr
   (int htmlIsBooleanAttr (xmlChar*))))


;;;; lists interfaces
;;
;;Header file "list.h".
;;

(define-c-functions libxml2-shared-object
  (xmlListCreate
   (xmlListPtr xmlListCreate (xmlListDeallocator xmlListDataCompare)))
  (xmlListDelete
   (void xmlListDelete (xmlListPtr)))
  (xmlListSearch
   (void* xmlListSearch (xmlListPtr void*)))
  (xmlListReverseSearch
   (void* xmlListReverseSearch (xmlListPtr void*)))
  (xmlListInsert
   (int xmlListInsert (xmlListPtr void*)))
  (xmlListAppend
   (int xmlListAppend (xmlListPtr void*)))
  (xmlListRemoveFirst
   (int xmlListRemoveFirst (xmlListPtr void*)))
  (xmlListRemoveLast
   (int xmlListRemoveLast (xmlListPtr void*)))
  (xmlListRemoveAll
   (int xmlListRemoveAll (xmlListPtr void*)))
  (xmlListClear
   (void xmlListClear (xmlListPtr)))
  (xmlListEmpty
   (int xmlListEmpty (xmlListPtr)))
  (xmlListFront
   (xmlLinkPtr xmlListFront (xmlListPtr)))
  (xmlListEnd
   (xmlLinkPtr xmlListEnd (xmlListPtr)))
  (xmlListSize
   (int xmlListSize (xmlListPtr)))
  (xmlListPopFront
   (void xmlListPopFront (xmlListPtr)))
  (xmlListPopBack
   (void xmlListPopBack (xmlListPtr)))
  (xmlListPushFront
   (int xmlListPushFront (xmlListPtr void*)))
  (xmlListPushBack
   (int xmlListPushBack (xmlListPtr void*)))
  (xmlListReverse
   (void xmlListReverse (xmlListPtr)))
  (xmlListSort
   (void xmlListSort (xmlListPtr)))
  (xmlListWalk
   (void xmlListWalk (xmlListPtr xmlListWalker void*)))
  (xmlListReverseWalk
   (void xmlListReverseWalk (xmlListPtr xmlListWalker void*)))
  (xmlListMerge
   (void xmlListMerge (xmlListPtr xmlListPtr)))
  (xmlListDup
   (xmlListPtr xmlListDup (xmlListPtr)))
  (xmlListCopy
   (int xmlListCopy (xmlListPtr xmlListPtr)))
  (xmlLinkGetData
   (void* xmlLinkGetData (xmlLinkPtr))))


;;;; minimal FTP implementation
;;
;;Header file "nanoftp.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNanoFTPInit
   (void xmlNanoFTPInit (void)))
  (xmlNanoFTPCleanup
   (void xmlNanoFTPCleanup (void)))
  (xmlNanoFTPNewCtxt
   (void* xmlNanoFTPNewCtxt (char*)))
  (xmlNanoFTPFreeCtxt
   (void xmlNanoFTPFreeCtxt (void*)))
  (xmlNanoFTPConnectTo
   (void* xmlNanoFTPConnectTo (char* int)))
  (xmlNanoFTPOpen
   (void* xmlNanoFTPOpen (char*)))
  (xmlNanoFTPConnect
   (int xmlNanoFTPConnect (void*)))
  (xmlNanoFTPClose
   (int xmlNanoFTPClose (void*)))
  (xmlNanoFTPQuit
   (int xmlNanoFTPQuit (void*)))
  (xmlNanoFTPScanProxy
   (void xmlNanoFTPScanProxy (char*)))
  (xmlNanoFTPProxy
   (void xmlNanoFTPProxy (char* int char* char* int)))
  (xmlNanoFTPUpdateURL
   (int xmlNanoFTPUpdateURL (void* char*)))
  (xmlNanoFTPGetResponse
   (int xmlNanoFTPGetResponse (void*)))
  (xmlNanoFTPCheckResponse
   (int xmlNanoFTPCheckResponse (void*)))
  (xmlNanoFTPCwd
   (int xmlNanoFTPCwd (void* char*)))
  (xmlNanoFTPDele
   (int xmlNanoFTPDele (void* char*)))
  (xmlNanoFTPGetConnection
   (int xmlNanoFTPGetConnection (void*)))
  (xmlNanoFTPCloseConnection
   (int xmlNanoFTPCloseConnection (void*)))
  (xmlNanoFTPList
   (int xmlNanoFTPList (void* ftpListCallback void* char*)))
  (xmlNanoFTPGetSocket
   (int xmlNanoFTPGetSocket (void* char*)))
  (xmlNanoFTPGet
   (int xmlNanoFTPGet (void* ftpDataCallback void* char*)))
  (xmlNanoFTPRead
   (int xmlNanoFTPRead (void* void* int))))


;;;; minimal HTTP implementation
;;
;;Header file "nanohttp.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNanoHTTPInit
   (void xmlNanoHTTPInit (void)))
  (xmlNanoHTTPCleanup
   (void xmlNanoHTTPCleanup (void)))
  (xmlNanoHTTPScanProxy
   (void xmlNanoHTTPScanProxy (char*)))
  (xmlNanoHTTPFetch
   (int xmlNanoHTTPFetch (char* char* char**)))
  (xmlNanoHTTPMethod
   (void* xmlNanoHTTPMethod (char* char* char* char** char* int)))
  (xmlNanoHTTPMethodRedir
   (void* xmlNanoHTTPMethodRedir (char* char* char* char** char** char* int)))
  (xmlNanoHTTPOpen
   (void* xmlNanoHTTPOpen (char* char**)))
  (xmlNanoHTTPOpenRedir
   (void* xmlNanoHTTPOpenRedir (char* char** char**)))
  (xmlNanoHTTPReturnCode
   (int xmlNanoHTTPReturnCode (void*)))
  (xmlNanoHTTPAuthHeader
   (char* xmlNanoHTTPAuthHeader	(void*)))
  (xmlNanoHTTPRedir
   (char* xmlNanoHTTPRedir (void*)))
  (xmlNanoHTTPContentLength
   (int xmlNanoHTTPContentLength (void*)))
  (xmlNanoHTTPEncoding
   (char* xmlNanoHTTPEncoding (void*)))
  (xmlNanoHTTPMimeType
   (char* xmlNanoHTTPMimeType (void*)))
  (xmlNanoHTTPRead
   (int xmlNanoHTTPRead (void* void* int)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlNanoHTTPSave
   (int xmlNanoHTTPSave (void* char*)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlNanoHTTPClose
   (void xmlNanoHTTPClose (void*))))


;;;; the core parser module
;;
;;Header file "parser.h".
;;

(define-c-functions libxml2-shared-object
  (xmlInitParser
   (void xmlInitParser (void)))
  (xmlCleanupParser
   (void xmlCleanupParser (void)))
  (xmlParserInputRead
   (int xmlParserInputRead (xmlParserInputPtr int)))
  (xmlParserInputGrow
   (int xmlParserInputGrow (xmlParserInputPtr int)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlParseDoc
   (xmlDocPtr xmlParseDoc (xmlChar*)))
  (xmlParseFile
   (xmlDocPtr xmlParseFile (char*)))
  (xmlParseMemory
   (xmlDocPtr xmlParseMemory (char* int)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlSubstituteEntitiesDefault
   (int xmlSubstituteEntitiesDefault (int)))
  (xmlKeepBlanksDefault
   (int xmlKeepBlanksDefault (int)))
  (xmlStopParser
   (void xmlStopParser (xmlParserCtxtPtr)))
  (xmlPedanticParserDefault
   (int xmlPedanticParserDefault (int)))
  (xmlLineNumbersDefault
   (int xmlLineNumbersDefault (int)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlRecoverDoc
   (xmlDocPtr xmlRecoverDoc (xmlChar*)))
  (xmlRecoverMemory
   (xmlDocPtr xmlRecoverMemory (char* int)))
  (xmlRecoverFile
   (xmlDocPtr xmlRecoverFile (char*)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlParseDocument
   (int xmlParseDocument (xmlParserCtxtPtr)))
  (xmlParseExtParsedEnt
   (int xmlParseExtParsedEnt (xmlParserCtxtPtr)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlSAXUserParseFile
   (int xmlSAXUserParseFile (xmlSAXHandlerPtr void* char*)))
  (xmlSAXUserParseMemory
   (int xmlSAXUserParseMemory (xmlSAXHandlerPtr void* char* int)))
  (xmlSAXParseDoc
   (xmlDocPtr xmlSAXParseDoc (xmlSAXHandlerPtr xmlChar* int)))
  (xmlSAXParseMemory
   (xmlDocPtr xmlSAXParseMemory	(xmlSAXHandlerPtr char* int int)))
  (xmlSAXParseMemoryWithData
   (xmlDocPtr xmlSAXParseMemoryWithData (xmlSAXHandlerPtr char* int int void*)))
  (xmlSAXParseFile
   (xmlDocPtr xmlSAXParseFile (xmlSAXHandlerPtr char* int)))
  (xmlSAXParseFileWithData
   (xmlDocPtr xmlSAXParseFileWithData (xmlSAXHandlerPtr char* int void*)))
  (xmlSAXParseEntity
   (xmlDocPtr xmlSAXParseEntity	(xmlSAXHandlerPtr char*)))
  (xmlParseEntity
   (xmlDocPtr xmlParseEntity (char*)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  ;; #ifdef LIBXML_VALID_ENABLED
  (xmlSAXParseDTD
   (xmlDtdPtr xmlSAXParseDTD (xmlSAXHandlerPtr xmlChar* xmlChar*)))
  (xmlParseDTD
   (xmlDtdPtr xmlParseDTD (xmlChar* xmlChar*)))
  (xmlIOParseDTD
   (xmlDtdPtr xmlIOParseDTD (xmlSAXHandlerPtr xmlParserInputBufferPtr xmlCharEncoding)))
  ;; #endif /* LIBXML_VALID_ENABLE */
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlParseBalancedChunkMemory
   (int xmlParseBalancedChunkMemory (xmlDocPtr xmlSAXHandlerPtr void* int xmlChar* xmlNodePtr*)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlParseInNodeContext
   (xmlParserErrors xmlParseInNodeContext (xmlNodePtr char* int int xmlNodePtr*)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlParseBalancedChunkMemoryRecover
   (int xmlParseBalancedChunkMemoryRecover
	(xmlDocPtr xmlSAXHandlerPtr void* int xmlChar* xmlNodePtr* int)))
  (xmlParseExternalEntity
   (int xmlParseExternalEntity
	(xmlDocPtr xmlSAXHandlerPtr void* int xmlChar* xmlChar* xmlNodePtr*)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlParseCtxtExternalEntity
   (int xmlParseCtxtExternalEntity (xmlParserCtxtPtr xmlChar* xmlChar* xmlNodePtr*)))
  (xmlNewParserCtxt
   (xmlParserCtxtPtr xmlNewParserCtxt (void)))
  (xmlInitParserCtxt
   (int xmlInitParserCtxt (xmlParserCtxtPtr)))
  (xmlClearParserCtxt
   (void xmlClearParserCtxt (xmlParserCtxtPtr)))
  (xmlFreeParserCtxt
   (void xmlFreeParserCtxt (xmlParserCtxtPtr)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlSetupParserForBuffer
   (void xmlSetupParserForBuffer (xmlParserCtxtPtr xmlChar* char*)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlCreateDocParserCtxt
   (xmlParserCtxtPtr xmlCreateDocParserCtxt (xmlChar*)))
  ;; #ifdef LIBXML_LEGACY_ENABLED
  (xmlGetFeaturesList
   (int xmlGetFeaturesList (int* char**)))
  (xmlGetFeature
   (int xmlGetFeature (xmlParserCtxtPtr char* void*)))
  (xmlSetFeature
   (int xmlSetFeature (xmlParserCtxtPtr char* void*)))
  ;; #endif /* LIBXML_LEGACY_ENABLED */
  ;; #ifdef LIBXML_PUSH_ENABLED
  (xmlCreatePushParserCtxt
   (xmlParserCtxtPtr xmlCreatePushParserCtxt (xmlSAXHandlerPtr void* char* int char*)))
  (xmlParseChunk
   (int xmlParseChunk (xmlParserCtxtPtr char* int int)))
  ;; #endif /* LIBXML_PUSH_ENABLED */
  (xmlCreateIOParserCtxt
   (xmlParserCtxtPtr xmlCreateIOParserCtxt
		     (xmlSAXHandlerPtr void* xmlInputReadCallback xmlInputCloseCallback void*
				       xmlCharEncoding)))

  (xmlNewIOInputStream
   (xmlParserInputPtr xmlNewIOInputStream (xmlParserCtxtPtr xmlParserInputBufferPtr xmlCharEncoding)))

  (xmlParserFindNodeInfo
   (xmlParserNodeInfo* xmlParserFindNodeInfo (xmlParserCtxtPtr xmlNodePtr)))
  (xmlInitNodeInfoSeq
   (void xmlInitNodeInfoSeq (xmlParserNodeInfoSeqPtr)))
  (xmlClearNodeInfoSeq
   (void xmlClearNodeInfoSeq (xmlParserNodeInfoSeqPtr)))
  (xmlParserFindNodeInfoIndex
   (unsigned-long xmlParserFindNodeInfoIndex (xmlParserNodeInfoSeqPtr xmlNodePtr)))
  (xmlParserAddNodeInfo
   (void xmlParserAddNodeInfo (xmlParserCtxtPtr xmlParserNodeInfoPtr)))
  (xmlSetExternalEntityLoader
   (void xmlSetExternalEntityLoader (xmlExternalEntityLoader)))
  (xmlGetExternalEntityLoader
   (xmlExternalEntityLoader xmlGetExternalEntityLoader (void)))
  (xmlLoadExternalEntity
   (xmlParserInputPtr xmlLoadExternalEntity (char* char* xmlParserCtxtPtr)))
  (xmlByteConsumed
   (long xmlByteConsumed (xmlParserCtxtPtr)))
  (xmlCtxtReset
   (void xmlCtxtReset (xmlParserCtxtPtr)))
  (xmlCtxtResetPush
   (int xmlCtxtResetPush (xmlParserCtxtPtr char* int char* char*)))
  (xmlCtxtUseOptions
   (int xmlCtxtUseOptions (xmlParserCtxtPtr int)))
  (xmlReadDoc
   (xmlDocPtr xmlReadDoc (xmlChar* char* char* int)))
  (xmlReadFile
   (xmlDocPtr xmlReadFile (char* char* int)))
  (xmlReadMemory
   (xmlDocPtr xmlReadMemory (char* int char* char* int)))
  (xmlReadFd
   (xmlDocPtr xmlReadFd (int char* char* int)))
  (xmlReadIO
   (xmlDocPtr xmlReadIO (xmlInputReadCallback xmlInputCloseCallback void* char* char* int)))
  (xmlCtxtReadDoc
   (xmlDocPtr xmlCtxtReadDoc (xmlParserCtxtPtr xmlChar* char* char* int)))
  (xmlCtxtReadFile
   (xmlDocPtr xmlCtxtReadFile (xmlParserCtxtPtr char* char* int)))
  (xmlCtxtReadMemory
   (xmlDocPtr xmlCtxtReadMemory (xmlParserCtxtPtr char* int char* char* int)))
  (xmlCtxtReadFd
   (xmlDocPtr xmlCtxtReadFd (xmlParserCtxtPtr int char* char* int)))
  (xmlCtxtReadIO
   (xmlDocPtr xmlCtxtReadIO
	      (xmlParserCtxtPtr xmlInputReadCallback xmlInputCloseCallback void* char* char* int)))
  (xmlHasFeature
   (int xmlHasFeature (xmlFeature))))


;;;; pattern expression handling
;;
;;Header file "pattern.h"
;;

(define-c-functions libxml2-shared-object
  (xmlFreePattern
   (void xmlFreePattern (xmlPatternPtr)))
  (xmlFreePatternList
   (void xmlFreePatternList (xmlPatternPtr)))
  (xmlPatterncompile
   (xmlPatternPtr xmlPatterncompile (xmlChar* xmlDict* int xmlChar**)))
  (xmlPatternMatch
   (int xmlPatternMatch (xmlPatternPtr xmlNodePtr)))
  (xmlPatternStreamable
   (int xmlPatternStreamable (xmlPatternPtr)))
  (xmlPatternMaxDepth
   (int xmlPatternMaxDepth (xmlPatternPtr)))
  (xmlPatternMinDepth
   (int xmlPatternMinDepth (xmlPatternPtr)))
  (xmlPatternFromRoot
   (int xmlPatternFromRoot (xmlPatternPtr)))
  (xmlPatternGetStreamCtxt
   (xmlStreamCtxtPtr xmlPatternGetStreamCtxt (xmlPatternPtr)))
  (xmlFreeStreamCtxt
   (void xmlFreeStreamCtxt (xmlStreamCtxtPtr)))
  (xmlStreamPushNode
   (int xmlStreamPushNode (xmlStreamCtxtPtr xmlChar* xmlChar* int)))
  (xmlStreamPush
   (int xmlStreamPush (xmlStreamCtxtPtr xmlChar* xmlChar*)))
  (xmlStreamPushAttr
   (int xmlStreamPushAttr (xmlStreamCtxtPtr xmlChar* xmlChar*)))
  (xmlStreamPop
   (int xmlStreamPop (xmlStreamCtxtPtr)))
  (xmlStreamWantsAnyNode
   (int xmlStreamWantsAnyNode (xmlStreamCtxtPtr))))


;;;; implementation of the Relax-NG validation
;;
;;Header file "relaxng.h"
;;

(define-c-functions libxml2-shared-object
  (xmlRelaxNGInitTypes
   (int xmlRelaxNGInitTypes (void)))
  (xmlRelaxNGCleanupTypes
   (void xmlRelaxNGCleanupTypes (void)))
  (xmlRelaxNGNewParserCtxt
   (xmlRelaxNGParserCtxtPtr xmlRelaxNGNewParserCtxt (char*)))
  (xmlRelaxNGNewMemParserCtxt
   (xmlRelaxNGParserCtxtPtr xmlRelaxNGNewMemParserCtxt (char* int)))
  (xmlRelaxNGNewDocParserCtxt
   (xmlRelaxNGParserCtxtPtr xmlRelaxNGNewDocParserCtxt (xmlDocPtr)))
  (xmlRelaxParserSetFlag
   (int xmlRelaxParserSetFlag (xmlRelaxNGParserCtxtPtr int)))
  (xmlRelaxNGFreeParserCtxt
   (void xmlRelaxNGFreeParserCtxt (xmlRelaxNGParserCtxtPtr)))
  (xmlRelaxNGSetParserErrors
   (void xmlRelaxNGSetParserErrors
	 (xmlRelaxNGParserCtxtPtr xmlRelaxNGValidityErrorFunc xmlRelaxNGValidityWarningFunc void*)))
  (xmlRelaxNGGetParserErrors
   (int xmlRelaxNGGetParserErrors
	(xmlRelaxNGParserCtxtPtr xmlRelaxNGValidityErrorFunc xmlRelaxNGValidityWarningFunc void**)))
  (xmlRelaxNGSetParserStructuredErrors
   (void xmlRelaxNGSetParserStructuredErrors
	 (xmlRelaxNGParserCtxtPtr xmlStructuredErrorFunc void*)))
  (xmlRelaxNGParse
   (xmlRelaxNGPtr xmlRelaxNGParse (xmlRelaxNGParserCtxtPtr)))
  (xmlRelaxNGFree
   (void xmlRelaxNGFree (xmlRelaxNGPtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlRelaxNGDump
   (void xmlRelaxNGDump (FILE* xmlRelaxNGPtr)))
  (xmlRelaxNGDumpTree
   (void xmlRelaxNGDumpTree (FILE* xmlRelaxNGPtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlRelaxNGSetValidErrors
   (void xmlRelaxNGSetValidErrors
	 (xmlRelaxNGValidCtxtPtr xmlRelaxNGValidityErrorFunc* xmlRelaxNGValidityWarningFunc* void*)))
  (xmlRelaxNGGetValidErrors
   (int xmlRelaxNGGetValidErrors
	(xmlRelaxNGValidCtxtPtr xmlRelaxNGValidityErrorFunc* xmlRelaxNGValidityWarningFunc* void**)))
  (xmlRelaxNGSetValidStructuredErrors
   (void xmlRelaxNGSetValidStructuredErrors
	 (xmlRelaxNGValidCtxtPtr xmlStructuredErrorFunc void*)))
  (xmlRelaxNGNewValidCtxt
   (xmlRelaxNGValidCtxtPtr xmlRelaxNGNewValidCtxt (xmlRelaxNGPtr)))
  (xmlRelaxNGFreeValidCtxt
   (void xmlRelaxNGFreeValidCtxt (xmlRelaxNGValidCtxtPtr)))
  (xmlRelaxNGValidateDoc
   (int xmlRelaxNGValidateDoc (xmlRelaxNGValidCtxtPtr xmlDocPtr)))
  (xmlRelaxNGValidatePushElement
   (int xmlRelaxNGValidatePushElement (xmlRelaxNGValidCtxtPtr xmlDocPtr xmlNodePtr)))
  (xmlRelaxNGValidatePushCData
   (int xmlRelaxNGValidatePushCData (xmlRelaxNGValidCtxtPtr xmlChar* int)))
  (xmlRelaxNGValidatePopElement
   (int xmlRelaxNGValidatePopElement (xmlRelaxNGValidCtxtPtr xmlDocPtr xmlNodePtr)))
  (xmlRelaxNGValidateFullElement
   (int xmlRelaxNGValidateFullElement (xmlRelaxNGValidCtxtPtr xmlDocPtr xmlNodePtr))))


;;;; SAX2 parser interface used to build the DOM tree
;;
;;Header file "SAX2.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSAX2GetPublicId
   (xmlChar* xmlSAX2GetPublicId (void*)))
  (xmlSAX2GetSystemId
   (xmlChar* xmlSAX2GetSystemId (void*)))
  (xmlSAX2SetDocumentLocator
   (void xmlSAX2SetDocumentLocator (void* xmlSAXLocatorPtr)))
  (xmlSAX2GetLineNumber
   (int xmlSAX2GetLineNumber (void*)))
  (xmlSAX2GetColumnNumber
   (int xmlSAX2GetColumnNumber (void*)))
  (xmlSAX2IsStandalone
   (int xmlSAX2IsStandalone (void*)))
  (xmlSAX2HasInternalSubset
   (int xmlSAX2HasInternalSubset (void*)))
  (xmlSAX2HasExternalSubset
   (int xmlSAX2HasExternalSubset (void*)))
  (xmlSAX2InternalSubset
   (void xmlSAX2InternalSubset (void* xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2ExternalSubset
   (void xmlSAX2ExternalSubset (void* xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2GetEntity
   (xmlEntityPtr xmlSAX2GetEntity (void* xmlChar*)))
  (xmlSAX2GetParameterEntity
   (xmlEntityPtr xmlSAX2GetParameterEntity (void* xmlChar*)))
  (xmlSAX2ResolveEntity
   (xmlParserInputPtr xmlSAX2ResolveEntity (void* xmlChar* xmlChar*)))
  (xmlSAX2EntityDecl
   (void xmlSAX2EntityDecl (void* xmlChar* int xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2AttributeDecl
   (void xmlSAX2AttributeDecl (void* xmlChar* xmlChar* int xmlChar* xmlEnumerationPtr)))
  (xmlSAX2ElementDecl
   (void xmlSAX2ElementDecl (void* xmlChar* int xmlElementContentPtr)))
  (xmlSAX2NotationDecl
   (void xmlSAX2NotationDecl (void* xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2UnparsedEntityDecl
   (void xmlSAX2UnparsedEntityDecl (void* xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2StartDocument
   (void xmlSAX2StartDocument (void*)))
  (xmlSAX2EndDocument
   (void xmlSAX2EndDocument (void*)))
  ;; #if defined(LIBXML_SAX1_ENABLED) || defined(LIBXML_HTML_ENABLED) || \
  ;;     defined(LIBXML_WRITER_ENABLED) || defined(LIBXML_DOCB_ENABLED)
  (xmlSAX2StartElement
   (void xmlSAX2StartElement (void* xmlChar* xmlChar**)))
  (xmlSAX2EndElement
   (void xmlSAX2EndElement (void* xmlChar*)))
  ;; #endif /* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED */
  (xmlSAX2StartElementNs
   (void xmlSAX2StartElementNs (void* xmlChar* xmlChar* xmlChar* int xmlChar** int int xmlChar**)))
  (xmlSAX2EndElementNs
   (void xmlSAX2EndElementNs (void* xmlChar* xmlChar* xmlChar*)))
  (xmlSAX2Reference
   (void xmlSAX2Reference (void* xmlChar*)))
  (xmlSAX2Characters
   (void xmlSAX2Characters (void* xmlChar* int)))
  (xmlSAX2IgnorableWhitespace
   (void xmlSAX2IgnorableWhitespace (void* xmlChar* int)))
  (xmlSAX2ProcessingInstruction
   (void xmlSAX2ProcessingInstruction (void* xmlChar* xmlChar*)))
  (xmlSAX2Comment
   (void xmlSAX2Comment (void* xmlChar*)))
  (xmlSAX2CDataBlock
   (void xmlSAX2CDataBlock (void* xmlChar* int)))
  ;; #ifdef LIBXML_SAX1_ENABLED
  (xmlSAXDefaultVersion
   (int xmlSAXDefaultVersion (int)))
  ;; #endif /* LIBXML_SAX1_ENABLED */
  (xmlSAXVersion
   (int xmlSAXVersion (xmlSAXHandler* int)))
  (xmlSAX2InitDefaultSAXHandler
   (void xmlSAX2InitDefaultSAXHandler (xmlSAXHandler* int)))
  ;; #ifdef LIBXML_HTML_ENABLED
  (xmlSAX2InitHtmlDefaultSAXHandler
   (void xmlSAX2InitHtmlDefaultSAXHandler (xmlSAXHandler*)))
  (htmlDefaultSAXHandlerInit
   (void htmlDefaultSAXHandlerInit (void)))
  ;; #endif
  ;; #ifdef LIBXML_DOCB_ENABLED
  (xmlSAX2InitDocbDefaultSAXHandler
   (void xmlSAX2InitDocbDefaultSAXHandler (xmlSAXHandler*)))
  (docbDefaultSAXHandlerInit
   (void docbDefaultSAXHandlerInit (void)))
  ;; #endif
  (xmlDefaultSAXHandlerInit
   (void xmlDefaultSAXHandlerInit (void))))



;;;; XML Schematron implementation
;;
;;Header file "schematron.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSchematronNewParserCtxt
   (xmlSchematronParserCtxtPtr xmlSchematronNewParserCtxt (char*)))
  (xmlSchematronNewMemParserCtxt
   (xmlSchematronParserCtxtPtr xmlSchematronNewMemParserCtxt (char* int)))
  (xmlSchematronNewDocParserCtxt
   (xmlSchematronParserCtxtPtr xmlSchematronNewDocParserCtxt (xmlDocPtr)))
  (xmlSchematronFreeParserCtxt
   (void xmlSchematronFreeParserCtxt (xmlSchematronParserCtxtPtr)))
;;; (xmlSchematronSetParserErrors
;;;  (void xmlSchematronSetParserErrors (xmlSchematronParserCtxtPtr
;;; 				       xmlSchematronValidityErrorFunc
;;; 				       xmlSchematronValidityWarningFunc
;;; 				       void*)))
;;; (xmlSchematronGetParserErrors
;;;  (int xmlSchematronGetParserErrors (xmlSchematronParserCtxtPtr
;;; 				      xmlSchematronValidityErrorFunc*
;;; 				      xmlSchematronValidityWarningFunc*
;;; 				      void**)))
;;; (xmlSchematronIsValid
;;;  (int xmlSchematronIsValid (xmlSchematronValidCtxtPtr)))
  (xmlSchematronParse
   (xmlSchematronPtr xmlSchematronParse (xmlSchematronParserCtxtPtr)))
  (xmlSchematronFree
   (void xmlSchematronFree (xmlSchematronPtr)))
  (xmlSchematronSetValidStructuredErrors
   (void xmlSchematronSetValidStructuredErrors (xmlSchematronValidCtxtPtr
						xmlStructuredErrorFunc
						void*)))
;;; (xmlSchematronSetValidErrors
;;;  (void xmlSchematronSetValidErrors (xmlSchematronValidCtxtPtr
;;; 				      xmlSchematronValidityErrorFunc
;;; 				      xmlSchematronValidityWarningFunc
;;; 				      void*)))
;;; (xmlSchematronGetValidErrors
;;;  (int xmlSchematronGetValidErrors (xmlSchematronValidCtxtPtr
;;; 				     xmlSchematronValidityErrorFunc*
;;; 				     xmlSchematronValidityWarningFunc*
;;; 				     void**)))
;;; (xmlSchematronSetValidOptions
;;;  (int xmlSchematronSetValidOptions (xmlSchematronValidCtxtPtr int)))
;;; (xmlSchematronValidCtxtGetOptions
;;;  (int xmlSchematronValidCtxtGetOptions (xmlSchematronValidCtxtPtr)))
;;; (xmlSchematronValidateOneElement
;;;  (int xmlSchematronValidateOneElement (xmlSchematronValidCtxtPtr xmlNodePtr)))
  (xmlSchematronNewValidCtxt
   (xmlSchematronValidCtxtPtr xmlSchematronNewValidCtxt (xmlSchematronPtr int)))
  (xmlSchematronFreeValidCtxt
   (void xmlSchematronFreeValidCtxt (xmlSchematronValidCtxtPtr)))
  (xmlSchematronValidateDoc
   (int xmlSchematronValidateDoc (xmlSchematronValidCtxtPtr xmlDocPtr))))



;;;; interfaces for thread handling
;;
;;Header file "thread.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNewMutex
   (xmlMutexPtr xmlNewMutex (void)))
  (xmlMutexLock
   (void xmlMutexLock (xmlMutexPtr)))
  (xmlMutexUnlock
   (void xmlMutexUnlock (xmlMutexPtr)))
  (xmlFreeMutex
   (void xmlFreeMutex (xmlMutexPtr)))
  (xmlNewRMutex
   (xmlRMutexPtr xmlNewRMutex (void)))
  (xmlRMutexLock
   (void xmlRMutexLock (xmlRMutexPtr)))
  (xmlRMutexUnlock
   (void xmlRMutexUnlock (xmlRMutexPtr)))
  (xmlFreeRMutex
   (void xmlFreeRMutex (xmlRMutexPtr)))
  (xmlInitThreads
   (void xmlInitThreads (void)))
  (xmlLockLibrary
   (void xmlLockLibrary (void)))
  (xmlUnlockLibrary
   (void xmlUnlockLibrary (void)))
  (xmlGetThreadId
   (int xmlGetThreadId (void)))
  (xmlIsMainThread
   (int xmlIsMainThread (void)))
  (xmlCleanupThreads
   (void xmlCleanupThreads (void)))
  (xmlGetGlobalState
   (xmlGlobalStatePtr xmlGetGlobalState (void)))
  ;; #if defined(HAVE_WIN32_THREADS) && !defined(HAVE_COMPILER_TLS) && defined(LIBXML_STATIC_FOR_DLL)
;;;  (xmlDllMain
;;;   (int xmlDllMain (void* unsigned-long void*)))
  ;; #endif
  )


;;;; interfaces for tree manipulation
;;
;;Header file "tree.h".
;;

(define-c-functions libxml2-shared-object
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) || \
  ;;     defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_DEBUG_ENABLED) || \
  ;;     defined (LIBXML_HTML_ENABLED) || defined(LIBXML_SAX1_ENABLED) || \
  ;;     defined(LIBXML_HTML_ENABLED) || defined(LIBXML_WRITER_ENABLED) || \
  ;;     defined(LIBXML_DOCB_ENABLED)
  (xmlValidateNCName
   (int xmlValidateNCName (xmlChar* int)))
  ;; #endif
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlValidateQName
   (int xmlValidateQName (xmlChar* int)))
  (xmlValidateName
   (int xmlValidateName (xmlChar* int)))
  (xmlValidateNMToken
   (int xmlValidateNMToken (xmlChar* int)))
  ;; #endif
  (xmlBuildQName
   (xmlChar* xmlBuildQName (xmlChar* xmlChar* xmlChar* int)))
  (xmlSplitQName2
   (xmlChar* xmlSplitQName2 (xmlChar* xmlChar**)))
  (xmlSplitQName3
   (xmlChar* xmlSplitQName3 (xmlChar* int*)))
  (xmlSetBufferAllocationScheme
   (void xmlSetBufferAllocationScheme (xmlBufferAllocationScheme)))
  (xmlGetBufferAllocationScheme
   (xmlBufferAllocationScheme xmlGetBufferAllocationScheme (void)))
  (xmlBufferCreate
   (xmlBufferPtr xmlBufferCreate (void)))
  (xmlBufferCreateSize
   (xmlBufferPtr xmlBufferCreateSize (size_t)))
  (xmlBufferCreateStatic
   (xmlBufferPtr xmlBufferCreateStatic (void* size_t)))
  (xmlBufferResize
   (int xmlBufferResize (xmlBufferPtr unsigned-int)))
  (xmlBufferFree
   (void xmlBufferFree (xmlBufferPtr)))
  (xmlBufferDump
   (int xmlBufferDump (FILE* xmlBufferPtr)))
  (xmlBufferAdd
   (int xmlBufferAdd (xmlBufferPtr xmlChar* int)))
  (xmlBufferAddHead
   (int xmlBufferAddHead (xmlBufferPtr xmlChar* int)))
  (xmlBufferCat
   (int xmlBufferCat (xmlBufferPtr xmlChar*)))
  (xmlBufferCCat
   (int xmlBufferCCat (xmlBufferPtr char*)))
  (xmlBufferShrink
   (int xmlBufferShrink (xmlBufferPtr unsigned-int)))
  (xmlBufferGrow
   (int xmlBufferGrow (xmlBufferPtr unsigned-int)))
  (xmlBufferEmpty
   (void xmlBufferEmpty (xmlBufferPtr)))
  (xmlBufferContent
   (xmlChar* xmlBufferContent (xmlBufferPtr)))
  (xmlBufferSetAllocationScheme
   (void xmlBufferSetAllocationScheme (xmlBufferPtr xmlBufferAllocationScheme)))
  (xmlBufferLength
   (int xmlBufferLength (xmlBufferPtr)))
  (xmlCreateIntSubset
   (xmlDtdPtr xmlCreateIntSubset (xmlDocPtr xmlChar* xmlChar* xmlChar*)))
  (xmlNewDtd
   (xmlDtdPtr xmlNewDtd (xmlDocPtr xmlChar* xmlChar* xmlChar*)))
  (xmlGetIntSubset
   (xmlDtdPtr xmlGetIntSubset (xmlDocPtr)))
  (xmlFreeDtd
   (void xmlFreeDtd (xmlDtdPtr)))
  ;; #ifdef LIBXML_LEGACY_ENABLED
  (xmlNewGlobalNs
   (xmlNsPtr xmlNewGlobalNs (xmlDocPtr xmlChar* xmlChar*)))
  ;; #endif /* LIBXML_LEGACY_ENABLED */
  (xmlNewNs
   (xmlNsPtr xmlNewNs (xmlNodePtr xmlChar* xmlChar*)))
  (xmlFreeNs
   (void xmlFreeNs (xmlNsPtr)))
  (xmlFreeNsList
   (void xmlFreeNsList (xmlNsPtr)))
  (xmlNewDoc
   (xmlDocPtr xmlNewDoc (xmlChar*)))
  (xmlFreeDoc
   (void xmlFreeDoc (xmlDocPtr)))
  (xmlNewDocProp
   (xmlAttrPtr xmlNewDocProp (xmlDocPtr xmlChar* xmlChar*)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_HTML_ENABLED) || \
  ;;     defined(LIBXML_SCHEMAS_ENABLED)
  (xmlNewProp
   (xmlAttrPtr xmlNewProp (xmlNodePtr xmlChar* xmlChar*)))
  ;; #endif
  (xmlNewNsProp
   (xmlAttrPtr xmlNewNsProp (xmlNodePtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlNewNsPropEatName
   (xmlAttrPtr xmlNewNsPropEatName (xmlNodePtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlFreePropList
   (void xmlFreePropList (xmlAttrPtr)))
  (xmlFreeProp
   (void xmlFreeProp (xmlAttrPtr)))
  (xmlCopyProp
   (xmlAttrPtr xmlCopyProp (xmlNodePtr xmlAttrPtr)))
  (xmlCopyPropList
   (xmlAttrPtr xmlCopyPropList (xmlNodePtr xmlAttrPtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyDtd
   (xmlDtdPtr xmlCopyDtd (xmlDtdPtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlCopyDoc
   (xmlDocPtr xmlCopyDoc (xmlDocPtr int)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) */
  (xmlNewDocNode
   (xmlNodePtr xmlNewDocNode (xmlDocPtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlNewDocNodeEatName
   (xmlNodePtr xmlNewDocNodeEatName (xmlDocPtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlNewNode
   (xmlNodePtr xmlNewNode (xmlNsPtr xmlChar*)))
  (xmlNewNodeEatName
   (xmlNodePtr xmlNewNodeEatName (xmlNsPtr xmlChar*)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlNewChild
   (xmlNodePtr xmlNewChild (xmlNodePtr xmlNsPtr xmlChar* xmlChar*)))
  ;; #endif
  (xmlNewDocText
   (xmlNodePtr xmlNewDocText (xmlDocPtr xmlChar*)))
  (xmlNewText
   (xmlNodePtr xmlNewText (xmlChar*)))
  (xmlNewDocPI
   (xmlNodePtr xmlNewDocPI (xmlDocPtr xmlChar* xmlChar*)))
  (xmlNewPI
   (xmlNodePtr xmlNewPI (xmlChar* xmlChar*)))
  (xmlNewDocTextLen
   (xmlNodePtr xmlNewDocTextLen (xmlDocPtr xmlChar* int)))
  (xmlNewTextLen
   (xmlNodePtr xmlNewTextLen (xmlChar* int)))
  (xmlNewDocComment
   (xmlNodePtr xmlNewDocComment (xmlDocPtr xmlChar*)))
  (xmlNewComment
   (xmlNodePtr xmlNewComment (xmlChar*)))
  (xmlNewCDataBlock
   (xmlNodePtr xmlNewCDataBlock (xmlDocPtr xmlChar* int)))
  (xmlNewCharRef
   (xmlNodePtr xmlNewCharRef (xmlDocPtr xmlChar*)))
  (xmlNewReference
   (xmlNodePtr xmlNewReference (xmlDocPtr xmlChar*)))
  (xmlCopyNode
   (xmlNodePtr xmlCopyNode (xmlNodePtr int)))
  (xmlDocCopyNode
   (xmlNodePtr xmlDocCopyNode (xmlNodePtr xmlDocPtr int)))
  (xmlDocCopyNodeList
   (xmlNodePtr xmlDocCopyNodeList (xmlDocPtr xmlNodePtr)))
  (xmlCopyNodeList
   (xmlNodePtr xmlCopyNodeList (xmlNodePtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlNewTextChild
   (xmlNodePtr xmlNewTextChild (xmlNodePtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlNewDocRawNode
   (xmlNodePtr xmlNewDocRawNode (xmlDocPtr xmlNsPtr xmlChar* xmlChar*)))
  (xmlNewDocFragment
   (xmlNodePtr xmlNewDocFragment (xmlDocPtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlGetLineNo
   (long xmlGetLineNo (xmlNodePtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED)
  (xmlGetNodePath
   (xmlChar* xmlGetNodePath (xmlNodePtr)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED) */
  (xmlDocGetRootElement
   (xmlNodePtr xmlDocGetRootElement (xmlDocPtr)))
  (xmlGetLastChild
   (xmlNodePtr xmlGetLastChild (xmlNodePtr)))
  (xmlNodeIsText
   (int xmlNodeIsText (xmlNodePtr)))
  (xmlIsBlankNode
   (int xmlIsBlankNode (xmlNodePtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED)
  (xmlDocSetRootElement
   (xmlNodePtr xmlDocSetRootElement (xmlDocPtr xmlNodePtr)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) */
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlNodeSetName
   (void xmlNodeSetName (xmlNodePtr xmlChar*)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlAddChild
   (xmlNodePtr xmlAddChild (xmlNodePtr xmlNodePtr)))
  (xmlAddChildList
   (xmlNodePtr xmlAddChildList (xmlNodePtr xmlNodePtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED)
  (xmlReplaceNode
   (xmlNodePtr xmlReplaceNode (xmlNodePtr xmlNodePtr)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) */
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_HTML_ENABLED) || \
  ;;     defined(LIBXML_SCHEMAS_ENABLED)
  (xmlAddPrevSibling
   (xmlNodePtr xmlAddPrevSibling (xmlNodePtr xmlNodePtr)))
  ;; #endif /* LIBXML_TREE_ENABLED || LIBXML_HTML_ENABLED || LIBXML_SCHEMAS_ENABLED */
  (xmlAddSibling
   (xmlNodePtr xmlAddSibling (xmlNodePtr xmlNodePtr)))
  (xmlAddNextSibling
   (xmlNodePtr xmlAddNextSibling (xmlNodePtr xmlNodePtr)))
  (xmlUnlinkNode
   (void xmlUnlinkNode (xmlNodePtr)))
  (xmlTextMerge
   (xmlNodePtr xmlTextMerge (xmlNodePtr xmlNodePtr)))
  (xmlTextConcat
   (int xmlTextConcat (xmlNodePtr xmlChar* int)))
  (xmlFreeNodeList
   (void xmlFreeNodeList (xmlNodePtr)))
  (xmlFreeNode
   (void xmlFreeNode (xmlNodePtr)))
  (xmlSetTreeDoc
   (void xmlSetTreeDoc (xmlNodePtr xmlDocPtr)))
  (xmlSetListDoc
   (void xmlSetListDoc (xmlNodePtr xmlDocPtr)))
  (xmlSearchNs
   (xmlNsPtr xmlSearchNs (xmlDocPtr xmlNodePtr xmlChar*)))
  (xmlSearchNsByHref
   (xmlNsPtr xmlSearchNsByHref (xmlDocPtr xmlNodePtr xmlChar*)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlGetNsList
   (xmlNsPtr* xmlGetNsList (xmlDocPtr xmlNodePtr)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) */
  (xmlSetNs
   (void xmlSetNs (xmlNodePtr xmlNsPtr)))
  (xmlCopyNamespace
   (xmlNsPtr xmlCopyNamespace (xmlNsPtr)))
  (xmlCopyNamespaceList
   (xmlNsPtr xmlCopyNamespaceList (xmlNsPtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED) || \
  ;;     defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_HTML_ENABLED)
  (xmlSetProp
   (xmlAttrPtr xmlSetProp (xmlNodePtr xmlChar* xmlChar*)))
  (xmlSetNsProp
   (xmlAttrPtr xmlSetNsProp (xmlNodePtr xmlNsPtr xmlChar* xmlChar*)))
  ;; #endif
  (xmlGetNoNsProp
   (xmlChar* xmlGetNoNsProp (xmlNodePtr xmlChar*)))
  (xmlGetProp
   (xmlChar* xmlGetProp (xmlNodePtr xmlChar*)))
  (xmlHasProp
   (xmlAttrPtr xmlHasProp (xmlNodePtr xmlChar*)))
  (xmlHasNsProp
   (xmlAttrPtr xmlHasNsProp (xmlNodePtr xmlChar* xmlChar*)))
  (xmlGetNsProp
   (xmlChar* xmlGetNsProp (xmlNodePtr xmlChar* xmlChar*)))
  (xmlStringGetNodeList
   (xmlNodePtr xmlStringGetNodeList (xmlDocPtr xmlChar*)))
  (xmlStringLenGetNodeList
   (xmlNodePtr xmlStringLenGetNodeList (xmlDocPtr xmlChar* int)))
  (xmlNodeListGetString
   (xmlChar* xmlNodeListGetString (xmlDocPtr xmlNodePtr int)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlNodeListGetRawString
   (xmlChar* xmlNodeListGetRawString (xmlDocPtr xmlNodePtr int)))
  ;; #endif
  (xmlNodeSetContent
   (void xmlNodeSetContent (xmlNodePtr xmlChar*)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlNodeSetContentLen
   (void xmlNodeSetContentLen (xmlNodePtr xmlChar* int)))
  ;; #endif
  (xmlNodeAddContent
   (void xmlNodeAddContent (xmlNodePtr xmlChar*)))
  (xmlNodeAddContentLen
   (void xmlNodeAddContentLen (xmlNodePtr xmlChar* int)))
  (xmlNodeGetContent
   (xmlChar* xmlNodeGetContent (xmlNodePtr)))
  (xmlNodeBufGetContent
   (int xmlNodeBufGetContent (xmlBufferPtr xmlNodePtr)))
  (xmlNodeGetLang
   (xmlChar* xmlNodeGetLang (xmlNodePtr)))
  (xmlNodeGetSpacePreserve
   (int xmlNodeGetSpacePreserve (xmlNodePtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlNodeSetLang
   (void xmlNodeSetLang (xmlNodePtr xmlChar*)))
  (xmlNodeSetSpacePreserve
   (void xmlNodeSetSpacePreserve (xmlNodePtr int)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlNodeGetBase
   (xmlChar* xmlNodeGetBase (xmlDocPtr xmlNodePtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED)
  (xmlNodeSetBase
   (void xmlNodeSetBase (xmlNodePtr xmlChar*)))
  ;; #endif
  (xmlRemoveProp
   (int xmlRemoveProp (xmlAttrPtr)))
  ;; #if defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlUnsetNsProp
   (int xmlUnsetNsProp (xmlNodePtr xmlNsPtr xmlChar*)))
  (xmlUnsetProp
   (int xmlUnsetProp (xmlNodePtr xmlChar*)))
  ;; #endif /* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) */
  (xmlBufferWriteCHAR
   (void xmlBufferWriteCHAR (xmlBufferPtr xmlChar*)))
  (xmlBufferWriteChar
   (void xmlBufferWriteChar (xmlBufferPtr char*)))
  (xmlBufferWriteQuotedString
   (void xmlBufferWriteQuotedString (xmlBufferPtr xmlChar*)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlAttrSerializeTxtContent
   (void xmlAttrSerializeTxtContent (xmlBufferPtr xmlDocPtr xmlAttrPtr xmlChar*)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlReconciliateNs
   (int xmlReconciliateNs (xmlDocPtr xmlNodePtr)))
  ;; #endif
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlDocDumpFormatMemory
   (void xmlDocDumpFormatMemory (xmlDocPtr xmlChar** int* int)))
  (xmlDocDumpMemory
   (void xmlDocDumpMemory (xmlDocPtr xmlChar** int*)))
  (xmlDocDumpMemoryEnc
   (void xmlDocDumpMemoryEnc (xmlDocPtr xmlChar** int* char*)))
  (xmlDocDumpFormatMemoryEnc
   (void xmlDocDumpFormatMemoryEnc (xmlDocPtr xmlChar** int* char* int)))
  (xmlDocFormatDump
   (int xmlDocFormatDump (FILE* xmlDocPtr int)))
  (xmlDocDump
   (int xmlDocDump (FILE* xmlDocPtr)))
  (xmlElemDump
   (void xmlElemDump (FILE* xmlDocPtr xmlNodePtr)))
  (xmlSaveFile
   (int xmlSaveFile (char* xmlDocPtr)))
  (xmlSaveFormatFile
   (int xmlSaveFormatFile (char* xmlDocPtr int)))
  (xmlNodeDump
   (int xmlNodeDump (xmlBufferPtr xmlDocPtr xmlNodePtr int int)))
  (xmlSaveFileTo
   (int xmlSaveFileTo (xmlOutputBufferPtr xmlDocPtr char*)))
  (xmlSaveFormatFileTo
   (int xmlSaveFormatFileTo (xmlOutputBufferPtr xmlDocPtr char* int)))
  (xmlNodeDumpOutput
   (void xmlNodeDumpOutput (xmlOutputBufferPtr xmlDocPtr xmlNodePtr int int char*)))
  (xmlSaveFormatFileEnc
   (int xmlSaveFormatFileEnc (char* xmlDocPtr char* int)))
  (xmlSaveFileEnc
   (int xmlSaveFileEnc (char* xmlDocPtr char*)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlIsXHTML
   (int xmlIsXHTML (xmlChar* xmlChar*)))
  (xmlGetDocCompressMode
   (int xmlGetDocCompressMode (xmlDocPtr)))
  (xmlSetDocCompressMode
   (void xmlSetDocCompressMode (xmlDocPtr int)))
  (xmlGetCompressMode
   (int xmlGetCompressMode (void)))
  (xmlSetCompressMode
   (void xmlSetCompressMode (int)))
  (xmlDOMWrapNewCtxt
   (xmlDOMWrapCtxtPtr xmlDOMWrapNewCtxt (void)))
  (xmlDOMWrapFreeCtxt
   (void xmlDOMWrapFreeCtxt (xmlDOMWrapCtxtPtr)))
  (xmlDOMWrapReconcileNamespaces
   (int xmlDOMWrapReconcileNamespaces (xmlDOMWrapCtxtPtr xmlNodePtr int)))
  (xmlDOMWrapAdoptNode
   (int xmlDOMWrapAdoptNode (xmlDOMWrapCtxtPtr xmlDocPtr xmlNodePtr xmlDocPtr xmlNodePtr int)))
  (xmlDOMWrapRemoveNode
   (int xmlDOMWrapRemoveNode (xmlDOMWrapCtxtPtr xmlDocPtr xmlNodePtr int)))
  (xmlDOMWrapCloneNode
   (int xmlDOMWrapCloneNode
	(xmlDOMWrapCtxtPtr xmlDocPtr xmlNodePtr xmlNodePtr* xmlDocPtr xmlNodePtr int int)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlChildElementCount
   (unsigned-long xmlChildElementCount (xmlNodePtr)))
  (xmlNextElementSibling
   (xmlNodePtr xmlNextElementSibling (xmlNodePtr)))
  (xmlFirstElementChild
   (xmlNodePtr xmlFirstElementChild (xmlNodePtr)))
  (xmlLastElementChild
   (xmlNodePtr xmlLastElementChild (xmlNodePtr)))
  (xmlPreviousElementSibling
   (xmlNodePtr xmlPreviousElementSibling (xmlNodePtr)))
  ;; #endif
  )


;;;; library of generic URI related routines
;;
;;Header file "uri.h".
;;

(define-c-functions libxml2-shared-object
  (xmlCreateURI
   (xmlURIPtr xmlCreateURI (void)))
  (xmlBuildURI
   (xmlChar* xmlBuildURI (xmlChar* xmlChar*)))
  (xmlBuildRelativeURI
   (xmlChar* xmlBuildRelativeURI (xmlChar* xmlChar*)))
  (xmlParseURI
   (xmlURIPtr xmlParseURI (char*)))
  (xmlParseURIRaw
   (xmlURIPtr xmlParseURIRaw (char* int)))
  (xmlParseURIReference
   (int xmlParseURIReference (xmlURIPtr char*)))
  (xmlSaveUri
   (xmlChar* xmlSaveUri (xmlURIPtr)))
  (xmlPrintURI
   (void xmlPrintURI (FILE* xmlURIPtr)))
  (xmlURIEscapeStr
   (xmlChar* xmlURIEscapeStr (xmlChar* xmlChar*)))
  (xmlURIUnescapeString
   (char* xmlURIUnescapeString (char* int char*)))
  (xmlNormalizeURIPath
   (int xmlNormalizeURIPath (char*)))
  (xmlURIEscape
   (xmlChar* xmlURIEscape (xmlChar*)))
  (xmlFreeURI
   (void xmlFreeURI (xmlURIPtr)))
  (xmlCanonicPath
   (xmlChar* xmlCanonicPath (xmlChar*)))
  (xmlPathToURI
   (xmlChar* xmlPathToURI (xmlChar*))))


;;;; The DTD validation
;;
;;Header file "valid.h".
;;

(define-c-functions libxml2-shared-object
  (xmlAddNotationDecl
   (xmlNotationPtr xmlAddNotationDecl (xmlValidCtxtPtr xmlDtdPtr xmlChar* xmlChar* xmlChar*)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyNotationTable
   (xmlNotationTablePtr xmlCopyNotationTable (xmlNotationTablePtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlFreeNotationTable
   (void xmlFreeNotationTable (xmlNotationTablePtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlDumpNotationDecl
   (void xmlDumpNotationDecl (xmlBufferPtr xmlNotationPtr)))
  (xmlDumpNotationTable
   (void xmlDumpNotationTable (xmlBufferPtr xmlNotationTablePtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlNewElementContent
   (xmlElementContentPtr xmlNewElementContent (xmlChar* xmlElementContentType)))
  (xmlCopyElementContent
   (xmlElementContentPtr xmlCopyElementContent (xmlElementContentPtr)))
  (xmlFreeElementContent
   (void xmlFreeElementContent (xmlElementContentPtr)))
  (xmlNewDocElementContent
   (xmlElementContentPtr xmlNewDocElementContent (xmlDocPtr xmlChar* xmlElementContentType)))
  (xmlCopyDocElementContent
   (xmlElementContentPtr xmlCopyDocElementContent (xmlDocPtr xmlElementContentPtr)))
  (xmlFreeDocElementContent
   (void xmlFreeDocElementContent (xmlDocPtr xmlElementContentPtr)))
  (xmlSnprintfElementContent
   (void xmlSnprintfElementContent (char* int xmlElementContentPtr int)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlSprintfElementContent
   (void xmlSprintfElementContent (char* xmlElementContentPtr int)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlAddElementDecl
   (xmlElementPtr xmlAddElementDecl
		  (xmlValidCtxtPtr xmlDtdPtr xmlChar* xmlElementTypeVal xmlElementContentPtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyElementTable
   (xmlElementTablePtr xmlCopyElementTable (xmlElementTablePtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlFreeElementTable
   (void xmlFreeElementTable (xmlElementTablePtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlDumpElementTable
   (void xmlDumpElementTable (xmlBufferPtr xmlElementTablePtr)))
  (xmlDumpElementDecl
   (void xmlDumpElementDecl (xmlBufferPtr xmlElementPtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlCreateEnumeration
   (xmlEnumerationPtr xmlCreateEnumeration (xmlChar*)))
  (xmlFreeEnumeration
   (void xmlFreeEnumeration (xmlEnumerationPtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyEnumeration
   (xmlEnumerationPtr xmlCopyEnumeration (xmlEnumerationPtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlAddAttributeDecl
   (xmlAttributePtr xmlAddAttributeDecl
		    (xmlValidCtxtPtr xmlDtdPtr xmlChar* xmlChar* xmlChar*
				     xmlAttributeType xmlAttributeDefault xmlChar* xmlEnumerationPtr)))
  ;; #ifdef LIBXML_TREE_ENABLED
  (xmlCopyAttributeTable
   (xmlAttributeTablePtr xmlCopyAttributeTable (xmlAttributeTablePtr)))
  ;; #endif /* LIBXML_TREE_ENABLED */
  (xmlFreeAttributeTable
   (void xmlFreeAttributeTable (xmlAttributeTablePtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlDumpAttributeTable
   (void xmlDumpAttributeTable (xmlBufferPtr xmlAttributeTablePtr)))
  (xmlDumpAttributeDecl
   (void xmlDumpAttributeDecl (xmlBufferPtr xmlAttributePtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlAddID
   (xmlIDPtr xmlAddID (xmlValidCtxtPtr xmlDocPtr xmlChar* xmlAttrPtr)))
  (xmlFreeIDTable
   (void xmlFreeIDTable (xmlIDTablePtr)))
  (xmlGetID
   (xmlAttrPtr xmlGetID (xmlDocPtr xmlChar*)))
  (xmlIsID
   (int xmlIsID (xmlDocPtr xmlNodePtr xmlAttrPtr)))
  (xmlRemoveID
   (int xmlRemoveID (xmlDocPtr xmlAttrPtr)))
  (xmlAddRef
   (xmlRefPtr xmlAddRef (xmlValidCtxtPtr xmlDocPtr xmlChar* xmlAttrPtr)))
  (xmlFreeRefTable
   (void xmlFreeRefTable (xmlRefTablePtr)))
  (xmlIsRef
   (int xmlIsRef (xmlDocPtr xmlNodePtr xmlAttrPtr)))
  (xmlRemoveRef
   (int xmlRemoveRef (xmlDocPtr xmlAttrPtr)))
  (xmlGetRefs
   (xmlListPtr xmlGetRefs (xmlDocPtr xmlChar*)))
  ;; #ifdef LIBXML_VALID_ENABLED
  (xmlNewValidCtxt
   (xmlValidCtxtPtr xmlNewValidCtxt (void)))
  (xmlFreeValidCtxt
   (void xmlFreeValidCtxt (xmlValidCtxtPtr)))
  (xmlValidateRoot
   (int xmlValidateRoot (xmlValidCtxtPtr xmlDocPtr)))
  (xmlValidateElementDecl
   (int xmlValidateElementDecl (xmlValidCtxtPtr xmlDocPtr xmlElementPtr)))
  (xmlValidNormalizeAttributeValue
   (xmlChar* xmlValidNormalizeAttributeValue (xmlDocPtr xmlNodePtr xmlChar* xmlChar*)))
  (xmlValidCtxtNormalizeAttributeValue
   (xmlChar* xmlValidCtxtNormalizeAttributeValue
	     (xmlValidCtxtPtr xmlDocPtr xmlNodePtr xmlChar* xmlChar*)))
  (xmlValidateAttributeDecl
   (int xmlValidateAttributeDecl (xmlValidCtxtPtr xmlDocPtr xmlAttributePtr)))
  (xmlValidateAttributeValue
   (int xmlValidateAttributeValue (xmlAttributeType xmlChar*)))
  (xmlValidateNotationDecl
   (int xmlValidateNotationDecl (xmlValidCtxtPtr xmlDocPtr xmlNotationPtr)))
  (xmlValidateDtd
   (int xmlValidateDtd (xmlValidCtxtPtr xmlDocPtr xmlDtdPtr)))
  (xmlValidateDtdFinal
   (int xmlValidateDtdFinal (xmlValidCtxtPtr xmlDocPtr)))
  (xmlValidateDocument
   (int xmlValidateDocument (xmlValidCtxtPtr xmlDocPtr)))
  (xmlValidateElement
   (int xmlValidateElement (xmlValidCtxtPtr xmlDocPtr xmlNodePtr)))
  (xmlValidateOneElement
   (int xmlValidateOneElement (xmlValidCtxtPtr xmlDocPtr xmlNodePtr)))
  (xmlValidateOneAttribute
   (int xmlValidateOneAttribute (xmlValidCtxtPtr xmlDocPtr xmlNodePtr xmlAttrPtr xmlChar*)))
  (xmlValidateOneNamespace
   (int xmlValidateOneNamespace (xmlValidCtxtPtr xmlDocPtr xmlNodePtr xmlChar* xmlNsPtr xmlChar*)))
  (xmlValidateDocumentFinal
   (int xmlValidateDocumentFinal (xmlValidCtxtPtr xmlDocPtr)))
  ;; #endif /* LIBXML_VALID_ENABLED */
  ;; #if defined(LIBXML_VALID_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlValidateNotationUse
   (int xmlValidateNotationUse (xmlValidCtxtPtr xmlDocPtr xmlChar*)))
  ;; #endif /* LIBXML_VALID_ENABLED or LIBXML_SCHEMAS_ENABLED */
  (xmlIsMixedElement
   (int xmlIsMixedElement (xmlDocPtr xmlChar*)))
  (xmlGetDtdAttrDesc
   (xmlAttributePtr xmlGetDtdAttrDesc (xmlDtdPtr xmlChar* xmlChar*)))
  (xmlGetDtdQAttrDesc
   (xmlAttributePtr xmlGetDtdQAttrDesc (xmlDtdPtr xmlChar* xmlChar* xmlChar*)))
  (xmlGetDtdNotationDesc
   (xmlNotationPtr xmlGetDtdNotationDesc (xmlDtdPtr xmlChar*)))
  (xmlGetDtdQElementDesc
   (xmlElementPtr xmlGetDtdQElementDesc (xmlDtdPtr xmlChar* xmlChar*)))
  (xmlGetDtdElementDesc
   (xmlElementPtr xmlGetDtdElementDesc (xmlDtdPtr xmlChar*)))
  ;; #ifdef LIBXML_VALID_ENABLED
  (xmlValidGetPotentialChildren
   (int xmlValidGetPotentialChildren (xmlElementContent* xmlChar** int* int)))
  (xmlValidGetValidElements
   (int xmlValidGetValidElements (xmlNode* xmlNode* xmlChar** int)))
  (xmlValidateNameValue
   (int xmlValidateNameValue (xmlChar*)))
  (xmlValidateNamesValue
   (int xmlValidateNamesValue (xmlChar*)))
  (xmlValidateNmtokenValue
   (int xmlValidateNmtokenValue (xmlChar*)))
  (xmlValidateNmtokensValue
   (int xmlValidateNmtokensValue (xmlChar*)))
  ;; #ifdef LIBXML_REGEXP_ENABLED
  (xmlValidBuildContentModel
   (int xmlValidBuildContentModel (xmlValidCtxtPtr xmlElementPtr)))
  (xmlValidatePushElement
   (int xmlValidatePushElement (xmlValidCtxtPtr xmlDocPtr xmlNodePtr xmlChar*)))
  (xmlValidatePushCData
   (int xmlValidatePushCData (xmlValidCtxtPtr xmlChar* int)))
  (xmlValidatePopElement
   (int xmlValidatePopElement (xmlValidCtxtPtr xmlDocPtr xmlNodePtr xmlChar*)))
  ;; #endif /* LIBXML_REGEXP_ENABLED */
  )


;;;; implementation of XInclude
;;
;;Header file "xinclude.h".
;;

(define-c-functions libxml2-shared-object
  (xmlXIncludeProcess
   (int xmlXIncludeProcess (xmlDocPtr)))
  (xmlXIncludeProcessFlags
   (int xmlXIncludeProcessFlags (xmlDocPtr int)))
  (xmlXIncludeProcessFlagsData
   (int xmlXIncludeProcessFlagsData (xmlDocPtr int void*)))
  (xmlXIncludeProcessTreeFlagsData
   (int xmlXIncludeProcessTreeFlagsData (xmlNodePtr int void*)))
  (xmlXIncludeProcessTree
   (int xmlXIncludeProcessTree (xmlNodePtr)))
  (xmlXIncludeProcessTreeFlags
   (int xmlXIncludeProcessTreeFlags (xmlNodePtr int)))
  (xmlXIncludeNewContext
   (xmlXIncludeCtxtPtr xmlXIncludeNewContext (xmlDocPtr)))
  (xmlXIncludeSetFlags
   (int xmlXIncludeSetFlags (xmlXIncludeCtxtPtr int)))
  (xmlXIncludeFreeContext
   (void xmlXIncludeFreeContext (xmlXIncludeCtxtPtr)))
  (xmlXIncludeProcessNode
   (int xmlXIncludeProcessNode (xmlXIncludeCtxtPtr xmlNodePtr))))



;;;; API to build regexp automata
;;
;;Header file "xmlautomata.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNewAutomata
   (xmlAutomataPtr xmlNewAutomata (void)))
  (xmlFreeAutomata
   (void xmlFreeAutomata (xmlAutomataPtr)))
  (xmlAutomataGetInitState
   (xmlAutomataStatePtr xmlAutomataGetInitState (xmlAutomataPtr)))
  (xmlAutomataSetFinalState
   (int xmlAutomataSetFinalState (xmlAutomataPtr xmlAutomataStatePtr)))
  (xmlAutomataNewState
   (xmlAutomataStatePtr xmlAutomataNewState (xmlAutomataPtr)))
  (xmlAutomataNewTransition
   (xmlAutomataStatePtr xmlAutomataNewTransition
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* void*)))
  (xmlAutomataNewTransition2
   (xmlAutomataStatePtr xmlAutomataNewTransition2
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* xmlChar*
					void*)))
  (xmlAutomataNewNegTrans
   (xmlAutomataStatePtr xmlAutomataNewNegTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* xmlChar* void*)))
  (xmlAutomataNewCountTrans
   (xmlAutomataStatePtr xmlAutomataNewCountTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* int int void*)))
  (xmlAutomataNewCountTrans2
   (xmlAutomataStatePtr xmlAutomataNewCountTrans2
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* xmlChar*
					int int void*)))
  (xmlAutomataNewOnceTrans
   (xmlAutomataStatePtr xmlAutomataNewOnceTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* int int void*)))
  (xmlAutomataNewOnceTrans2
   (xmlAutomataStatePtr xmlAutomataNewOnceTrans2
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr xmlChar* xmlChar*
					int int void*)))
  (xmlAutomataNewAllTrans
   (xmlAutomataStatePtr xmlAutomataNewAllTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr int)))
  (xmlAutomataNewEpsilon
   (xmlAutomataStatePtr xmlAutomataNewEpsilon (xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr)))
  (xmlAutomataNewCountedTrans
   (xmlAutomataStatePtr xmlAutomataNewCountedTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr int)))
  (xmlAutomataNewCounterTrans
   (xmlAutomataStatePtr xmlAutomataNewCounterTrans
			(xmlAutomataPtr xmlAutomataStatePtr xmlAutomataStatePtr int)))
  (xmlAutomataNewCounter
   (int xmlAutomataNewCounter (xmlAutomataPtr int int)))
  (xmlAutomataCompile
   (xmlRegexpPtr xmlAutomataCompile (xmlAutomataPtr)))
  (xmlAutomataIsDeterminist
   (int xmlAutomataIsDeterminist (xmlAutomataPtr))))


;;;; error handling
;;
;;Header file "xmlerror.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSetGenericErrorFunc
   (void xmlSetGenericErrorFunc (void* xmlGenericErrorFunc)))
  (initGenericErrorDefaultFunc
   (void initGenericErrorDefaultFunc (xmlGenericErrorFunc*)))
  (xmlSetStructuredErrorFunc
   (void xmlSetStructuredErrorFunc (void* xmlStructuredErrorFunc)))
  ;;Variadic.
  ;;
  ;; (xmlParserError
  ;;  (void xmlParserError (void* char* ...))
  ;; (xmlParserWarning
  ;;  (void xmlParserWarning (void* char* ...))
  ;; (xmlParserValidityError
  ;;  (void xmlParserValidityError (void* char* ...)))
  ;; (xmlParserValidityWarning
  ;;  (void xmlParserValidityWarning (void* char* ...)))
  (xmlParserPrintFileInfo
   (void xmlParserPrintFileInfo (xmlParserInputPtr)))
  (xmlParserPrintFileContext
   (void xmlParserPrintFileContext (xmlParserInputPtr)))
  (xmlGetLastError
   (xmlErrorPtr xmlGetLastError (void)))
  (xmlResetLastError
   (void xmlResetLastError (void)))
  (xmlCtxtGetLastError
   (xmlErrorPtr xmlCtxtGetLastError (void*)))
  (xmlCtxtResetLastError
   (void xmlCtxtResetLastError (void*)))
  (xmlResetError
   (void xmlResetError (xmlErrorPtr)))
  (xmlCopyError
   (int xmlCopyError (xmlErrorPtr xmlErrorPtr))))


;;;; interface for the I/O interfaces used by the parser
;;
;;Header file "XMLio.h".
;;

(define-c-functions libxml2-shared-object
  (xmlCleanupInputCallbacks
   (void xmlCleanupInputCallbacks (void)))
  (xmlPopInputCallbacks
   (int xmlPopInputCallbacks (void)))
  (xmlRegisterDefaultInputCallbacks
   (void xmlRegisterDefaultInputCallbacks (void)))
  (xmlAllocParserInputBuffer
   (xmlParserInputBufferPtr xmlAllocParserInputBuffer (xmlCharEncoding)))
  (xmlParserInputBufferCreateFilename
   (xmlParserInputBufferPtr xmlParserInputBufferCreateFilename (char* xmlCharEncoding)))
  (xmlParserInputBufferCreateFile
   (xmlParserInputBufferPtr xmlParserInputBufferCreateFile (FILE* xmlCharEncoding)))
  (xmlParserInputBufferCreateFd
   (xmlParserInputBufferPtr xmlParserInputBufferCreateFd (int xmlCharEncoding)))
  (xmlParserInputBufferCreateMem
   (xmlParserInputBufferPtr xmlParserInputBufferCreateMem (char* int xmlCharEncoding)))
  (xmlParserInputBufferCreateStatic
   (xmlParserInputBufferPtr xmlParserInputBufferCreateStatic (char* int xmlCharEncoding)))
  (xmlParserInputBufferCreateIO
   (xmlParserInputBufferPtr xmlParserInputBufferCreateIO
			    (xmlInputReadCallback xmlInputCloseCallback void* xmlCharEncoding)))
  (xmlParserInputBufferRead
   (int xmlParserInputBufferRead (xmlParserInputBufferPtr int)))
  (xmlParserInputBufferGrow
   (int xmlParserInputBufferGrow (xmlParserInputBufferPtr int)))
  (xmlParserInputBufferPush
   (int xmlParserInputBufferPush (xmlParserInputBufferPtr int char*)))
  (xmlFreeParserInputBuffer
   (void xmlFreeParserInputBuffer (xmlParserInputBufferPtr)))
  (xmlParserGetDirectory
   (char* xmlParserGetDirectory (char*)))
  (xmlRegisterInputCallbacks
   (int xmlRegisterInputCallbacks
	(xmlInputMatchCallback xmlInputOpenCallback xmlInputReadCallback xmlInputCloseCallback)))
  (__xmlParserInputBufferCreateFilename
   (xmlParserInputBufferPtr __xmlParserInputBufferCreateFilename (char* xmlCharEncoding)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlCleanupOutputCallbacks
   (void xmlCleanupOutputCallbacks (void)))
  (xmlRegisterDefaultOutputCallbacks
   (void xmlRegisterDefaultOutputCallbacks (void)))
  (xmlAllocOutputBuffer
   (xmlOutputBufferPtr xmlAllocOutputBuffer (xmlCharEncodingHandlerPtr)))
  (xmlOutputBufferCreateFilename
   (xmlOutputBufferPtr xmlOutputBufferCreateFilename (char* xmlCharEncodingHandlerPtr int)))
  (xmlOutputBufferCreateFile
   (xmlOutputBufferPtr xmlOutputBufferCreateFile (FILE* xmlCharEncodingHandlerPtr)))
  (xmlOutputBufferCreateBuffer
   (xmlOutputBufferPtr xmlOutputBufferCreateBuffer (xmlBufferPtr xmlCharEncodingHandlerPtr)))
  (xmlOutputBufferCreateFd
   (xmlOutputBufferPtr xmlOutputBufferCreateFd (int xmlCharEncodingHandlerPtr)))
  (xmlOutputBufferCreateIO
   (xmlOutputBufferPtr xmlOutputBufferCreateIO
		       (xmlOutputWriteCallback xmlOutputCloseCallback void* xmlCharEncodingHandlerPtr)))
  (xmlOutputBufferWrite
   (int xmlOutputBufferWrite (xmlOutputBufferPtr int char*)))
  (xmlOutputBufferWriteString
   (int xmlOutputBufferWriteString (xmlOutputBufferPtr char*)))
  (xmlOutputBufferWriteEscape
   (int xmlOutputBufferWriteEscape (xmlOutputBufferPtr xmlChar* xmlCharEncodingOutputFunc)))
  (xmlOutputBufferFlush
   (int xmlOutputBufferFlush (xmlOutputBufferPtr)))
  (xmlOutputBufferClose
   (int xmlOutputBufferClose (xmlOutputBufferPtr)))
  (xmlRegisterOutputCallbacks
   (int xmlRegisterOutputCallbacks (xmlOutputMatchCallback
				    xmlOutputOpenCallback
				    xmlOutputWriteCallback
				    xmlOutputCloseCallback)))
  (__xmlOutputBufferCreateFilename
   (xmlOutputBufferPtr __xmlOutputBufferCreateFilename (char* xmlCharEncodingHandlerPtr int)))
  ;; #ifdef LIBXML_HTTP_ENABLED
  (xmlRegisterHTTPPostCallbacks
   (void xmlRegisterHTTPPostCallbacks (void)))
  ;; #endif /* LIBXML_HTTP_ENABLED */
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlCheckHTTPInput
   (xmlParserInputPtr xmlCheckHTTPInput (xmlParserCtxtPtr xmlParserInputPtr)))
  (xmlNoNetExternalEntityLoader
   (xmlParserInputPtr xmlNoNetExternalEntityLoader (char* char* xmlParserCtxtPtr)))
  (xmlNormalizeWindowsPath
   (xmlChar* xmlNormalizeWindowsPath (xmlChar*)))
  (xmlCheckFilename
   (int xmlCheckFilename (char*)))
  (xmlFileMatch
   (int xmlFileMatch (char*)))
  (xmlFileOpen
   (void* xmlFileOpen (char*)))
  (xmlFileRead
   (int xmlFileRead (void* char* int)))
  (xmlFileClose
   (int xmlFileClose (void*)))
  ;; #ifdef LIBXML_HTTP_ENABLED
  (xmlIOHTTPMatch
   (int xmlIOHTTPMatch (char*)))
  (xmlIOHTTPOpen
   (void* xmlIOHTTPOpen (char*)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlIOHTTPOpenW
   (void* xmlIOHTTPOpenW (char* int)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlIOHTTPRead
   (int xmlIOHTTPRead (void* char* int)))
  (xmlIOHTTPClose
   (int xmlIOHTTPClose (void*)))
  ;; #endif /* LIBXML_HTTP_ENABLED */
  ;; #ifdef LIBXML_FTP_ENABLED
  (xmlIOFTPMatch
   (int xmlIOFTPMatch (char*)))
  (xmlIOFTPOpen
   (void* xmlIOFTPOpen (char*)))
  (xmlIOFTPRead
   (int xmlIOFTPRead (void* char* int)))
  (xmlIOFTPClose
   (int xmlIOFTPClose (void*)))
  ;;#endif /* LIBXML_FTP_ENABLED */
  )



;;;; interface for the memory allocator
;;
;;Header file "xmlmemory.h".
;;

(define-c-functions libxml2-shared-object
  (xmlMemSetup
   (int xmlMemSetup (xmlFreeFunc xmlMallocFunc xmlReallocFunc xmlStrdupFunc)))
  (xmlMemGet
   (int xmlMemGet (xmlFreeFunc* xmlMallocFunc* xmlReallocFunc* xmlStrdupFunc*)))
  (xmlGcMemSetup
   (int xmlGcMemSetup (xmlFreeFunc xmlMallocFunc xmlMallocFunc xmlReallocFunc xmlStrdupFunc)))
  (xmlGcMemGet
   (int xmlGcMemGet (xmlFreeFunc* xmlMallocFunc* xmlMallocFunc* xmlReallocFunc* xmlStrdupFunc*)))
  (xmlInitMemory
   (int xmlInitMemory (void)))
  (xmlCleanupMemory
   (void xmlCleanupMemory (void)))
  (xmlMemUsed
   (int xmlMemUsed (void)))
  (xmlMemBlocks
   (int xmlMemBlocks (void)))
  (xmlMemDisplay
   (void xmlMemDisplay (FILE*)))
  (xmlMemDisplayLast
   (void xmlMemDisplayLast (FILE* long)))
  (xmlMemShow
   (void xmlMemShow (FILE* int)))
  (xmlMemoryDump
   (void xmlMemoryDump (void)))
  (xmlMemMalloc
   (void* xmlMemMalloc (size_t)))
  (xmlMemRealloc
   (void* xmlMemRealloc (void* size_t)))
  (xmlMemFree
   (void xmlMemFree (void*)))
  (xmlMemoryStrdup
   (char* xmlMemoryStrdup (char*)))
  (xmlMallocLoc
   (void* xmlMallocLoc (size_t char* int)))
  (xmlReallocLoc
   (void* xmlReallocLoc (void* size_t char* int)))
  (xmlMallocAtomicLoc
   (void* xmlMallocAtomicLoc (size_t char* int)))
  (xmlMemStrdupLoc
   (char* xmlMemStrdupLoc (char* char* int))))


;;;; dynamic module loading
;;
;;Header file "xmlmodule.h".
;;

(define-c-functions libxml2-shared-object
  (xmlModuleOpen
   (xmlModulePtr xmlModuleOpen (char* int)))
  (xmlModuleSymbol
   (int xmlModuleSymbol (xmlModulePtr char* void**)))
  (xmlModuleClose
   (int xmlModuleClose (xmlModulePtr)))
  (xmlModuleFree
   (int xmlModuleFree (xmlModulePtr))))



;;;; the XMLReader implementation
;;
;;Header file "xmlreader.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNewTextReader
   (xmlTextReaderPtr xmlNewTextReader (xmlParserInputBufferPtr char*)))
  (xmlNewTextReaderFilename
   (xmlTextReaderPtr xmlNewTextReaderFilename (char*)))
  (xmlFreeTextReader
   (void xmlFreeTextReader (xmlTextReaderPtr)))
  (xmlTextReaderSetup
   (int xmlTextReaderSetup (xmlTextReaderPtr xmlParserInputBufferPtr char* char* int)))
  (xmlTextReaderRead
   (int xmlTextReaderRead (xmlTextReaderPtr)))
  ;; #ifdef LIBXML_WRITER_ENABLED
  (xmlTextReaderReadInnerXml
   (xmlChar* xmlTextReaderReadInnerXml (xmlTextReaderPtr)))
  (xmlTextReaderReadOuterXml
   (xmlChar* xmlTextReaderReadOuterXml (xmlTextReaderPtr)))
  ;; #endif
  (xmlTextReaderReadString
   (xmlChar* xmlTextReaderReadString (xmlTextReaderPtr)))
  (xmlTextReaderReadAttributeValue
   (int xmlTextReaderReadAttributeValue (xmlTextReaderPtr)))
  (xmlTextReaderAttributeCount
   (int xmlTextReaderAttributeCount (xmlTextReaderPtr)))
  (xmlTextReaderDepth
   (int xmlTextReaderDepth (xmlTextReaderPtr)))
  (xmlTextReaderHasAttributes
   (int xmlTextReaderHasAttributes (xmlTextReaderPtr)))
  (xmlTextReaderHasValue
   (int xmlTextReaderHasValue (xmlTextReaderPtr)))
  (xmlTextReaderIsDefault
   (int xmlTextReaderIsDefault (xmlTextReaderPtr)))
  (xmlTextReaderIsEmptyElement
   (int xmlTextReaderIsEmptyElement (xmlTextReaderPtr)))
  (xmlTextReaderNodeType
   (int xmlTextReaderNodeType (xmlTextReaderPtr)))
  (xmlTextReaderQuoteChar
   (int xmlTextReaderQuoteChar (xmlTextReaderPtr)))
  (xmlTextReaderReadState
   (int xmlTextReaderReadState (xmlTextReaderPtr)))
  (xmlTextReaderIsNamespaceDecl
   (int xmlTextReaderIsNamespaceDecl (xmlTextReaderPtr)))
  (xmlTextReaderConstBaseUri
   (xmlChar* xmlTextReaderConstBaseUri (xmlTextReaderPtr)))
  (xmlTextReaderConstLocalName
   (xmlChar* xmlTextReaderConstLocalName (xmlTextReaderPtr)))
  (xmlTextReaderConstName
   (xmlChar* xmlTextReaderConstName (xmlTextReaderPtr)))
  (xmlTextReaderConstNamespaceUri
   (xmlChar* xmlTextReaderConstNamespaceUri (xmlTextReaderPtr)))
  (xmlTextReaderConstPrefix
   (xmlChar* xmlTextReaderConstPrefix (xmlTextReaderPtr)))
  (xmlTextReaderConstXmlLang
   (xmlChar* xmlTextReaderConstXmlLang (xmlTextReaderPtr)))
  (xmlTextReaderConstString
   (xmlChar* xmlTextReaderConstString (xmlTextReaderPtr xmlChar*)))
  (xmlTextReaderConstValue
   (xmlChar* xmlTextReaderConstValue (xmlTextReaderPtr)))
  (xmlTextReaderBaseUri
   (xmlChar* xmlTextReaderBaseUri (xmlTextReaderPtr)))
  (xmlTextReaderLocalName
   (xmlChar* xmlTextReaderLocalName (xmlTextReaderPtr)))
  (xmlTextReaderName
   (xmlChar* xmlTextReaderName (xmlTextReaderPtr)))
  (xmlTextReaderNamespaceUri
   (xmlChar* xmlTextReaderNamespaceUri (xmlTextReaderPtr)))
  (xmlTextReaderPrefix
   (xmlChar* xmlTextReaderPrefix (xmlTextReaderPtr)))
  (xmlTextReaderXmlLang
   (xmlChar* xmlTextReaderXmlLang (xmlTextReaderPtr)))
  (xmlTextReaderValue
   (xmlChar* xmlTextReaderValue (xmlTextReaderPtr)))
  (xmlTextReaderClose
   (int xmlTextReaderClose (xmlTextReaderPtr)))
  (xmlTextReaderGetAttributeNo
   (xmlChar* xmlTextReaderGetAttributeNo (xmlTextReaderPtr int)))
  (xmlTextReaderGetAttribute
   (xmlChar* xmlTextReaderGetAttribute (xmlTextReaderPtr xmlChar*)))
  (xmlTextReaderGetAttributeNs
   (xmlChar* xmlTextReaderGetAttributeNs (xmlTextReaderPtr xmlChar* xmlChar*)))
  (xmlTextReaderGetRemainder
   (xmlParserInputBufferPtr xmlTextReaderGetRemainder (xmlTextReaderPtr)))
  (xmlTextReaderLookupNamespace
   (xmlChar* xmlTextReaderLookupNamespace (xmlTextReaderPtr xmlChar*)))
  (xmlTextReaderMoveToAttributeNo
   (int xmlTextReaderMoveToAttributeNo (xmlTextReaderPtr int)))
  (xmlTextReaderMoveToAttribute
   (int xmlTextReaderMoveToAttribute (xmlTextReaderPtr xmlChar*)))
  (xmlTextReaderMoveToAttributeNs
   (int xmlTextReaderMoveToAttributeNs (xmlTextReaderPtr xmlChar* xmlChar*)))
  (xmlTextReaderMoveToFirstAttribute
   (int xmlTextReaderMoveToFirstAttribute (xmlTextReaderPtr)))
  (xmlTextReaderMoveToNextAttribute
   (int xmlTextReaderMoveToNextAttribute (xmlTextReaderPtr)))
  (xmlTextReaderMoveToElement
   (int xmlTextReaderMoveToElement (xmlTextReaderPtr)))
  (xmlTextReaderNormalization
   (int xmlTextReaderNormalization (xmlTextReaderPtr)))
  (xmlTextReaderConstEncoding
   (xmlChar* xmlTextReaderConstEncoding (xmlTextReaderPtr)))
  (xmlTextReaderSetParserProp
   (int xmlTextReaderSetParserProp (xmlTextReaderPtr int int)))
  (xmlTextReaderGetParserProp
   (int xmlTextReaderGetParserProp (xmlTextReaderPtr int)))
  (xmlTextReaderCurrentNode
   (xmlNodePtr xmlTextReaderCurrentNode (xmlTextReaderPtr)))
  (xmlTextReaderGetParserLineNumber
   (int xmlTextReaderGetParserLineNumber (xmlTextReaderPtr)))
  (xmlTextReaderGetParserColumnNumber
   (int xmlTextReaderGetParserColumnNumber (xmlTextReaderPtr)))
  (xmlTextReaderPreserve
   (xmlNodePtr xmlTextReaderPreserve (xmlTextReaderPtr)))
  ;; #ifdef LIBXML_PATTERN_ENABLED
  (xmlTextReaderPreservePattern
   (int xmlTextReaderPreservePattern (xmlTextReaderPtr xmlChar* xmlChar**)))
  ;; #endif /* LIBXML_PATTERN_ENABLED */
  (xmlTextReaderCurrentDoc
   (xmlDocPtr xmlTextReaderCurrentDoc (xmlTextReaderPtr)))
  (xmlTextReaderExpand
   (xmlNodePtr xmlTextReaderExpand (xmlTextReaderPtr)))
  (xmlTextReaderNext
   (int xmlTextReaderNext (xmlTextReaderPtr)))
  (xmlTextReaderNextSibling
   (int xmlTextReaderNextSibling (xmlTextReaderPtr)))
  (xmlTextReaderIsValid
   (int xmlTextReaderIsValid (xmlTextReaderPtr)))
  ;; #ifdef LIBXML_SCHEMAS_ENABLED
  (xmlTextReaderRelaxNGValidate
   (int xmlTextReaderRelaxNGValidate (xmlTextReaderPtr char*)))
  (xmlTextReaderRelaxNGSetSchema
   (int xmlTextReaderRelaxNGSetSchema (xmlTextReaderPtr xmlRelaxNGPtr)))
  (xmlTextReaderSchemaValidate
   (int xmlTextReaderSchemaValidate (xmlTextReaderPtr char*)))
  (xmlTextReaderSchemaValidateCtxt
   (int xmlTextReaderSchemaValidateCtxt (xmlTextReaderPtr xmlSchemaValidCtxtPtr int)))
  (xmlTextReaderSetSchema
   (int xmlTextReaderSetSchema (xmlTextReaderPtr xmlSchemaPtr)))
  ;; #endif
  (xmlTextReaderConstXmlVersion
   (xmlChar* xmlTextReaderConstXmlVersion (xmlTextReaderPtr)))
  (xmlTextReaderStandalone
   (int xmlTextReaderStandalone (xmlTextReaderPtr)))
  (xmlTextReaderByteConsumed
   (long xmlTextReaderByteConsumed (xmlTextReaderPtr)))
  (xmlReaderWalker
   (xmlTextReaderPtr xmlReaderWalker (xmlDocPtr)))
  (xmlReaderForDoc
   (xmlTextReaderPtr xmlReaderForDoc (xmlChar* char* char* int)))
  (xmlReaderForFile
   (xmlTextReaderPtr xmlReaderForFile (char* char* int)))
  (xmlReaderForMemory
   (xmlTextReaderPtr xmlReaderForMemory (char* int char* char* int)))
  (xmlReaderForFd
   (xmlTextReaderPtr xmlReaderForFd (int char* char* int)))
  (xmlReaderForIO
   (xmlTextReaderPtr xmlReaderForIO
		     (xmlInputReadCallback xmlInputCloseCallback void* char* char* int)))
  (xmlReaderNewWalker
   (int xmlReaderNewWalker (xmlTextReaderPtr xmlDocPtr)))
  (xmlReaderNewDoc
   (int xmlReaderNewDoc (xmlTextReaderPtr xmlChar* char* char* int)))
  (xmlReaderNewFile
   (int xmlReaderNewFile (xmlTextReaderPtr char* char* int)))
  (xmlReaderNewMemory
   (int xmlReaderNewMemory (xmlTextReaderPtr char* int char* char* int)))
  (xmlReaderNewFd
   (int xmlReaderNewFd (xmlTextReaderPtr int char* char* int)))
  (xmlReaderNewIO
   (int xmlReaderNewIO
	(xmlTextReaderPtr xmlInputReadCallback xmlInputCloseCallback void* char* char* int)))

  (xmlTextReaderLocatorLineNumber
   (int xmlTextReaderLocatorLineNumber (xmlTextReaderLocatorPtr)))
;;; int xmlTextReaderLocatorLinePosition (xmlTextReaderLocatorPtr locator)))
  (xmlTextReaderLocatorBaseURI
   (xmlChar* xmlTextReaderLocatorBaseURI (xmlTextReaderLocatorPtr)))
  (xmlTextReaderSetErrorHandler
   (void xmlTextReaderSetErrorHandler (xmlTextReaderPtr xmlTextReaderErrorFunc void*)))
  (xmlTextReaderSetStructuredErrorHandler
   (void xmlTextReaderSetStructuredErrorHandler (xmlTextReaderPtr xmlStructuredErrorFunc void*)))
  (xmlTextReaderGetErrorHandler
   (void xmlTextReaderGetErrorHandler (xmlTextReaderPtr xmlTextReaderErrorFunc* void**))))


;;;; regular expressions handling
;;
;;Header file "xmlregexp.h"
;;

(define-c-functions libxml2-shared-object
  (xmlRegexpCompile
   (xmlRegexpPtr xmlRegexpCompile (xmlChar*)))
  (xmlRegFreeRegexp
   (void xmlRegFreeRegexp (xmlRegexpPtr)))
  (xmlRegexpExec
   (int xmlRegexpExec (xmlRegexpPtr xmlChar*)))
  (xmlRegexpPrint
   (void xmlRegexpPrint (FILE* xmlRegexpPtr)))
  (xmlRegexpIsDeterminist
   (int xmlRegexpIsDeterminist (xmlRegexpPtr)))
  (xmlRegNewExecCtxt
   (xmlRegExecCtxtPtr xmlRegNewExecCtxt (xmlRegexpPtr xmlRegExecCallbacks void*)))
  (xmlRegFreeExecCtxt
   (void xmlRegFreeExecCtxt (xmlRegExecCtxtPtr)))
  (xmlRegExecPushString
   (int xmlRegExecPushString (xmlRegExecCtxtPtr xmlChar* void*)))
  (xmlRegExecPushString2
   (int xmlRegExecPushString2 (xmlRegExecCtxtPtr xmlChar* xmlChar* void*)))
  (xmlRegExecNextValues
   (int xmlRegExecNextValues (xmlRegExecCtxtPtr int* int* xmlChar** int*)))
  (xmlRegExecErrInfo
   (int xmlRegExecErrInfo (xmlRegExecCtxtPtr xmlChar** int* int* xmlChar** int*)))
  ;; #ifdef LIBXML_EXPR_ENABLED
  (xmlExpFreeCtxt
   (void xmlExpFreeCtxt (xmlExpCtxtPtr)))
  (xmlExpNewCtxt
   (xmlExpCtxtPtr xmlExpNewCtxt (int xmlDictPtr)))
  (xmlExpCtxtNbNodes
   (int xmlExpCtxtNbNodes (xmlExpCtxtPtr)))
  (xmlExpCtxtNbCons
   (int xmlExpCtxtNbCons (xmlExpCtxtPtr)))
  (xmlExpFree
   (void xmlExpFree (xmlExpCtxtPtr xmlExpNodePtr)))
  (xmlExpRef
   (void xmlExpRef (xmlExpNodePtr)))
  (xmlExpParse
   (xmlExpNodePtr xmlExpParse (xmlExpCtxtPtr char*)))
  (xmlExpNewAtom
   (xmlExpNodePtr xmlExpNewAtom (xmlExpCtxtPtr xmlChar* int)))
  (xmlExpNewOr
   (xmlExpNodePtr xmlExpNewOr (xmlExpCtxtPtr xmlExpNodePtr xmlExpNodePtr)))
  (xmlExpNewSeq
   (xmlExpNodePtr xmlExpNewSeq (xmlExpCtxtPtr xmlExpNodePtr xmlExpNodePtr)))
  (xmlExpNewRange
   (xmlExpNodePtr xmlExpNewRange (xmlExpCtxtPtr xmlExpNodePtr int int)))
  (xmlExpIsNillable
   (int xmlExpIsNillable (xmlExpNodePtr)))
  (xmlExpMaxToken
   (int xmlExpMaxToken (xmlExpNodePtr)))
  (xmlExpGetLanguage
   (int xmlExpGetLanguage (xmlExpCtxtPtr xmlExpNodePtr xmlChar** int)))
  (xmlExpGetStart
   (int xmlExpGetStart (xmlExpCtxtPtr xmlExpNodePtr xmlChar** int)))
  (xmlExpStringDerive
   (xmlExpNodePtr xmlExpStringDerive (xmlExpCtxtPtr xmlExpNodePtr xmlChar* int)))
  (xmlExpExpDerive
   (xmlExpNodePtr xmlExpExpDerive (xmlExpCtxtPtr xmlExpNodePtr xmlExpNodePtr)))
  (xmlExpSubsume
   (int xmlExpSubsume (xmlExpCtxtPtr xmlExpNodePtr xmlExpNodePtr)))
  (xmlExpDump
   (void xmlExpDump (xmlBufferPtr xmlExpNodePtr))))


;;;; the XML document serializer
;;
;;Header file "xmlsave.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSaveToFd
   (xmlSaveCtxtPtr xmlSaveToFd (int char* int)))
  (xmlSaveToFilename
   (xmlSaveCtxtPtr xmlSaveToFilename (char* char* int)))
  (xmlSaveToBuffer
   (xmlSaveCtxtPtr xmlSaveToBuffer (xmlBufferPtr char* int)))
  (xmlSaveToIO
   (xmlSaveCtxtPtr xmlSaveToIO (xmlOutputWriteCallback xmlOutputCloseCallback void* char* int)))
  (xmlSaveDoc
   (long xmlSaveDoc (xmlSaveCtxtPtr xmlDocPtr)))
  (xmlSaveTree
   (long xmlSaveTree (xmlSaveCtxtPtr xmlNodePtr)))
  (xmlSaveFlush
   (int xmlSaveFlush (xmlSaveCtxtPtr)))
  (xmlSaveClose
   (int xmlSaveClose (xmlSaveCtxtPtr)))
  (xmlSaveSetEscape
   (int xmlSaveSetEscape (xmlSaveCtxtPtr xmlCharEncodingOutputFunc)))
  (xmlSaveSetAttrEscape
   (int xmlSaveSetAttrEscape (xmlSaveCtxtPtr xmlCharEncodingOutputFunc))))


;;;; internal interfaces for XML Schemas
;;
;;Header file "schemasInternals.h"
;;

(define-c-functions libxml2-shared-object
  (xmlSchemaFreeType
   (void xmlSchemaFreeType (xmlSchemaTypePtr)))
  (xmlSchemaFreeWildcard
   (void xmlSchemaFreeWildcard (xmlSchemaWildcardPtr))))


;;;; incomplete XML Schemas structure implementation
;;
;;Header file "xmlschemas.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSchemaNewParserCtxt
   (xmlSchemaParserCtxtPtr xmlSchemaNewParserCtxt (char*)))
  (xmlSchemaNewMemParserCtxt
   (xmlSchemaParserCtxtPtr xmlSchemaNewMemParserCtxt (char* int)))
  (xmlSchemaNewDocParserCtxt
   (xmlSchemaParserCtxtPtr xmlSchemaNewDocParserCtxt (xmlDocPtr)))
  (xmlSchemaFreeParserCtxt
   (void xmlSchemaFreeParserCtxt (xmlSchemaParserCtxtPtr)))
  (xmlSchemaSetParserErrors
   (void xmlSchemaSetParserErrors
	 (xmlSchemaParserCtxtPtr xmlSchemaValidityErrorFunc xmlSchemaValidityWarningFunc void*)))
  (xmlSchemaSetParserStructuredErrors
   (void xmlSchemaSetParserStructuredErrors (xmlSchemaParserCtxtPtr xmlStructuredErrorFunc void*)))
  (xmlSchemaGetParserErrors
   (int xmlSchemaGetParserErrors (xmlSchemaParserCtxtPtr
				  xmlSchemaValidityErrorFunc*
				  xmlSchemaValidityWarningFunc*
				  void**)))
  (xmlSchemaIsValid
   (int xmlSchemaIsValid (xmlSchemaValidCtxtPtr)))
  (xmlSchemaParse
   (xmlSchemaPtr xmlSchemaParse (xmlSchemaParserCtxtPtr)))
  (xmlSchemaFree
   (void xmlSchemaFree (xmlSchemaPtr)))
  ;; #ifdef LIBXML_OUTPUT_ENABLED
  (xmlSchemaDump
   (void xmlSchemaDump (FILE* xmlSchemaPtr)))
  ;; #endif /* LIBXML_OUTPUT_ENABLED */
  (xmlSchemaSetValidErrors
   (void xmlSchemaSetValidErrors (xmlSchemaValidCtxtPtr
				  xmlSchemaValidityErrorFunc
				  xmlSchemaValidityWarningFunc
				  void*)))
  (xmlSchemaSetValidStructuredErrors
   (void xmlSchemaSetValidStructuredErrors (xmlSchemaValidCtxtPtr xmlStructuredErrorFunc void*)))
  (xmlSchemaGetValidErrors
   (int xmlSchemaGetValidErrors (xmlSchemaValidCtxtPtr
				 xmlSchemaValidityErrorFunc*
				 xmlSchemaValidityWarningFunc*
				 void**)))
  (xmlSchemaSetValidOptions
   (int xmlSchemaSetValidOptions (xmlSchemaValidCtxtPtr int)))
  (xmlSchemaValidCtxtGetOptions
   (int xmlSchemaValidCtxtGetOptions (xmlSchemaValidCtxtPtr)))
  (xmlSchemaNewValidCtxt
   (xmlSchemaValidCtxtPtr xmlSchemaNewValidCtxt (xmlSchemaPtr)))
  (xmlSchemaFreeValidCtxt
   (void xmlSchemaFreeValidCtxt (xmlSchemaValidCtxtPtr)))
  (xmlSchemaValidateDoc
   (int xmlSchemaValidateDoc (xmlSchemaValidCtxtPtr xmlDocPtr)))
  (xmlSchemaValidateOneElement
   (int xmlSchemaValidateOneElement (xmlSchemaValidCtxtPtr xmlNodePtr)))
  (xmlSchemaValidateStream
   (int xmlSchemaValidateStream (xmlSchemaValidCtxtPtr
				 xmlParserInputBufferPtr
				 xmlCharEncoding
				 xmlSAXHandlerPtr
				 void*)))
  (xmlSchemaValidateFile
   (int xmlSchemaValidateFile (xmlSchemaValidCtxtPtr char* int)))
  (xmlSchemaValidCtxtGetParserCtxt
   (xmlParserCtxtPtr xmlSchemaValidCtxtGetParserCtxt (xmlSchemaValidCtxtPtr)))
  (xmlSchemaSAXPlug
   (xmlSchemaSAXPlugPtr xmlSchemaSAXPlug (xmlSchemaValidCtxtPtr xmlSAXHandlerPtr* void**)))
  (xmlSchemaSAXUnplug
   (int xmlSchemaSAXUnplug (xmlSchemaSAXPlugPtr))))


;;;; implementation of XML Schema Datatypes
;;
;;Header file "xmlschemastypes.h".
;;

(define-c-functions libxml2-shared-object
  (xmlSchemaInitTypes
   (void xmlSchemaInitTypes (void)))
  (xmlSchemaCleanupTypes
   (void xmlSchemaCleanupTypes (void)))
  (xmlSchemaGetPredefinedType
   (xmlSchemaTypePtr xmlSchemaGetPredefinedType (xmlChar* xmlChar*)))
  (xmlSchemaValidatePredefinedType
   (int xmlSchemaValidatePredefinedType (xmlSchemaTypePtr xmlChar* xmlSchemaValPtr*)))
  (xmlSchemaValPredefTypeNode
   (int xmlSchemaValPredefTypeNode (xmlSchemaTypePtr xmlChar* xmlSchemaValPtr* xmlNodePtr)))
  (xmlSchemaValidateFacet
   (int xmlSchemaValidateFacet (xmlSchemaTypePtr xmlSchemaFacetPtr xmlChar* xmlSchemaValPtr)))
  (xmlSchemaValidateFacetWhtsp
   (int xmlSchemaValidateFacetWhtsp (xmlSchemaFacetPtr
				     xmlSchemaWhitespaceValueType
				     xmlSchemaValType
				     xmlChar*
				     xmlSchemaValPtr
				     xmlSchemaWhitespaceValueType)))
  (xmlSchemaFreeValue
   (void xmlSchemaFreeValue (xmlSchemaValPtr)))
  (xmlSchemaNewFacet
   (xmlSchemaFacetPtr xmlSchemaNewFacet (void)))
  (xmlSchemaCheckFacet
   (int xmlSchemaCheckFacet (xmlSchemaFacetPtr xmlSchemaTypePtr xmlSchemaParserCtxtPtr xmlChar*)))
  (xmlSchemaFreeFacet
   (void xmlSchemaFreeFacet (xmlSchemaFacetPtr)))
  (xmlSchemaCompareValues
   (int xmlSchemaCompareValues (xmlSchemaValPtr xmlSchemaValPtr)))
  (xmlSchemaGetBuiltInListSimpleTypeItemType
   (xmlSchemaTypePtr xmlSchemaGetBuiltInListSimpleTypeItemType (xmlSchemaTypePtr)))
  (xmlSchemaValidateListSimpleTypeFacet
   (int xmlSchemaValidateListSimpleTypeFacet (xmlSchemaFacetPtr xmlChar* unsigned-long unsigned-long*)))
  (xmlSchemaGetBuiltInType
   (xmlSchemaTypePtr xmlSchemaGetBuiltInType (xmlSchemaValType)))
  (xmlSchemaIsBuiltInTypeFacet
   (int xmlSchemaIsBuiltInTypeFacet (xmlSchemaTypePtr int)))
  (xmlSchemaCollapseString
   (xmlChar* xmlSchemaCollapseString (xmlChar*)))
  (xmlSchemaWhiteSpaceReplace
   (xmlChar* xmlSchemaWhiteSpaceReplace (xmlChar*)))
  (xmlSchemaGetFacetValueAsULong
   (unsigned-long xmlSchemaGetFacetValueAsULong	(xmlSchemaFacetPtr)))
  (xmlSchemaValidateLengthFacet
   (int xmlSchemaValidateLengthFacet
	(xmlSchemaTypePtr xmlSchemaFacetPtr xmlChar* xmlSchemaValPtr unsigned-long*)))
  (xmlSchemaValidateLengthFacetWhtsp
   (int xmlSchemaValidateLengthFacetWhtsp (xmlSchemaFacetPtr
					   xmlSchemaValType
					   xmlChar*
					   xmlSchemaValPtr
					   unsigned-long*
					   xmlSchemaWhitespaceValueType)))
  (xmlSchemaValPredefTypeNodeNoNorm
   (int xmlSchemaValPredefTypeNodeNoNorm (xmlSchemaTypePtr xmlChar* xmlSchemaValPtr* xmlNodePtr)))
  (xmlSchemaGetCanonValue
   (int xmlSchemaGetCanonValue (xmlSchemaValPtr xmlChar**)))
  (xmlSchemaGetCanonValueWhtsp
   (int xmlSchemaGetCanonValueWhtsp (xmlSchemaValPtr xmlChar** xmlSchemaWhitespaceValueType)))
  (xmlSchemaValueAppend
   (int xmlSchemaValueAppend (xmlSchemaValPtr xmlSchemaValPtr)))
  (xmlSchemaValueGetNext
   (xmlSchemaValPtr xmlSchemaValueGetNext (xmlSchemaValPtr)))
  (xmlSchemaValueGetAsString
   (xmlChar* xmlSchemaValueGetAsString (xmlSchemaValPtr)))
  (xmlSchemaValueGetAsBoolean
   (int xmlSchemaValueGetAsBoolean (xmlSchemaValPtr)))
  (xmlSchemaNewStringValue
   (xmlSchemaValPtr xmlSchemaNewStringValue (xmlSchemaValType xmlChar*)))
  (xmlSchemaNewNOTATIONValue
   (xmlSchemaValPtr xmlSchemaNewNOTATIONValue (xmlChar* xmlChar*)))
  (xmlSchemaNewQNameValue
   (xmlSchemaValPtr xmlSchemaNewQNameValue (xmlChar* xmlChar*)))
  (xmlSchemaCompareValuesWhtsp
   (int xmlSchemaCompareValuesWhtsp (xmlSchemaValPtr
				     xmlSchemaWhitespaceValueType
				     xmlSchemaValPtr
				     xmlSchemaWhitespaceValueType)))
  (xmlSchemaCopyValue
   (xmlSchemaValPtr xmlSchemaCopyValue (xmlSchemaValPtr)))
  (xmlSchemaGetValType
   (xmlSchemaValType xmlSchemaGetValType (xmlSchemaValPtr))))


;;;; set of routines to process strings
;;
;;Header file "xmlstring.h".
;;

(define-c-functions libxml2-shared-object
  (xmlStrdup
   (xmlChar* xmlStrdup (xmlChar*)))
  (xmlStrndup
   (xmlChar* xmlStrndup (xmlChar* int)))
  (xmlCharStrndup
   (xmlChar* xmlCharStrndup (char* int)))
  (xmlCharStrdup
   (xmlChar* xmlCharStrdup (char)))
  (xmlStrsub
   (xmlChar* xmlStrsub (xmlChar* int int)))
  (xmlStrchr
   (xmlChar* xmlStrchr (xmlChar* xmlChar)))
  (xmlStrstr
   (xmlChar* xmlStrstr (xmlChar* xmlChar*)))
  (xmlStrcasestr
   (xmlChar* xmlStrcasestr (xmlChar* xmlChar*)))
  (xmlStrcmp
   (int xmlStrcmp (xmlChar* xmlChar*)))
  (xmlStrncmp
   (int xmlStrncmp (xmlChar* xmlChar* int)))
  (xmlStrcasecmp
   (int xmlStrcasecmp (xmlChar* xmlChar*)))
  (xmlStrncasecmp
   (int xmlStrncasecmp (xmlChar* xmlChar* int)))
  (xmlStrEqual
   (int xmlStrEqual (xmlChar* xmlChar*)))
  (xmlStrQEqual
   (int xmlStrQEqual (xmlChar* xmlChar* xmlChar*)))
  (xmlStrlen
   (int xmlStrlen (xmlChar*)))
  (xmlStrcat
   (xmlChar* xmlStrcat (xmlChar* xmlChar*)))
  (xmlStrncat
   (xmlChar* xmlStrncat (xmlChar* xmlChar* int)))
  (xmlStrncatNew
   (xmlChar* xmlStrncatNew (xmlChar* xmlChar* int)))
  ;;Variadic!!!
  ;;
  ;; (xmlStrPrintf
  ;;  (int xmlStrPrintf (xmlChar* int xmlChar* ...)))
  ;; (xmlStrVPrintf
  ;;  (int xmlStrVPrintf (xmlChar* int xmlChar* va_list)))
  (xmlGetUTF8Char
   (int xmlGetUTF8Char (unsigned-char* int*)))
  (xmlCheckUTF8
   (int xmlCheckUTF8 (unsigned-char*)))
  (xmlUTF8Strsize
   (int xmlUTF8Strsize (xmlChar* int)))
  (xmlUTF8Strndup
   (xmlChar* xmlUTF8Strndup (xmlChar* int)))
  (xmlUTF8Strpos
   (xmlChar* xmlUTF8Strpos (xmlChar* int)))
  (xmlUTF8Strloc
   (int xmlUTF8Strloc (xmlChar* xmlChar*)))
  (xmlUTF8Strsub
   (xmlChar* xmlUTF8Strsub (xmlChar* int int)))
  (xmlUTF8Strlen
   (int xmlUTF8Strlen (xmlChar*)))
  (xmlUTF8Size
   (int xmlUTF8Size (xmlChar*)))
  (xmlUTF8Charcmp
   (int xmlUTF8Charcmp (xmlChar* xmlChar*))))


;;;; Unicode character APIs
;;
;;Header file "xmlunicode.h".
;;

(define-c-functions libxml2-shared-object
  (xmlUCSIsAegeanNumbers
   (int xmlUCSIsAegeanNumbers (int)))
  (xmlUCSIsAlphabeticPresentationForms
   (int xmlUCSIsAlphabeticPresentationForms (int)))
  (xmlUCSIsArabic
   (int xmlUCSIsArabic (int)))
  (xmlUCSIsArabicPresentationFormsA
   (int xmlUCSIsArabicPresentationFormsA (int)))
  (xmlUCSIsArabicPresentationFormsB
   (int xmlUCSIsArabicPresentationFormsB (int)))
  (xmlUCSIsArmenian
   (int xmlUCSIsArmenian (int)))
  (xmlUCSIsArrows
   (int xmlUCSIsArrows (int)))
  (xmlUCSIsBasicLatin
   (int xmlUCSIsBasicLatin (int)))
  (xmlUCSIsBengali
   (int xmlUCSIsBengali (int)))
  (xmlUCSIsBlockElements
   (int xmlUCSIsBlockElements (int)))
  (xmlUCSIsBopomofo
   (int xmlUCSIsBopomofo (int)))
  (xmlUCSIsBopomofoExtended
   (int xmlUCSIsBopomofoExtended (int)))
  (xmlUCSIsBoxDrawing
   (int xmlUCSIsBoxDrawing (int)))
  (xmlUCSIsBraillePatterns
   (int xmlUCSIsBraillePatterns (int)))
  (xmlUCSIsBuhid
   (int xmlUCSIsBuhid (int)))
  (xmlUCSIsByzantineMusicalSymbols
   (int xmlUCSIsByzantineMusicalSymbols (int)))
  (xmlUCSIsCJKCompatibility
   (int xmlUCSIsCJKCompatibility (int)))
  (xmlUCSIsCJKCompatibilityForms
   (int xmlUCSIsCJKCompatibilityForms (int)))
  (xmlUCSIsCJKCompatibilityIdeographs
   (int xmlUCSIsCJKCompatibilityIdeographs (int)))
  (xmlUCSIsCJKCompatibilityIdeographsSupplement
   (int xmlUCSIsCJKCompatibilityIdeographsSupplement (int)))
  (xmlUCSIsCJKRadicalsSupplement
   (int xmlUCSIsCJKRadicalsSupplement (int)))
  (xmlUCSIsCJKSymbolsandPunctuation
   (int xmlUCSIsCJKSymbolsandPunctuation (int)))
  (xmlUCSIsCJKUnifiedIdeographs
   (int xmlUCSIsCJKUnifiedIdeographs (int)))
  (xmlUCSIsCJKUnifiedIdeographsExtensionA
   (int xmlUCSIsCJKUnifiedIdeographsExtensionA (int)))
  (xmlUCSIsCJKUnifiedIdeographsExtensionB
   (int xmlUCSIsCJKUnifiedIdeographsExtensionB (int)))
  (xmlUCSIsCherokee
   (int xmlUCSIsCherokee (int)))
  (xmlUCSIsCombiningDiacriticalMarks
   (int xmlUCSIsCombiningDiacriticalMarks (int)))
  (xmlUCSIsCombiningDiacriticalMarksforSymbols
   (int xmlUCSIsCombiningDiacriticalMarksforSymbols (int)))
  (xmlUCSIsCombiningHalfMarks
   (int xmlUCSIsCombiningHalfMarks (int)))
  (xmlUCSIsCombiningMarksforSymbols
   (int xmlUCSIsCombiningMarksforSymbols (int)))
  (xmlUCSIsControlPictures
   (int xmlUCSIsControlPictures (int)))
  (xmlUCSIsCurrencySymbols
   (int xmlUCSIsCurrencySymbols (int)))
  (xmlUCSIsCypriotSyllabary
   (int xmlUCSIsCypriotSyllabary (int)))
  (xmlUCSIsCyrillic
   (int xmlUCSIsCyrillic (int)))
  (xmlUCSIsCyrillicSupplement
   (int xmlUCSIsCyrillicSupplement (int)))
  (xmlUCSIsDeseret
   (int xmlUCSIsDeseret (int)))
  (xmlUCSIsDevanagari
   (int xmlUCSIsDevanagari (int)))
  (xmlUCSIsDingbats
   (int xmlUCSIsDingbats (int)))
  (xmlUCSIsEnclosedAlphanumerics
   (int xmlUCSIsEnclosedAlphanumerics (int)))
  (xmlUCSIsEnclosedCJKLettersandMonths
   (int xmlUCSIsEnclosedCJKLettersandMonths (int)))
  (xmlUCSIsEthiopic
   (int xmlUCSIsEthiopic (int)))
  (xmlUCSIsGeneralPunctuation
   (int xmlUCSIsGeneralPunctuation (int)))
  (xmlUCSIsGeometricShapes
   (int xmlUCSIsGeometricShapes (int)))
  (xmlUCSIsGeorgian
   (int xmlUCSIsGeorgian (int)))
  (xmlUCSIsGothic
   (int xmlUCSIsGothic (int)))
  (xmlUCSIsGreek
   (int xmlUCSIsGreek (int)))
  (xmlUCSIsGreekExtended
   (int xmlUCSIsGreekExtended (int)))
  (xmlUCSIsGreekandCoptic
   (int xmlUCSIsGreekandCoptic (int)))
  (xmlUCSIsGujarati
   (int xmlUCSIsGujarati (int)))
  (xmlUCSIsGurmukhi
   (int xmlUCSIsGurmukhi (int)))
  (xmlUCSIsHalfwidthandFullwidthForms
   (int xmlUCSIsHalfwidthandFullwidthForms (int)))
  (xmlUCSIsHangulCompatibilityJamo
   (int xmlUCSIsHangulCompatibilityJamo (int)))
  (xmlUCSIsHangulJamo
   (int xmlUCSIsHangulJamo (int)))
  (xmlUCSIsHangulSyllables
   (int xmlUCSIsHangulSyllables (int)))
  (xmlUCSIsHanunoo
   (int xmlUCSIsHanunoo (int)))
  (xmlUCSIsHebrew
   (int xmlUCSIsHebrew (int)))
  (xmlUCSIsHighPrivateUseSurrogates
   (int xmlUCSIsHighPrivateUseSurrogates (int)))
  (xmlUCSIsHighSurrogates
   (int xmlUCSIsHighSurrogates (int)))
  (xmlUCSIsHiragana
   (int xmlUCSIsHiragana (int)))
  (xmlUCSIsIPAExtensions
   (int xmlUCSIsIPAExtensions (int)))
  (xmlUCSIsIdeographicDescriptionCharacters
   (int xmlUCSIsIdeographicDescriptionCharacters (int)))
  (xmlUCSIsKanbun
   (int xmlUCSIsKanbun (int)))
  (xmlUCSIsKangxiRadicals
   (int xmlUCSIsKangxiRadicals (int)))
  (xmlUCSIsKannada
   (int xmlUCSIsKannada (int)))
  (xmlUCSIsKatakana
   (int xmlUCSIsKatakana (int)))
  (xmlUCSIsKatakanaPhoneticExtensions
   (int xmlUCSIsKatakanaPhoneticExtensions (int)))
  (xmlUCSIsKhmer
   (int xmlUCSIsKhmer (int)))
  (xmlUCSIsKhmerSymbols
   (int xmlUCSIsKhmerSymbols (int)))
  (xmlUCSIsLao
   (int xmlUCSIsLao (int)))
  (xmlUCSIsLatin1Supplement
   (int xmlUCSIsLatin1Supplement (int)))
  (xmlUCSIsLatinExtendedA
   (int xmlUCSIsLatinExtendedA (int)))
  (xmlUCSIsLatinExtendedB
   (int xmlUCSIsLatinExtendedB (int)))
  (xmlUCSIsLatinExtendedAdditional
   (int xmlUCSIsLatinExtendedAdditional (int)))
  (xmlUCSIsLetterlikeSymbols
   (int xmlUCSIsLetterlikeSymbols (int)))
  (xmlUCSIsLimbu
   (int xmlUCSIsLimbu (int)))
  (xmlUCSIsLinearBIdeograms
   (int xmlUCSIsLinearBIdeograms (int)))
  (xmlUCSIsLinearBSyllabary
   (int xmlUCSIsLinearBSyllabary (int)))
  (xmlUCSIsLowSurrogates
   (int xmlUCSIsLowSurrogates (int)))
  (xmlUCSIsMalayalam
   (int xmlUCSIsMalayalam (int)))
  (xmlUCSIsMathematicalAlphanumericSymbols
   (int xmlUCSIsMathematicalAlphanumericSymbols (int)))
  (xmlUCSIsMathematicalOperators
   (int xmlUCSIsMathematicalOperators (int)))
  (xmlUCSIsMiscellaneousMathematicalSymbolsA
   (int xmlUCSIsMiscellaneousMathematicalSymbolsA (int)))
  (xmlUCSIsMiscellaneousMathematicalSymbolsB
   (int xmlUCSIsMiscellaneousMathematicalSymbolsB (int)))
  (xmlUCSIsMiscellaneousSymbols
   (int xmlUCSIsMiscellaneousSymbols (int)))
  (xmlUCSIsMiscellaneousSymbolsandArrows
   (int xmlUCSIsMiscellaneousSymbolsandArrows (int)))
  (xmlUCSIsMiscellaneousTechnical
   (int xmlUCSIsMiscellaneousTechnical (int)))
  (xmlUCSIsMongolian
   (int xmlUCSIsMongolian (int)))
  (xmlUCSIsMusicalSymbols
   (int xmlUCSIsMusicalSymbols (int)))
  (xmlUCSIsMyanmar
   (int xmlUCSIsMyanmar (int)))
  (xmlUCSIsNumberForms
   (int xmlUCSIsNumberForms (int)))
  (xmlUCSIsOgham
   (int xmlUCSIsOgham (int)))
  (xmlUCSIsOldItalic
   (int xmlUCSIsOldItalic (int)))
  (xmlUCSIsOpticalCharacterRecognition
   (int xmlUCSIsOpticalCharacterRecognition (int)))
  (xmlUCSIsOriya
   (int xmlUCSIsOriya (int)))
  (xmlUCSIsOsmanya
   (int xmlUCSIsOsmanya (int)))
  (xmlUCSIsPhoneticExtensions
   (int xmlUCSIsPhoneticExtensions (int)))
  (xmlUCSIsPrivateUse
   (int xmlUCSIsPrivateUse (int)))
  (xmlUCSIsPrivateUseArea
   (int xmlUCSIsPrivateUseArea (int)))
  (xmlUCSIsRunic
   (int xmlUCSIsRunic (int)))
  (xmlUCSIsShavian
   (int xmlUCSIsShavian (int)))
  (xmlUCSIsSinhala
   (int xmlUCSIsSinhala (int)))
  (xmlUCSIsSmallFormVariants
   (int xmlUCSIsSmallFormVariants (int)))
  (xmlUCSIsSpacingModifierLetters
   (int xmlUCSIsSpacingModifierLetters (int)))
  (xmlUCSIsSpecials
   (int xmlUCSIsSpecials (int)))
  (xmlUCSIsSuperscriptsandSubscripts
   (int xmlUCSIsSuperscriptsandSubscripts (int)))
  (xmlUCSIsSupplementalArrowsA
   (int xmlUCSIsSupplementalArrowsA (int)))
  (xmlUCSIsSupplementalArrowsB
   (int xmlUCSIsSupplementalArrowsB (int)))
  (xmlUCSIsSupplementalMathematicalOperators
   (int xmlUCSIsSupplementalMathematicalOperators (int)))
  (xmlUCSIsSupplementaryPrivateUseAreaA
   (int xmlUCSIsSupplementaryPrivateUseAreaA (int)))
  (xmlUCSIsSupplementaryPrivateUseAreaB
   (int xmlUCSIsSupplementaryPrivateUseAreaB (int)))
  (xmlUCSIsSyriac
   (int xmlUCSIsSyriac (int)))
  (xmlUCSIsTagalog
   (int xmlUCSIsTagalog (int)))
  (xmlUCSIsTagbanwa
   (int xmlUCSIsTagbanwa (int)))
  (xmlUCSIsTags
   (int xmlUCSIsTags (int)))
  (xmlUCSIsTaiLe
   (int xmlUCSIsTaiLe (int)))
  (xmlUCSIsTaiXuanJingSymbols
   (int xmlUCSIsTaiXuanJingSymbols (int)))
  (xmlUCSIsTamil
   (int xmlUCSIsTamil (int)))
  (xmlUCSIsTelugu
   (int xmlUCSIsTelugu (int)))
  (xmlUCSIsThaana
   (int xmlUCSIsThaana (int)))
  (xmlUCSIsThai
   (int xmlUCSIsThai (int)))
  (xmlUCSIsTibetan
   (int xmlUCSIsTibetan (int)))
  (xmlUCSIsUgaritic
   (int xmlUCSIsUgaritic (int)))
  (xmlUCSIsUnifiedCanadianAboriginalSyllabics
   (int xmlUCSIsUnifiedCanadianAboriginalSyllabics (int)))
  (xmlUCSIsVariationSelectors
   (int xmlUCSIsVariationSelectors (int)))
  (xmlUCSIsVariationSelectorsSupplement
   (int xmlUCSIsVariationSelectorsSupplement (int)))
  (xmlUCSIsYiRadicals
   (int xmlUCSIsYiRadicals (int)))
  (xmlUCSIsYiSyllables
   (int xmlUCSIsYiSyllables (int)))
  (xmlUCSIsYijingHexagramSymbols
   (int xmlUCSIsYijingHexagramSymbols (int)))
  (xmlUCSIsBlock
   (int xmlUCSIsBlock (int char*)))
  (xmlUCSIsCatC
   (int xmlUCSIsCatC (int)))
  (xmlUCSIsCatCc
   (int xmlUCSIsCatCc (int)))
  (xmlUCSIsCatCf
   (int xmlUCSIsCatCf (int)))
  (xmlUCSIsCatCo
   (int xmlUCSIsCatCo (int)))
  (xmlUCSIsCatCs
   (int xmlUCSIsCatCs (int)))
  (xmlUCSIsCatL
   (int xmlUCSIsCatL (int)))
  (xmlUCSIsCatLl
   (int xmlUCSIsCatLl (int)))
  (xmlUCSIsCatLm
   (int xmlUCSIsCatLm (int)))
  (xmlUCSIsCatLo
   (int xmlUCSIsCatLo (int)))
  (xmlUCSIsCatLt
   (int xmlUCSIsCatLt (int)))
  (xmlUCSIsCatLu
   (int xmlUCSIsCatLu (int)))
  (xmlUCSIsCatM
   (int xmlUCSIsCatM (int)))
  (xmlUCSIsCatMc
   (int xmlUCSIsCatMc (int)))
  (xmlUCSIsCatMe
   (int xmlUCSIsCatMe (int)))
  (xmlUCSIsCatMn
   (int xmlUCSIsCatMn (int)))
  (xmlUCSIsCatN
   (int xmlUCSIsCatN (int)))
  (xmlUCSIsCatNd
   (int xmlUCSIsCatNd (int)))
  (xmlUCSIsCatNl
   (int xmlUCSIsCatNl (int)))
  (xmlUCSIsCatNo
   (int xmlUCSIsCatNo (int)))
  (xmlUCSIsCatP
   (int xmlUCSIsCatP (int)))
  (xmlUCSIsCatPc
   (int xmlUCSIsCatPc (int)))
  (xmlUCSIsCatPd
   (int xmlUCSIsCatPd (int)))
  (xmlUCSIsCatPe
   (int xmlUCSIsCatPe (int)))
  (xmlUCSIsCatPf
   (int xmlUCSIsCatPf (int)))
  (xmlUCSIsCatPi
   (int xmlUCSIsCatPi (int)))
  (xmlUCSIsCatPo
   (int xmlUCSIsCatPo (int)))
  (xmlUCSIsCatPs
   (int xmlUCSIsCatPs (int)))
  (xmlUCSIsCatS
   (int xmlUCSIsCatS (int)))
  (xmlUCSIsCatSc
   (int xmlUCSIsCatSc (int)))
  (xmlUCSIsCatSk
   (int xmlUCSIsCatSk (int)))
  (xmlUCSIsCatSm
   (int xmlUCSIsCatSm (int)))
  (xmlUCSIsCatSo
   (int xmlUCSIsCatSo (int)))
  (xmlUCSIsCatZ
   (int xmlUCSIsCatZ (int)))
  (xmlUCSIsCatZl
   (int xmlUCSIsCatZl (int)))
  (xmlUCSIsCatZp
   (int xmlUCSIsCatZp (int)))
  (xmlUCSIsCatZs
   (int xmlUCSIsCatZs (int)))
  (xmlUCSIsCat
   (int xmlUCSIsCat (int char*))))



;;;; compile-time version informations
;;
;;Header file "xmlversion.h".
;;

(define-c-functions libxml2-shared-object
  ;; #ifndef LIBXML2_COMPILING_MSCCDEF
  (xmlCheckVersion
   (void xmlCheckVersion (int)))
  ;; #endif /* LIBXML2_COMPILING_MSCCDEF */
  )


;;;; text writing API for XML
;;
;;Header file "xmlwriter.h".
;;

(define-c-functions libxml2-shared-object
  (xmlNewTextWriter
   (xmlTextWriterPtr xmlNewTextWriter (xmlOutputBufferPtr)))
  (xmlNewTextWriterFilename
   (xmlTextWriterPtr xmlNewTextWriterFilename (char* int)))
  (xmlNewTextWriterMemory
   (xmlTextWriterPtr xmlNewTextWriterMemory (xmlBufferPtr int)))
  (xmlNewTextWriterPushParser
   (xmlTextWriterPtr xmlNewTextWriterPushParser (xmlParserCtxtPtr int)))
  (xmlNewTextWriterDoc
   (xmlTextWriterPtr xmlNewTextWriterDoc (xmlDocPtr* int)))
  (xmlNewTextWriterTree
   (xmlTextWriterPtr xmlNewTextWriterTree (xmlDocPtr xmlNodePtr int)))
  (xmlFreeTextWriter
   (void xmlFreeTextWriter (xmlTextWriterPtr)))
  (xmlTextWriterStartDocument
   (int xmlTextWriterStartDocument (xmlTextWriterPtr char* char* char*)))
  (xmlTextWriterEndDocument
   (int xmlTextWriterEndDocument (xmlTextWriterPtr)))
  (xmlTextWriterStartComment
   (int xmlTextWriterStartComment (xmlTextWriterPtr)))
  (xmlTextWriterEndComment
   (int xmlTextWriterEndComment (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatComment
  ;;  (int xmlTextWriterWriteFormatComment (xmlTextWriterPtr char* ...)))
  ;; (xmlTextWriterWriteVFormatComment
  ;;  (int xmlTextWriterWriteVFormatComment (xmlTextWriterPtr char* va_list)))
  (xmlTextWriterWriteComment
   (int xmlTextWriterWriteComment (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterStartElement
   (int xmlTextWriterStartElement (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterStartElementNS
   (int xmlTextWriterStartElementNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterEndElement
   (int xmlTextWriterEndElement (xmlTextWriterPtr)))
  (xmlTextWriterFullEndElement
   (int xmlTextWriterFullEndElement (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatElement
  ;;  (int xmlTextWriterWriteFormatElement (xmlTextWriterPtr xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatElement
  ;;  (int xmlTextWriterWriteVFormatElement (xmlTextWriterPtr xmlChar* char* va_list)))
  (xmlTextWriterWriteElement
   (int xmlTextWriterWriteElement (xmlTextWriterPtr xmlChar* xmlChar*)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatElementNS
  ;;  (int xmlTextWriterWriteFormatElementNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatElementNS
  ;;  (int xmlTextWriterWriteVFormatElementNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* va_list)))
  (xmlTextWriterWriteElementNS
   (int xmlTextWriterWriteElementNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* xmlChar*)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatRaw
  ;;  (int xmlTextWriterWriteFormatRaw (xmlTextWriterPtr char* ...)))
  ;; (xmlTextWriterWriteVFormatRaw
  ;;  (int xmlTextWriterWriteVFormatRaw (xmlTextWriterPtr char* va_list)))
  (xmlTextWriterWriteRawLen
   (int xmlTextWriterWriteRawLen (xmlTextWriterPtr xmlChar* int)))
  (xmlTextWriterWriteRaw
   (int xmlTextWriterWriteRaw (xmlTextWriterPtr xmlChar*)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatString
  ;;  (int xmlTextWriterWriteFormatString (xmlTextWriterPtr char*format ...)))
  ;; (xmlTextWriterWriteVFormatString
  ;;  (int xmlTextWriterWriteVFormatString (xmlTextWriterPtr char* va_list)))
  (xmlTextWriterWriteString
   (int xmlTextWriterWriteString (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterWriteBase64
   (int xmlTextWriterWriteBase64 (xmlTextWriterPtr char* int int)))
  (xmlTextWriterWriteBinHex
   (int xmlTextWriterWriteBinHex (xmlTextWriterPtr char* int int)))
  (xmlTextWriterStartAttribute
   (int xmlTextWriterStartAttribute (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterStartAttributeNS
   (int xmlTextWriterStartAttributeNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterEndAttribute
   (int xmlTextWriterEndAttribute (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatAttribute
  ;;  (int xmlTextWriterWriteFormatAttribute (xmlTextWriterPtr xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatAttribute
  ;;  (int xmlTextWriterWriteVFormatAttribute (xmlTextWriterPtr xmlChar* char* va_list)))
  (xmlTextWriterWriteAttribute
   (int xmlTextWriterWriteAttribute (xmlTextWriterPtr xmlChar* xmlChar*)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatAttributeNS
  ;;  (int xmlTextWriterWriteFormatAttributeNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatAttributeNS
  ;;  (int xmlTextWriterWriteVFormatAttributeNS
  ;; 	(xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* va_list)))
  (xmlTextWriterWriteAttributeNS
   (int xmlTextWriterWriteAttributeNS (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterStartPI
   (int xmlTextWriterStartPI (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterEndPI
   (int xmlTextWriterEndPI (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatPI
  ;;  (int xmlTextWriterWriteFormatPI (xmlTextWriterPtr xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatPI
  ;;  (int xmlTextWriterWriteVFormatPI (xmlTextWriterPtr xmlChar* char* va_list)))
  (xmlTextWriterWritePI
   (int xmlTextWriterWritePI (xmlTextWriterPtr xmlChar* xmlChar*)))
  (xmlTextWriterStartCDATA
   (int xmlTextWriterStartCDATA (xmlTextWriterPtr)))
  (xmlTextWriterEndCDATA
   (int xmlTextWriterEndCDATA (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatCDATA
  ;;  (int xmlTextWriterWriteFormatCDATA (xmlTextWriterPtr char* ...)))
  ;; (xmlTextWriterWriteVFormatCDATA
  ;;  (int xmlTextWriterWriteVFormatCDATA (xmlTextWriterPtr char* va_list)))
  (xmlTextWriterWriteCDATA
   (int xmlTextWriterWriteCDATA (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterStartDTD
   (int xmlTextWriterStartDTD (xmlTextWriterPtr xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterEndDTD
   (int xmlTextWriterEndDTD (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatDTD
  ;;  (int xmlTextWriterWriteFormatDTD (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatDTD
  ;;  (int xmlTextWriterWriteVFormatDTD (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* char* va_list)))
  (xmlTextWriterWriteDTD
   (int xmlTextWriterWriteDTD (xmlTextWriterPtr xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterStartDTDElement
   (int xmlTextWriterStartDTDElement (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterEndDTDElement
   (int xmlTextWriterEndDTDElement (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatDTDElement
  ;;  (int xmlTextWriterWriteFormatDTDElement (xmlTextWriterPtr xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatDTDElement
  ;;  (int xmlTextWriterWriteVFormatDTDElement (xmlTextWriterPtr xmlChar* char* va_list)))
  (xmlTextWriterWriteDTDElement
   (int xmlTextWriterWriteDTDElement (xmlTextWriterPtr xmlChar* xmlChar*)))
  (xmlTextWriterStartDTDAttlist
   (int xmlTextWriterStartDTDAttlist (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterEndDTDAttlist
   (int xmlTextWriterEndDTDAttlist (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatDTDAttlist
  ;;  (int xmlTextWriterWriteFormatDTDAttlist (xmlTextWriterPtr xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatDTDAttlist
  ;;  (int xmlTextWriterWriteVFormatDTDAttlist (xmlTextWriterPtr xmlChar* char* va_list)))
  (xmlTextWriterWriteDTDAttlist
   (int xmlTextWriterWriteDTDAttlist (xmlTextWriterPtr xmlChar* xmlChar*)))
  (xmlTextWriterStartDTDEntity
   (int xmlTextWriterStartDTDEntity (xmlTextWriterPtr int xmlChar*)))
  (xmlTextWriterEndDTDEntity
   (int xmlTextWriterEndDTDEntity (xmlTextWriterPtr)))
  ;;Variadic!!!
  ;;
  ;; (xmlTextWriterWriteFormatDTDInternalEntity
  ;;  (int xmlTextWriterWriteFormatDTDInternalEntity (xmlTextWriterPtr int xmlChar* char* ...)))
  ;; (xmlTextWriterWriteVFormatDTDInternalEntity
  ;;  (int xmlTextWriterWriteVFormatDTDInternalEntity (xmlTextWriterPtr int xmlChar* char* va_list)))
  (xmlTextWriterWriteDTDInternalEntity
   (int xmlTextWriterWriteDTDInternalEntity (xmlTextWriterPtr int xmlChar* xmlChar*)))
  (xmlTextWriterWriteDTDExternalEntity
   (int xmlTextWriterWriteDTDExternalEntity (xmlTextWriterPtr int xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterWriteDTDExternalEntityContents
   (int xmlTextWriterWriteDTDExternalEntityContents (xmlTextWriterPtr xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterWriteDTDEntity
   (int xmlTextWriterWriteDTDEntity (xmlTextWriterPtr int xmlChar* xmlChar* xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterWriteDTDNotation
   (int xmlTextWriterWriteDTDNotation (xmlTextWriterPtr xmlChar* xmlChar* xmlChar*)))
  (xmlTextWriterSetIndent
   (int xmlTextWriterSetIndent (xmlTextWriterPtr int)))
  (xmlTextWriterSetIndentString
   (int xmlTextWriterSetIndentString (xmlTextWriterPtr xmlChar*)))
  (xmlTextWriterFlush
   (int xmlTextWriterFlush (xmlTextWriterPtr))))



;;;; XML Path Language implementation
;;
;;Header file "xpath.h".
;;

(define-c-functions libxml2-shared-object
  (xmlXPathFreeObject
   (void xmlXPathFreeObject (xmlXPathObjectPtr)))
  (xmlXPathNodeSetCreate
   (xmlNodeSetPtr xmlXPathNodeSetCreate (xmlNodePtr)))
  (xmlXPathFreeNodeSetList
   (void xmlXPathFreeNodeSetList (xmlXPathObjectPtr)))
  (xmlXPathFreeNodeSet
   (void xmlXPathFreeNodeSet (xmlNodeSetPtr)))
  (xmlXPathObjectCopy
   (xmlXPathObjectPtr xmlXPathObjectCopy (xmlXPathObjectPtr)))
  (xmlXPathCmpNodes
   (int xmlXPathCmpNodes (xmlNodePtr xmlNodePtr)))
  (xmlXPathCastNumberToBoolean
   (int xmlXPathCastNumberToBoolean (double)))
  (xmlXPathCastStringToBoolean
   (int xmlXPathCastStringToBoolean (xmlChar*)))
  (xmlXPathCastNodeSetToBoolean
   (int xmlXPathCastNodeSetToBoolean (xmlNodeSetPtr)))
  (xmlXPathCastToBoolean
   (int xmlXPathCastToBoolean (xmlXPathObjectPtr)))
  (xmlXPathCastBooleanToNumber
   (double xmlXPathCastBooleanToNumber (int)))
  (xmlXPathCastStringToNumber
   (double xmlXPathCastStringToNumber (xmlChar*)))
  (xmlXPathCastNodeToNumber
   (double xmlXPathCastNodeToNumber (xmlNodePtr)))
  (xmlXPathCastNodeSetToNumber
   (double xmlXPathCastNodeSetToNumber (xmlNodeSetPtr)))
  (xmlXPathCastToNumber
   (double xmlXPathCastToNumber (xmlXPathObjectPtr)))
  (xmlXPathCastBooleanToString
   (xmlChar* xmlXPathCastBooleanToString (int)))
  (xmlXPathCastNumberToString
   (xmlChar* xmlXPathCastNumberToString (double)))
  (xmlXPathCastNodeToString
   (xmlChar* xmlXPathCastNodeToString (xmlNodePtr)))
  (xmlXPathCastNodeSetToString
   (xmlChar* xmlXPathCastNodeSetToString (xmlNodeSetPtr)))
  (xmlXPathCastToString
   (xmlChar* xmlXPathCastToString (xmlXPathObjectPtr)))
  (xmlXPathConvertBoolean
   (xmlXPathObjectPtr xmlXPathConvertBoolean (xmlXPathObjectPtr)))
  (xmlXPathConvertNumber
   (xmlXPathObjectPtr xmlXPathConvertNumber (xmlXPathObjectPtr)))
  (xmlXPathConvertString
   (xmlXPathObjectPtr xmlXPathConvertString (xmlXPathObjectPtr)))
  (xmlXPathNewContext
   (xmlXPathContextPtr xmlXPathNewContext (xmlDocPtr)))
  (xmlXPathFreeContext
   (void xmlXPathFreeContext (xmlXPathContextPtr)))
  (xmlXPathContextSetCache
   (int xmlXPathContextSetCache (xmlXPathContextPtr int int int)))
  (xmlXPathOrderDocElems
   (long xmlXPathOrderDocElems (xmlDocPtr)))
  (xmlXPathEval
   (xmlXPathObjectPtr xmlXPathEval (xmlChar* xmlXPathContextPtr)))
  (xmlXPathEvalExpression
   (xmlXPathObjectPtr xmlXPathEvalExpression (xmlChar* xmlXPathContextPtr)))
  (xmlXPathEvalPredicate
   (int xmlXPathEvalPredicate (xmlXPathContextPtr xmlXPathObjectPtr)))
  (xmlXPathCompile
   (xmlXPathCompExprPtr xmlXPathCompile (xmlChar*)))
  (xmlXPathCtxtCompile
   (xmlXPathCompExprPtr xmlXPathCtxtCompile (xmlXPathContextPtr xmlChar*)))
  (xmlXPathCompiledEval
   (xmlXPathObjectPtr xmlXPathCompiledEval (xmlXPathCompExprPtr xmlXPathContextPtr)))
  (xmlXPathCompiledEvalToBoolean
   (int xmlXPathCompiledEvalToBoolean (xmlXPathCompExprPtr xmlXPathContextPtr)))
  (xmlXPathFreeCompExpr
   (void xmlXPathFreeCompExpr (xmlXPathCompExprPtr)))
  ;; #endif /* LIBXML_XPATH_ENABLED */
  ;; #if defined(LIBXML_XPATH_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED)
  (xmlXPathInit
   (void xmlXPathInit (void)))
  (xmlXPathIsNaN
   (int xmlXPathIsNaN (double)))
  (xmlXPathIsInf
   (int xmlXPathIsInf (double)))
  ;; #endif /* LIBXML_XPATH_ENABLED or LIBXML_SCHEMAS_ENABLED*/
  )


;;;; API to handle XML Pointers
;;
;;Header file "xpointer.h".
;;

(define-c-functions libxml2-shared-object
  (xmlXPtrLocationSetCreate
   (xmlLocationSetPtr xmlXPtrLocationSetCreate (xmlXPathObjectPtr)))
  (xmlXPtrFreeLocationSet
   (void xmlXPtrFreeLocationSet (xmlLocationSetPtr)))
  (xmlXPtrLocationSetMerge
   (xmlLocationSetPtr xmlXPtrLocationSetMerge (xmlLocationSetPtr xmlLocationSetPtr)))
  (xmlXPtrNewRange
   (xmlXPathObjectPtr xmlXPtrNewRange (xmlNodePtr int xmlNodePtr int)))
  (xmlXPtrNewRangePoints
   (xmlXPathObjectPtr xmlXPtrNewRangePoints (xmlXPathObjectPtr xmlXPathObjectPtr)))
  (xmlXPtrNewRangeNodePoint
   (xmlXPathObjectPtr xmlXPtrNewRangeNodePoint (xmlNodePtr xmlXPathObjectPtr)))
  (xmlXPtrNewRangePointNode
   (xmlXPathObjectPtr xmlXPtrNewRangePointNode (xmlXPathObjectPtr xmlNodePtr)))
  (xmlXPtrNewRangeNodes
   (xmlXPathObjectPtr xmlXPtrNewRangeNodes (xmlNodePtr xmlNodePtr)))
  (xmlXPtrNewLocationSetNodes
   (xmlXPathObjectPtr xmlXPtrNewLocationSetNodes (xmlNodePtr xmlNodePtr)))
  (xmlXPtrNewLocationSetNodeSet
   (xmlXPathObjectPtr xmlXPtrNewLocationSetNodeSet (xmlNodeSetPtr)))
  (xmlXPtrNewRangeNodeObject
   (xmlXPathObjectPtr xmlXPtrNewRangeNodeObject (xmlNodePtr xmlXPathObjectPtr)))
  (xmlXPtrNewCollapsedRange
   (xmlXPathObjectPtr xmlXPtrNewCollapsedRange (xmlNodePtr)))
  (xmlXPtrLocationSetAdd
   (void xmlXPtrLocationSetAdd (xmlLocationSetPtr xmlXPathObjectPtr)))
  (xmlXPtrWrapLocationSet
   (xmlXPathObjectPtr xmlXPtrWrapLocationSet (xmlLocationSetPtr)))
  (xmlXPtrLocationSetDel
   (void xmlXPtrLocationSetDel (xmlLocationSetPtr xmlXPathObjectPtr)))
  (xmlXPtrLocationSetRemove
   (void xmlXPtrLocationSetRemove (xmlLocationSetPtr int)))
  (xmlXPtrNewContext
   (xmlXPathContextPtr xmlXPtrNewContext (xmlDocPtr xmlNodePtr xmlNodePtr)))
  (xmlXPtrEval
   (xmlXPathObjectPtr xmlXPtrEval (xmlChar* xmlXPathContextPtr)))
  (xmlXPtrRangeToFunction
   (void xmlXPtrRangeToFunction (xmlXPathParserContextPtr int)))
  (xmlXPtrBuildNodeList
   (xmlNodePtr xmlXPtrBuildNodeList (xmlXPathObjectPtr)))
  (xmlXPtrEvalRangePredicate
   (void xmlXPtrEvalRangePredicate (xmlXPathParserContextPtr))))


;;;; done

)

;;; end of file
