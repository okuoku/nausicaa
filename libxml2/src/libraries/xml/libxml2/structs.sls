;;; (xml libxml2 structs) --
;;;
;;;Part of: Nausicaa
;;;Contents: foreign library structs fields identifier accessors
;;;Date: Tue Mar 23, 2010
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


(library
  (xml libxml2 structs)
  (export
    define-xmlChSRange
    with-struct-xmlChSRange
    define-xmlChLRange
    with-struct-xmlChLRange
    define-xmlChRangeGroup
    with-struct-xmlChRangeGroup
    define-xmlShellCtxt
    with-struct-xmlShellCtxt
    define-xmlCharEncodingHandler
    with-struct-xmlCharEncodingHandler
    define-xmlEntity
    with-struct-xmlEntity
    define-htmlElemDesc
    with-struct-htmlElemDesc
    define-htmlEntityDesc
    with-struct-htmlEntityDesc
    define-xmlParserInput
    with-struct-xmlParserInput
    define-xmlParserNodeInfo
    with-struct-xmlParserNodeInfo
    define-xmlParserNodeInfoSeq
    with-struct-xmlParserNodeInfoSeq
    define-xmlParserCtxt
    with-struct-xmlParserCtxt
    define-xmlSAXLocator
    with-struct-xmlSAXLocator
    define-xmlBuffer
    with-struct-xmlBuffer
    define-xmlNotation
    with-struct-xmlNotation
    define-xmlEnumeration
    with-struct-xmlEnumeration
    define-xmlAttribute
    with-struct-xmlAttribute
    define-xmlElementContent
    with-struct-xmlElementContent
    define-xmlElement
    with-struct-xmlElement
    define-xmlNs
    with-struct-xmlNs
    define-xmlDtd
    with-struct-xmlDtd
    define-xmlAttr
    with-struct-xmlAttr
    define-xmlID
    with-struct-xmlID
    define-xmlRef
    with-struct-xmlRef
    define-xmlNode
    with-struct-xmlNode
    define-xmlDoc
    with-struct-xmlDoc
    define-xmlDOMWrapCtxt
    with-struct-xmlDOMWrapCtxt
    define-xmlURI
    with-struct-xmlURI
    define-xmlValidCtxt
    with-struct-xmlValidCtxt
    define-xmlError
    with-struct-xmlError
    define-xmlParserInputBuffer
    with-struct-xmlParserInputBuffer
    define-xmlOutputBuffer
    with-struct-xmlOutputBuffer
    define-xmlSchemaAnnot
    with-struct-xmlSchemaAnnot
    define-xmlSchemaAttribute
    with-struct-xmlSchemaAttribute
    define-xmlSchemaAttributeLink
    with-struct-xmlSchemaAttributeLink
    define-xmlSchemaWildcardNs
    with-struct-xmlSchemaWildcardNs
    define-xmlSchemaWildcard
    with-struct-xmlSchemaWildcard
    define-xmlSchemaAttributeGroup
    with-struct-xmlSchemaAttributeGroup
    define-xmlSchemaTypeLink
    with-struct-xmlSchemaTypeLink
    define-xmlSchemaFacetLink
    with-struct-xmlSchemaFacetLink
    define-xmlSchemaElement
    with-struct-xmlSchemaElement
    define-xmlSchemaNotation
    with-struct-xmlSchemaNotation
    define-xmlNodeSet
    with-struct-xmlNodeSet
    define-xmlXPathObject
    with-struct-xmlXPathObject
    define-xmlXPathType
    with-struct-xmlXPathType
    define-xmlXPathVariable
    with-struct-xmlXPathVariable
    define-xmlXPathFunct
    with-struct-xmlXPathFunct
    define-xmlXPathAxis
    with-struct-xmlXPathAxis
    define-xmlXPathContext
    with-struct-xmlXPathContext
    define-xmlXPathParserContext
    with-struct-xmlXPathParserContext
    define-xmlLocationSet
    with-struct-xmlLocationSet)
  (import
    (rnrs)
    (foreign ffi)
    (foreign ffi utilities)
    (xml libxml2 sizeof))
  (define-struct-fields
    xmlChSRange
    (high high)
    (low low))
  (define-with-struct
    xmlChSRange
    (high high)
    (low low))
  (define-struct-fields
    xmlChLRange
    (high high)
    (low low))
  (define-with-struct
    xmlChLRange
    (high high)
    (low low))
  (define-struct-fields
    xmlChRangeGroup
    (longRange longRange)
    (shortRange shortRange)
    (nbLongRange nbLongRange)
    (nbShortRange nbShortRange))
  (define-with-struct
    xmlChRangeGroup
    (longRange longRange)
    (shortRange shortRange)
    (nbLongRange nbLongRange)
    (nbShortRange nbShortRange))
  (define-struct-fields
    xmlShellCtxt
    (input input)
    (output output)
    (loaded loaded)
    (pctxt pctxt)
    (node node)
    (doc doc)
    (filename filename))
  (define-with-struct
    xmlShellCtxt
    (input input)
    (output output)
    (loaded loaded)
    (pctxt pctxt)
    (node node)
    (doc doc)
    (filename filename))
  (define-struct-fields
    xmlCharEncodingHandler
    (iconv_out iconv_out)
    (iconv_in iconv_in)
    (output output)
    (input input)
    (name name))
  (define-with-struct
    xmlCharEncodingHandler
    (iconv_out iconv_out)
    (iconv_in iconv_in)
    (output output)
    (input input)
    (name name))
  (define-struct-fields
    xmlEntity
    (checked checked)
    (owner owner)
    (URI URI)
    (nexte nexte)
    (SystemID SystemID)
    (ExternalID ExternalID)
    (etype etype)
    (length length)
    (content content)
    (orig orig)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type type)
    (_private _private))
  (define-with-struct
    xmlEntity
    (checked checked)
    (owner owner)
    (URI URI)
    (nexte nexte)
    (SystemID SystemID)
    (ExternalID ExternalID)
    (etype etype)
    (length length)
    (content content)
    (orig orig)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type type)
    (_private _private))
  (define-struct-fields
    htmlElemDesc
    (attrs_req attrs_req)
    (attrs_depr attrs_depr)
    (attrs_opt attrs_opt)
    (defaultsubelt defaultsubelt)
    (subelts subelts)
    (desc desc)
    (isinline isinline)
    (dtd dtd)
    (depr depr)
    (empty empty)
    (saveEndTag saveEndTag)
    (endTag endTag)
    (startTag startTag)
    (name name))
  (define-with-struct
    htmlElemDesc
    (attrs_req attrs_req)
    (attrs_depr attrs_depr)
    (attrs_opt attrs_opt)
    (defaultsubelt defaultsubelt)
    (subelts subelts)
    (desc desc)
    (isinline isinline)
    (dtd dtd)
    (depr depr)
    (empty empty)
    (saveEndTag saveEndTag)
    (endTag endTag)
    (startTag startTag)
    (name name))
  (define-struct-fields
    htmlEntityDesc
    (desc desc)
    (name name)
    (value value))
  (define-with-struct
    htmlEntityDesc
    (desc desc)
    (name name)
    (value value))
  (define-struct-fields
    xmlParserInput
    (id id)
    (standalone standalone)
    (version version)
    (encoding encoding)
    (free free)
    (consumed consumed)
    (col col)
    (line line)
    (length length)
    (end end)
    (cur cur)
    (base base)
    (directory directory)
    (filename filename)
    (buf buf))
  (define-with-struct
    xmlParserInput
    (id id)
    (standalone standalone)
    (version version)
    (encoding encoding)
    (free free)
    (consumed consumed)
    (col col)
    (line line)
    (length length)
    (end end)
    (cur cur)
    (base base)
    (directory directory)
    (filename filename)
    (buf buf))
  (define-struct-fields
    xmlParserNodeInfo
    (end_line end_line)
    (end_pos end_pos)
    (begin_line begin_line)
    (begin_pos begin_pos)
    (node node))
  (define-with-struct
    xmlParserNodeInfo
    (end_line end_line)
    (end_pos end_pos)
    (begin_line begin_line)
    (begin_pos begin_pos)
    (node node))
  (define-struct-fields
    xmlParserNodeInfoSeq
    (buffer buffer)
    (length length)
    (maximum maximum))
  (define-with-struct
    xmlParserNodeInfoSeq
    (buffer buffer)
    (length length)
    (maximum maximum))
  (define-struct-fields
    xmlParserCtxt
    (sizeentities sizeentities)
    (nbentities nbentities)
    (xmlParserMode)
    (lastError)
    (freeAttrs freeAttrs)
    (freeAttrsNr freeAttrsNr)
    (freeElems freeElems)
    (freeElemsNr freeElemsNr)
    (dictNames dictNames)
    (options options)
    (nsWellFormed nsWellFormed)
    (attsSpecial attsSpecial)
    (attsDefault attsDefault)
    (pushTab pushTab)
    (attallocs attallocs)
    (nsTab nsTab)
    (nsMax nsMax)
    (nsNr nsNr)
    (sax2 sax2)
    (str_xml_ns str_xml_ns)
    (str_xmlns str_xmlns)
    (str_xml str_xml)
    (docdict docdict)
    (maxatts maxatts)
    (atts atts)
    (dict dict)
    (progressive progressive)
    (recovery recovery)
    (catalogs catalogs)
    (linenumbers linenumbers)
    (loadsubset loadsubset)
    (_private _private)
    (pedantic pedantic)
    (nodemem nodemem)
    (nodelen nodelen)
    (charset charset)
    (entity entity)
    (depth depth)
    (spaceTab spaceTab)
    (spaceMax spaceMax)
    (spaceNr spaceNr)
    (space space)
    (extSubSystem extSubSystem)
    (extSubURI extSubURI)
    (intSubName intSubName)
    (inSubset inSubset)
    (disableSAX disableSAX)
    (keepBlanks keepBlanks)
    (checkIndex checkIndex)
    (nbChars nbChars)
    (nameTab nameTab)
    (nameMax nameMax)
    (nameNr nameNr)
    (name name)
    (directory directory)
    (token token)
    (instate)
    (vctxt)
    (validate validate)
    (valid valid)
    (external external)
    (hasPErefs hasPErefs)
    (hasExternalSubset hasExternalSubset)
    (errNo errNo)
    (node_seq)
    (record_info record_info)
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr)
    (node node)
    (inputTab inputTab)
    (inputMax inputMax)
    (inputNr inputNr)
    (input input)
    (html html)
    (standalone standalone)
    (encoding encoding)
    (version version)
    (replaceEntities replaceEntities)
    (wellFormed wellFormed)
    (myDoc myDoc)
    (userData userData)
    (sax sax))
  (define-with-struct
    xmlParserCtxt
    (sizeentities sizeentities)
    (nbentities nbentities)
    (xmlParserMode)
    (lastError)
    (freeAttrs freeAttrs)
    (freeAttrsNr freeAttrsNr)
    (freeElems freeElems)
    (freeElemsNr freeElemsNr)
    (dictNames dictNames)
    (options options)
    (nsWellFormed nsWellFormed)
    (attsSpecial attsSpecial)
    (attsDefault attsDefault)
    (pushTab pushTab)
    (attallocs attallocs)
    (nsTab nsTab)
    (nsMax nsMax)
    (nsNr nsNr)
    (sax2 sax2)
    (str_xml_ns str_xml_ns)
    (str_xmlns str_xmlns)
    (str_xml str_xml)
    (docdict docdict)
    (maxatts maxatts)
    (atts atts)
    (dict dict)
    (progressive progressive)
    (recovery recovery)
    (catalogs catalogs)
    (linenumbers linenumbers)
    (loadsubset loadsubset)
    (_private _private)
    (pedantic pedantic)
    (nodemem nodemem)
    (nodelen nodelen)
    (charset charset)
    (entity entity)
    (depth depth)
    (spaceTab spaceTab)
    (spaceMax spaceMax)
    (spaceNr spaceNr)
    (space space)
    (extSubSystem extSubSystem)
    (extSubURI extSubURI)
    (intSubName intSubName)
    (inSubset inSubset)
    (disableSAX disableSAX)
    (keepBlanks keepBlanks)
    (checkIndex checkIndex)
    (nbChars nbChars)
    (nameTab nameTab)
    (nameMax nameMax)
    (nameNr nameNr)
    (name name)
    (directory directory)
    (token token)
    (instate)
    (vctxt)
    (validate validate)
    (valid valid)
    (external external)
    (hasPErefs hasPErefs)
    (hasExternalSubset hasExternalSubset)
    (errNo errNo)
    (node_seq)
    (record_info record_info)
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr)
    (node node)
    (inputTab inputTab)
    (inputMax inputMax)
    (inputNr inputNr)
    (input input)
    (html html)
    (standalone standalone)
    (encoding encoding)
    (version version)
    (replaceEntities replaceEntities)
    (wellFormed wellFormed)
    (myDoc myDoc)
    (userData userData)
    (sax sax))
  (define-struct-fields
    xmlSAXLocator
    (getColumnNumber getColumnNumber)
    (getLineNumber getLineNumber)
    (getSystemId getSystemId)
    (getPublicId getPublicId))
  (define-with-struct
    xmlSAXLocator
    (getColumnNumber getColumnNumber)
    (getLineNumber getLineNumber)
    (getSystemId getSystemId)
    (getPublicId getPublicId))
  (define-struct-fields
    xmlBuffer
    (contentIO contentIO)
    (alloc)
    (size size)
    (use use)
    (content content))
  (define-with-struct
    xmlBuffer
    (contentIO contentIO)
    (alloc)
    (size size)
    (use use)
    (content content))
  (define-struct-fields
    xmlNotation
    (SystemID SystemID)
    (PublicID PublicID)
    (name name))
  (define-with-struct
    xmlNotation
    (SystemID SystemID)
    (PublicID PublicID)
    (name name))
  (define-struct-fields
    xmlEnumeration
    (name name)
    (next next))
  (define-with-struct
    xmlEnumeration
    (name name)
    (next next))
  (define-struct-fields
    xmlAttribute
    (elem elem)
    (prefix prefix)
    (tree tree)
    (defaultValue defaultValue)
    (def)
    (atype)
    (nexth nexth)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlAttribute
    (elem elem)
    (prefix prefix)
    (tree tree)
    (defaultValue defaultValue)
    (def)
    (atype)
    (nexth nexth)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlElementContent
    (prefix prefix)
    (parent parent)
    (c2 c2)
    (c1 c1)
    (name name)
    (ocur)
    (type))
  (define-with-struct
    xmlElementContent
    (prefix prefix)
    (parent parent)
    (c2 c2)
    (c1 c1)
    (name name)
    (ocur)
    (type))
  (define-struct-fields
    xmlElement
    (contModel contModel)
    (prefix prefix)
    (attributes attributes)
    (content content)
    (etype)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlElement
    (contModel contModel)
    (prefix prefix)
    (attributes attributes)
    (content content)
    (etype)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlNs
    (context context)
    (_private _private)
    (prefix prefix)
    (href href)
    (type)
    (next next))
  (define-with-struct
    xmlNs
    (context context)
    (_private _private)
    (prefix prefix)
    (href href)
    (type)
    (next next))
  (define-struct-fields
    xmlDtd
    (pentities pentities)
    (SystemID SystemID)
    (ExternalID ExternalID)
    (entities entities)
    (attributes attributes)
    (elements elements)
    (notations notations)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlDtd
    (pentities pentities)
    (SystemID SystemID)
    (ExternalID ExternalID)
    (entities entities)
    (attributes attributes)
    (elements elements)
    (notations notations)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlAttr
    (psvi psvi)
    (atype)
    (ns ns)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlAttr
    (psvi psvi)
    (atype)
    (ns ns)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlID
    (doc doc)
    (lineno lineno)
    (name name)
    (attr)
    (value value)
    (next next))
  (define-with-struct
    xmlID
    (doc doc)
    (lineno lineno)
    (name name)
    (attr)
    (value value)
    (next next))
  (define-struct-fields
    xmlRef
    (lineno lineno)
    (name name)
    (attr)
    (value value)
    (next next))
  (define-with-struct
    xmlRef
    (lineno lineno)
    (name name)
    (attr)
    (value value)
    (next next))
  (define-struct-fields
    xmlNode
    (extra extra)
    (line line)
    (psvi psvi)
    (nsDef nsDef)
    (properties properties)
    (content content)
    (ns ns)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlNode
    (extra extra)
    (line line)
    (psvi psvi)
    (nsDef nsDef)
    (properties properties)
    (content content)
    (ns ns)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlDoc
    (properties properties)
    (parseFlags parseFlags)
    (psvi psvi)
    (dict dict)
    (charset charset)
    (URL URL)
    (refs refs)
    (ids ids)
    (encoding encoding)
    (version version)
    (oldNs oldNs)
    (extSubset extSubset)
    (intSubset intSubset)
    (standalone standalone)
    (compression compression)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-with-struct
    xmlDoc
    (properties properties)
    (parseFlags parseFlags)
    (psvi psvi)
    (dict dict)
    (charset charset)
    (URL URL)
    (refs refs)
    (ids ids)
    (encoding encoding)
    (version version)
    (oldNs oldNs)
    (extSubset extSubset)
    (intSubset intSubset)
    (standalone standalone)
    (compression compression)
    (doc doc)
    (prev prev)
    (next next)
    (parent parent)
    (last last)
    (children children)
    (name name)
    (type)
    (_private _private))
  (define-struct-fields
    xmlDOMWrapCtxt
    (getNsForNodeFunc getNsForNodeFunc)
    (namespaceMap namespaceMap)
    (type type)
    (_private _private))
  (define-with-struct
    xmlDOMWrapCtxt
    (getNsForNodeFunc getNsForNodeFunc)
    (namespaceMap namespaceMap)
    (type type)
    (_private _private))
  (define-struct-fields
    xmlURI
    (query_raw query_raw)
    (cleanup cleanup)
    (fragment fragment)
    (query query)
    (path path)
    (port port)
    (user user)
    (server server)
    (authority authority)
    (opaque opaque)
    (scheme scheme))
  (define-with-struct
    xmlURI
    (query_raw query_raw)
    (cleanup cleanup)
    (fragment fragment)
    (query query)
    (path path)
    (port port)
    (user user)
    (server server)
    (authority authority)
    (opaque opaque)
    (scheme scheme))
  (define-struct-fields
    xmlValidCtxt
    (state state)
    (am am)
    (vstateTab vstateTab)
    (vstateMax vstateMax)
    (vstateNr vstateNr)
    (vstate vstate)
    (valid valid)
    (doc doc)
    (finishDtd finishDtd)
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr)
    (node node)
    (warning warning)
    (error error)
    (userData userData))
  (define-with-struct
    xmlValidCtxt
    (state state)
    (am am)
    (vstateTab vstateTab)
    (vstateMax vstateMax)
    (vstateNr vstateNr)
    (vstate vstate)
    (valid valid)
    (doc doc)
    (finishDtd finishDtd)
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr)
    (node node)
    (warning warning)
    (error error)
    (userData userData))
  (define-struct-fields
    xmlError
    (node node)
    (ctxt ctxt)
    (int2 int2)
    (int1 int1)
    (str3 str3)
    (str2 str2)
    (str1 str1)
    (line line)
    (file file)
    (level level)
    (message message)
    (code code)
    (domain domain))
  (define-with-struct
    xmlError
    (node node)
    (ctxt ctxt)
    (int2 int2)
    (int1 int1)
    (str3 str3)
    (str2 str2)
    (str1 str1)
    (line line)
    (file file)
    (level level)
    (message message)
    (code code)
    (domain domain))
  (define-struct-fields
    xmlParserInputBuffer
    (rawconsumed rawconsumed)
    (error error)
    (compressed compressed)
    (raw raw)
    (buffer buffer)
    (encoder encoder)
    (closecallback closecallback)
    (readcallback readcallback)
    (context context))
  (define-with-struct
    xmlParserInputBuffer
    (rawconsumed rawconsumed)
    (error error)
    (compressed compressed)
    (raw raw)
    (buffer buffer)
    (encoder encoder)
    (closecallback closecallback)
    (readcallback readcallback)
    (context context))
  (define-struct-fields
    xmlOutputBuffer
    (error error)
    (written written)
    (conv conv)
    (buffer buffer)
    (encoder encoder)
    (closecallback closecallback)
    (writecallback writecallback)
    (context context))
  (define-with-struct
    xmlOutputBuffer
    (error error)
    (written written)
    (conv conv)
    (buffer buffer)
    (encoder encoder)
    (closecallback closecallback)
    (writecallback writecallback)
    (context context))
  (define-struct-fields
    xmlSchemaAnnot
    (content content)
    (next next))
  (define-with-struct
    xmlSchemaAnnot
    (content content)
    (next next))
  (define-struct-fields
    xmlSchemaAttribute
    (refDecl refDecl)
    (defVal defVal)
    (refPrefix refPrefix)
    (flags flags)
    (targetNamespace targetNamespace)
    (node node)
    (subtypes subtypes)
    (defValue defValue)
    (occurs occurs)
    (base base)
    (annot annot)
    (typeNs typeNs)
    (typeName typeName)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-with-struct
    xmlSchemaAttribute
    (refDecl refDecl)
    (defVal defVal)
    (refPrefix refPrefix)
    (flags flags)
    (targetNamespace targetNamespace)
    (node node)
    (subtypes subtypes)
    (defValue defValue)
    (occurs occurs)
    (base base)
    (annot annot)
    (typeNs typeNs)
    (typeName typeName)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-struct-fields
    xmlSchemaAttributeLink
    (attr attr)
    (next next))
  (define-with-struct
    xmlSchemaAttributeLink
    (attr attr)
    (next next))
  (define-struct-fields
    xmlSchemaWildcardNs
    (value value)
    (next next))
  (define-with-struct
    xmlSchemaWildcardNs
    (value value)
    (next next))
  (define-struct-fields
    xmlSchemaWildcard
    (flags flags)
    (negNsSet negNsSet)
    (nsSet nsSet)
    (any any)
    (processContents processContents)
    (maxOccurs maxOccurs)
    (minOccurs minOccurs)
    (xmlNodePtr xmlNodePtr)
    (annot annot)
    (id id)
    (type type))
  (define-with-struct
    xmlSchemaWildcard
    (flags flags)
    (negNsSet negNsSet)
    (nsSet nsSet)
    (any any)
    (processContents processContents)
    (maxOccurs maxOccurs)
    (minOccurs minOccurs)
    (xmlNodePtr xmlNodePtr)
    (annot annot)
    (id id)
    (type type))
  (define-struct-fields
    xmlSchemaAttributeGroup
    (attrUses attrUses)
    (targetNamespace targetNamespace)
    (refItem refItem)
    (refPrefix refPrefix)
    (attributeWildcard attributeWildcard)
    (flags flags)
    (node node)
    (attributes attributes)
    (annot annot)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-with-struct
    xmlSchemaAttributeGroup
    (attrUses attrUses)
    (targetNamespace targetNamespace)
    (refItem refItem)
    (refPrefix refPrefix)
    (attributeWildcard attributeWildcard)
    (flags flags)
    (node node)
    (attributes attributes)
    (annot annot)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-struct-fields
    xmlSchemaTypeLink
    (type type)
    (next next))
  (define-with-struct
    xmlSchemaTypeLink
    (type type)
    (next next))
  (define-struct-fields
    xmlSchemaFacetLink
    (facet facet)
    (next next))
  (define-with-struct
    xmlSchemaFacetLink
    (facet facet)
    (next next))
  (define-struct-fields
    xmlSchemaElement
    (idcs idcs)
    (defVal defVal)
    (refPrefix refPrefix)
    (contentType contentType)
    (contModel contModel)
    (refDecl refDecl)
    (value value)
    (scope scope)
    (substGroupNs substGroupNs)
    (substGroup substGroup)
    (namedTypeNs namedTypeNs)
    (namedType namedType)
    (targetNamespace targetNamespace)
    (flags flags)
    (maxOccurs maxOccurs)
    (minOccurs minOccurs)
    (node node)
    (attributes attributes)
    (subtypes subtypes)
    (annot annot)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-with-struct
    xmlSchemaElement
    (idcs idcs)
    (defVal defVal)
    (refPrefix refPrefix)
    (contentType contentType)
    (contModel contModel)
    (refDecl refDecl)
    (value value)
    (scope scope)
    (substGroupNs substGroupNs)
    (substGroup substGroup)
    (namedTypeNs namedTypeNs)
    (namedType namedType)
    (targetNamespace targetNamespace)
    (flags flags)
    (maxOccurs maxOccurs)
    (minOccurs minOccurs)
    (node node)
    (attributes attributes)
    (subtypes subtypes)
    (annot annot)
    (refNs refNs)
    (ref ref)
    (id id)
    (name name)
    (next next)
    (type type))
  (define-struct-fields
    xmlSchemaNotation
    (targetNamespace targetNamespace)
    (identifier identifier)
    (annot annot)
    (name name)
    (type type))
  (define-with-struct
    xmlSchemaNotation
    (targetNamespace targetNamespace)
    (identifier identifier)
    (annot annot)
    (name name)
    (type type))
  (define-struct-fields
    xmlNodeSet
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr))
  (define-with-struct
    xmlNodeSet
    (nodeTab nodeTab)
    (nodeMax nodeMax)
    (nodeNr nodeNr))
  (define-struct-fields
    xmlXPathObject
    (index2 index2)
    (user2 user2)
    (index index)
    (user user)
    (stringval stringval)
    (floatval floatval)
    (boolval boolval)
    (nodesetval nodesetval)
    (type type))
  (define-with-struct
    xmlXPathObject
    (index2 index2)
    (user2 user2)
    (index index)
    (user user)
    (stringval stringval)
    (floatval floatval)
    (boolval boolval)
    (nodesetval nodesetval)
    (type type))
  (define-struct-fields
    xmlXPathType
    (func func)
    (name name))
  (define-with-struct
    xmlXPathType
    (func func)
    (name name))
  (define-struct-fields
    xmlXPathVariable
    (value value)
    (name name))
  (define-with-struct
    xmlXPathVariable
    (value value)
    (name name))
  (define-struct-fields
    xmlXPathFunct
    (func func)
    (name name))
  (define-with-struct
    xmlXPathFunct
    (func func)
    (name name))
  (define-struct-fields
    xmlXPathAxis
    (func func)
    (name name))
  (define-with-struct
    xmlXPathAxis
    (func func)
    (name name))
  (define-struct-fields
    xmlXPathContext
    (cache cache)
    (flags flags)
    (dict dict)
    (debugNode debugNode)
    (lastError)
    (error error)
    (userData userData)
    (tmpNsNr tmpNsNr)
    (tmpNsList tmpNsList)
    (funcLookupData funcLookupData)
    (funcLookupFunc funcLookupFunc)
    (functionURI functionURI)
    (function function)
    (extra extra)
    (varLookupData varLookupData)
    (varLookupFunc varLookupFunc)
    (nsHash nsHash)
    (origin origin)
    (here here)
    (xptr xptr)
    (proximityPosition proximityPosition)
    (contextSize contextSize)
    (user user)
    (nsNr nsNr)
    (namespaces namespaces)
    (axis axis)
    (max_axis max_axis)
    (nb_axis nb_axis)
    (funcHash funcHash)
    (max_funcs_unused max_funcs_unused)
    (nb_funcs_unused nb_funcs_unused)
    (types types)
    (max_types max_types)
    (nb_types nb_types)
    (varHash varHash)
    (max_variables_unused max_variables_unused)
    (nb_variables_unused nb_variables_unused)
    (node node)
    (doc doc))
  (define-with-struct
    xmlXPathContext
    (cache cache)
    (flags flags)
    (dict dict)
    (debugNode debugNode)
    (lastError)
    (error error)
    (userData userData)
    (tmpNsNr tmpNsNr)
    (tmpNsList tmpNsList)
    (funcLookupData funcLookupData)
    (funcLookupFunc funcLookupFunc)
    (functionURI functionURI)
    (function function)
    (extra extra)
    (varLookupData varLookupData)
    (varLookupFunc varLookupFunc)
    (nsHash nsHash)
    (origin origin)
    (here here)
    (xptr xptr)
    (proximityPosition proximityPosition)
    (contextSize contextSize)
    (user user)
    (nsNr nsNr)
    (namespaces namespaces)
    (axis axis)
    (max_axis max_axis)
    (nb_axis nb_axis)
    (funcHash funcHash)
    (max_funcs_unused max_funcs_unused)
    (nb_funcs_unused nb_funcs_unused)
    (types types)
    (max_types max_types)
    (nb_types nb_types)
    (varHash varHash)
    (max_variables_unused max_variables_unused)
    (nb_variables_unused nb_variables_unused)
    (node node)
    (doc doc))
  (define-struct-fields
    xmlXPathParserContext
    (ancestor ancestor)
    (xptr xptr)
    (comp comp)
    (valueTab valueTab)
    (valueMax valueMax)
    (valueNr valueNr)
    (value value)
    (context context)
    (error error)
    (base base)
    (cur cur))
  (define-with-struct
    xmlXPathParserContext
    (ancestor ancestor)
    (xptr xptr)
    (comp comp)
    (valueTab valueTab)
    (valueMax valueMax)
    (valueNr valueNr)
    (value value)
    (context context)
    (error error)
    (base base)
    (cur cur))
  (define-struct-fields
    xmlLocationSet
    (locTab locTab)
    (locMax locMax)
    (locNr locNr))
  (define-with-struct
    xmlLocationSet
    (locTab locTab)
    (locMax locMax)
    (locNr locNr)))


;;; end of file
