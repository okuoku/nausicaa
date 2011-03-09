;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for XML documents
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;	A lexer  for XML  aiming to be  compliant with  the ``Extensible
;;;	Markup Language (XML)  1.0 (Fifth Edition)'', W3C Recommendation
;;;	26 November 2008:
;;;
;;;		http://www.w3.org/TR/REC-xml/
;;;		http://www.w3.org/XML/xml-V10-5e-errata
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;page

;; production [2]
Char			#x9|#xA|#xD|[#x20-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]

;; production [3]
S			(#x20|#x9|#xD|#xA)+

;;Like Char but  without #x2D which is the dash  character '-'.  This is
;;used in the regexps below when the XML Recommendation documents uses:
;;
;;  Char - '-'
;;
;;which means "Char minus dash".
;;
CharNoDash		#x9|#xA|#xD|[#x20-#x2C]|[#x2E-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]

;;page

;; [4]
NameStartChar		":"|[A-Z]|"_"|[a-z]|[#xC0-#xD6]|[#xD8-#xF6]|[#xF8-#x2FF]|[#x370-#x37D]|[#x37F-#x1FFF]|[#x200C-#x200D]|[#x2070-#x218F]|[#x2C00-#x2FEF]|[#x3001-#xD7FF]|[#xF900-#xFDCF]|[#xFDF0-#xFFFD]|[#x10000-#xEFFFF]

;; [4a]
NameChar		{NameStartChar}|"-"|"."|[0-9]|#xB7|[#x0300-#x036F]|[#x203F-#x2040]

;; [5]
Name			{NameStartChar}{NameChar}*

;; [6]
Names			{Name}(#x20{Name})*

;; [7]
Nmtoken			({NameChar})+

;; [8]
Nmtokens		{Nmtoken}(#x20{Nmtoken})*

;;page
;;Like "NameStartChar" but without "l", "L", "m", "M", "x", "X".
;;
NameStartCharNoXML	":"|[A-KN-WYZ]|"_"|[a-kn-wyz]|[#xC0-#xD6]|[#xD8-#xF6]|[#xF8-#x2FF]|[#x370-#x37D]|[#x37F-#x1FFF]|[#x200C-#x200D]|[#x2070-#x218F]|[#x2C00-#x2FEF]|[#x3001-#xD7FF]|[#xF900-#xFDCF]|[#xFDF0-#xFFFD]|[#x10000-#xEFFFF]

;;Like "NameChar" but without "l", "L", "m", "M", "x", "X".
;;
NameCharNoXML		{NameStartCharNoXML}|"-"|"."|[0-9]|#xB7|[#x0300-#x036F]|[#x203F-#x2040]

;;All the sequences with "l", "L", "m", "M", "x", "X" not forming "[Xx][Mm][Ll]"
;;
NameNoXML1		(([Xx][^Mm]?)|([Mm][Ll]?)|[Ll]|([Xx][Mm][^Ll]?)|([Xx][^Mm]?[Ll])|([^Xx]?[Mm][Ll]))

;;Like "Name" but without "[Xx][Mm][Ll]" in any position.
NameNoXML		({NameStartCharNoXML}|{NameNoXML1})|({NameStartCharNoXML}|{NameNoXML1})*

;;page
;;;; character and entity references

;; [66]
CharRef			("&#"[0-9]+";")|("&#x"[0-9a-fA-F]+";")

;; [68]
EntityRef		"&"{Name}";"

;; [69]
PEReference		"%"{Name}";"

;; [67]
Reference		{EntityRef}|{CharRef}

;;page
;; [9]
EntityValue		\"([^%&\"]|{PEReference}|{Reference})*\"|"'"([^%&']|{PEReference}|{Reference})*"'"

;; [10]
AttValue	      	\"([^<&\"]|{Reference})*\"|'([^<&']|{Reference})*'

;; [11]
SystemLiteral	      	(\"[^\"]\")|('[^']*')

;; [13]
PubidChar		#x20|#xD|#xA|[a-zA-Z0-9]|[\-'\(\)\+,\./:=\?\;!\*#@\$_%]

;;Like PubidChar  but without the tick  character.  This is  used in the
;;regexps below when the XML Recommendation documents uses:
;;
;;  PubidChar - "'"
;;
;;which means "PubidChar minus tick".
;;
PubidCharNoTick		#x20|#xD|#xA|[a-zA-Z0-9]|[\-\(\)\+,\./:=\?\;!\*#@\$_%]

;; [12]
PubidLiteral	      	\"{PubidChar}*\"|"'"{PubidCharNoTick}*"'"

;;page
;;;; character data and comments

;;The original production [14] is:
;;
;;  CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)
;;
;;which means:  any sequence of  chars except "<"  and "&", but  not the
;;sequence "x]]>x"  where "x"  is any char  except "<" and  "&".
;;
;;*** NOTE ***
;;
;;The empty  string matches  production [14]!!!  We  modify it  below to
;;require at least one character.
;;

CharData1		[^<>&\]]
CharData2		((\][^\]]?)|(\]\][^>]?))

;; [14]
CharData		({CharData1}|{CharData2})+

;;The original production [15] is:
;;
;;  Comment	   ::=   	'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
;;
;; [15]
Comment			"<!--"({CharNoDash}|("-"{CharNoDash}))*"-->"

;;page
;;;; processor instructions sections

;;Like  "Char" but  with  neither  question marks,  which  is #x3F,  nor
;;greater than  chars, which  are #x3E.  It  is used in  the productions
;;below when the XML Recommendatation uses:
;;
;;  (Char* - (Char* '?>' Char*))*
;;
;;which  means  a  sequence of  one  or  more  "Char" but  without  "?>"
;;sequences.
;;
CharNoPiEnd1		#x9|#xA|#xD|[#x20-#x3D][#x40-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]
CharNoPiEnd2		\?|>|\?[^>]
CharNoPiEnd		({CharNoPiEnd1}|{CharNoPiEnd2})+

;; [17]
PITarget		{NameNoXML}

;; [16]
PI			"<?"{PITarget}({S}{CharNoPiEnd}*)?"?>"

;;page
;;;; CDATA sections

;;The original production [20] is:
;;
;;  CData	   ::=   	(Char* - (Char* ']]>' Char*))
;;
;;which means: like "Char" but without sequences "]]>".
;;

;;Like "Char" but with neither ">" which is #x3E, nor "]" which is #x5D.
;;
CData1			#x9|#xA|#xD|[#x20#x3D][#x3F-#x5C][#x5E-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]

;;All the allowed sequences with "]" and ">".
;;
CData2			((\][^\]]?)|(\]\][^>]?))

;; [20]
CData			({CData1}|{CData2})*

;; [19]
CDStart			"<![CDATA["

;; [21]
CDEnd			"]]>"

;; [18]
CDSect			{CDStart}{CData}{CDEnd}

;;page
;;;; element type declarations
;;
;;FIXME There are recursive macros here!!!

;; [75]
ExternalID	("SYSTEM"{S}{SystemLiteral})|("PUBLIC"{S}{PubidLiteral}{S}{SystemLiteral})

;; [76]
NDataDecl	{S}"NDATA"{S}{Name}

;; [48]
cp		({Name}|{choice}|{seq})("?"|"*"|"+")?

;; [50]
seq		"("{S}?{cp}({S}?","{S}?{cp})*{S}?")"

;; [49]
choice		"("{S}?{cp}({S}?"|"{S}?{cp})+{S}?")"

;; [47]
children	({choice}|{seq})("?"|"*"|"+")?

;; [51]
Mixed		("("{S}?"#PCDATA"({S}?"|"{S}?{Name})*{S}?")*")|("("{S}?"#PCDATA"{S}?")")

;; [46]
contentspec	"EMPTY"|"ANY"|{Mixed}|{children}

;; [45]
elementdecl	"<!ELEMENT"{S}{Name}{S}{contentspec}{S}?">"

;;page
;;;; attribute list declarations

;; [55]
StringType	"CDATA"

;; [56]
TokenizedType	"ID"|"IDREF"|"IDREFS"|"ENTITY"|"ENTITIES"|"NMTOKEN"|"NMTOKENS"

;; [58]
NotationType	"NOTATION"{S}"("{S}?{Name}({S}?"|"{S}?{Name})*{S}?")"

;; [59]
Enumeration	"("{S}?{Nmtoken}({S}?"|"{S}?{Nmtoken})*{S}?")"

;; [57]
EnumeratedType	{NotationType}|{Enumeration}

;; [54]
AttType		{StringType}|{TokenizedType}|{EnumeratedType}

;; [60]
DefaultDecl	"#REQUIRED"|"#IMPLIED"|(("#FIXED"{S})?{AttValue})

;; [53]
AttDef		{S}{Name}{S}{AttType}{S}{DefaultDecl}

;; [52]
AttlistDecl	"<!ATTLIST"{S}{Name}{AttDef}*{S}?">"

;;page
;;;; external subset
;;
;;We put  it here as reference,  but it is  NOT used by this  lexer: the
;;external subset is... external.
;;

;; [31]
;;extSubsetDecl		({markupdecl}|{conditionalSect}|{DeclSep})*

;; [62]
;;includeSect		"<!["{S}?"INCLUDE"{S}?"["{extSubsetDecl}"]]>"

;; [65] for this we must define a special "Char" macro
;;Ignore		{Char}* - (Char* ('<![' | ']]>') Char*)

;; [64] This is recursive!!!
;; ignoreSectContents	{Ignore}("<!["{ignoreSectContents}"]]>"{Ignore})*

;; [63]
;;ignoreSect		"<!["{S}?"IGNORE"{S}?"["{ignoreSectContents}*"]]>"

;; [61]
;;conditionalSect		{includeSect}|{ignoreSect}

;; [30]
;;extSubset		{TextDecl}?{extSubsetDecl}

;;page
;;;; parsed entities

;; [77]
TextDecl	"<?xml"{VersionInfo}?{EncodingDecl}{S}?"?>"

;; [78]
extParsedEnt	{TextDecl}?{content}

;; [81]
EncName		[A-Za-z]([A-Za-z0-9._]|"-")*

;; [80]
EncodingDecl	{S}"encoding"{Eq}(("\""{EncName}"\"")|("'"{EncName}"'"))

;;page
;;;; notation declarations

;; [83]
PublicID	"PUBLIC"{S}{PubidLiteral}

;; [82]
NotationDecl	"<!NOTATION"{S}{Name}{S}({ExternalID}|{PublicID}){S}?">"

;;page
;;;; entity declarations

;; [73]
EntityDef	{EntityValue}|({ExternalID}{NDataDecl}?)

;; [71]
GEDecl		"<!ENTITY"{S}{Name}{S}{EntityDef}{S}?">"

;; [74]
PEDef		{EntityValue}|{ExternalID}

;; [72]
PEDecl		"<!ENTITY"{S}"%"{S}{Name}{S}{PEDef}{S}?">"

;; [70]
EntityDecl	{GEDecl}|{PEDecl}

;;page
;;;; standalone document declaration

;; [32]
SDDecl		{S}"standalone"{Eq}(("'"("yes"|"no")"'")|(\"("yes"|"no")\"))

;;page
;;;; document type definition

;; [28]
doctypedecl		"<!DOCTYPE"{S}{Name}({S}{ExternalID})?{S}?("["{intSubset}"]"{S}?)?">"

;; [28a]
DeclSep			{PEReference}|{S}

;; [28b]
intSubset		({markupdecl}|{DeclSep})*

;; [29]
markupdecl		{elementdecl}|{AttlistDecl}|{EntityDecl}|{NotationDecl}|{PI}|{Comment}

;;page
;;;; prolog

;; [25]
Eq			{S}?"="{S}?

;; [26]
VersionNum		"1."[0-9]+

;; [27]
Misc			{Comment}|{PI}|{S}

;; [24]
VersionInfo		{S}"version"{Eq}(("'"{VersionNum}"'")|(\"{VersionNum}\"))

;; [23]
XMLDecl			"<?xml"{VersionInfo}{EncodingDecl}?{SDDecl}?{S}?"?>"

;; [22]
prolog			{XMLDecl}?{Misc}*({doctypedecl}{Misc}*)?

;;page
;;;; elements

;; [41]
Attribute	{Name}{Eq}{AttValue}

;; [40]
STag		"<"{Name}({S}{Attribute})*{S}?">"

;; [42]
ETag		"</"{Name}{S}?">"

;; [43]
content		{CharData}?(({element}|{Reference}|{CDSect}|{PI}|{Comment}){CharData}?)*

;; [44]
EmptyElemTag	"<"{Name}({S}{Attribute})*{S}?"/>"

;; [39]
element		{EmptyElemTag}|({STag}{content}{ETag})

;;page
%%

{CharData}		((char-data-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{Comment}		((comment-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{CDSect}		((cdsect-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{PI}			((pi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)

{prolog}		((prolog-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
