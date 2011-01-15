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
Name			{NameStartChar}({NameChar})*

;; [6]
Names			{Name}(#x20{Name})*

;; [7]
Nmtoken			({NameChar})+

;; [8]
Nmtokens		{Nmtoken}(#x20{Nmtoken})*

;;page
;;;In  this page  we  produce  "NameNoXML", which  must  be like  "Name"
;;;without "[Xx][Mm][Ll]" in any position.
;;;

;;Like NameStartChar, NameChar  but without "x" and "X",  which are #x78
;;and #x58.
;;
NameStartCharNoX	":"|[A-WYZ]|"_"|[a-wyz]|[#xC0-#xD6]|[#xD8-#xF6]|[#xF8-#x2FF]|[#x370-#x37D]|[#x37F-#x1FFF]|[#x200C-#x200D]|[#x2070-#x218F]|[#x2C00-#x2FEF]|[#x3001-#xD7FF]|[#xF900-#xFDCF]|[#xFDF0-#xFFFD]|[#x10000-#xEFFFF]
NameCharNoX		{NameStartCharNoX}|"-"|"."|[0-9]|#xB7|[#x0300-#x036F]|[#x203F-#x2040]

;;A Name with neither "x" nor "X".
NAMENOXML1		{NameStartCharNoX}({NameCharNoX})*

;;A Name without "[Xx][Mm][Ll]"  inside but allowing "[Xx][Mm]", but not
;;as prefix.
NameNoXML2		({NameStartCharNoX}{NameCharNoX}*[Xx][Mm][^Ll]{NameCharNoX}*)*|({NameStartCharNoX}{NameCharNoX}*[Xx][Mm])*

;;A Name without "[Xx][Mm][Ll]" inside, having "[Xx][Mm]" as prefix.
NameNoXML3		[Xx][Mm]([^Ll]|{NameNoXML2})

;;A Name without "[Xx][Mm][Ll]" in any position.
NameNoXML		{NameNoXML1}|{NameNoXML2}|{NameNoXML3}

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

;;The original production [14] is:
;;
;;  CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)
;;
;;which means: any number of characters  except "<" and "&", but not the
;;sequence "x]]>x" where "x" is any char except "<" and "&".
;;
;;FIXME The regexp below will include the forbidden sequences "x]]>x".
;;
;; [14]
CharData		[^<&]*

;;The original production [15] is:
;;
;;  Comment	   ::=   	'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
;;
;; [15]
Comment			"<!--"({CharNoDash}|("-"{CharNoDash}))*"-->"

;;page
;;;In this page we produece "CharNoPiEnd", which is like "Char" but with
;;;no "?>" in any position.

;;Like  "Char" but  with  neither  question marks,  which  is #x3F,  nor
;;greater than chars, which are #x3E.
;;
CharNoPiEnd1	#x9|#xA|#xD|[#x20-#x3D][#x40-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]

CharNoPiEnd2	\?|>|\?[^>]
CharNoPiEnd	{CharNoPiEnd1}|{CharNoPiEnd2}

;;;; 2.6 Processing Instructions

;;page

;; [17]
PITarget		{NameNoXML}

;; [16]	FIXME CharNoPiEnd
PI			"<?"{PITarget}({S}(Char - (Char* '?>' Char*)))?"?>"

;;page
%%

{Char}		((open-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
