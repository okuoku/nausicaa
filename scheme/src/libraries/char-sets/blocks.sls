;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: character sets from "Blocks.txt"
;;;Date: Tue Jun 23, 2009
;;;
;;;Abstract
;;;
;;;	This file holds  one character set definition for  each range of
;;;	code points  defined in the  file "Blocks.txt" from  the Unicode
;;;	Characters Database:
;;;
;;;		<ftp://ftp.unicode.org/Public/UNIDATA/Blocks.txt>
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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



(library (char-sets blocks)
  (export
    char-set:basic-latin
    char-set:latin-1-supplement
    char-set:latin-extended-a
    char-set:latin-extended-b
    char-set:ipa-extensions
    char-set:spacing-modifier-letters
    char-set:combining-diacritical-marks
    char-set:greek-and-coptic
    char-set:cyrillic
    char-set:cyrillic-supplement
    char-set:armenian
    char-set:hebrew
    char-set:arabic
    char-set:syriac
    char-set:arabic-supplement
    char-set:thaana
    char-set:nko
    char-set:devanagari
    char-set:bengali
    char-set:gurmukhi
    char-set:gujarati
    char-set:oriya
    char-set:tamil
    char-set:telugu
    char-set:kannada
    char-set:malayalam
    char-set:sinhala
    char-set:thai
    char-set:lao
    char-set:tibetan
    char-set:myanmar
    char-set:georgian
    char-set:hangul-jamo
    char-set:ethiopic
    char-set:ethiopic-supplement
    char-set:cherokee
    char-set:unified-canadian-aboriginal-syllabics
    char-set:ogham
    char-set:runic
    char-set:tagalog
    char-set:hanunoo
    char-set:buhid
    char-set:tagbanwa
    char-set:khmer
    char-set:mongolian
    char-set:limbu
    char-set:tai-le
    char-set:new-tai-lue
    char-set:khmer-symbols
    char-set:buginese
    char-set:balinese
    char-set:sundanese
    char-set:lepcha
    char-set:ol-chiki
    char-set:phonetic-extensions
    char-set:phonetic-extensions-supplement
    char-set:combining-diacritical-marks-supplement
    char-set:latin-extended-additional
    char-set:greek-extended
    char-set:general-punctuation
    char-set:superscripts-and-subscripts
    char-set:currency-symbols
    char-set:combining-diacritical-mark-for-symbols
    char-set:letterlike-symbols
    char-set:number-forms
    char-set:arrows
    char-set:mathematical-operators
    char-set:miscellaneous-technical
    char-set:control-pictures
    char-set:optical-character-recognition
    char-set:enclosed-alphanumerics
    char-set:box-drawing
    char-set:block-elements
    char-set:geometric-shapes
    char-set:miscellaneous-symbols
    char-set:dingbats
    char-set:miscellaneous-mathematical-symbols-a
    char-set:supplemental-arrows-a
    char-set:braille-patterns
    char-set:supplemental-arrows-b
    char-set:miscellaneous-mathematical-symbols-b
    char-set:supplemental-mathematical-operators
    char-set:miscellaneous-symbols-and-arrows
    char-set:glagolitic
    char-set:latin-extended-c
    char-set:coptic
    char-set:georgian-supplement
    char-set:tifinagh
    char-set:ethiopic-extended
    char-set:cyrillic-extended-a
    char-set:supplemental-punctuation
    char-set:cjk-radicals-supplement
    char-set:kangxi-radicals
    char-set:ideographic-description-characters
    char-set:cjk-symbols-and-punctuation
    char-set:hiragana
    char-set:katakana
    char-set:bopomofo
    char-set:hangul-compatibility-jamo
    char-set:kanbun
    char-set:bopomofo-extended
    char-set:cjk-strokes
    char-set:katakana-phonetic-extensions
    char-set:enclosed-cjk-letters-and-months
    char-set:cjk-compatibility
    char-set:cjk-unified-Ideographs-extension-a
    char-set:yijing-hexagram-symbols
    char-set:cjk-unified-ideographs
    char-set:yi-syllables
    char-set:yi-radicals
    char-set:vai
    char-set:cyrillic-extended-b
    char-set:modifier-tone-letters
    char-set:latin-extended-d
    char-set:syloti-nagri
    char-set:phags-pa
    char-set:saurashtra
    char-set:kayah-li
    char-set:Rejang
    char-set:cham
    char-set:hangul-syllables
    char-set:private-use-area
    char-set:cjk-compatibility-ideographs
    char-set:alphabetic-presentation-forms
    char-set:arabic-presentation-forms-a
    char-set:variation-selectors
    char-set:vertical-forms
    char-set:combining-half-marks
    char-set:cjk-compatibility-forms
    char-set:small-form-variants
    char-set:arabic-presentation-forms-b
    char-set:halfwidth-and-fullwidth-forms
    char-set:specials
    char-set:linear-b-syllabary
    char-set:linear-b-ideograms
    char-set:aegean-numbers
    char-set:ancient-greek-numbers
    char-set:ancient-symbols
    char-set:phaistos-disc
    char-set:lycian
    char-set:carian
    char-set:old-italic
    char-set:gothic
    char-set:ugaritic
    char-set:old-persian
    char-set:deseret
    char-set:shavian
    char-set:osmanya
    char-set:cypriot-syllabary
    char-set:phoenician
    char-set:lydian
    char-set:kharoshthi
    char-set:cuneiform
    char-set:cuneiform-numbers-and-punctuation
    char-set:byzantine-musical-symbols
    char-set:musical-symbols
    char-set:ancient-greek-musical-notation
    char-set:tai-xuan-jing-symbols
    char-set:counting-rod-numerals
    char-set:mathematical-alphanumeric-symbols
    char-set:mahjong-tiles
    char-set:domino-tiles
    char-set:cjk-unified-ideographs-extension-b
    char-set:cjk-compatibility-ideographs-supplement
    char-set:tags
    char-set:variation-selectors-supplement
    char-set:supplementary-private-use-area-a
    char-set:supplementary-private-use-area-b)
  (import (rnrs)
    (char-sets))



(define char-set:basic-latin				'(#\x0000 . #\x007F))
(define char-set:latin-1-supplement			'(#\x0080 . #\x00FF))
(define char-set:latin-extended-a			'(#\x0100 . #\x017F))
(define char-set:latin-extended-b			'(#\x0180 . #\x024F))
(define char-set:ipa-extensions				'(#\x0250 . #\x02AF))
(define char-set:spacing-modifier-letters		'(#\x02B0 . #\x02FF))
(define char-set:combining-diacritical-marks		'(#\x0300 . #\x036F))
(define char-set:greek-and-coptic			'(#\x0370 . #\x03FF))
(define char-set:cyrillic				'(#\x0400 . #\x04FF))
(define char-set:cyrillic-supplement			'(#\x0500 . #\x052F))
(define char-set:armenian				'(#\x0530 . #\x058F))
(define char-set:hebrew					'(#\x0590 . #\x05FF))
(define char-set:arabic					'(#\x0600 . #\x06FF))
(define char-set:syriac					'(#\x0700 . #\x074F))
(define char-set:arabic-supplement			'(#\x0750 . #\x077F))
(define char-set:thaana					'(#\x0780 . #\x07BF))
(define char-set:nko					'(#\x07C0 . #\x07FF))
(define char-set:devanagari				'(#\x0900 . #\x097F))
(define char-set:bengali				'(#\x0980 . #\x09FF))
(define char-set:gurmukhi				'(#\x0A00 . #\x0A7F))
(define char-set:gujarati				'(#\x0A80 . #\x0AFF))
(define char-set:oriya					'(#\x0B00 . #\x0B7F))
(define char-set:tamil					'(#\x0B80 . #\x0BFF))
(define char-set:telugu					'(#\x0C00 . #\x0C7F))
(define char-set:kannada				'(#\x0C80 . #\x0CFF))
(define char-set:malayalam				'(#\x0D00 . #\x0D7F))
(define char-set:sinhala				'(#\x0D80 . #\x0DFF))
(define char-set:thai					'(#\x0E00 . #\x0E7F))
(define char-set:lao					'(#\x0E80 . #\x0EFF))
(define char-set:tibetan				'(#\x0F00 . #\x0FFF))
(define char-set:myanmar				'(#\x1000 . #\x109F))
(define char-set:georgian				'(#\x10A0 . #\x10FF))
(define char-set:hangul-jamo				'(#\x1100 . #\x11FF))
(define char-set:ethiopic				'(#\x1200 . #\x137F))
(define char-set:ethiopic-supplement			'(#\x1380 . #\x139F))
(define char-set:cherokee				'(#\x13A0 . #\x13FF))
(define char-set:unified-canadian-aboriginal-syllabics '(#\x1400 . #\x167F))
(define char-set:ogham					'(#\x1680 . #\x169F))
(define char-set:runic					'(#\x16A0 . #\x16FF))
(define char-set:tagalog				'(#\x1700 . #\x171F))
(define char-set:hanunoo				'(#\x1720 . #\x173F))
(define char-set:buhid					'(#\x1740 . #\x175F))
(define char-set:tagbanwa				'(#\x1760 . #\x177F))
(define char-set:khmer					'(#\x1780 . #\x17FF))
(define char-set:mongolian				'(#\x1800 . #\x18AF))
(define char-set:limbu					'(#\x1900 . #\x194F))
(define char-set:tai-le					'(#\x1950 . #\x197F))
(define char-set:new-tai-lue				'(#\x1980 . #\x19DF))
(define char-set:khmer-symbols				'(#\x19E0 . #\x19FF))
(define char-set:buginese				'(#\x1A00 . #\x1A1F))
(define char-set:balinese				'(#\x1B00 . #\x1B7F))
(define char-set:sundanese				'(#\x1B80 . #\x1BBF))
(define char-set:lepcha					'(#\x1C00 . #\x1C4F))
(define char-set:ol-chiki				'(#\x1C50 . #\x1C7F))
(define char-set:phonetic-extensions			'(#\x1D00 . #\x1D7F))
(define char-set:phonetic-extensions-supplement		'(#\x1D80 . #\x1DBF))
(define char-set:combining-diacritical-marks-supplement '(#\x1DC0 . #\x1DFF))
(define char-set:latin-extended-additional		'(#\x1E00 . #\x1EFF))
(define char-set:greek-extended				'(#\x1F00 . #\x1FFF))
(define char-set:general-punctuation			'(#\x2000 . #\x206F))
(define char-set:superscripts-and-subscripts		'(#\x2070 . #\x209F))
(define char-set:currency-symbols			'(#\x20A0 . #\x20CF))
(define char-set:combining-diacritical-mark-for-symbols '(#\x20D0 . #\x20FF))
(define char-set:letterlike-symbols			'(#\x2100 . #\x214F))
(define char-set:number-forms				'(#\x2150 . #\x218F))
(define char-set:arrows					'(#\x2190 . #\x21FF))
(define char-set:mathematical-operators			'(#\x2200 . #\x22FF))
(define char-set:miscellaneous-technical		'(#\x2300 . #\x23FF))
(define char-set:control-pictures			'(#\x2400 . #\x243F))
(define char-set:optical-character-recognition		'(#\x2440 . #\x245F))
(define char-set:enclosed-alphanumerics			'(#\x2460 . #\x24FF))
(define char-set:box-drawing				'(#\x2500 . #\x257F))
(define char-set:block-elements				'(#\x2580 . #\x259F))
(define char-set:geometric-shapes			'(#\x25A0 . #\x25FF))
(define char-set:miscellaneous-symbols			'(#\x2600 . #\x26FF))
(define char-set:dingbats				'(#\x2700 . #\x27BF))
(define char-set:miscellaneous-mathematical-symbols-a	'(#\x27C0 . #\x27EF))
(define char-set:supplemental-arrows-a			'(#\x27F0 . #\x27FF))
(define char-set:braille-patterns			'(#\x2800 . #\x28FF))
(define char-set:supplemental-arrows-b			'(#\x2900 . #\x297F))
(define char-set:miscellaneous-mathematical-symbols-b	'(#\x2980 . #\x29FF))
(define char-set:supplemental-mathematical-operators	'(#\x2A00 . #\x2AFF))
(define char-set:miscellaneous-symbols-and-arrows	'(#\x2B00 . #\x2BFF))
(define char-set:glagolitic				'(#\x2C00 . #\x2C5F))
(define char-set:latin-extended-c			'(#\x2C60 . #\x2C7F))
(define char-set:coptic					'(#\x2C80 . #\x2CFF))
(define char-set:georgian-supplement			'(#\x2D00 . #\x2D2F))
(define char-set:tifinagh				'(#\x2D30 . #\x2D7F))
(define char-set:ethiopic-extended			'(#\x2D80 . #\x2DDF))
(define char-set:cyrillic-extended-a			'(#\x2DE0 . #\x2DFF))
(define char-set:supplemental-punctuation		'(#\x2E00 . #\x2E7F))
(define char-set:cjk-radicals-supplement		'(#\x2E80 . #\x2EFF))
(define char-set:kangxi-radicals			'(#\x2F00 . #\x2FDF))
(define char-set:ideographic-description-characters	'(#\x2FF0 . #\x2FFF))
(define char-set:cjk-symbols-and-punctuation		'(#\x3000 . #\x303F))
(define char-set:hiragana				'(#\x3040 . #\x309F))
(define char-set:katakana				'(#\x30A0 . #\x30FF))
(define char-set:bopomofo				'(#\x3100 . #\x312F))
(define char-set:hangul-compatibility-jamo		'(#\x3130 . #\x318F))
(define char-set:kanbun					'(#\x3190 . #\x319F))
(define char-set:bopomofo-extended			'(#\x31A0 . #\x31BF))
(define char-set:cjk-strokes				'(#\x31C0 . #\x31EF))
(define char-set:katakana-phonetic-extensions		'(#\x31F0 . #\x31FF))
(define char-set:enclosed-cjk-letters-and-months	'(#\x3200 . #\x32FF))
(define char-set:cjk-compatibility			'(#\x3300 . #\x33FF))
(define char-set:cjk-unified-Ideographs-extension-a	'(#\x3400 . #\x4DBF))
(define char-set:yijing-hexagram-symbols		'(#\x4DC0 . #\x4DFF))
(define char-set:cjk-unified-ideographs			'(#\x4E00 . #\x9FFF))
(define char-set:yi-syllables				'(#\xA000 . #\xA48F))
(define char-set:yi-radicals				'(#\xA490 . #\xA4CF))
(define char-set:vai					'(#\xA500 . #\xA63F))
(define char-set:cyrillic-extended-b			'(#\xA640 . #\xA69F))
(define char-set:modifier-tone-letters			'(#\xA700 . #\xA71F))
(define char-set:latin-extended-d			'(#\xA720 . #\xA7FF))
(define char-set:syloti-nagri				'(#\xA800 . #\xA82F))
(define char-set:phags-pa				'(#\xA840 . #\xA87F))
(define char-set:saurashtra				'(#\xA880 . #\xA8DF))
(define char-set:kayah-li				'(#\xA900 . #\xA92F))
(define char-set:Rejang					'(#\xA930 . #\xA95F))
(define char-set:cham					'(#\xAA00 . #\xAA5F))
(define char-set:hangul-syllables			'(#\xAC00 . #\xD7AF))
;;;These are excluded from the definition of characters in R6RS.
;;;(define char-set:high-surrogates			'(#\xD800 . #\xDB7F))
;;;(define char-set:high-private-use-surrogates		'(#\xDB80 . #\xDBFF))
;;;(define char-set:low-surrogates			'(#\xDC00 . #\xDFFF))
(define char-set:private-use-area			'(#\xE000 . #\xF8FF))
(define char-set:cjk-compatibility-ideographs		'(#\xF900 . #\xFAFF))
(define char-set:alphabetic-presentation-forms		'(#\xFB00 . #\xFB4F))
(define char-set:arabic-presentation-forms-a		'(#\xFB50 . #\xFDFF))
(define char-set:variation-selectors			'(#\xFE00 . #\xFE0F))
(define char-set:vertical-forms				'(#\xFE10 . #\xFE1F))
(define char-set:combining-half-marks			'(#\xFE20 . #\xFE2F))
(define char-set:cjk-compatibility-forms		'(#\xFE30 . #\xFE4F))
(define char-set:small-form-variants			'(#\xFE50 . #\xFE6F))
(define char-set:arabic-presentation-forms-b		'(#\xFE70 . #\xFEFF))
(define char-set:halfwidth-and-fullwidth-forms		'(#\xFF00 . #\xFFEF))
(define char-set:specials				'(#\xFFF0 . #\xFFFF))
(define char-set:linear-b-syllabary			'(#\x10000 . #\x1007F))
(define char-set:linear-b-ideograms			'(#\x10080 . #\x100FF))
(define char-set:aegean-numbers				'(#\x10100 . #\x1013F))
(define char-set:ancient-greek-numbers			'(#\x10140 . #\x1018F))
(define char-set:ancient-symbols			'(#\x10190 . #\x101CF))
(define char-set:phaistos-disc				'(#\x101D0 . #\x101FF))
(define char-set:lycian					'(#\x10280 . #\x1029F))
(define char-set:carian					'(#\x102A0 . #\x102DF))
(define char-set:old-italic				'(#\x10300 . #\x1032F))
(define char-set:gothic					'(#\x10330 . #\x1034F))
(define char-set:ugaritic				'(#\x10380 . #\x1039F))
(define char-set:old-persian				'(#\x103A0 . #\x103DF))
(define char-set:deseret				'(#\x10400 . #\x1044F))
(define char-set:shavian				'(#\x10450 . #\x1047F))
(define char-set:osmanya				'(#\x10480 . #\x104AF))
(define char-set:cypriot-syllabary			'(#\x10800 . #\x1083F))
(define char-set:phoenician				'(#\x10900 . #\x1091F))
(define char-set:lydian					'(#\x10920 . #\x1093F))
(define char-set:kharoshthi				'(#\x10A00 . #\x10A5F))
(define char-set:cuneiform				'(#\x12000 . #\x123FF))
(define char-set:cuneiform-numbers-and-punctuation	'(#\x12400 . #\x1247F))
(define char-set:byzantine-musical-symbols		'(#\x1D000 . #\x1D0FF))
(define char-set:musical-symbols			'(#\x1D100 . #\x1D1FF))
(define char-set:ancient-greek-musical-notation		'(#\x1D200 . #\x1D24F))
(define char-set:tai-xuan-jing-symbols			'(#\x1D300 . #\x1D35F))
(define char-set:counting-rod-numerals			'(#\x1D360 . #\x1D37F))
(define char-set:mathematical-alphanumeric-symbols	'(#\x1D400 . #\x1D7FF))
(define char-set:mahjong-tiles				'(#\x1F000 . #\x1F02F))
(define char-set:domino-tiles				'(#\x1F030 . #\x1F09F))
(define char-set:cjk-unified-ideographs-extension-b	'(#\x20000 . #\x2A6DF))
(define char-set:cjk-compatibility-ideographs-supplement '(#\x2F800 . #\x2FA1F))
(define char-set:tags					'(#\xE0000 . #\xE007F))
(define char-set:variation-selectors-supplement		'(#\xE0100 . #\xE01EF))
(define char-set:supplementary-private-use-area-a	'(#\xF0000 . #\xFFFFF))
(define char-set:supplementary-private-use-area-b	'(#\x100000 . #\x10FFFF))


;;; done

)

;;; end of file
