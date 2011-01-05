;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for R6RS source code
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

OPAREN			\(
CPAREN			\)
OBRACKET		\[
CBRACKET		\]
TICK			'
BACKTICK		`
COMMA			,
COMMAAT			,@
DOT			\.
DOUBLEQUOTE		\"
SEMICOLON		\;
SHARP			#
SHARPPAREN		#\(
SHARPVU8PAREN		#vu8\(
SHARPTICK		#'
SHARPBACKTICK		#`
SHARPCOMMA		#,
SHARPCOMMAAT		#,@
SHARPSEMICOLON		#\;

;;<whitespace> is one among:
;;
;;		\9	char tabulation
;;		\10	linefeed
;;		\11	line tabulation
;;		\12	form feed
;;		\13	carriage return
;;		\133	next line
;;
;;and  any char  whose  Unicode category  is  Zs, Zl  or  Zp; these  are
;;(http://www.fileformat.info/info/unicode/category/index.htm):
;;
;;Zs		#x0020		\32
;;		#x00A0		\160
;;		#x1680		\5760
;;		#x180E		\6158
;;		#x2000		\8192
;;		#x2001		\8193
;;		#x2002		\8194
;;		#x2003		\8195
;;		#x2004		\8196
;;		#x2005		\8197
;;		#x2006		\8198
;;		#x2007		\8199
;;		#x2008		\8200
;;		#x2009		\8201
;;		#x200A		\8202
;;		#x202F		\8239
;;		#x205F		\8287
;;		#x3000		\12288
;;Zl		#x2028		\8232
;;Zp		#x2029		\8233
;;
WHITESPACE		[\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233]+

;;<line ending> is:
;;
;;		\10		line feed
;;		\13		carriage return
;;		\133		next line
;;		\10\13		carriage return, line feed
;;		\13\133		carriage return, next line
;;		\8232		line separator
;;
LINEENDING		[\10\13\133\8232]|(\13\10)|(\13\133)

WHITESPACE-CHARS	\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233
DELIMITER		{OPAREN}{CPAREN}{OBRACKET}{CBRACKET}{DOUBLEQUOTE}{SEMICOLON}{SHARP}{WHITESPACE-CHARS}

;;When a valid lexeme is followed by this tail it is wrong.  This is the
;;negation of the <delimiter> element.
TAIL_OF_WRONG		[^\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233\(\)\[\]\"\;#]+

;;page
;;;; comments

;;A simple comment is everything between a semicolon and a LINEENDING or
;;a paragraph separator (#x2029 = \8233).
;;
LINECOMMENT		\;.*({LINEENDING}|\8233)
LINECOMMENT-NOEND	\;[^\10\13\133\8232\8233]*

;;Multiline comments can contain nested multiline comments, so we need a
;;sublexer.
;;
ONESTEDCOMMENT		#\|

;;page
;;;; special silex regex macros needed for identifiers

;;Characters with Unicode category Nd, Mc, Me:
;;(http://www.fileformat.info/info/unicode/category/index.htm)
;;
;; Nd:		#x0030		\48		#x0031		\49
;;		#x0032		\50		#x0033		\51
;;		#x0034		\52		#x0035		\53
;;		#x0036		\54		#x0037		\55
;;		#x0038		\56		#x0039		\57
;;		#x0660		\1632		#x0661		\1633
;;		#x0662		\1634		#x0663		\1635
;;		#x0664		\1636		#x0665		\1637
;;		#x0666		\1638		#x0667		\1639
;;		#x0668		\1640		#x0669		\1641
;;		#x06F0		\1776		#x06F1		\1777
;;		#x06F2		\1778		#x06F3		\1779
;;		#x06F4		\1780		#x06F5		\1781
;;		#x06F6		\1782		#x06F7		\1783
;;		#x06F8		\1784		#x06F9		\1785
;;		#x07C0		\1984		#x07C1		\1985
;;		#x07C2		\1986		#x07C3		\1987
;;		#x07C4		\1988		#x07C5		\1989
;;		#x07C6		\1990		#x07C7		\1991
;;		#x07C8		\1992		#x07C9		\1993
;;		#x0966		\2406		#x0967		\2407
;;		#x0968		\2408		#x0969		\2409
;;		#x096A		\2410		#x096B		\2411
;;		#x096C		\2412		#x096D		\2413
;;		#x096E		\2414		#x096F		\2415
;;		#x09E6		\2534		#x09E7		\2535
;;		#x09E8		\2536		#x09E9		\2537
;;		#x09EA		\2538		#x09EB		\2539
;;		#x09EC		\2540		#x09ED		\2541
;;		#x09EE		\2542		#x09EF		\2543
;;		#x0A66		\2662		#x0A67		\2663
;;		#x0A68		\2664		#x0A69		\2665
;;		#x0A6A		\2666		#x0A6B		\2667
;;		#x0A6C		\2668		#x0A6D		\2669
;;		#x0A6E		\2670		#x0A6F		\2671
;;		#x0AE6		\2790		#x0AE7		\2791
;;		#x0AE8		\2792		#x0AE9		\2793
;;		#x0AEA		\2794		#x0AEB		\2795
;;		#x0AEC		\2796		#x0AED		\2797
;;		#x0AEE		\2798		#x0AEF		\2799
;;		#x0B66		\2918		#x0B67		\2919
;;		#x0B68		\2920		#x0B69		\2921
;;		#x0B6A		\2922		#x0B6B		\2923
;;		#x0B6C		\2924		#x0B6D		\2925
;;		#x0B6E		\2926		#x0B6F		\2927
;;		#x0BE6		\3046		#x0BE7		\3047
;;		#x0BE8		\3048		#x0BE9		\3049
;;		#x0BEA		\3050		#x0BEB		\3051
;;		#x0BEC		\3052		#x0BED		\3053
;;		#x0BEE		\3054		#x0BEF		\3055
;;		#x0C66		\3174		#x0C67		\3175
;;		#x0C68		\3176		#x0C69		\3177
;;		#x0C6A		\3178		#x0C6B		\3179
;;		#x0C6C		\3180		#x0C6D		\3181
;;		#x0C6E		\3182		#x0C6F		\3183
;;		#x0CE6		\3302		#x0CE7		\3303
;;		#x0CE8		\3304		#x0CE9		\3305
;;		#x0CEA		\3306		#x0CEB		\3307
;;		#x0CEC		\3308		#x0CED		\3309
;;		#x0CEE		\3310		#x0CEF		\3311
;;		#x0D66		\3430		#x0D67		\3431
;;		#x0D68		\3432		#x0D69		\3433
;;		#x0D6A		\3434		#x0D6B		\3435
;;		#x0D6C		\3436		#x0D6D		\3437
;;		#x0D6E		\3438		#x0D6F		\3439
;;		#x0E50		\3664		#x0E51		\3665
;;		#x0E52		\3666		#x0E53		\3667
;;		#x0E54		\3668		#x0E55		\3669
;;		#x0E56		\3670		#x0E57		\3671
;;		#x0E58		\3672		#x0E59		\3673
;;		#x0ED0		\3792		#x0ED1		\3793
;;		#x0ED2		\3794		#x0ED3		\3795
;;		#x0ED4		\3796		#x0ED5		\3797
;;		#x0ED6		\3798		#x0ED7		\3799
;;		#x0ED8		\3800		#x0ED9		\3801
;;		#x0F20		\3872		#x0F21		\3873
;;		#x0F22		\3874		#x0F23		\3875
;;		#x0F24		\3876		#x0F25		\3877
;;		#x0F26		\3878		#x0F27		\3879
;;		#x0F28		\3880		#x0F29		\3881
;;		#x1040		\4160		#x1041		\4161
;;		#x1042		\4162		#x1043		\4163
;;		#x1044		\4164		#x1045		\4165
;;		#x1046		\4166		#x1047		\4167
;;		#x1048		\4168		#x1049		\4169
;;		#x1090		\4240		#x1091		\4241
;;		#x1092		\4242		#x1093		\4243
;;		#x1094		\4244		#x1095		\4245
;;		#x1096		\4246		#x1097		\4247
;;		#x1098		\4248		#x1099		\4249
;;		#x17E0		\6112		#x17E1		\6113
;;		#x17E2		\6114		#x17E3		\6115
;;		#x17E4		\6116		#x17E5		\6117
;;		#x17E6		\6118		#x17E7		\6119
;;		#x17E8		\6120		#x17E9		\6121
;;		#x1810		\6160		#x1811		\6161
;;		#x1812		\6162		#x1813		\6163
;;		#x1814		\6164		#x1815		\6165
;;		#x1816		\6166		#x1817		\6167
;;		#x1818		\6168		#x1819		\6169
;;		#x1946		\6470		#x1947		\6471
;;		#x1948		\6472		#x1949		\6473
;;		#x194A		\6474		#x194B		\6475
;;		#x194C		\6476		#x194D		\6477
;;		#x194E		\6478		#x194F		\6479
;;		#x19D0		\6608		#x19D1		\6609
;;		#x19D2		\6610		#x19D3		\6611
;;		#x19D4		\6612		#x19D5		\6613
;;		#x19D6		\6614		#x19D7		\6615
;;		#x19D8		\6616		#x19D9		\6617
;;		#x1A80		\6784		#x1A81		\6785
;;		#x1A82		\6786		#x1A83		\6787
;;		#x1A84		\6788		#x1A85		\6789
;;		#x1A86		\6790		#x1A87		\6791
;;		#x1A88		\6792		#x1A89		\6793
;;		#x1A90		\6800		#x1A91		\6801
;;		#x1A92		\6802		#x1A93		\6803
;;		#x1A94		\6804		#x1A95		\6805
;;		#x1A96		\6806		#x1A97		\6807
;;		#x1A98		\6808		#x1A99		\6809
;;		#x1B50		\6992		#x1B51		\6993
;;		#x1B52		\6994		#x1B53		\6995
;;		#x1B54		\6996		#x1B55		\6997
;;		#x1B56		\6998		#x1B57		\6999
;;		#x1B58		\7000		#x1B59		\7001
;;		#x1BB0		\7088		#x1BB1		\7089
;;		#x1BB2		\7090		#x1BB3		\7091
;;		#x1BB4		\7092		#x1BB5		\7093
;;		#x1BB6		\7094		#x1BB7		\7095
;;		#x1BB8		\7096		#x1BB9		\7097
;;		#x1C40		\7232		#x1C41		\7233
;;		#x1C42		\7234		#x1C43		\7235
;;		#x1C44		\7236		#x1C45		\7237
;;		#x1C46		\7238		#x1C47		\7239
;;		#x1C48		\7240		#x1C49		\7241
;;		#x1C50		\7248		#x1C51		\7249
;;		#x1C52		\7250		#x1C53		\7251
;;		#x1C54		\7252		#x1C55		\7253
;;		#x1C56		\7254		#x1C57		\7255
;;		#x1C58		\7256		#x1C59		\7257
;;		#xA620		\42528		#xA621		\42529
;;		#xA622		\42530		#xA623		\42531
;;		#xA624		\42532		#xA625		\42533
;;		#xA626		\42534		#xA627		\42535
;;		#xA628		\42536		#xA629		\42537
;;		#xA8D0		\43216		#xA8D1		\43217
;;		#xA8D2		\43218		#xA8D3		\43219
;;		#xA8D4		\43220		#xA8D5		\43221
;;		#xA8D6		\43222		#xA8D7		\43223
;;		#xA8D8		\43224		#xA8D9		\43225
;;		#xA900		\43264		#xA901		\43265
;;		#xA902		\43266		#xA903		\43267
;;		#xA904		\43268		#xA905		\43269
;;		#xA906		\43270		#xA907		\43271
;;		#xA908		\43272		#xA909		\43273
;;		#xA9D0		\43472		#xA9D1		\43473
;;		#xA9D2		\43474		#xA9D3		\43475
;;		#xA9D4		\43476		#xA9D5		\43477
;;		#xA9D6		\43478		#xA9D7		\43479
;;		#xA9D8		\43480		#xA9D9		\43481
;;		#xAA50		\43600		#xAA51		\43601
;;		#xAA52		\43602		#xAA53		\43603
;;		#xAA54		\43604		#xAA55		\43605
;;		#xAA56		\43606		#xAA57		\43607
;;		#xAA58		\43608		#xAA59		\43609
;;		#xABF0		\44016		#xABF1		\44017
;;		#xABF2		\44018		#xABF3		\44019
;;		#xABF4		\44020		#xABF5		\44021
;;		#xABF6		\44022		#xABF7		\44023
;;		#xABF8		\44024		#xABF9		\44025
;;		#xFF10		\65296		#xFF11		\65297
;;		#xFF12		\65298		#xFF13		\65299
;;		#xFF14		\65300		#xFF15		\65301
;;		#xFF16		\65302		#xFF17		\65303
;;		#xFF18		\65304		#xFF19		\65305
;;		#x104A0		\66720		#x104A1		\66721
;;		#x104A2		\66722		#x104A3		\66723
;;		#x104A4		\66724		#x104A5		\66725
;;		#x104A6		\66726		#x104A7		\66727
;;		#x104A8		\66728		#x104A9		\66729
;;		#x11066		\69734		#x11067		\69735
;;		#x11068		\69736		#x11069		\69737
;;		#x1106A		\69738		#x1106B		\69739
;;		#x1106C		\69740		#x1106D		\69741
;;		#x1106E		\69742		#x1106F		\69743
;;		#x1D7CE		\120782		#x1D7CF		\120783
;;		#x1D7D0		\120784		#x1D7D1		\120785
;;		#x1D7D2		\120786		#x1D7D3		\120787
;;		#x1D7D4		\120788		#x1D7D5		\120789
;;		#x1D7D6		\120790		#x1D7D7		\120791
;;		#x1D7D8		\120792		#x1D7D9		\120793
;;		#x1D7DA		\120794		#x1D7DB		\120795
;;		#x1D7DC		\120796		#x1D7DD		\120797
;;		#x1D7DE		\120798		#x1D7DF		\120799
;;		#x1D7E0		\120800		#x1D7E1		\120801
;;		#x1D7E2		\120802		#x1D7E3		\120803
;;		#x1D7E4		\120804		#x1D7E5		\120805
;;		#x1D7E6		\120806		#x1D7E7		\120807
;;		#x1D7E8		\120808		#x1D7E9		\120809
;;		#x1D7EA		\120810		#x1D7EB		\120811
;;		#x1D7EC		\120812		#x1D7ED		\120813
;;		#x1D7EE		\120814		#x1D7EF		\120815
;;		#x1D7F0		\120816		#x1D7F1		\120817
;;		#x1D7F2		\120818		#x1D7F3		\120819
;;		#x1D7F4		\120820		#x1D7F5		\120821
;;		#x1D7F6		\120822		#x1D7F7		\120823
;;		#x1D7F8		\120824		#x1D7F9		\120825
;;		#x1D7FA		\120826		#x1D7FB		\120827
;;		#x1D7FC		\120828		#x1D7FD		\120829
;;		#x1D7FE		\120830		#x1D7FF		\120831
;;
;; Mc:		#x0903		\2307		#x093B		\2363
;;		#x093E		\2366		#x093F		\2367
;;		#x0940		\2368		#x0949		\2377
;;		#x094A		\2378		#x094B		\2379
;;		#x094C		\2380		#x094E		\2382
;;		#x094F		\2383		#x0982		\2434
;;		#x0983		\2435		#x09BE		\2494
;;		#x09BF		\2495		#x09C0		\2496
;;		#x09C7		\2503		#x09C8		\2504
;;		#x09CB		\2507		#x09CC		\2508
;;		#x09D7		\2519		#x0A03		\2563
;;		#x0A3E		\2622		#x0A3F		\2623
;;		#x0A40		\2624		#x0A83		\2691
;;		#x0ABE		\2750		#x0ABF		\2751
;;		#x0AC0		\2752		#x0AC9		\2761
;;		#x0ACB		\2763		#x0ACC		\2764
;;		#x0B02		\2818		#x0B03		\2819
;;		#x0B3E		\2878		#x0B40		\2880
;;		#x0B47		\2887		#x0B48		\2888
;;		#x0B4B		\2891		#x0B4C		\2892
;;		#x0B57		\2903		#x0BBE		\3006
;;		#x0BBF		\3007		#x0BC1		\3009
;;		#x0BC2		\3010		#x0BC6		\3014
;;		#x0BC7		\3015		#x0BC8		\3016
;;		#x0BCA		\3018		#x0BCB		\3019
;;		#x0BCC		\3020		#x0BD7		\3031
;;		#x0C01		\3073		#x0C02		\3074
;;		#x0C03		\3075		#x0C41		\3137
;;		#x0C42		\3138		#x0C43		\3139
;;		#x0C44		\3140		#x0C82		\3202
;;		#x0C83		\3203		#x0CBE		\3262
;;		#x0CC0		\3264		#x0CC1		\3265
;;		#x0CC2		\3266		#x0CC3		\3267
;;		#x0CC4		\3268		#x0CC7		\3271
;;		#x0CC8		\3272		#x0CCA		\3274
;;		#x0CCB		\3275		#x0CD5		\3285
;;		#x0CD6		\3286		#x0D02		\3330
;;		#x0D03		\3331		#x0D3E		\3390
;;		#x0D3F		\3391		#x0D40		\3392
;;		#x0D46		\3398		#x0D47		\3399
;;		#x0D48		\3400		#x0D4A		\3402
;;		#x0D4B		\3403		#x0D4C		\3404
;;		#x0D57		\3415		#x0D82		\3458
;;		#x0D83		\3459		#x0DCF		\3535
;;		#x0DD0		\3536		#x0DD1		\3537
;;		#x0DD8		\3544		#x0DD9		\3545
;;		#x0DDA		\3546		#x0DDB		\3547
;;		#x0DDC		\3548		#x0DDD		\3549
;;		#x0DDE		\3550		#x0DDF		\3551
;;		#x0DF2		\3570		#x0DF3		\3571
;;		#x0F3E		\3902		#x0F3F		\3903
;;		#x0F7F		\3967		#x102B		\4139
;;		#x102C		\4140		#x1031		\4145
;;		#x1038		\4152		#x103B		\4155
;;		#x103C		\4156		#x1056		\4182
;;		#x1057		\4183		#x1062		\4194
;;		#x1063		\4195		#x1064		\4196
;;		#x1067		\4199		#x1068		\4200
;;		#x1069		\4201		#x106A		\4202
;;		#x106B		\4203		#x106C		\4204
;;		#x106D		\4205		#x1083		\4227
;;		#x1084		\4228		#x1087		\4231
;;		#x1088		\4232		#x1089		\4233
;;		#x108A		\4234		#x108B		\4235
;;		#x108C		\4236		#x108F		\4239
;;		#x109A		\4250		#x109B		\4251
;;		#x109C		\4252		#x17B6		\6070
;;		#x17BE		\6078		#x17BF		\6079
;;		#x17C0		\6080		#x17C1		\6081
;;		#x17C2		\6082		#x17C3		\6083
;;		#x17C4		\6084		#x17C5		\6085
;;		#x17C7		\6087		#x17C8		\6088
;;		#x1923		\6435		#x1924		\6436
;;		#x1925		\6437		#x1926		\6438
;;		#x1929		\6441		#x192A		\6442
;;		#x192B		\6443		#x1930		\6448
;;		#x1931		\6449		#x1933		\6451
;;		#x1934		\6452		#x1935		\6453
;;		#x1936		\6454		#x1937		\6455
;;		#x1938		\6456		#x19B0		\6576
;;		#x19B1		\6577		#x19B2		\6578
;;		#x19B3		\6579		#x19B4		\6580
;;		#x19B5		\6581		#x19B6		\6582
;;		#x19B7		\6583		#x19B8		\6584
;;		#x19B9		\6585		#x19BA		\6586
;;		#x19BB		\6587		#x19BC		\6588
;;		#x19BD		\6589		#x19BE		\6590
;;		#x19BF		\6591		#x19C0		\6592
;;		#x19C8		\6600		#x19C9		\6601
;;		#x1A19		\6681		#x1A1A		\6682
;;		#x1A1B		\6683		#x1A55		\6741
;;		#x1A57		\6743		#x1A61		\6753
;;		#x1A63		\6755		#x1A64		\6756
;;		#x1A6D		\6765		#x1A6E		\6766
;;		#x1A6F		\6767		#x1A70		\6768
;;		#x1A71		\6769		#x1A72		\6770
;;		#x1B04		\6916		#x1B35		\6965
;;		#x1B3B		\6971		#x1B3D		\6973
;;		#x1B3E		\6974		#x1B3F		\6975
;;		#x1B40		\6976		#x1B41		\6977
;;		#x1B43		\6979		#x1B44		\6980
;;		#x1B82		\7042		#x1BA1		\7073
;;		#x1BA6		\7078		#x1BA7		\7079
;;		#x1BAA		\7082		#x1BE7		\7143
;;		#x1BEA		\7146		#x1BEB		\7147
;;		#x1BEC		\7148		#x1BEE		\7150
;;		#x1BF2		\7154		#x1BF3		\7155
;;		#x1C24		\7204		#x1C25		\7205
;;		#x1C26		\7206		#x1C27		\7207
;;		#x1C28		\7208		#x1C29		\7209
;;		#x1C2A		\7210		#x1C2B		\7211
;;		#x1C34		\7220		#x1C35		\7221
;;		#x1CE1		\7393		#x1CF2		\7410
;;		#xA823		\43043		#xA824		\43044
;;		#xA827		\43047		#xA880		\43136
;;		#xA881		\43137		#xA8B4		\43188
;;		#xA8B5		\43189		#xA8B6		\43190
;;		#xA8B7		\43191		#xA8B8		\43192
;;		#xA8B9		\43193		#xA8BA		\43194
;;		#xA8BB		\43195		#xA8BC		\43196
;;		#xA8BD		\43197		#xA8BE		\43198
;;		#xA8BF		\43199		#xA8C0		\43200
;;		#xA8C1		\43201		#xA8C2		\43202
;;		#xA8C3		\43203		#xA952		\43346
;;		#xA953		\43347		#xA983		\43395
;;		#xA9B4		\43444		#xA9B5		\43445
;;		#xA9BA		\43450		#xA9BB		\43451
;;		#xA9BD		\43453		#xA9BE		\43454
;;		#xA9BF		\43455		#xA9C0		\43456
;;		#xAA2F		\43567		#xAA30		\43568
;;		#xAA33		\43571		#xAA34		\43572
;;		#xAA4D		\43597		#xAA7B		\43643
;;		#xABE3		\44003		#xABE4		\44004
;;		#xABE6		\44006		#xABE7		\44007
;;		#xABE9		\44009		#xABEA		\44010
;;		#xABEC		\44012		#x11000		\69632
;;		#x11002		\69634		#x11082		\69762
;;		#x110B0		\69808		#x110B1		\69809
;;		#x110B2		\69810		#x110B7		\69815
;;		#x110B8		\69816		#x1D165		\119141
;;		#x1D166		\119142		#x1D16D		\119149
;;		#x1D16E		\119150		#x1D16F		\119151
;;		#x1D170		\119152		#x1D171		\119153
;;		#x1D172		\119154
;;
;; Me:		#x0488		\1160		#x0489		\1161
;;		#x20DD		\8413		#x20DE		\8414
;;		#x20DF		\8415		#x20E0		\8416
;;		#x20E2		\8418		#x20E3		\8419
;;		#x20E4		\8420		#xA670		\42608
;;		#xA671		\42609		#xA672		\42610
;;
CHARS-ND	[\48\49\50\51\52\53\54\55\56\57\1632\1633\1634\1635\1636\1637\1638\1639\1640\1641\1776\1777\1778\1779\1780\1781\1782\1783\1784\1785\1984\1985\1986\1987\1988\1989\1990\1991\1992\1993\2406\2407\2408\2409\2410\2411\2412\2413\2414\2415\2534\2535\2536\2537\2538\2539\2540\2541\2542\2543\2662\2663\2664\2665\2666\2667\2668\2669\2670\2671\2790\2791\2792\2793\2794\2795\2796\2797\2798\2799\2918\2919\2920\2921\2922\2923\2924\2925\2926\2927\3046\3047\3048\3049\3050\3051\3052\3053\3054\3055\3174\3175\3176\3177\3178\3179\3180\3181\3182\3183\3302\3303\3304\3305\3306\3307\3308\3309\3310\3311\3430\3431\3432\3433\3434\3435\3436\3437\3438\3439\3664\3665\3666\3667\3668\3669\3670\3671\3672\3673\3792\3793\3794\3795\3796\3797\3798\3799\3800\3801\3872\3873\3874\3875\3876\3877\3878\3879\3880\3881\4160\4161\4162\4163\4164\4165\4166\4167\4168\4169\4240\4241\4242\4243\4244\4245\4246\4247\4248\4249\6112\6113\6114\6115\6116\6117\6118\6119\6120\6121\6160\6161\6162\6163\6164\6165\6166\6167\6168\6169\6470\6471\6472\6473\6474\6475\6476\6477\6478\6479\6608\6609\6610\6611\6612\6613\6614\6615\6616\6617\6784\6785\6786\6787\6788\6789\6790\6791\6792\6793\6800\6801\6802\6803\6804\6805\6806\6807\6808\6809\6992\6993\6994\6995\6996\6997\6998\6999\7000\7001\7088\7089\7090\7091\7092\7093\7094\7095\7096\7097\7232\7233\7234\7235\7236\7237\7238\7239\7240\7241\7248\7249\7250\7251\7252\7253\7254\7255\7256\7257\42528\42529\42530\42531\42532\42533\42534\42535\42536\42537\43216\43217\43218\43219\43220\43221\43222\43223\43224\43225\43264\43265\43266\43267\43268\43269\43270\43271\43272\43273\43472\43473\43474\43475\43476\43477\43478\43479\43480\43481\43600\43601\43602\43603\43604\43605\43606\43607\43608\43609\44016\44017\44018\44019\44020\44021\44022\44023\44024\44025\65296\65297\65298\65299\65300\65301\65302\65303\65304\65305\66720\66721\66722\66723\66724\66725\66726\66727\66728\66729\69734\69735\69736\69737\69738\69739\69740\69741\69742\69743\120782\120783\120784\120785\120786\120787\120788\120789\120790\120791\120792\120793\120794\120795\120796\120797\120798\120799\120800\120801\120802\120803\120804\120805\120806\120807\120808\120809\120810\120811\120812\120813\120814\120815\120816\120817\120818\120819\120820\120821\120822\120823\120824\120825\120826\120827\120828\120829\120830\120831]

CHARS-MC	[\2307\2363\2366\2367\2368\2377\2378\2379\2380\2382\2383\2434\2435\2494\2495\2496\2503\2504\2507\2508\2519\2563\2622\2623\2624\2691\2750\2751\2752\2761\2763\2764\2818\2819\2878\2880\2887\2888\2891\2892\2903\3006\3007\3009\3010\3014\3015\3016\3018\3019\3020\3031\3073\3074\3075\3137\3138\3139\3140\3202\3203\3262\3264\3265\3266\3267\3268\3271\3272\3274\3275\3285\3286\3330\3331\3390\3391\3392\3398\3399\3400\3402\3403\3404\3415\3458\3459\3535\3536\3537\3544\3545\3546\3547\3548\3549\3550\3551\3570\3571\3902\3903\3967\4139\4140\4145\4152\4155\4156\4182\4183\4194\4195\4196\4199\4200\4201\4202\4203\4204\4205\4227\4228\4231\4232\4233\4234\4235\4236\4239\4250\4251\4252\6070\6078\6079\6080\6081\6082\6083\6084\6085\6087\6088\6435\6436\6437\6438\6441\6442\6443\6448\6449\6451\6452\6453\6454\6455\6456\6576\6577\6578\6579\6580\6581\6582\6583\6584\6585\6586\6587\6588\6589\6590\6591\6592\6600\6601\6681\6682\6683\6741\6743\6753\6755\6756\6765\6766\6767\6768\6769\6770\6916\6965\6971\6973\6974\6975\6976\6977\6979\6980\7042\7073\7078\7079\7082\7143\7146\7147\7148\7150\7154\7155\7204\7205\7206\7207\7208\7209\7210\7211\7220\7221\7393\7410\43043\43044\43047\43136\43137\43188\43189\43190\43191\43192\43193\43194\43195\43196\43197\43198\43199\43200\43201\43202\43203\43346\43347\43395\43444\43445\43450\43451\43453\43454\43455\43456\43567\43568\43571\43572\43597\43643\44003\44004\44006\44007\44009\44010\44012\69632\69634\69762\69808\69809\69810\69815\69816\119141\119142\119149\119150\119151\119152\119153\119154]

CHARS-ME	[\1160\1161\8413\8414\8415\8416\8418\8419\8420\42608\42609\42610]
CHARS-ND-MC-ME	{CHARS-ND}|{CHARS-MC}|{CHARS-ME}

;;page
;;;; identifiers

LETTER			[a-zA-Z]
CONSTITUENT		{LETTER}|[\127-\55295]|[\57344-\1114111]
SPECIAL_INITIAL		[!\$%&\*/:<=>\?\^_~]
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
SPECIAL_SUBSEQUENT	[\+\-\.@]
HEX_SCALAR_VALUE	{HEXDIGIT}+
INLINE_HEX_ESCAPE	\\x{HEX_SCALAR_VALUE}\;
INITIAL			{CONSTITUENT}|{SPECIAL_INITIAL}|{INLINE_HEX_ESCAPE}
SUBSEQUENT		{INITIAL}|{DIGIT}|{CHARS-ND-MC-ME}|{SPECIAL_SUBSEQUENT}
IDENTIFIER		{INITIAL}{SUBSEQUENT}*
PECULIAR_IDENTIFIER	[\+\-]|"..."|(\->{SUBSEQUENT}*)

WRONG_IDENTIFIER		{IDENTIFIER}{TAIL_OF_WRONG}

;;We do not include the peculiar  identifiers "+" and "-" here, else the
;;numbers  starting  with  +  and   -  would  be  interpreted  as  wrong
;;identifiers.
;;
WRONG_PECULIAR_IDENTIFIER	("..."|(\->{SUBSEQUENT}*)){TAIL_OF_WRONG}

;;page
;;;; characters

SHARPBACKSLASH		#\\
CHARACTER_NAME		nul|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete
NAMED_CHARACTER		{SHARPBACKSLASH}{CHARACTER_NAME}
HEX_CHARACTER		{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}
LITERAL_CHARACTER	{SHARPBACKSLASH}.

WRONG_NAMED_CHARACTER	{NAMED_CHARACTER}{TAIL_OF_WRONG}
WRONG_HEX_CHARACTER	{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}{TAIL_OF_WRONG}
WRONG_LITERAL_CHARACTER	{SHARPBACKSLASH}.{TAIL_OF_WRONG}

;;page
;;;; numbers

NANINF			"nan.0"|"inf.0"
IMAG			i
DIGIT2			[01]
DIGIT8			[01234567]
DIGIT10			{DIGIT}
DIGIT16			{HEXDIGIT}
RADIX2			(#b)|(#B)
RADIX8			(#o)|(#O)
RADIX10			((#d)|(#D))?
RADIX16			(#x)|(#X)
EXACTNESS		((#i)|(#I)|(#e)|(#E))?
EXPONENT-MARKER		e|E|s|S|f|F|d|D|l|L
SIGN			(\+|\-)?
SUFFIX			({EXPONENT-MARKER}{SIGN}{DIGIT10}+)?
MANTISSA-WIDTH		(\|{DIGIT10}+)?

;;; --------------------------------------------------------------------

UINTEGER2		{DIGIT2}+
UREAL2			{UINTEGER2}|({UINTEGER2}/{UINTEGER2})
REAL2			({SIGN}{UREAL2})|(\+{NANINF})|(\-{NANINF})

PREFIX2			({RADIX2}{EXACTNESS})|({EXACTNESS}{RADIX2})
COMPLEX2		{REAL2}|{REAL2}@{REAL2}|({REAL2}\+{UREAL2}{IMAG})|({REAL2}\-{UREAL2}{IMAG})|({REAL2}\+{NANINF}{IMAG})|({REAL2}\-{NANINF}{IMAG})|({REAL2}\+{IMAG})|({REAL2}\-{IMAG})|(\+{UREAL2}{IMAG})|(\-{UREAL2}{IMAG})
NUMBER2			{PREFIX2}{COMPLEX2}

;;; --------------------------------------------------------------------

UINTEGER8		{DIGIT8}+
UREAL8			{UINTEGER8}|({UINTEGER8}/{UINTEGER8})
REAL8			({SIGN}{UREAL8})|(\+{NANINF})|(\-{NANINF})

PREFIX8			({RADIX8}{EXACTNESS})|({EXACTNESS}{RADIX8})
COMPLEX8		{REAL8}|{REAL8}@{REAL8}|({REAL8}\+{UREAL8}{IMAG})|({REAL8}\-{UREAL8}{IMAG})|({REAL8}\+{NANINF}{IMAG})|({REAL8}\-{NANINF}{IMAG})|({REAL8}\+{IMAG})|({REAL8}\-{IMAG})|(\+{UREAL8}{IMAG})|(\-{UREAL8}{IMAG})
NUMBER8			{PREFIX8}{COMPLEX8}

;;; --------------------------------------------------------------------

UINTEGER10		{DIGIT10}+
;;This rule is unique for base 10.
DECIMAL10		({UINTEGER10}{SUFFIX})|(\.{DIGIT10}+{SUFFIX})|({DIGIT10}+\.{DIGIT10}*{SUFFIX})|({DIGIT10}+\.{SUFFIX})
;;This rule is unique for base 10 in the last alternative.
UREAL10			({UINTEGER10})|({UINTEGER10}/{UINTEGER10})|({DECIMAL10}{MANTISSA-WIDTH})
REAL10			({SIGN}{UREAL10})|(\+{NANINF})|(\-{NANINF})

PREFIX10		({RADIX10}{EXACTNESS})|({EXACTNESS}{RADIX10})
COMPLEX10		{REAL10}|{REAL10}@{REAL10}|({REAL10}\+{UREAL10}{IMAG})|({REAL10}\-{UREAL10}{IMAG})|({REAL10}\+{NANINF}{IMAG})|({REAL10}\-{NANINF}{IMAG})|({REAL10}\+{IMAG})|({REAL10}\-{IMAG})|(\+{UREAL10}{IMAG})|(\-{UREAL10}{IMAG})|(\+{NANINF}{IMAG})|(\-{NANINF}{IMAG})|(\+{IMAG})|(\-{IMAG})
NUMBER10		{PREFIX10}{COMPLEX10}

;;; --------------------------------------------------------------------

UINTEGER16		{DIGIT16}+
UREAL16			{UINTEGER16}|({UINTEGER16}/{UINTEGER16})
REAL16			({SIGN}{UREAL16})|(\+{NANINF})|(\-{NANINF})

PREFIX16		({RADIX16}{EXACTNESS})|({EXACTNESS}{RADIX16})
COMPLEX16		{REAL16}|{REAL16}@{REAL16}|({REAL16}\+{UREAL16}{IMAG})|({REAL16}\-{UREAL16}{IMAG})|({REAL16}\+{NANINF}{IMAG})|({REAL16}\-{NANINF}{IMAG})|({REAL16}\+{IMAG})|({REAL16}\-{IMAG})|(\+{UREAL16}{IMAG})|(\-{UREAL16}{IMAG})|(\+{NANINF}{IMAG})
NUMBER16		{PREFIX16}{COMPLEX16}

;;; --------------------------------------------------------------------

NUMBER			{NUMBER2}|{NUMBER8}|{NUMBER10}|{NUMBER16}
WRONG_NUMBER		{NUMBER}{TAIL_OF_WRONG}

;;page
;;;; misc stuff

SHARPBANGR6RS		#!r6rs
SHARPBANG		#!

BOOLEAN			#t|#f|#T|#F
WRONG_BOOLEAN		{BOOLEAN}{TAIL_OF_WRONG}

;;page
%%

{OPAREN}		((open-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{CPAREN}		((close-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{OBRACKET}		((open-bracket-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{CBRACKET}		((close-bracket-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{TICK}			((tick-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{BACKTICK}		((back-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{COMMAAT}		((comma-at-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{COMMA}			((comma-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{DOT}			((dot-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{DOUBLEQUOTE}		((double-quote-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SEMICOLON}		((semicolon-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPPAREN}		((sharp-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPVU8PAREN}		((sharp-vu8paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPTICK}		((sharp-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBACKTICK}		((sharp-back-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPCOMMAAT}		((sharp-comma-at-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPCOMMA}		((sharp-comma-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPSEMICOLON}	((sharp-semicolon-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
{LINECOMMENT}		((line-comment-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LINECOMMENT-NOEND}	((line-comment-noend-token-maker)
			 yygetc yyungetc yytext yyline yycolumn yyoffset)
{ONESTEDCOMMENT}	((open-nested-comment-token-maker)
			 yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBANGR6RS}		((sharp-bang-r6rs-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBANG}		((sharp-bang-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WHITESPACE}		((white-space-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LINEENDING}		((line-ending-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;; identifiers

{IDENTIFIER}		((identifier-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{PECULIAR_IDENTIFIER}	((identifier-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_IDENTIFIER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_PECULIAR_IDENTIFIER} ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; booleans

{BOOLEAN}		((boolean-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_BOOLEAN}		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; characters

{NAMED_CHARACTER}	((named-character-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{HEX_CHARACTER}		((hex-character-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LITERAL_CHARACTER}	((literal-character-token-maker)yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_NAMED_CHARACTER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_HEX_CHARACTER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_LITERAL_CHARACTER} ((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; numbers

{NUMBER}		((number-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_NUMBER}		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done

<<EOF>>			((eof-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
