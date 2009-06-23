;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: character sets representing categories
;;;Date: Tue Jun 23, 2009
;;;
;;;Abstract
;;;
;;;	This file  holds one character  set definition for  each general
;;;	category  of  characters   defined  in  the  Unicode  Characters
;;;	Database:
;;;
;;;	    <ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt>
;;;
;;;	for the list of defined categories see:
;;;
;;;	    <ftp://ftp.unicode.org/Public/UNIDATA/UCD.html>
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


(library (char-sets categories)
  (export
    char-set:letter-uppercase		char-set:letter-lowercase
    char-set:letter-titlecase		char-set:letter-modifier
    char-set:letter-other		char-set:mark-nospacing
    char-set:mark-spacing-combining
    char-set:mark-enclosing
    char-set:number-decimal-digit
    char-set:number-letter
    char-set:number-other
    char-set:puncutation-connector
    char-set:punctuation-dash
    char-set:punctuation-open
    char-set:punctuation-close
    char-set:punctuation-initial-quote
    char-set:punctuation-final-quote
    char-set:punctuation-other
    char-set:symbol-math
    char-set:symbol-currency
    char-set:symbol-modifier
    char-set:symbol-other
    char-set:separator-space
    char-set:separator-line
    char-set:separator-paragraph
    char-set:control
    char-set:format
    char-set:surrogate
    char-set:private-use
    char-set:not-assigned)
  (import (rnrs)
    (char-sets))


(define char-set:letter-uppercase
  ;; Category code: Lu
  (char-set
   (#\x41 . #\x5a)
   (#\xc0 . #\xd6)
   (#\xd8 . #\xde)
   (#\x100 . #\x100)
   (#\x102 . #\x102)
   (#\x104 . #\x104)
   (#\x106 . #\x106)
   (#\x108 . #\x108)
   (#\x10a . #\x10a)
   (#\x10c . #\x10c)
   (#\x10e . #\x10e)
   (#\x110 . #\x110)
   (#\x112 . #\x112)
   (#\x114 . #\x114)
   (#\x116 . #\x116)
   (#\x118 . #\x118)
   (#\x11a . #\x11a)
   (#\x11c . #\x11c)
   (#\x11e . #\x11e)
   (#\x120 . #\x120)
   (#\x122 . #\x122)
   (#\x124 . #\x124)
   (#\x126 . #\x126)
   (#\x128 . #\x128)
   (#\x12a . #\x12a)
   (#\x12c . #\x12c)
   (#\x12e . #\x12e)
   (#\x130 . #\x130)
   (#\x132 . #\x132)
   (#\x134 . #\x134)
   (#\x136 . #\x136)
   (#\x139 . #\x139)
   (#\x13b . #\x13b)
   (#\x13d . #\x13d)
   (#\x13f . #\x13f)
   (#\x141 . #\x141)
   (#\x143 . #\x143)
   (#\x145 . #\x145)
   (#\x147 . #\x147)
   (#\x14a . #\x14a)
   (#\x14c . #\x14c)
   (#\x14e . #\x14e)
   (#\x150 . #\x150)
   (#\x152 . #\x152)
   (#\x154 . #\x154)
   (#\x156 . #\x156)
   (#\x158 . #\x158)
   (#\x15a . #\x15a)
   (#\x15c . #\x15c)
   (#\x15e . #\x15e)
   (#\x160 . #\x160)
   (#\x162 . #\x162)
   (#\x164 . #\x164)
   (#\x166 . #\x166)
   (#\x168 . #\x168)
   (#\x16a . #\x16a)
   (#\x16c . #\x16c)
   (#\x16e . #\x16e)
   (#\x170 . #\x170)
   (#\x172 . #\x172)
   (#\x174 . #\x174)
   (#\x176 . #\x176)
   (#\x178 . #\x179)
   (#\x17b . #\x17b)
   (#\x17d . #\x17d)
   (#\x181 . #\x182)
   (#\x184 . #\x184)
   (#\x186 . #\x187)
   (#\x189 . #\x18b)
   (#\x18e . #\x191)
   (#\x193 . #\x194)
   (#\x196 . #\x198)
   (#\x19c . #\x19d)
   (#\x19f . #\x1a0)
   (#\x1a2 . #\x1a2)
   (#\x1a4 . #\x1a4)
   (#\x1a6 . #\x1a7)
   (#\x1a9 . #\x1a9)
   (#\x1ac . #\x1ac)
   (#\x1ae . #\x1af)
   (#\x1b1 . #\x1b3)
   (#\x1b5 . #\x1b5)
   (#\x1b7 . #\x1b8)
   (#\x1bc . #\x1bc)
   (#\x1c4 . #\x1c4)
   (#\x1c7 . #\x1c7)
   (#\x1ca . #\x1ca)
   (#\x1cd . #\x1cd)
   (#\x1cf . #\x1cf)
   (#\x1d1 . #\x1d1)
   (#\x1d3 . #\x1d3)
   (#\x1d5 . #\x1d5)
   (#\x1d7 . #\x1d7)
   (#\x1d9 . #\x1d9)
   (#\x1db . #\x1db)
   (#\x1de . #\x1de)
   (#\x1e0 . #\x1e0)
   (#\x1e2 . #\x1e2)
   (#\x1e4 . #\x1e4)
   (#\x1e6 . #\x1e6)
   (#\x1e8 . #\x1e8)
   (#\x1ea . #\x1ea)
   (#\x1ec . #\x1ec)
   (#\x1ee . #\x1ee)
   (#\x1f1 . #\x1f1)
   (#\x1f4 . #\x1f4)
   (#\x1f6 . #\x1f8)
   (#\x1fa . #\x1fa)
   (#\x1fc . #\x1fc)
   (#\x1fe . #\x1fe)
   (#\x200 . #\x200)
   (#\x202 . #\x202)
   (#\x204 . #\x204)
   (#\x206 . #\x206)
   (#\x208 . #\x208)
   (#\x20a . #\x20a)
   (#\x20c . #\x20c)
   (#\x20e . #\x20e)
   (#\x210 . #\x210)
   (#\x212 . #\x212)
   (#\x214 . #\x214)
   (#\x216 . #\x216)
   (#\x218 . #\x218)
   (#\x21a . #\x21a)
   (#\x21c . #\x21c)
   (#\x21e . #\x21e)
   (#\x220 . #\x220)
   (#\x222 . #\x222)
   (#\x224 . #\x224)
   (#\x226 . #\x226)
   (#\x228 . #\x228)
   (#\x22a . #\x22a)
   (#\x22c . #\x22c)
   (#\x22e . #\x22e)
   (#\x230 . #\x230)
   (#\x232 . #\x232)
   (#\x23a . #\x23b)
   (#\x23d . #\x23e)
   (#\x241 . #\x241)
   (#\x243 . #\x246)
   (#\x248 . #\x248)
   (#\x24a . #\x24a)
   (#\x24c . #\x24c)
   (#\x24e . #\x24e)
   (#\x370 . #\x370)
   (#\x372 . #\x372)
   (#\x376 . #\x376)
   (#\x386 . #\x386)
   (#\x388 . #\x38a)
   (#\x38c . #\x38c)
   (#\x38e . #\x38f)
   (#\x391 . #\x3a1)
   (#\x3a3 . #\x3ab)
   (#\x3cf . #\x3cf)
   (#\x3d2 . #\x3d4)
   (#\x3d8 . #\x3d8)
   (#\x3da . #\x3da)
   (#\x3dc . #\x3dc)
   (#\x3de . #\x3de)
   (#\x3e0 . #\x3e0)
   (#\x3e2 . #\x3e2)
   (#\x3e4 . #\x3e4)
   (#\x3e6 . #\x3e6)
   (#\x3e8 . #\x3e8)
   (#\x3ea . #\x3ea)
   (#\x3ec . #\x3ec)
   (#\x3ee . #\x3ee)
   (#\x3f4 . #\x3f4)
   (#\x3f7 . #\x3f7)
   (#\x3f9 . #\x3fa)
   (#\x3fd . #\x42f)
   (#\x460 . #\x460)
   (#\x462 . #\x462)
   (#\x464 . #\x464)
   (#\x466 . #\x466)
   (#\x468 . #\x468)
   (#\x46a . #\x46a)
   (#\x46c . #\x46c)
   (#\x46e . #\x46e)
   (#\x470 . #\x470)
   (#\x472 . #\x472)
   (#\x474 . #\x474)
   (#\x476 . #\x476)
   (#\x478 . #\x478)
   (#\x47a . #\x47a)
   (#\x47c . #\x47c)
   (#\x47e . #\x47e)
   (#\x480 . #\x480)
   (#\x48a . #\x48a)
   (#\x48c . #\x48c)
   (#\x48e . #\x48e)
   (#\x490 . #\x490)
   (#\x492 . #\x492)
   (#\x494 . #\x494)
   (#\x496 . #\x496)
   (#\x498 . #\x498)
   (#\x49a . #\x49a)
   (#\x49c . #\x49c)
   (#\x49e . #\x49e)
   (#\x4a0 . #\x4a0)
   (#\x4a2 . #\x4a2)
   (#\x4a4 . #\x4a4)
   (#\x4a6 . #\x4a6)
   (#\x4a8 . #\x4a8)
   (#\x4aa . #\x4aa)
   (#\x4ac . #\x4ac)
   (#\x4ae . #\x4ae)
   (#\x4b0 . #\x4b0)
   (#\x4b2 . #\x4b2)
   (#\x4b4 . #\x4b4)
   (#\x4b6 . #\x4b6)
   (#\x4b8 . #\x4b8)
   (#\x4ba . #\x4ba)
   (#\x4bc . #\x4bc)
   (#\x4be . #\x4be)
   (#\x4c0 . #\x4c1)
   (#\x4c3 . #\x4c3)
   (#\x4c5 . #\x4c5)
   (#\x4c7 . #\x4c7)
   (#\x4c9 . #\x4c9)
   (#\x4cb . #\x4cb)
   (#\x4cd . #\x4cd)
   (#\x4d0 . #\x4d0)
   (#\x4d2 . #\x4d2)
   (#\x4d4 . #\x4d4)
   (#\x4d6 . #\x4d6)
   (#\x4d8 . #\x4d8)
   (#\x4da . #\x4da)
   (#\x4dc . #\x4dc)
   (#\x4de . #\x4de)
   (#\x4e0 . #\x4e0)
   (#\x4e2 . #\x4e2)
   (#\x4e4 . #\x4e4)
   (#\x4e6 . #\x4e6)
   (#\x4e8 . #\x4e8)
   (#\x4ea . #\x4ea)
   (#\x4ec . #\x4ec)
   (#\x4ee . #\x4ee)
   (#\x4f0 . #\x4f0)
   (#\x4f2 . #\x4f2)
   (#\x4f4 . #\x4f4)
   (#\x4f6 . #\x4f6)
   (#\x4f8 . #\x4f8)
   (#\x4fa . #\x4fa)
   (#\x4fc . #\x4fc)
   (#\x4fe . #\x4fe)
   (#\x500 . #\x500)
   (#\x502 . #\x502)
   (#\x504 . #\x504)
   (#\x506 . #\x506)
   (#\x508 . #\x508)
   (#\x50a . #\x50a)
   (#\x50c . #\x50c)
   (#\x50e . #\x50e)
   (#\x510 . #\x510)
   (#\x512 . #\x512)
   (#\x514 . #\x514)
   (#\x516 . #\x516)
   (#\x518 . #\x518)
   (#\x51a . #\x51a)
   (#\x51c . #\x51c)
   (#\x51e . #\x51e)
   (#\x520 . #\x520)
   (#\x522 . #\x522)
   (#\x531 . #\x556)
   (#\x10a0 . #\x10c5)
   (#\x1e00 . #\x1e00)
   (#\x1e02 . #\x1e02)
   (#\x1e04 . #\x1e04)
   (#\x1e06 . #\x1e06)
   (#\x1e08 . #\x1e08)
   (#\x1e0a . #\x1e0a)
   (#\x1e0c . #\x1e0c)
   (#\x1e0e . #\x1e0e)
   (#\x1e10 . #\x1e10)
   (#\x1e12 . #\x1e12)
   (#\x1e14 . #\x1e14)
   (#\x1e16 . #\x1e16)
   (#\x1e18 . #\x1e18)
   (#\x1e1a . #\x1e1a)
   (#\x1e1c . #\x1e1c)
   (#\x1e1e . #\x1e1e)
   (#\x1e20 . #\x1e20)
   (#\x1e22 . #\x1e22)
   (#\x1e24 . #\x1e24)
   (#\x1e26 . #\x1e26)
   (#\x1e28 . #\x1e28)
   (#\x1e2a . #\x1e2a)
   (#\x1e2c . #\x1e2c)
   (#\x1e2e . #\x1e2e)
   (#\x1e30 . #\x1e30)
   (#\x1e32 . #\x1e32)
   (#\x1e34 . #\x1e34)
   (#\x1e36 . #\x1e36)
   (#\x1e38 . #\x1e38)
   (#\x1e3a . #\x1e3a)
   (#\x1e3c . #\x1e3c)
   (#\x1e3e . #\x1e3e)
   (#\x1e40 . #\x1e40)
   (#\x1e42 . #\x1e42)
   (#\x1e44 . #\x1e44)
   (#\x1e46 . #\x1e46)
   (#\x1e48 . #\x1e48)
   (#\x1e4a . #\x1e4a)
   (#\x1e4c . #\x1e4c)
   (#\x1e4e . #\x1e4e)
   (#\x1e50 . #\x1e50)
   (#\x1e52 . #\x1e52)
   (#\x1e54 . #\x1e54)
   (#\x1e56 . #\x1e56)
   (#\x1e58 . #\x1e58)
   (#\x1e5a . #\x1e5a)
   (#\x1e5c . #\x1e5c)
   (#\x1e5e . #\x1e5e)
   (#\x1e60 . #\x1e60)
   (#\x1e62 . #\x1e62)
   (#\x1e64 . #\x1e64)
   (#\x1e66 . #\x1e66)
   (#\x1e68 . #\x1e68)
   (#\x1e6a . #\x1e6a)
   (#\x1e6c . #\x1e6c)
   (#\x1e6e . #\x1e6e)
   (#\x1e70 . #\x1e70)
   (#\x1e72 . #\x1e72)
   (#\x1e74 . #\x1e74)
   (#\x1e76 . #\x1e76)
   (#\x1e78 . #\x1e78)
   (#\x1e7a . #\x1e7a)
   (#\x1e7c . #\x1e7c)
   (#\x1e7e . #\x1e7e)
   (#\x1e80 . #\x1e80)
   (#\x1e82 . #\x1e82)
   (#\x1e84 . #\x1e84)
   (#\x1e86 . #\x1e86)
   (#\x1e88 . #\x1e88)
   (#\x1e8a . #\x1e8a)
   (#\x1e8c . #\x1e8c)
   (#\x1e8e . #\x1e8e)
   (#\x1e90 . #\x1e90)
   (#\x1e92 . #\x1e92)
   (#\x1e94 . #\x1e94)
   (#\x1e9e . #\x1e9e)
   (#\x1ea0 . #\x1ea0)
   (#\x1ea2 . #\x1ea2)
   (#\x1ea4 . #\x1ea4)
   (#\x1ea6 . #\x1ea6)
   (#\x1ea8 . #\x1ea8)
   (#\x1eaa . #\x1eaa)
   (#\x1eac . #\x1eac)
   (#\x1eae . #\x1eae)
   (#\x1eb0 . #\x1eb0)
   (#\x1eb2 . #\x1eb2)
   (#\x1eb4 . #\x1eb4)
   (#\x1eb6 . #\x1eb6)
   (#\x1eb8 . #\x1eb8)
   (#\x1eba . #\x1eba)
   (#\x1ebc . #\x1ebc)
   (#\x1ebe . #\x1ebe)
   (#\x1ec0 . #\x1ec0)
   (#\x1ec2 . #\x1ec2)
   (#\x1ec4 . #\x1ec4)
   (#\x1ec6 . #\x1ec6)
   (#\x1ec8 . #\x1ec8)
   (#\x1eca . #\x1eca)
   (#\x1ecc . #\x1ecc)
   (#\x1ece . #\x1ece)
   (#\x1ed0 . #\x1ed0)
   (#\x1ed2 . #\x1ed2)
   (#\x1ed4 . #\x1ed4)
   (#\x1ed6 . #\x1ed6)
   (#\x1ed8 . #\x1ed8)
   (#\x1eda . #\x1eda)
   (#\x1edc . #\x1edc)
   (#\x1ede . #\x1ede)
   (#\x1ee0 . #\x1ee0)
   (#\x1ee2 . #\x1ee2)
   (#\x1ee4 . #\x1ee4)
   (#\x1ee6 . #\x1ee6)
   (#\x1ee8 . #\x1ee8)
   (#\x1eea . #\x1eea)
   (#\x1eec . #\x1eec)
   (#\x1eee . #\x1eee)
   (#\x1ef0 . #\x1ef0)
   (#\x1ef2 . #\x1ef2)
   (#\x1ef4 . #\x1ef4)
   (#\x1ef6 . #\x1ef6)
   (#\x1ef8 . #\x1ef8)
   (#\x1efa . #\x1efa)
   (#\x1efc . #\x1efc)
   (#\x1efe . #\x1efe)
   (#\x1f08 . #\x1f0f)
   (#\x1f18 . #\x1f1d)
   (#\x1f28 . #\x1f2f)
   (#\x1f38 . #\x1f3f)
   (#\x1f48 . #\x1f4d)
   (#\x1f59 . #\x1f59)
   (#\x1f5b . #\x1f5b)
   (#\x1f5d . #\x1f5d)
   (#\x1f5f . #\x1f5f)
   (#\x1f68 . #\x1f6f)
   (#\x1fb8 . #\x1fbb)
   (#\x1fc8 . #\x1fcb)
   (#\x1fd8 . #\x1fdb)
   (#\x1fe8 . #\x1fec)
   (#\x1ff8 . #\x1ffb)
   (#\x2102 . #\x2102)
   (#\x2107 . #\x2107)
   (#\x210b . #\x210d)
   (#\x2110 . #\x2112)
   (#\x2115 . #\x2115)
   (#\x2119 . #\x211d)
   (#\x2124 . #\x2124)
   (#\x2126 . #\x2126)
   (#\x2128 . #\x2128)
   (#\x212a . #\x212d)
   (#\x2130 . #\x2133)
   (#\x213e . #\x213f)
   (#\x2145 . #\x2145)
   (#\x2183 . #\x2183)
   (#\x2c00 . #\x2c2e)
   (#\x2c60 . #\x2c60)
   (#\x2c62 . #\x2c64)
   (#\x2c67 . #\x2c67)
   (#\x2c69 . #\x2c69)
   (#\x2c6b . #\x2c6b)
   (#\x2c6d . #\x2c6f)
   (#\x2c72 . #\x2c72)
   (#\x2c75 . #\x2c75)
   (#\x2c80 . #\x2c80)
   (#\x2c82 . #\x2c82)
   (#\x2c84 . #\x2c84)
   (#\x2c86 . #\x2c86)
   (#\x2c88 . #\x2c88)
   (#\x2c8a . #\x2c8a)
   (#\x2c8c . #\x2c8c)
   (#\x2c8e . #\x2c8e)
   (#\x2c90 . #\x2c90)
   (#\x2c92 . #\x2c92)
   (#\x2c94 . #\x2c94)
   (#\x2c96 . #\x2c96)
   (#\x2c98 . #\x2c98)
   (#\x2c9a . #\x2c9a)
   (#\x2c9c . #\x2c9c)
   (#\x2c9e . #\x2c9e)
   (#\x2ca0 . #\x2ca0)
   (#\x2ca2 . #\x2ca2)
   (#\x2ca4 . #\x2ca4)
   (#\x2ca6 . #\x2ca6)
   (#\x2ca8 . #\x2ca8)
   (#\x2caa . #\x2caa)
   (#\x2cac . #\x2cac)
   (#\x2cae . #\x2cae)
   (#\x2cb0 . #\x2cb0)
   (#\x2cb2 . #\x2cb2)
   (#\x2cb4 . #\x2cb4)
   (#\x2cb6 . #\x2cb6)
   (#\x2cb8 . #\x2cb8)
   (#\x2cba . #\x2cba)
   (#\x2cbc . #\x2cbc)
   (#\x2cbe . #\x2cbe)
   (#\x2cc0 . #\x2cc0)
   (#\x2cc2 . #\x2cc2)
   (#\x2cc4 . #\x2cc4)
   (#\x2cc6 . #\x2cc6)
   (#\x2cc8 . #\x2cc8)
   (#\x2cca . #\x2cca)
   (#\x2ccc . #\x2ccc)
   (#\x2cce . #\x2cce)
   (#\x2cd0 . #\x2cd0)
   (#\x2cd2 . #\x2cd2)
   (#\x2cd4 . #\x2cd4)
   (#\x2cd6 . #\x2cd6)
   (#\x2cd8 . #\x2cd8)
   (#\x2cda . #\x2cda)
   (#\x2cdc . #\x2cdc)
   (#\x2cde . #\x2cde)
   (#\x2ce0 . #\x2ce0)
   (#\x2ce2 . #\x2ce2)
   (#\xa640 . #\xa640)
   (#\xa642 . #\xa642)
   (#\xa644 . #\xa644)
   (#\xa646 . #\xa646)
   (#\xa648 . #\xa648)
   (#\xa64a . #\xa64a)
   (#\xa64c . #\xa64c)
   (#\xa64e . #\xa64e)
   (#\xa650 . #\xa650)
   (#\xa652 . #\xa652)
   (#\xa654 . #\xa654)
   (#\xa656 . #\xa656)
   (#\xa658 . #\xa658)
   (#\xa65a . #\xa65a)
   (#\xa65c . #\xa65c)
   (#\xa65e . #\xa65e)
   (#\xa662 . #\xa662)
   (#\xa664 . #\xa664)
   (#\xa666 . #\xa666)
   (#\xa668 . #\xa668)
   (#\xa66a . #\xa66a)
   (#\xa66c . #\xa66c)
   (#\xa680 . #\xa680)
   (#\xa682 . #\xa682)
   (#\xa684 . #\xa684)
   (#\xa686 . #\xa686)
   (#\xa688 . #\xa688)
   (#\xa68a . #\xa68a)
   (#\xa68c . #\xa68c)
   (#\xa68e . #\xa68e)
   (#\xa690 . #\xa690)
   (#\xa692 . #\xa692)
   (#\xa694 . #\xa694)
   (#\xa696 . #\xa696)
   (#\xa722 . #\xa722)
   (#\xa724 . #\xa724)
   (#\xa726 . #\xa726)
   (#\xa728 . #\xa728)
   (#\xa72a . #\xa72a)
   (#\xa72c . #\xa72c)
   (#\xa72e . #\xa72e)
   (#\xa732 . #\xa732)
   (#\xa734 . #\xa734)
   (#\xa736 . #\xa736)
   (#\xa738 . #\xa738)
   (#\xa73a . #\xa73a)
   (#\xa73c . #\xa73c)
   (#\xa73e . #\xa73e)
   (#\xa740 . #\xa740)
   (#\xa742 . #\xa742)
   (#\xa744 . #\xa744)
   (#\xa746 . #\xa746)
   (#\xa748 . #\xa748)
   (#\xa74a . #\xa74a)
   (#\xa74c . #\xa74c)
   (#\xa74e . #\xa74e)
   (#\xa750 . #\xa750)
   (#\xa752 . #\xa752)
   (#\xa754 . #\xa754)
   (#\xa756 . #\xa756)
   (#\xa758 . #\xa758)
   (#\xa75a . #\xa75a)
   (#\xa75c . #\xa75c)
   (#\xa75e . #\xa75e)
   (#\xa760 . #\xa760)
   (#\xa762 . #\xa762)
   (#\xa764 . #\xa764)
   (#\xa766 . #\xa766)
   (#\xa768 . #\xa768)
   (#\xa76a . #\xa76a)
   (#\xa76c . #\xa76c)
   (#\xa76e . #\xa76e)
   (#\xa779 . #\xa779)
   (#\xa77b . #\xa77b)
   (#\xa77d . #\xa77e)
   (#\xa780 . #\xa780)
   (#\xa782 . #\xa782)
   (#\xa784 . #\xa784)
   (#\xa786 . #\xa786)
   (#\xa78b . #\xa78b)
   (#\xff21 . #\xff3a)
   (#\x10400 . #\x10427)
   (#\x1d400 . #\x1d419)
   (#\x1d434 . #\x1d44d)
   (#\x1d468 . #\x1d481)
   (#\x1d49c . #\x1d49c)
   (#\x1d49e . #\x1d49f)
   (#\x1d4a2 . #\x1d4a2)
   (#\x1d4a5 . #\x1d4a6)
   (#\x1d4a9 . #\x1d4ac)
   (#\x1d4ae . #\x1d4b5)
   (#\x1d4d0 . #\x1d4e9)
   (#\x1d504 . #\x1d505)
   (#\x1d507 . #\x1d50a)
   (#\x1d50d . #\x1d514)
   (#\x1d516 . #\x1d51c)
   (#\x1d538 . #\x1d539)
   (#\x1d53b . #\x1d53e)
   (#\x1d540 . #\x1d544)
   (#\x1d546 . #\x1d546)
   (#\x1d54a . #\x1d550)
   (#\x1d56c . #\x1d585)
   (#\x1d5a0 . #\x1d5b9)
   (#\x1d5d4 . #\x1d5ed)
   (#\x1d608 . #\x1d621)
   (#\x1d63c . #\x1d655)
   (#\x1d670 . #\x1d689)
   (#\x1d6a8 . #\x1d6c0)
   (#\x1d6e2 . #\x1d6fa)
   (#\x1d71c . #\x1d734)
   (#\x1d756 . #\x1d76e)
   (#\x1d790 . #\x1d7a8)
   (#\x1d7ca . #\x1d7ca)
   ))


(define char-set:letter-lowercase
  ;; Category code: Ll
  (char-set
   ))


(define char-set:letter-titlecase
  ;; Category code: Lt
  (char-set
   ))


(define char-set:letter-modifier
  ;; Category code: Lm
  (char-set
   ))


(define char-set:letter-other
  ;; Category code: Lo
  (char-set
   ))


(define char-set:mark-nospacing
  ;; Category code: Mn
  (char-set
   ))


(define char-set:mark-spacing-combining
  ;; Category code: Mc
  (char-set
   ))


(define char-set:mark-enclosing
  ;; Category code: Me
  (char-set
   ))


(define char-set:number-decimal-digit
  ;; Category code: Nd
  (char-set
   ))


(define char-set:number-letter
  ;; Category code: Nl
  (char-set
   ))


(define char-set:number-other
  ;; Category code: No
  (char-set
   ))


(define char-set:puncutation-connector
  ;; Category code: Pc
  (char-set
   ))


(define char-set:punctuation-dash
  ;; Category code: Pd
  (char-set
   ))


(define char-set:punctuation-open
  ;; Category code: Ps
  (char-set
   ))


(define char-set:punctuation-close
  ;; Category code: Pe
  (char-set
   ))


(define char-set:punctuation-initial-quote
  ;; Category code: Pi
  (char-set
   ))


(define char-set:punctuation-final-quote
  ;; Category code: Pf
  (char-set
   ))


(define char-set:punctuation-other
  ;; Category code: Po
  (char-set
   ))


(define char-set:symbol-math
  ;; Category code: Sm
  (char-set
   ))


(define char-set:symbol-currency
  ;; Category code: Sc
  (char-set
   ))


(define char-set:symbol-modifier
  ;; Category code: Sk
  (char-set
   ))


(define char-set:symbol-other
  ;; Category code: So
  (char-set
   ))


(define char-set:separator-space
  ;; Category code: Zs
  (char-set
   ))


(define char-set:separator-line
  ;; Category code: Zl
  (char-set
   ))


(define char-set:separator-paragraph
  ;; Category code: Zp
  (char-set
   ))


(define char-set:control
  ;; Category code: Cc
  (char-set
   ))


(define char-set:format
  ;; Category code: Cf
  (char-set
   ))


(define char-set:surrogate
  ;; Category code: Cs
  (char-set
   ))


(define char-set:private-use
  ;; Category code: Co
  (char-set
   ))


(define char-set:not-assigned
  ;; Category code: Cn
  ;;
  ;; No characters in the file have this property.
  (char-set))


;;;; done

  )

;;; end of file
