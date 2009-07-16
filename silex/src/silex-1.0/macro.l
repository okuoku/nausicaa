; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

space   " "
tab     "	"
comment ";".*
hblank  {space}|{tab}|{comment}
vblank  "\n"

digit   [0123456789]
letter  [abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
specini "!"|"$"|"%"|"&"|"*"|"/"|":"|"<"|"="|">"|"?"|"~"|"_"|"^"
specsub "."|"+"|"-"
initial {letter}|{specini}
subseq  {letter}|{digit}|{specini}|{specsub}
peculid "+"|"-"|"..."
id      {initial}{subseq}*|{peculid}

%%

{hblank} (make-tok hblank-tok          yytext yyline yycolumn)
{vblank} (make-tok vblank-tok          yytext yyline yycolumn)
"%%"     (make-tok percent-percent-tok yytext yyline yycolumn)
{id}     (parse-id                     yytext yyline yycolumn)
[^]      (make-tok illegal-tok         yytext yyline yycolumn)
<<EOF>>  (make-tok eof-tok             yytext yyline yycolumn)
