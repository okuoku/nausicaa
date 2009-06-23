# extract-unicode-categories.sh --
#

for item in \
    Lu  \
    Ll  \
    Lt  \
    Lm  \
    Lo  \
    Mn  \
    Mc  \
    Me  \
    Nd  \
    Nl  \
    No  \
    Pc  \
    Pd  \
    Ps  \
    Pe  \
    Pi  \
    Pf  \
    Po  \
    Sm  \
    Sc  \
    Sk  \
    So  \
    Zs  \
    Zl  \
    Zp  \
    Cc  \
    Cf  \
    Cs  \
    Co
do
    {
        grep ";$item;" UnicodeData.txt | cut -f1 -d';' | while read code
        do printf '\t#\\x%s\n' $code
        done
    } >$item.codes
done

### end of file

