# prepare.sh --

set -x

(cd ..
    if test \
        configure -ot prepare.sh                                -o \
        configure -ot configure.ac                              -o \
        configure -ot aclocal.m4                                -o \
        configure -ot infrastructure/develstuff.autoconf        -o \
        configure -ot configuration/nausicaa.autoconf           -o \
        configure -ot configuration/posix-inspector.m4
        then
        autoconf
        autoheader
    fi)

../configure \
    --config-cache                              \
    --with-abi=local-slackware                  \
    --enable-use-sudo                           \
    --enable-vicare                             \
    --enable-mosh                               \
    --disable-ypsilon                           \
    --disable-petite                            \
    --enable-doc-ps                             \
    --enable-time-tests                         \
    "$@"

### end of file
