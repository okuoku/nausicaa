# prepare.sh --

set -x

(cd ..
    if test \
        configure -ot prepare.sh                                -o \
        configure -ot configure.ac                              -o \
        configure -ot aclocal.m4                                -o \
        configure -ot infrastructure/develstuff.autoconf        -o \
        configure -ot configuration/nausicaa.autoconf
        then
        autoconf
    fi)

../configure \
    --config-cache                              \
    --with-abi=local-slackware                  \
    --enable-use-sudo                           \
    --enable-mosh                               \
    --enable-petite                             \
    --enable-racket                             \
    --enable-vicare                             \
    --disable-ypsilon                           \
    --enable-binfmt                             \
    --enable-doc-ps                             \
    --enable-time-tests                         \
    "$@"

#    --enable-ikarus
#    --disable-larceny

### end of file
