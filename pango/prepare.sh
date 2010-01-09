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
    --enable-ikarus                             \
    --disable-larceny                           \
    --enable-mosh                               \
    --enable-ypsilon                            \
    --enable-doc-ps                             \
    --enable-time-tests                         \
    "$@"

# --with-glib-includedir=/usr/local/include/glib-2.0
# --with-glib-libincludedir=/usr/local/lib/glib-2.0/include
# --with-pango-includedir=/usr/local/include/pango-1.0


### end of file
