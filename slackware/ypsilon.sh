# ypsilon.sh --
#
# Unofficial Slackware build script for Ypsilon.

set -ex

## ------------------------------------------------------------
## Setup.

srcdir=${PWD}

: ${TMPDIR:=/tmp}
: ${prefix:=/usr/local}
: ${ARCHITECTURE:=i686}
# : ${CFLAGS:='-O3 -march=i686 -mtune=i686'}
# : ${CONFIG_OPTIONS:=}
# : ${CPPFLAGS:=}

NAME=ypsilon
VERSION=${1:?'missing version parameter'}
BUILD_VERSION=${2:?'missing build version parameter'}

PACKAGE_NAME=${NAME}-${VERSION}-${ARCHITECTURE}-${BUILD_VERSION}.tgz

DESTDIR=${TMPDIR}/${NAME}-${VERSION}

## ------------------------------------------------------------
## Programs.

: ${CHMOD:=/usr/bin/chmod}
: ${CHOWN:=/usr/bin/chown}
: ${CP:=/bin/cp}
: ${FIND:=/usr/bin/find}
: ${MAKE:=/usr/bin/make}
: ${MAKEPKG:=/sbin/makepkg}
: ${RM:=/bin/rm}
: ${SUDO:=/usr/bin/sudo}
: ${STRIP:=/usr/bin/strip}
: ${XARGS:=/usr/bin/xargs}

## ------------------------------------------------------------
## Configuration.

# Currently (checkout 285, Wed  Nov 26, 2008) Ypsilon has no
# "configure" script.

## ------------------------------------------------------------
## Building.

"${MAKE}" all PREFIX="${prefix}"
"${SUDO}" "${RM}" -frv "${DESTDIR}"
"${MAKE}" install PREFIX="${prefix}" DESTDIR="${DESTDIR}"

cd "${DESTDIR}${prefix}"
{
    "${FIND}"        -type d | "${XARGS}" "${CHMOD}" 0755
    ("${FIND}" bin   -type f | "${XARGS}" "${STRIP}" -s) || true
    ("${FIND}" bin   -type f | "${XARGS}" "${CHMOD}" 0555) || true
    ("${FIND}" lib   -type f | "${XARGS}" "${CHMOD}" 0444) || true
    ("${FIND}" share -type f | "${XARGS}" "${CHMOD}" 0444) || true
}

cd "${DESTDIR}"
{
    "${SUDO}" "${CHOWN}" root.root * --recursive
    "${SUDO}" "${MAKEPKG}" --chown y "${PACKAGE_NAME}"
    "${SUDO}" "${CHMOD}" 0666 "${PACKAGE_NAME}"
    "${CP}" "${PACKAGE_NAME}" "${srcdir}"
    "${SUDO}" "${RM}" -frv "${DESTDIR}"
}

exit 0

### end of file
