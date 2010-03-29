## rebuild-and-test-project.sh --
##
##

set -e

PROJECT=${1:?missing project pathname}

if test -d "$PROJECT"
then
    echo "--- processing $PROJECT"
    cd "$PROJECT"
    if test -f prepare.sh
    then
        echo "--- found prepare.sh"
        if test -f ./make-inspector.sps
        then
            echo "--- found make-inspector.sps"
            ./make-inspector.sps
        fi

        test -d "=build" || mkdir "=build"
        cd "=build"
        if test -f Makefile
        then yes yes | make clean-builddir
        fi
        sh ../prepare.sh
        make dist
        PKG_ID=$(make echo-variable VARIABLE=PKG_ID)
        DESTDIR=$(make echo-variable VARIABLE=ds_dist_DESTDIR)
        ARCHIVE=$(make echo-variable VARIABLE=ds_dist_ARCHIVE)
        test -d ~/var/build/nausicaa || mkdir -p ~/var/build/nausicaa
        cp --force "$DESTDIR/$ARCHIVE" ~/var/build/nausicaa
        (set -e
            cd ~/var/build/nausicaa
            test -d "$PKG_ID" && rm -fr "$PKG_ID"
            tar --extract --bzip2 --file="$ARCHIVE"
            cd "$PKG_ID"
            mkdir "=build"
            cd "=build"
            sh ../prepare.sh
            make abu
            make fasl-installed
            make test-installed)
    else
        echo "*** prepare.sh not found, aborting"
        exit 2
    fi
else
    echo "*** selected project directory not found ($PROJECT)"
    exit 2
fi

exit 0

### end of file
