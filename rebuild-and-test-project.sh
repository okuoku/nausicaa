## rebuild-and-test-project.sh --
##
##

PROJECT=${1:?missing project pathname}

if test -d "$PROJECT"
then
    echo "--- processing $PROJECT"
    cd "$PROJECT"
    if test -f prepare.sh
    then
        echo "--- found prepare.sh"
        test -d "=build" || mkdir "=build"
        cd "=build"
        test -f Makefile && yes yes | make clean-builddir
        sh ../prepare.sh
        make -k all test
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
