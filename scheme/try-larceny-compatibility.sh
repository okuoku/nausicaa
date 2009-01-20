## try-larceny-compatibility.sh --
##

export LARCENY_LIBPATH=$PWD:$LARCENY_LIBPATH

exec larceny -r6rs -program tests/r6rs/run.sps

### end of file
