## start.sh --
##

set -x

CONFIG_FILE=/home/marco/src/devel/scheme/nausicaa/curl/src/lighttpd/config
PATH=/usr/local/sbin:/usr/sbin:$PATH

exec lighttpd -f "$CONFIG_FILE" -D &

### end of file
