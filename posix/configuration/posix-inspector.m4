dnl (foreign posix sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Sun Dec  6, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl


NAUSICAA_INSPECT_TYPE([BLKCNT_T],[blkcnt_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([CLOCK_T],[clock_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([DEV_T],[dev_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([GID_T],[gid_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([INO_T],[ino_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([MODE_T],[mode_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([NLINK_T],[nlink_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([OFF_T],[off_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([PID_T],[pid_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([TIME_T],[time_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([UID_T],[uid_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([WCHAR_T],[wchar_t],[signed-int],[#f])

dnl Struct inspection: flock
NAUSICAA_INSPECT_STRUCT_TYPE([FLOCK],[struct flock],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([FLOCK_L_TYPE],[struct flock],[l_type],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FLOCK_L_WHENCE],[struct flock],[l_whence],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FLOCK_L_START],[struct flock],[l_start],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FLOCK_L_LEN],[struct flock],[l_len],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FLOCK_L_PID],[struct flock],[l_pid],[signed-int])

dnl Struct inspection: timeval
NAUSICAA_INSPECT_STRUCT_TYPE([TIMEVAL],[struct timeval],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEVAL_TV_SEC],[struct timeval],[tv_sec],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEVAL_TV_USEC],[struct timeval],[tv_usec],[signed-int])

dnl Struct inspection: timespec
NAUSICAA_INSPECT_STRUCT_TYPE([TIMESPEC],[struct timespec],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([TIMESPEC_TV_SEC],[struct timespec],[tv_sec],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMESPEC_TV_NSEC],[struct timespec],[tv_nsec],[signed-int])

dnl Struct inspection: tms
NAUSICAA_INSPECT_STRUCT_TYPE([TMS],[struct tms],[#f])

dnl Struct inspection: dirent
NAUSICAA_INSPECT_STRUCT_TYPE([DIRENT],[struct dirent],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([DIRENT_D_INO],[struct dirent],[d_ino],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([DIRENT_D_OFF],[struct dirent],[d_off],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([DIRENT_D_RECLEN],[struct dirent],[d_reclen],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([DIRENT_D_TYPE],[struct dirent],[d_type],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([DIRENT_D_NAME],[struct dirent],[d_name])

AC_CHECK_MEMBERS([struct stat.st_atime_usec])
AC_CHECK_MEMBERS([struct stat.st_mtime_usec])
AC_CHECK_MEMBERS([struct stat.st_ctime_usec])


dnl Struct inspection: utimbuf
NAUSICAA_INSPECT_STRUCT_TYPE([UTIMBUF],[struct utimbuf],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([UTIMBUF_ACTIME],[struct utimbuf],[actime],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([UTIMBUF_MODTIME],[struct utimbuf],[modtime],[signed-int])

dnl Struct inspection: timezone
NAUSICAA_INSPECT_STRUCT_TYPE([TIMEZONE],[struct timezone],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEZONE_TZ_MINUTESWEST],[struct timezone],[tz_minuteswest],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEZONE_TZ_DSTTIME],[struct timezone],[tz_dsttime],[signed-int])

dnl Struct inspection: tm
NAUSICAA_INSPECT_STRUCT_TYPE([TM],[struct tm],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_SEC],[struct tm],[tm_sec],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_MIN],[struct tm],[tm_min],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_HOUR],[struct tm],[tm_hour],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_MDAY],[struct tm],[tm_mday],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_MON],[struct tm],[tm_mon],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_YEAR],[struct tm],[tm_year],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_WDAY],[struct tm],[tm_wday],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_YDAY],[struct tm],[tm_yday],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_ISDST],[struct tm],[tm_isdst],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_GMTOFF],[struct tm],[tm_gmtoff],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TM_TM_ZONE],[struct tm],[tm_zone],[pointer])

dnl Struct inspection: ntptimeval
NAUSICAA_INSPECT_STRUCT_TYPE([NTPTIMEVAL],[struct ntptimeval],[#f])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([NTPTIMEVAL_TIME],[struct ntptimeval],[time])
NAUSICAA_INSPECT_FIELD_TYPE([NTPTIMEVAL_MAXERROR],[struct ntptimeval],[maxerror],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([NTPTIMEVAL_ESTERROR],[struct ntptimeval],[esterror],[signed-int])

dnl Struct inspection: timex
NAUSICAA_INSPECT_STRUCT_TYPE([TIMEX],[struct timex],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_MODES],[struct timex],[modes],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_OFFSET],[struct timex],[offset],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_FREQ],[struct timex],[freq],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_MAXERROR],[struct timex],[maxerror],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_ESTERROR],[struct timex],[esterror],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_STATUS],[struct timex],[status],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_CONSTANT],[struct timex],[constant],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_PRECISION],[struct timex],[precision],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_TOLERANCE],[struct timex],[tolerance],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([TIMEX_TIME],[struct timex],[time])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_TICK],[struct timex],[tick],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_PPSFREQ],[struct timex],[ppsfreq],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_JITTER],[struct timex],[jitter],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_SHIFT],[struct timex],[shift],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_STABIL],[struct timex],[stabil],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_JITCNT],[struct timex],[jitcnt],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_CALCNT],[struct timex],[calcnt],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_ERRCNT],[struct timex],[errcnt],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([TIMEX_STBCNT],[struct timex],[stbcnt],[signed-int])

dnl Struct inspection: itimerval
NAUSICAA_INSPECT_STRUCT_TYPE([ITIMERVAL],[struct itimerval],[#f])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([ITIMERVAL_IT_INTERVAL],[struct itimerval],[it_interval])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([ITIMERVAL_IT_VALUE],[struct itimerval],[it_value])

dnl Struct inspection: FTW
NAUSICAA_INSPECT_STRUCT_TYPE([FTW],[struct FTW],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([FTW_BASE],[struct FTW],[base],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FTW_LEVEL],[struct FTW],[level],[signed-int])
AC_CACHE_SAVE

dnl Preprocessor symbols: seek whence arguments
NAUSICAA_DEFINE_VALUE([SEEK_SET])
NAUSICAA_DEFINE_VALUE([SEEK_CUR])
NAUSICAA_DEFINE_VALUE([SEEK_END])

dnl Preprocessor symbols: file descriptor related flags
NAUSICAA_DEFINE_VALUE([O_ACCMODE])
NAUSICAA_DEFINE_VALUE([O_APPEND])
NAUSICAA_DEFINE_VALUE([O_ASYNC])
NAUSICAA_DEFINE_VALUE([O_CREAT])
NAUSICAA_DEFINE_VALUE([O_EXCL])
NAUSICAA_DEFINE_VALUE([O_EXEC])
NAUSICAA_DEFINE_VALUE([O_EXLOCK])
NAUSICAA_DEFINE_VALUE([O_FSYNC])
NAUSICAA_DEFINE_VALUE([O_IGNORE_CTTY])
NAUSICAA_DEFINE_VALUE([O_NDELAY])
NAUSICAA_DEFINE_VALUE([O_NOCTTY])
NAUSICAA_DEFINE_VALUE([O_NOLINK])
NAUSICAA_DEFINE_VALUE([O_NONBLOCK])
NAUSICAA_DEFINE_VALUE([O_NOTRANS])
NAUSICAA_DEFINE_VALUE([O_RDONLY])
NAUSICAA_DEFINE_VALUE([O_RDWR])
NAUSICAA_DEFINE_VALUE([O_READ])
NAUSICAA_DEFINE_VALUE([O_SHLOCK])
NAUSICAA_DEFINE_VALUE([O_SYNC])
NAUSICAA_DEFINE_VALUE([O_TRUNC])
NAUSICAA_DEFINE_VALUE([O_WRITE])
NAUSICAA_DEFINE_VALUE([O_WRONLY])
NAUSICAA_DEFINE_VALUE([O_NOATIME])
NAUSICAA_DEFINE_VALUE([FD_CLOEXEC])
AC_CACHE_SAVE

dnl Preprocessor symbols: ioctl action selection
NAUSICAA_DEFINE_VALUE([F_DUPFD])
NAUSICAA_DEFINE_VALUE([F_GETFD])
NAUSICAA_DEFINE_VALUE([F_GETFL])
NAUSICAA_DEFINE_VALUE([F_GETLK])
NAUSICAA_DEFINE_VALUE([F_GETOWN])
NAUSICAA_DEFINE_VALUE([F_SETFD])
NAUSICAA_DEFINE_VALUE([F_SETFL])
NAUSICAA_DEFINE_VALUE([F_SETLKW])
NAUSICAA_DEFINE_VALUE([F_SETLK])
NAUSICAA_DEFINE_VALUE([F_SETOWN])
NAUSICAA_DEFINE_VALUE([F_RDLCK])
NAUSICAA_DEFINE_VALUE([F_UNLCK])
NAUSICAA_DEFINE_VALUE([F_WRLCK])

dnl Preprocessor symbols: miscellaneous file-related constants
NAUSICAA_DEFINE_VALUE([WNOHANG])
NAUSICAA_DEFINE_VALUE([WUNTRACED])
NAUSICAA_DEFINE_VALUE([WCONTINUED])
NAUSICAA_DEFINE_VALUE([R_OK])
NAUSICAA_DEFINE_VALUE([W_OK])
NAUSICAA_DEFINE_VALUE([X_OK])
NAUSICAA_DEFINE_VALUE([F_OK])

dnl Preprocessor symbols: miscellaneous constants
NAUSICAA_DEFINE_VALUE([L_ctermid])
NAUSICAA_DEFINE_VALUE([L_tmpnam])
NAUSICAA_DEFINE_VALUE([CLOCKS_PER_SEC])

dnl Preprocessor symbols: mode bits
NAUSICAA_DEFINE_VALUE([S_IRUSR])
NAUSICAA_DEFINE_VALUE([S_IWUSR])
NAUSICAA_DEFINE_VALUE([S_IXUSR])
NAUSICAA_DEFINE_VALUE([S_IRGRP])
NAUSICAA_DEFINE_VALUE([S_IWGRP])
NAUSICAA_DEFINE_VALUE([S_IXGRP])
NAUSICAA_DEFINE_VALUE([S_IROTH])
NAUSICAA_DEFINE_VALUE([S_IWOTH])
NAUSICAA_DEFINE_VALUE([S_IXOTH])
NAUSICAA_DEFINE_VALUE([S_IRWXU])
NAUSICAA_DEFINE_VALUE([S_IRWXG])
NAUSICAA_DEFINE_VALUE([S_IRWXO])
NAUSICAA_DEFINE_VALUE([S_ISUID])
NAUSICAA_DEFINE_VALUE([S_ISGID])
NAUSICAA_DEFINE_VALUE([S_ISVTX])

AC_CHECK_DECL([_DIRENT_HAVE_D_NAMELEN],[NAU_DIRENT_HAVE_D_NAMELEN=#t],[NAU_DIRENT_HAVE_D_NAMELEN=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_RECLEN],[NAU_DIRENT_HAVE_D_RECLEN=#t],[NAU_DIRENT_HAVE_D_RECLEN=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_OFF],[NAU_DIRENT_HAVE_D_OFF=#t],[NAU_DIRENT_HAVE_D_OFF=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_TYPE],[NAU_DIRENT_HAVE_D_TYPE=#t],[NAU_DIRENT_HAVE_D_TYPE=#f],
  [NAU_POSIX_INCLUDES])
AC_SUBST([NAU_DIRENT_HAVE_D_NAMELEN])
AC_SUBST([NAU_DIRENT_HAVE_D_RECLEN])
AC_SUBST([NAU_DIRENT_HAVE_D_OFF])
AC_SUBST([NAU_DIRENT_HAVE_D_TYPE])


dnl Preprocessor symbols: dirent stuff
NAUSICAA_DEFINE_VALUE([DT_BLK])
NAUSICAA_DEFINE_VALUE([DT_CHR])
NAUSICAA_DEFINE_VALUE([DT_DIR])
NAUSICAA_DEFINE_VALUE([DT_FIFO])
NAUSICAA_DEFINE_VALUE([DT_LNK])
NAUSICAA_DEFINE_VALUE([DT_REG])
NAUSICAA_DEFINE_VALUE([DT_SOCK])
NAUSICAA_DEFINE_VALUE([DT_UNKNOWN])

dnl Preprocessor symbols: constants related to pathconf()
NAUSICAA_DEFINE_VALUE([_PC_LINK_MAX])
NAUSICAA_DEFINE_VALUE([_PC_MAX_CANON])
NAUSICAA_DEFINE_VALUE([_PC_MAX_INPUT])
NAUSICAA_DEFINE_VALUE([_PC_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_PC_PATH_MAX])
NAUSICAA_DEFINE_VALUE([_PC_PIPE_BUF])
NAUSICAA_DEFINE_VALUE([_PC_CHOWN_RESTRICTED])
NAUSICAA_DEFINE_VALUE([_PC_NO_TRUNC])
NAUSICAA_DEFINE_VALUE([_PC_VDISABLE])
NAUSICAA_DEFINE_VALUE([_PC_SYNC_IO])
NAUSICAA_DEFINE_VALUE([_PC_ASYNC_IO])
NAUSICAA_DEFINE_VALUE([_PC_PRIO_IO])
NAUSICAA_DEFINE_VALUE([_PC_FILESIZEBITS])
NAUSICAA_DEFINE_VALUE([_PC_REC_INCR_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([_PC_REC_MAX_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([_PC_REC_MIN_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([_PC_XFER_ALIGN])

dnl Preprocessor symbols: constants related to ftw() and nftw()
NAUSICAA_DEFINE_VALUE([FTW_F])
NAUSICAA_DEFINE_VALUE([FTW_D])
NAUSICAA_DEFINE_VALUE([FTW_NS])
NAUSICAA_DEFINE_VALUE([FTW_DNR])
NAUSICAA_DEFINE_VALUE([FTW_SL])
NAUSICAA_DEFINE_VALUE([FTW_DP])
NAUSICAA_DEFINE_VALUE([FTW_SLN])
NAUSICAA_DEFINE_VALUE([FTW_PHYS])
NAUSICAA_DEFINE_VALUE([FTW_MOUNT])
NAUSICAA_DEFINE_VALUE([FTW_CHDIR])
NAUSICAA_DEFINE_VALUE([FTW_DEPTH])
NAUSICAA_DEFINE_VALUE([FTW_ACTIONRETVAL])
NAUSICAA_DEFINE_VALUE([FTW_CONTINUE])
NAUSICAA_DEFINE_VALUE([FTW_STOP])
NAUSICAA_DEFINE_VALUE([FTW_SKIP_SUBTREE])
NAUSICAA_DEFINE_VALUE([FTW_SKIP_SIBLINGS])

dnl Preprocessor symbols: setitimer() stuff
NAUSICAA_DEFINE_VALUE([ITIMER_REAL])
NAUSICAA_DEFINE_VALUE([ITIMER_VIRTUAL])
NAUSICAA_DEFINE_VALUE([ITIMER_PROF])

dnl Preprocessor symbols: miscellaneous
NAUSICAA_DEFINE_VALUE([EOF])
NAUSICAA_DEFINE_VALUE([MAXSYMLINKS])
NAUSICAA_DEFINE_VALUE([NAME_MAX])
NAUSICAA_DEFINE_VALUE([PATH_MAX])
NAUSICAA_DEFINE_VALUE([MAXPATHLEN])
AC_CACHE_SAVE

dnl end of file
