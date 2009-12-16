dnl (foreign posix sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Wed Dec 16, 2009
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

dnl Struct inspection: iovec
NAUSICAA_INSPECT_STRUCT_TYPE([IOVEC],[struct iovec],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([IOVEC_IOV_BASE],[struct iovec],[iov_base],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([IOVEC_IOV_LEN],[struct iovec],[iov_len],[unsigned-int])

dnl Struct inspection: fdset
NAUSICAA_INSPECT_STRUCT_TYPE([FDSET],[fd_set],[#f])

dnl Struct inspection: passwd
NAUSICAA_INSPECT_STRUCT_TYPE([PASSWD],[struct passwd],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_NAME],[struct passwd],[pw_name],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_PASSWD],[struct passwd],[pw_passwd],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_UID],[struct passwd],[pw_uid],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_GID],[struct passwd],[pw_gid],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_GECOS],[struct passwd],[pw_gecos],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_DIR],[struct passwd],[pw_dir],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PASSWD_PW_SHELL],[struct passwd],[pw_shell],[pointer])

dnl Struct inspection: group
NAUSICAA_INSPECT_STRUCT_TYPE([GROUP],[struct group],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([GROUP_GR_NAME],[struct group],[gr_name],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([GROUP_GR_GID],[struct group],[gr_gid],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([GROUP_GR_MEM],[struct group],[gr_mem],[pointer])

dnl Struct inspection: utsname
NAUSICAA_INSPECT_STRUCT_TYPE([UTSNAME],[struct utsname],[#f])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_SYSNAME],[struct utsname],[sysname])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_RELEASE],[struct utsname],[release])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_VERSION],[struct utsname],[version])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_MACHINE],[struct utsname],[machine])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_NODENAME],[struct utsname],[nodename])
NAUSICAA_INSPECT_FIELD_TYPE_POINTER([UTSNAME_DOMAINNAME],[struct utsname],[domainname])

dnl Struct inspection: fstab
NAUSICAA_INSPECT_STRUCT_TYPE([FSTAB],[struct fstab],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_SPEC],[struct fstab],[fs_spec],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_FILE],[struct fstab],[fs_file],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_VFSTYPE],[struct fstab],[fs_vfstype],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_MNTOPS],[struct fstab],[fs_mntops],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_TYPE],[struct fstab],[fs_type],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_FREQ],[struct fstab],[fs_freq],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([FSTAB_FS_PASSNO],[struct fstab],[fs_passno],[signed-int])

dnl Struct inspection: mntent
NAUSICAA_INSPECT_STRUCT_TYPE([MNTENT],[struct mntent],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_FSNAME],[struct mntent],[mnt_fsname],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_DIR],[struct mntent],[mnt_dir],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_TYPE],[struct mntent],[mnt_type],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_OPTS],[struct mntent],[mnt_opts],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_FREQ],[struct mntent],[mnt_freq],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MNTENT_MNT_PASSNO],[struct mntent],[mnt_passno],[signed-int])
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
NAUSICAA_DEFINE_VALUE([MAXPATHLEN])

dnl Preprocessor symbols: mmap constants
NAUSICAA_DEFINE_VALUE([MAP_PRIVATE])
NAUSICAA_DEFINE_VALUE([MAP_SHARED])
NAUSICAA_DEFINE_VALUE([MAP_FIXED])
NAUSICAA_DEFINE_VALUE([MAP_ANON])
NAUSICAA_DEFINE_VALUE([MAP_ANONYMOUS])
NAUSICAA_DEFINE_VALUE([MAP_32BITS])
NAUSICAA_DEFINE_VALUE([MAP_DENYWRITE])
NAUSICAA_DEFINE_VALUE([MAP_EXECUTABLE])
NAUSICAA_DEFINE_VALUE([MAP_FILE])
NAUSICAA_DEFINE_VALUE([MAP_GROWSDOWN])
NAUSICAA_DEFINE_VALUE([MAP_LOCKED])
NAUSICAA_DEFINE_VALUE([MAP_NONBLOCK])
NAUSICAA_DEFINE_VALUE([MAP_NORESERVE])
NAUSICAA_DEFINE_VALUE([MAP_POPULATE])
NAUSICAA_DEFINE_VALUE([MAP_STACK])
NAUSICAA_DEFINE_VALUE([MAP_AUTOGROW])
NAUSICAA_DEFINE_VALUE([MAP_AUTORESRV])
NAUSICAA_DEFINE_VALUE([MAP_COPY])
NAUSICAA_DEFINE_VALUE([PROT_READ])
NAUSICAA_DEFINE_VALUE([PROT_WRITE])
NAUSICAA_DEFINE_VALUE([PROT_EXEC])
NAUSICAA_DEFINE_VALUE([PROT_NONE])
NAUSICAA_DEFINE_VALUE([MS_SYNC])
NAUSICAA_DEFINE_VALUE([MS_ASYNC])
NAUSICAA_DEFINE_VALUE([MREMAP_MAYMOVE])

dnl Preprocessor symbols: select related symbols
NAUSICAA_DEFINE_VALUE([FD_SETSIZE])

dnl Preprocessor symbols: max size of string for cuserid()
NAUSICAA_DEFINE_VALUE([L_cuserid])

dnl Preprocessor symbols: system capacity limitations
NAUSICAA_DEFINE_VALUE([ARG_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_ARG_MAX])
NAUSICAA_DEFINE_VALUE([CHILD_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_CHILD_MAX])
NAUSICAA_DEFINE_VALUE([OPEN_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_OPEN_MAX])
NAUSICAA_DEFINE_VALUE([STREAM_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_STREAM_MAX])
NAUSICAA_DEFINE_VALUE([TZNAME_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_TZNAME_MAX])
NAUSICAA_DEFINE_VALUE([NGROUPS_MAX])
NAUSICAA_DEFINE_VALUE([SSIZE_MAX])
NAUSICAA_DEFINE_VALUE([RE_DUP_MAX])

dnl Preprocessor symbols: overall system options
NAUSICAA_DEFINE_VALUE([_POSIX_JOB_CONTROL])
NAUSICAA_DEFINE_VALUE([_POSIX_SAVED_IDS])
NAUSICAA_DEFINE_VALUE([_POSIX2_C_DEV])
NAUSICAA_DEFINE_VALUE([_POSIX2_FORT_DEV])
NAUSICAA_DEFINE_VALUE([_POSIX2_FORT_RUN])
NAUSICAA_DEFINE_VALUE([_POSIX2_LOCALEDEF])
NAUSICAA_DEFINE_VALUE([_POSIX2_SW_DEV])

dnl Preprocessor symbols: supported POSIX version
NAUSICAA_DEFINE_VALUE([_POSIX_VERSION])
NAUSICAA_DEFINE_VALUE([_POSIX2_C_VERSION])

dnl Preprocessor symbols: sysconf constants
NAUSICAA_DEFINE_VALUE([_SC_ARG_MAX])
NAUSICAA_DEFINE_VALUE([_SC_CHILD_MAX])
NAUSICAA_DEFINE_VALUE([_SC_CLK_TCK])
NAUSICAA_DEFINE_VALUE([_SC_NGROUPS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_OPEN_MAX])
NAUSICAA_DEFINE_VALUE([_SC_STREAM_MAX])
NAUSICAA_DEFINE_VALUE([_SC_TZNAME_MAX])
NAUSICAA_DEFINE_VALUE([_SC_JOB_CONTROL])
NAUSICAA_DEFINE_VALUE([_SC_SAVED_IDS])
NAUSICAA_DEFINE_VALUE([_SC_REALTIME_SIGNALS])
NAUSICAA_DEFINE_VALUE([_SC_PRIORITY_SCHEDULING])
NAUSICAA_DEFINE_VALUE([_SC_TIMERS])
NAUSICAA_DEFINE_VALUE([_SC_ASYNCHRONOUS_IO])
NAUSICAA_DEFINE_VALUE([_SC_PRIORITIZED_IO])
NAUSICAA_DEFINE_VALUE([_SC_SYNCHRONIZED_IO])
NAUSICAA_DEFINE_VALUE([_SC_FSYNC])
NAUSICAA_DEFINE_VALUE([_SC_MAPPED_FILES])
NAUSICAA_DEFINE_VALUE([_SC_MEMLOCK])
NAUSICAA_DEFINE_VALUE([_SC_MEMLOCK_RANGE])
NAUSICAA_DEFINE_VALUE([_SC_MEMORY_PROTECTION])
NAUSICAA_DEFINE_VALUE([_SC_MESSAGE_PASSING])
NAUSICAA_DEFINE_VALUE([_SC_SEMAPHORES])
NAUSICAA_DEFINE_VALUE([_SC_SHARED_MEMORY_OBJECTS])
NAUSICAA_DEFINE_VALUE([_SC_AIO_LISTIO_MAX])
NAUSICAA_DEFINE_VALUE([_SC_AIO_MAX])
NAUSICAA_DEFINE_VALUE([_SC_AIO_PRIO_DELTA_MAX])
NAUSICAA_DEFINE_VALUE([_SC_DELAYTIMER_MAX])
NAUSICAA_DEFINE_VALUE([_SC_MQ_OPEN_MAX])
NAUSICAA_DEFINE_VALUE([_SC_MQ_PRIO_MAX])
NAUSICAA_DEFINE_VALUE([_SC_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_PAGESIZE])
NAUSICAA_DEFINE_VALUE([_SC_PAGE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_RTSIG_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SEM_NSEMS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SEM_VALUE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SIGQUEUE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_TIMER_MAX])
NAUSICAA_DEFINE_VALUE([_SC_BC_BASE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_BC_DIM_MAX])
NAUSICAA_DEFINE_VALUE([_SC_BC_SCALE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_BC_STRING_MAX])
NAUSICAA_DEFINE_VALUE([_SC_COLL_WEIGHTS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_EQUIV_CLASS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_EXPR_NEST_MAX])
NAUSICAA_DEFINE_VALUE([_SC_LINE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_RE_DUP_MAX])
NAUSICAA_DEFINE_VALUE([_SC_CHARCLASS_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_SC_2_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_2_C_BIND])
NAUSICAA_DEFINE_VALUE([_SC_2_C_DEV])
NAUSICAA_DEFINE_VALUE([_SC_2_FORT_DEV])
NAUSICAA_DEFINE_VALUE([_SC_2_FORT_RUN])
NAUSICAA_DEFINE_VALUE([_SC_2_SW_DEV])
NAUSICAA_DEFINE_VALUE([_SC_2_LOCALEDEF])
NAUSICAA_DEFINE_VALUE([_SC_PII])
NAUSICAA_DEFINE_VALUE([_SC_PII_XTI])
NAUSICAA_DEFINE_VALUE([_SC_PII_SOCKET])
NAUSICAA_DEFINE_VALUE([_SC_PII_INTERNET])
NAUSICAA_DEFINE_VALUE([_SC_PII_OSI])
NAUSICAA_DEFINE_VALUE([_SC_POLL])
NAUSICAA_DEFINE_VALUE([_SC_SELECT])
NAUSICAA_DEFINE_VALUE([_SC_UIO_MAXIOV])
NAUSICAA_DEFINE_VALUE([_SC_IOV_MAX])
NAUSICAA_DEFINE_VALUE([_SC_PII_INTERNET_STREAM])
NAUSICAA_DEFINE_VALUE([_SC_PII_INTERNET_DGRAM])
NAUSICAA_DEFINE_VALUE([_SC_PII_OSI_COTS])
NAUSICAA_DEFINE_VALUE([_SC_PII_OSI_CLTS])
NAUSICAA_DEFINE_VALUE([_SC_PII_OSI_M])
NAUSICAA_DEFINE_VALUE([_SC_T_IOV_MAX])
NAUSICAA_DEFINE_VALUE([_SC_THREADS])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_SAFE_FUNCTIONS])
NAUSICAA_DEFINE_VALUE([_SC_GETGR_R_SIZE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_GETPW_R_SIZE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_LOGIN_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_SC_TTY_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_DESTRUCTOR_ITERATIONS])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_KEYS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_STACK_MIN])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_THREADS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_ATTR_STACKADDR])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_ATTR_STACKSIZE])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_PRIORITY_SCHEDULING])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_PRIO_INHERIT])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_PRIO_PROTECT])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_PROCESS_SHARED])
NAUSICAA_DEFINE_VALUE([_SC_NPROCESSORS_CONF])
NAUSICAA_DEFINE_VALUE([_SC_NPROCESSORS_ONLN])
NAUSICAA_DEFINE_VALUE([_SC_PHYS_PAGES])
NAUSICAA_DEFINE_VALUE([_SC_AVPHYS_PAGES])
NAUSICAA_DEFINE_VALUE([_SC_ATEXIT_MAX])
NAUSICAA_DEFINE_VALUE([_SC_PASS_MAX])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_XCU_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_UNIX])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_CRYPT])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_ENH_I18N])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_SHM])
NAUSICAA_DEFINE_VALUE([_SC_2_CHAR_TERM])
NAUSICAA_DEFINE_VALUE([_SC_2_C_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_2_UPE])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_XPG2])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_XPG3])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_XPG4])
NAUSICAA_DEFINE_VALUE([_SC_CHAR_BIT])
NAUSICAA_DEFINE_VALUE([_SC_CHAR_MAX])
NAUSICAA_DEFINE_VALUE([_SC_CHAR_MIN])
NAUSICAA_DEFINE_VALUE([_SC_INT_MAX])
NAUSICAA_DEFINE_VALUE([_SC_INT_MIN])
NAUSICAA_DEFINE_VALUE([_SC_LONG_BIT])
NAUSICAA_DEFINE_VALUE([_SC_WORD_BIT])
NAUSICAA_DEFINE_VALUE([_SC_MB_LEN_MAX])
NAUSICAA_DEFINE_VALUE([_SC_NZERO])
NAUSICAA_DEFINE_VALUE([_SC_SSIZE_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SCHAR_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SCHAR_MIN])
NAUSICAA_DEFINE_VALUE([_SC_SHRT_MAX])
NAUSICAA_DEFINE_VALUE([_SC_SHRT_MIN])
NAUSICAA_DEFINE_VALUE([_SC_UCHAR_MAX])
NAUSICAA_DEFINE_VALUE([_SC_UINT_MAX])
NAUSICAA_DEFINE_VALUE([_SC_ULONG_MAX])
NAUSICAA_DEFINE_VALUE([_SC_USHRT_MAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_ARGMAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_LANGMAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_MSGMAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_NMAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_SETMAX])
NAUSICAA_DEFINE_VALUE([_SC_NL_TEXTMAX])
NAUSICAA_DEFINE_VALUE([_SC_XBS5_ILP32_OFF32])
NAUSICAA_DEFINE_VALUE([_SC_XBS5_ILP32_OFFBIG])
NAUSICAA_DEFINE_VALUE([_SC_XBS5_LP64_OFF64])
NAUSICAA_DEFINE_VALUE([_SC_XBS5_LPBIG_OFFBIG])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_LEGACY])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_REALTIME])
NAUSICAA_DEFINE_VALUE([_SC_XOPEN_REALTIME_THREADS])
NAUSICAA_DEFINE_VALUE([_SC_ADVISORY_INFO])
NAUSICAA_DEFINE_VALUE([_SC_BARRIERS])
NAUSICAA_DEFINE_VALUE([_SC_BASE])
NAUSICAA_DEFINE_VALUE([_SC_C_LANG_SUPPORT])
NAUSICAA_DEFINE_VALUE([_SC_C_LANG_SUPPORT_R])
NAUSICAA_DEFINE_VALUE([_SC_CLOCK_SELECTION])
NAUSICAA_DEFINE_VALUE([_SC_CPUTIME])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_CPUTIME])
NAUSICAA_DEFINE_VALUE([_SC_DEVICE_IO])
NAUSICAA_DEFINE_VALUE([_SC_DEVICE_SPECIFIC])
NAUSICAA_DEFINE_VALUE([_SC_DEVICE_SPECIFIC_R])
NAUSICAA_DEFINE_VALUE([_SC_FD_MGMT])
NAUSICAA_DEFINE_VALUE([_SC_FIFO])
NAUSICAA_DEFINE_VALUE([_SC_PIPE])
NAUSICAA_DEFINE_VALUE([_SC_FILE_ATTRIBUTES])
NAUSICAA_DEFINE_VALUE([_SC_FILE_LOCKING])
NAUSICAA_DEFINE_VALUE([_SC_FILE_SYSTEM])
NAUSICAA_DEFINE_VALUE([_SC_MONOTONIC_CLOCK])
NAUSICAA_DEFINE_VALUE([_SC_MULTI_PROCESS])
NAUSICAA_DEFINE_VALUE([_SC_SINGLE_PROCESS])
NAUSICAA_DEFINE_VALUE([_SC_NETWORKING])
NAUSICAA_DEFINE_VALUE([_SC_READER_WRITER_LOCKS])
NAUSICAA_DEFINE_VALUE([_SC_SPIN_LOCKS])
NAUSICAA_DEFINE_VALUE([_SC_REGEXP])
NAUSICAA_DEFINE_VALUE([_SC_REGEX_VERSION])
NAUSICAA_DEFINE_VALUE([_SC_SHELL])
NAUSICAA_DEFINE_VALUE([_SC_SIGNALS])
NAUSICAA_DEFINE_VALUE([_SC_SPAWN])
NAUSICAA_DEFINE_VALUE([_SC_SPORADIC_SERVER])
NAUSICAA_DEFINE_VALUE([_SC_THREAD_SPORADIC_SERVER])
NAUSICAA_DEFINE_VALUE([_SC_SYSTEM_DATABASE])
NAUSICAA_DEFINE_VALUE([_SC_SYSTEM_DATABASE_R])
NAUSICAA_DEFINE_VALUE([_SC_TIMEOUTS])
NAUSICAA_DEFINE_VALUE([_SC_TYPED_MEMORY_OBJECTS])
NAUSICAA_DEFINE_VALUE([_SC_USER_GROUPS])
NAUSICAA_DEFINE_VALUE([_SC_USER_GROUPS_R])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS_ACCOUNTING])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS_LOCATE])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS_MESSAGE])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS_TRACK])
NAUSICAA_DEFINE_VALUE([_SC_SYMLOOP_MAX])
NAUSICAA_DEFINE_VALUE([_SC_STREAMS])
NAUSICAA_DEFINE_VALUE([_SC_2_PBS_CHECKPOINT])
NAUSICAA_DEFINE_VALUE([_SC_V6_ILP32_OFF32])
NAUSICAA_DEFINE_VALUE([_SC_V6_ILP32_OFFBIG])
NAUSICAA_DEFINE_VALUE([_SC_V6_LP64_OFF64])
NAUSICAA_DEFINE_VALUE([_SC_V6_LPBIG_OFFBIG])
NAUSICAA_DEFINE_VALUE([_SC_HOST_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_SC_TRACE])
NAUSICAA_DEFINE_VALUE([_SC_TRACE_EVENT_FILTER])
NAUSICAA_DEFINE_VALUE([_SC_TRACE_INHERIT])
NAUSICAA_DEFINE_VALUE([_SC_TRACE_LOG])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_ICACHE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_ICACHE_ASSOC])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_ICACHE_LINESIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_DCACHE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_DCACHE_ASSOC])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL1_DCACHE_LINESIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL2_CACHE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL2_CACHE_ASSOC])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL2_CACHE_LINESIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL3_CACHE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL3_CACHE_ASSOC])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL3_CACHE_LINESIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL4_CACHE_SIZE])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL4_CACHE_ASSOC])
NAUSICAA_DEFINE_VALUE([_SC_LEVEL4_CACHE_LINESIZE])
NAUSICAA_DEFINE_VALUE([_SC_IPV6])
NAUSICAA_DEFINE_VALUE([_SC_RAW_SOCKETS])

dnl Preprocessor symbols: limits on file system capacity
NAUSICAA_DEFINE_VALUE([LINK_MAX])
NAUSICAA_DEFINE_VALUE([MAX_CANON])
NAUSICAA_DEFINE_VALUE([MAX_INPUT])
NAUSICAA_DEFINE_VALUE([NAME_MAX])
NAUSICAA_DEFINE_VALUE([PATH_MAX])
NAUSICAA_DEFINE_VALUE([PIPE_BUF])
NAUSICAA_DEFINE_VALUE([MAXNAMLEN])
NAUSICAA_DEFINE_VALUE([FILENAME_MAX])

dnl Preprocessor symbols: optional features in file support
NAUSICAA_DEFINE_VALUE([_POSIX_CHOWN_RESTRICTED])
NAUSICAA_DEFINE_VALUE([_POSIX_NO_TRUNC])
NAUSICAA_DEFINE_VALUE([_POSIX_VDISABLE])

dnl Preprocessor symbols: minimum values for file system limits
NAUSICAA_DEFINE_VALUE([_POSIX_LINK_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_MAX_CANON])
NAUSICAA_DEFINE_VALUE([_POSIX_MAX_INPUT])
NAUSICAA_DEFINE_VALUE([_POSIX_NAME_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_PATH_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_PIPE_BUF])
NAUSICAA_DEFINE_VALUE([SYMLINK_MAX])
NAUSICAA_DEFINE_VALUE([POSIX_REC_INCR_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([POSIX_REC_MAX_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([POSIX_REC_MIN_XFER_SIZE])
NAUSICAA_DEFINE_VALUE([POSIX_REC_XFER_ALIGN])

dnl Preprocessor symbols: constants for pathconf and fpathconf
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
NAUSICAA_DEFINE_VALUE([_PC_REC_XFER_ALIGN])

dnl Preprocessor symbols: glibc system inspection constants
NAUSICAA_DEFINE_VALUE([BC_BASE_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_BC_BASE_MAX])
NAUSICAA_DEFINE_VALUE([BC_DIM_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_BC_DIM_MAX])
NAUSICAA_DEFINE_VALUE([BC_SCALE_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_BC_SCALE_MAX])
NAUSICAA_DEFINE_VALUE([BC_STRING_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_BC_STRING_MAX])
NAUSICAA_DEFINE_VALUE([COLL_WEIGHTS_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_COLL_WEIGHTS_MAX])
NAUSICAA_DEFINE_VALUE([EXPR_NEXT_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_EXPR_NEXT_MAX])
NAUSICAA_DEFINE_VALUE([LINE_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_LINE_MAX])
NAUSICAA_DEFINE_VALUE([EQUIV_CLASS_MAX])
NAUSICAA_DEFINE_VALUE([_POSIX_EQUIV_CLASS_MAX])

dnl Preprocessor symbols: string-valued system configuration parameters
NAUSICAA_DEFINE_VALUE([_CS_PATH])
NAUSICAA_DEFINE_VALUE([_CS_LFS_CFLAGS])
NAUSICAA_DEFINE_VALUE([_CS_LFS_LDFLAGS])
NAUSICAA_DEFINE_VALUE([_CS_LFS_LIBS])
NAUSICAA_DEFINE_VALUE([_CS_LFS_LINTFLAGS])
NAUSICAA_DEFINE_VALUE([_CS_LFS64_CFLAGS])
NAUSICAA_DEFINE_VALUE([_CS_LFS64_LDFLAGS])
NAUSICAA_DEFINE_VALUE([_CS_LFS64_LIBS])
NAUSICAA_DEFINE_VALUE([_CS_LFS64_LINTFLAGS])

dnl String preprocessor symbols: file system files pathnames
NAUSICAA_STRING_TEST([_PATH_MNTTAB],[_PATH_MNTTAB])
NAUSICAA_STRING_TEST([_PATH_FSTAB],[_PATH_FSTAB])
NAUSICAA_STRING_TEST([_PATH_MOUNTED],[_PATH_MOUNTED])

dnl String preprocessor symbols: mount options
NAUSICAA_STRING_TEST([FSTAB_RW],[FSTAB_RW])
NAUSICAA_STRING_TEST([FSTAB_RQ],[FSTAB_RQ])
NAUSICAA_STRING_TEST([FSTAB_RO],[FSTAB_RO])
NAUSICAA_STRING_TEST([FSTAB_SW],[FSTAB_SW])
NAUSICAA_STRING_TEST([FSTAB_XX],[FSTAB_XX])

dnl String preprocessor symbols: values for the mnt_type field of struct mtab
NAUSICAA_STRING_TEST([MNTTYPE_IGNORE],[MNTTYPE_IGNORE])
NAUSICAA_STRING_TEST([MNTTYPE_NFS],[MNTTYPE_NFS])
NAUSICAA_STRING_TEST([MNTTYPE_SWAP],[MNTTYPE_SWAP])

dnl String preprocessor symbols: values for the mnt_opts field of struct mtab
NAUSICAA_STRING_TEST([MNTOPT_DEFAULTS],[MNTOPT_DEFAULTS])
NAUSICAA_STRING_TEST([MNTOPT_RO],[MNTOPT_RO])
NAUSICAA_STRING_TEST([MNTOPT_RW],[MNTOPT_RW])
NAUSICAA_STRING_TEST([MNTOPT_SUID],[MNTOPT_SUID])
NAUSICAA_STRING_TEST([MNTOPT_NOSUID],[MNTOPT_NOSUID])
NAUSICAA_STRING_TEST([MNTOPT_NOAUTO],[MNTOPT_NOAUTO])

dnl Preprocessor symbols: options for mount
NAUSICAA_DEFINE_VALUE([MS_MGC_MASK])
NAUSICAA_DEFINE_VALUE([MS_REMOUNT])
NAUSICAA_DEFINE_VALUE([MS_RDONLY])
NAUSICAA_DEFINE_VALUE([S_IMMUTABLE])
NAUSICAA_DEFINE_VALUE([S_APPEND])
NAUSICAA_DEFINE_VALUE([MS_NOSUID])
NAUSICAA_DEFINE_VALUE([MS_NOEXEC])
NAUSICAA_DEFINE_VALUE([MS_NODEV])
NAUSICAA_DEFINE_VALUE([MS_SYNCHRONOUS])
NAUSICAA_DEFINE_VALUE([MS_MANDLOCK])
NAUSICAA_DEFINE_VALUE([MS_NOATIME])
NAUSICAA_DEFINE_VALUE([MS_NODIRATIME])

dnl Preprocessor symbols: flags for umount2
NAUSICAA_DEFINE_VALUE([MNT_FORCE])
AC_CACHE_SAVE

dnl end of file
