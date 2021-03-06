/*
  Part of: Nausicaa/Stubs
  Contents: POSIX stub
  Date: Fri Dec 19, 2008

  Abstract

        Stub  functions for the  POSIX API.   This file  is meant  to be
        compiled in  a C shared  library and used by  the Nausicaa/POSIX
        Scheme library.

        The existence  of this libraries  is justified by  the following
        facts:

        (1) Some functions  are different on 32 bit  platforms and on 64
            bits platforms.   For example, under the GNU  C Library they
            come in  two flavors "func()"  and "func64()"; this  goes as
            far  as  defining  different  data structures  for  the  two
            platforms.

            There  is no  (easy) way  to acquire  through  "dlsym()" the
            correct version and to  determine the correct data structure
            accessors and mutators, while it  is very easy to export the
            correct ones through this library.

        (2)  Some  interesting  feature  is implemented  through  the  C
            preprocessor macros  only.  In this case it  is mandatory to
            export a wrapper function from this library.

  Copyright (c) 2008-2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this  program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** ------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_FTW_H
#  include <ftw.h>
#endif
#ifdef HAVE_NET_IF_H
#  include <net/if.h>
#endif
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
#ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
#endif
#ifdef HAVE_SYS_TIMEX_H
#  include <sys/timex.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

#ifdef __GNUC__
#  define POSIX_UNUSED		__attribute__((unused))
#else
#  define POSIX_UNUSED		/* empty */
#  define __attribute__(...)	/* empty */
#endif


/** --------------------------------------------------------------------
 ** Waitpid related prototypes.
 ** ----------------------------------------------------------------- */

extern int nausicaa_posix_wifexited	(int status);
extern int nausicaa_posix_wexitstatus	(int status);
extern int nausicaa_posix_wifsignaled	(int status);
extern int nausicaa_posix_wtermsig	(int status);
extern int nausicaa_posix_wcoredump	(int status);
extern int nausicaa_posix_wifstopped	(int status);
extern int nausicaa_posix_wstopsig	(int status);


/** --------------------------------------------------------------------
 ** Stat related prototypes.
 ** ----------------------------------------------------------------- */

extern int nausicaa_posix_stat		(const char * pathname, struct stat * buf);
extern int nausicaa_posix_fstat		(int filedes, struct stat * buf);
extern int nausicaa_posix_lstat		(const char * pathname, struct stat * buf);
extern int nausicaa_posix_mknod		(char*, int, int);

extern int nausicaa_posix_sizeof_stat	(void);

extern mode_t	nausicaa_posix_stat_st_mode_ref (struct stat * buf);
extern ino_t	nausicaa_posix_stat_st_ino_ref (struct stat * buf);
extern dev_t	nausicaa_posix_stat_st_dev_ref (struct stat * buf);
extern nlink_t	nausicaa_posix_stat_st_nlink_ref (struct stat * buf);
extern uid_t	nausicaa_posix_stat_st_uid_ref (struct stat * buf);
extern gid_t	nausicaa_posix_stat_st_gid_ref (struct stat * buf);
extern off_t	nausicaa_posix_stat_st_size_ref (struct stat * buf);
extern time_t	nausicaa_posix_stat_st_atime_ref (struct stat * buf);
extern unsigned long int nausicaa_posix_stat_st_atime_usec_ref (struct stat * buf);
extern time_t	nausicaa_posix_stat_st_mtime_ref (struct stat * buf);
extern unsigned long int nausicaa_posix_stat_st_mtime_usec_ref (struct stat * buf);
extern time_t	nausicaa_posix_stat_st_ctime_ref (struct stat * buf);
extern unsigned long int nausicaa_posix_stat_st_ctime_usec_ref (struct stat * buf);
extern blkcnt_t	nausicaa_posix_stat_st_blocks_ref (struct stat * buf);
extern unsigned	nausicaa_posix_stat_st_blksize_ref (struct stat * buf);

extern int nausicaa_posix_stat_is_dir	(mode_t m);
extern int nausicaa_posix_stat_is_chr	(mode_t m);
extern int nausicaa_posix_stat_is_blk	(mode_t m);
extern int nausicaa_posix_stat_is_reg	(mode_t m);
extern int nausicaa_posix_stat_is_fifo	(mode_t m);
extern int nausicaa_posix_stat_is_lnk	(mode_t m);
extern int nausicaa_posix_stat_is_sock	(mode_t m);

extern int nausicaa_posix_stat_typeismq	(struct stat * s);
extern int nausicaa_posix_stat_typeissem(struct stat * s);
extern int nausicaa_posix_stat_typeisshm(struct stat * s);


/** --------------------------------------------------------------------
 ** Prototypes for 32/64 bits function wrappers.
 ** ----------------------------------------------------------------- */

extern int nausicaa_posix_pread  (int filedes, void * buffer, size_t size, off_t offset);
extern int nausicaa_posix_pwrite (int filedes, const void * buffer, size_t size, off_t offset);

extern int nausicaa_posix_lseek (int filedes, off_t offset, int whence);
extern int nausicaa_posix_fseek  (FILE * stream, long int offset, int whence);
extern int nausicaa_posix_fseeko (FILE * stream, off_t offset, int whence);
extern off_t nausicaa_posix_ftello (FILE * stream);

extern int nausicaa_posix_truncate (const char * filename, off_t length);
extern int nausicaa_posix_ftruncate (int fd, off_t length);

extern struct dirent * nausicaa_posix_readdir	(DIR * dir);
extern int             nausicaa_posix_readdir_r (DIR * dir, struct dirent * entry,
						 struct dirent ** result);

extern int nausicaa_posix_scandir (const char * dir, struct dirent *** namelist,
				   int (*selector) (const struct dirent *),
				   int (*cmp) (const void *, const void *));

extern int nausicaa_posix_alphasort (const void * a, const void * b);
extern int nausicaa_posix_versionsort (const void * a, const void * b);

extern int nausicaa_posix_ftw (const char * filename, __ftw_func_t func, int descriptors);
extern int nausicaa_posix_nftw (const char * filename, __nftw_func_t func, int descriptors, int flag);

extern void * nausicaa_posix_mmap (void * address, size_t length, int protect,
				   int flags, int fd, off_t offset);


/** --------------------------------------------------------------------
 ** Time related function prototypes.
 ** ----------------------------------------------------------------- */

extern double nausicaa_posix_clock	(void);
extern double nausicaa_posix_times	(struct tms * buffer);
extern double nausicaa_posix_time	(void);
extern int    nausicaa_posix_stime	(double tim);
extern struct tm * nausicaa_posix_localtime_r (double tim, struct tm * result);
extern struct tm * nausicaa_posix_gmtime_r    (double tim, struct tm * result);
extern double nausicaa_posix_timelocal  (struct tm * buffer);
extern double nausicaa_posix_timegm     (struct tm * buffer);

/* "struct tms" accessors */
extern double nausicaa_posix_tms_utime_ref (struct tms * T);
extern double nausicaa_posix_tms_stime_ref (struct tms * T);
extern double nausicaa_posix_tms_cutime_ref (struct tms * T);
extern double nausicaa_posix_tms_cstime_ref (struct tms * T);

/* "struct tms" mutators */
extern void nausicaa_posix_tms_utime_set (struct tms * T, double time);
extern void nausicaa_posix_tms_stime_set (struct tms * T, double time);
extern void nausicaa_posix_tms_cutime_set (struct tms * T, double time);
extern void nausicaa_posix_tms_cstime_set (struct tms * T, double time);

extern char * nausicaa_posix_ctime_r (double tim, char * cstr);


/** --------------------------------------------------------------------
 ** Select related function prototypes.
 ** ----------------------------------------------------------------- */

extern void nausicaa_posix_FD_ZERO	(fd_set * set);
extern void nausicaa_posix_FD_SET	(int fd, fd_set * set);
extern void nausicaa_posix_FD_CLR	(int fd, fd_set * set);
extern int  nausicaa_posix_FD_ISSET	(int fd, const fd_set * set);


/** --------------------------------------------------------------------
 ** Interprocess signal handling.
 ** ----------------------------------------------------------------- */

extern void	nausicaa_posix_signal_bub_init		(void);
extern void	nausicaa_posix_signal_bub_final		(void);
extern void	nausicaa_posix_signal_bub_acquire	(void);
extern int	nausicaa_posix_signal_bub_delivered	(int signum);


/** --------------------------------------------------------------------
 ** Socket functions.
 ** ----------------------------------------------------------------- */

extern int nausicaa_posix_SUN_LEN (struct sockaddr_un * p);


/** --------------------------------------------------------------------
 ** Miscellaneous prototypes.
 ** ----------------------------------------------------------------- */

extern char * nausicaa_posix_dirent_d_name_ptr_ref (struct dirent * buf);



/** ------------------------------------------------------------
 ** Process termination status returned by waitpid functions.
 ** ----------------------------------------------------------*/

int
nausicaa_posix_wifexited (int status)
{
  return WIFEXITED(status);
}
int
nausicaa_posix_wexitstatus (int status)
{
  return WEXITSTATUS(status);
}
int
nausicaa_posix_wifsignaled (int status)
{
  return WIFSIGNALED(status);
}
int
nausicaa_posix_wtermsig (int status)
{
  return WTERMSIG(status);
}
int
nausicaa_posix_wcoredump (int status)
{
  return WCOREDUMP(status);
}
int
nausicaa_posix_wifstopped (int status)
{
  return WIFSTOPPED(status);
}
int
nausicaa_posix_wstopsig (int status)
{
  return WSTOPSIG(status);
}


/** ------------------------------------------------------------
 ** Time and date functions.
 ** ----------------------------------------------------------*/

double
nausicaa_posix_clock (void)
{
  clock_t	result = clock();
  return (double)result;
}
double
nausicaa_posix_times (struct tms * buffer)
{
  clock_t	result = times(buffer);
  return (double)result;
}
double
nausicaa_posix_time (void)
{
  time_t	result = time(NULL);
  return (double)result;
}
int
nausicaa_posix_stime (double tim)
{
  time_t	t = (time_t)tim;
  return stime(&t);
}
struct tm *
nausicaa_posix_localtime_r (double tim, struct tm * result)
{
  time_t	t = (time_t)tim;
  return localtime_r (&t, result);
}
struct tm *
nausicaa_posix_gmtime_r (double tim, struct tm * result)
{
  time_t	t = (time_t)tim;
  return gmtime_r (&t, result);
}
double
nausicaa_posix_timelocal (struct tm * buffer)
{
  time_t	result = timelocal(buffer);
  return (double)result;
}
double
nausicaa_posix_timegm (struct tm * buffer)
{
  time_t	result = timegm(buffer);
  return (double)result;
}

/* ------------------------------------------------------------------ */

double
nausicaa_posix_tms_utime_ref (struct tms * T)
{
  return (double)(T->tms_utime);
}
double
nausicaa_posix_tms_stime_ref (struct tms * T)
{
  return (double)(T->tms_stime);
}
double
nausicaa_posix_tms_cutime_ref (struct tms * T)
{
  return (double)(T->tms_cutime);
}
double
nausicaa_posix_tms_cstime_ref (struct tms * T)
{
  return (double)(T->tms_cstime);
}
void
nausicaa_posix_tms_utime_set (struct tms * T, double tim)
{
  T->tms_utime = (time_t)tim;
}
void
nausicaa_posix_tms_stime_set (struct tms * T, double tim)
{
  T->tms_stime = (time_t)tim;
}
void
nausicaa_posix_tms_cutime_set (struct tms * T, double tim)
{
  T->tms_cutime = (time_t)tim;
}
void
nausicaa_posix_tms_cstime_set (struct tms * T, double tim)
{
  T->tms_cstime = (time_t)tim;
}
char *
nausicaa_posix_ctime_r (double tim, char * cstr)
{
  time_t	t = (time_t)tim;
  return ctime_r(&t, cstr);
}


/** ------------------------------------------------------------
 ** Stat functions and related.
 ** ----------------------------------------------------------*/

/* It is  a misfortune that  stubs functions are  needed but
   invoking "dlsym()"  on the stat functions  fails with all
   the Scheme  implementations.  If someone  has a solution:
   email me!!! */

int
nausicaa_posix_stat (const char * pathname, struct stat * buf)
{
  return stat(pathname, buf);
}
int
nausicaa_posix_fstat (int filedes, struct stat * buf)
{
  return fstat(filedes, buf);
}
int
nausicaa_posix_lstat (const char * pathname, struct stat * buf)
{
  return lstat(pathname, buf);
}
int
nausicaa_posix_mknod (char* filename, int mode, int dev)
{
  return mknod(filename, mode, dev);
}

/* ------------------------------------------------------------------ */

int
nausicaa_posix_sizeof_stat (void)
{
  return sizeof(struct stat);
}

/* ------------------------------------------------------------------ */

mode_t
nausicaa_posix_stat_st_mode_ref (struct stat * buf)
{
  return buf->st_mode;
}
ino_t
nausicaa_posix_stat_st_ino_ref (struct stat * buf)
{
  return buf->st_ino;
}
dev_t
nausicaa_posix_stat_st_dev_ref (struct stat * buf)
{
  return buf->st_dev;
}
nlink_t
nausicaa_posix_stat_st_nlink_ref (struct stat * buf)
{
  return buf->st_nlink;
}
uid_t
nausicaa_posix_stat_st_uid_ref (struct stat * buf)
{
  return buf->st_uid;
}
gid_t
nausicaa_posix_stat_st_gid_ref (struct stat * buf)
{
  return buf->st_gid;
}
off_t
nausicaa_posix_stat_st_size_ref (struct stat * buf)
{
  return buf->st_size;
}
time_t
nausicaa_posix_stat_st_atime_ref (struct stat * buf)
{
  return buf->st_atime;
}
#ifdef HAVE_ST_ATIME_USEC_MEMBER
unsigned long int
nausicaa_posix_stat_st_atime_usec_ref (struct stat * buf)
{
  return buf->st_atime_usec;
}
#else
unsigned long int
nausicaa_posix_stat_st_atime_usec_ref (struct stat * buf POSIX_UNUSED)
{
  return 0;
}
#endif
time_t
nausicaa_posix_stat_st_mtime_ref (struct stat * buf)
{
  return buf->st_mtime;
}
#ifdef HAVE_ST_MTIME_USEC_MEMBER
unsigned long int
nausicaa_posix_stat_st_mtime_usec_ref (struct stat * buf)
{
  return buf->st_mtime_usec;
}
#else
unsigned long int
nausicaa_posix_stat_st_mtime_usec_ref (struct stat * buf POSIX_UNUSED)
{
  return 0;
}
#endif
time_t
nausicaa_posix_stat_st_ctime_ref (struct stat * buf)
{
  return buf->st_ctime;
}
#ifdef HAVE_ST_CTIME_USEC_MEMBER
unsigned long int
nausicaa_posix_stat_st_ctime_usec_ref (struct stat * buf)
{
  return buf->st_ctime_usec;
}
#else
unsigned long int
nausicaa_posix_stat_st_ctime_usec_ref (struct stat * buf POSIX_UNUSED)
{
  return 0;
}
#endif
blkcnt_t
nausicaa_posix_stat_st_blocks_ref (struct stat * buf)
{
  return buf->st_blocks;
}
unsigned int
nausicaa_posix_stat_st_blksize_ref (struct stat * buf)
{
  return buf->st_blksize;
}

/* ------------------------------------------------------------ */

int
nausicaa_posix_stat_is_dir (mode_t m)
{
  return S_ISDIR(m);
}
int
nausicaa_posix_stat_is_chr (mode_t m)
{
  return S_ISCHR(m);
}
int
nausicaa_posix_stat_is_blk (mode_t m)
{
  return S_ISBLK(m);
}
int
nausicaa_posix_stat_is_reg (mode_t m)
{
  return S_ISREG(m);
}
int
nausicaa_posix_stat_is_fifo (mode_t m)
{
  return S_ISFIFO(m);
}
int
nausicaa_posix_stat_is_lnk (mode_t m)
{
  return S_ISLNK(m);
}
int
nausicaa_posix_stat_is_sock (mode_t m)
{
  return S_ISSOCK(m);
}

/* ------------------------------------------------------------ */

int
nausicaa_posix_stat_typeismq (struct stat * s)
{
  return S_TYPEISMQ(s);
}
int
nausicaa_posix_stat_typeissem (struct stat * s)
{
  return S_TYPEISSEM(s);
}
int
nausicaa_posix_stat_typeisshm (struct stat * s)
{
  return S_TYPEISSEM(s);
}


/** --------------------------------------------------------------------
 ** 32/64 bits function wrappers.
 ** ----------------------------------------------------------------- */

int
nausicaa_posix_pread (int filedes, void * buffer, size_t size, off_t offset)
{
  return pread (filedes, buffer, size, offset);
}
int
nausicaa_posix_pwrite (int filedes, const void * buffer, size_t size, off_t offset)
{
  return pwrite (filedes, buffer, size, offset);
}

int
nausicaa_posix_lseek (int filedes, off_t offset, int whence)
{
  return lseek(filedes, offset, whence);
}
int
nausicaa_posix_fseek (FILE * stream, long int offset, int whence)
{
  return fseek (stream, offset, whence);
}
int
nausicaa_posix_fseeko (FILE * stream, off_t offset, int whence)
{
  return fseeko (stream, offset, whence);
}
off_t
nausicaa_posix_ftello (FILE * stream)
{
  return ftello (stream);
}
int
nausicaa_posix_truncate (const char * filename, off_t length)
{
  return truncate (filename, length);
}
int
nausicaa_posix_ftruncate (int fd, off_t length)
{
  return ftruncate (fd, length);
}
struct dirent *
nausicaa_posix_readdir (DIR * dir)
{
  return readdir(dir);
}
int
nausicaa_posix_readdir_r (DIR * dir, struct dirent * entry, struct dirent ** result)
{
  return readdir_r(dir, entry, result);
}
int
nausicaa_posix_scandir (const char * dir, struct dirent *** namelist,
			int (*selector) (const struct dirent *),
			int (*cmp) (const void *, const void *))
{
  return scandir (dir, namelist, selector, cmp);
}
int
nausicaa_posix_alphasort (const void * a, const void * b)
{
  return alphasort (a, b);
}
int
nausicaa_posix_versionsort (const void * a, const void * b)
{
  return versionsort (a, b);
}
int
nausicaa_posix_ftw (const char * filename, __ftw_func_t func, int descriptors)
{
  return ftw (filename, func, descriptors);
}
int
nausicaa_posix_nftw (const char * filename, __nftw_func_t func, int descriptors, int flag)
{
  return nftw (filename, func, descriptors, flag);
}
void *
nausicaa_posix_mmap (void * address, size_t length, int protect, int flags, int fd, off_t offset)
{
  return mmap(address, length, protect, flags, fd, offset);
}


/** --------------------------------------------------------------------
 ** Select related functions.
 ** ----------------------------------------------------------------- */

void
nausicaa_posix_FD_ZERO (fd_set * set)
{
  FD_ZERO(set);
}
void
nausicaa_posix_FD_SET (int fd, fd_set * set)
{
  FD_SET(fd, set);
}
void
nausicaa_posix_FD_CLR (int fd, fd_set * set)
{
  FD_CLR(fd, set);
}
int
nausicaa_posix_FD_ISSET (int fd, const fd_set * set)
{
  return FD_ISSET(fd, set);
}


/** --------------------------------------------------------------------
 ** Interprocess signal handling.
 ** ----------------------------------------------------------------- */

static int	arrived_signals[NSIG];
static sigset_t	all_signals_set;

static void
nausicaa_posix_signal_bub_handler (int signum)
{
  ++arrived_signals[signum];
}
void
nausicaa_posix_signal_bub_init (void)
{ /* Block all the signals and register our handler for each. */
  struct sigaction	ac = {
    .sa_handler	= nausicaa_posix_signal_bub_handler,
    .sa_flags	= SA_RESTART | SA_NOCLDSTOP
  };
  sigfillset(&all_signals_set);
  sigprocmask(SIG_BLOCK, &all_signals_set, NULL);
  for (int signum=0; signum<NSIG; ++signum) {
    arrived_signals[signum] = 0;
    sigaction(signum, &ac, NULL);
  }
}
void
nausicaa_posix_signal_bub_final (void)
{ /* Set all the handlers to SIG_IGN, then unblock the signals. */
  struct sigaction	ac = {
    .sa_handler	= SIG_IGN,
    .sa_flags	= SA_RESTART
  };
  for (int signum=0; signum<NSIG; ++signum) {
    arrived_signals[signum] = 0;
    sigaction(signum, &ac, NULL);
  }
  sigprocmask(SIG_UNBLOCK, &all_signals_set, NULL);
}
void
nausicaa_posix_signal_bub_acquire (void)
{ /* Unblock then block all the signals.  This causes blocked signals to
     be delivered. */
  sigprocmask(SIG_UNBLOCK, &all_signals_set, NULL);
  sigprocmask(SIG_BLOCK,   &all_signals_set, NULL);
}
int
nausicaa_posix_signal_bub_delivered (int signum)
{ /* Return true if  the signal SIGNUM has been  delivered at least once
     since  the   last  call  to  "nausicaa_posix_signal_bub_acquire()".
     Clear the signal flag. */
  int	is_set = arrived_signals[signum];
  arrived_signals[signum] = 0;
  return is_set;
}


/** --------------------------------------------------------------------
 ** Socket functions.
 ** ----------------------------------------------------------------- */

int
nausicaa_posix_SUN_LEN (struct sockaddr_un * p)
{
  return SUN_LEN(p);
}


/** --------------------------------------------------------------------
 ** Miscellaneous.
 ** ----------------------------------------------------------------- */

char *
nausicaa_posix_dirent_d_name_ptr_ref (struct dirent * buf)
{
  return &(buf->d_name[0]);
}


/* end of file */
