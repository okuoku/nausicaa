/*
   Part of: Nausicaa/Stubs
   Contents: POSIX stub
   Date: Fri Dec 19, 2008

   Abstract

	Stub  functions for the  POSIX API.   This file  is meant  to be
	compiled in  a C shared  library and used by  the Nausicaa/POSIX
	Scheme library.

	  The existence of this  libraries is justified by the following
	facts:

	(1) Some functions  are different on 32 bit  platforms and on 64
	    bits platforms.   For example, under the GNU  C Library they
	    come in  two flavors "func()"  and "func64()"; this  goes as
	    far  as  defining  different  data structures  for  the  two
	    platforms.

	      There is  no (easy) way  to acquire through  "dlsym()" the
	    correct version and to  determine the correct data structure
	    accessors and mutators, while it  is very easy to export the
	    correct ones through this library.

	(2) Some  interesting  feature  is  implemented  through  the  C
	    preprocessor macros  only.  In this case it  is mandatory to
	    export a wrapper function from this library.

   Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>

   This program is free software:  you can redistribute it and/or modify
   it under the terms of the  GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   This program is  distributed in the hope that it  will be useful, but
   WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
   MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
   General Public License for more details.

   You should  have received  a copy of  the GNU General  Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_UTIME_H
#  include <utime.h>
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


/** --------------------------------------------------------------------
 ** Miscellaneous prototypes.
 ** ----------------------------------------------------------------- */

extern double nausicaa_posix_clock	(void);
extern double nausicaa_posix_times	(double * tms);

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
  return (double)clock();
}
double
nausicaa_posix_times (double * tms)
{
  struct tms	t;
  clock_t	result;

  result = times(&t);
  tms[0] = t.tms_utime;
  tms[1] = t.tms_stime;
  tms[2] = t.tms_cutime;
  tms[3] = t.tms_cstime;
  return (double)result;
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


/** --------------------------------------------------------------------
 ** Dirent structure accessors.
 ** ----------------------------------------------------------------- */

char *
nausicaa_posix_dirent_d_name_ptr_ref (struct dirent * buf)
{
  return &(buf->d_name[0]);
}


/** --------------------------------------------------------------------
 ** Miscellaneous.
 ** ----------------------------------------------------------------- */

/* end of file */
