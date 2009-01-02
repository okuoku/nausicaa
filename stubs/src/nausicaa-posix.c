/*
   Part of: Nausicaa/Stubs
   Contents: POSIX stub
   Date: Fri Dec 19, 2008

   Abstract

	Stub  functions for  the  POSIX API.   This file  is
	meant to be compiled in  a C shared library and used
	by the Nausicaa/POSIX Scheme library.

   Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>

   This program  is free  software: you can  redistribute it
   and/or  modify it  under  the terms  of  the GNU  General
   Public  License   as  published  by   the  Free  Software
   Foundation, either version 3  of the License, or (at your
   option) any later version.

   This program is  distributed in the hope that  it will be
   useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the
   implied  warranty  of MERCHANTABILITY  or  FITNESS FOR  A
   PARTICULAR PURPOSE.   See the GNU  General Public License
   for more details.

   You should have received a copy of the GNU General Public
   License   along   with  this   program.    If  not,   see
   <http://www.gnu.org/licenses/>.
*/


/** ------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/times.h>
#include <sys/stat.h>

extern int nausicaa_posix_wifexited	(int status);
extern int nausicaa_posix_wexitstatus	(int status);
extern int nausicaa_posix_wifsignaled	(int status);
extern int nausicaa_posix_wtermsig	(int status);
extern int nausicaa_posix_wcoredump	(int status);
extern int nausicaa_posix_wifstopped	(int status);
extern int nausicaa_posix_wstopsig	(int status);

extern double nausicaa_posix_clock	(void);
extern double nausicaa_posix_times	(double * tms);

extern int nausicaa_posix_stat	(const char * pathname,
				 struct stat * buf);
extern int nausicaa_posix_fstat	(int filedes,
				 struct stat * buf);
extern int nausicaa_posix_lstat	(const char * pathname,
				 struct stat * buf);



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
 ** Stat functions.
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


/* end of file */
