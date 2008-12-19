/*
   Part of: Nausicaa/Stubs
   Contents: POSIX stub
   Date: Fri Dec 19, 2008
   Time-stamp: <2008-12-19 18:23:01 marco>

   Abstract



   Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>

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

#include <sys/wait.h>

extern int nausicaa_posix_wifexited	(int status);
extern int nausicaa_posix_wexitstatus	(int status);
extern int nausicaa_posix_wifsignaled	(int status);
extern int nausicaa_posix_wtermsig	(int status);
extern int nausicaa_posix_wcoredump	(int status);
extern int nausicaa_posix_wifstopped	(int status);
extern int nausicaa_posix_wstopsig	(int status);



/** ------------------------------------------------------------
 ** Code.
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

/* end of file */
