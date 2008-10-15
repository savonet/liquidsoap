/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************/

// Add missing socket options from Unix module
// Stolen from http://caml.inria.fr/mantis/view.php?id=4484

#include <sys/types.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <caml/memory.h>
 
value setsockopt_int(int *sockopt, value socket, int level, value option, value status); /* from ocaml-3.08 sockopt.o */
 
/*
 * set TCP_NODELAY option on given socket
 */
 
/* set TCP_NODELAY option on given socket */
CAMLprim value stub_set_tcp_nodelay(value socket, value status)
{
  CAMLparam2(socket, status);
  int x = TCP_NODELAY; /* pass as array, get 0-nth element */
  CAMLreturn(setsockopt_int(&x, socket, IPPROTO_TCP, 0, status));
}


