/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/*  Modified in 2004 by Julien Cristau <julien.cristau@ens-lyon.org>   */
/*  to use the MAP_ANONYMOUS flag.                                     */
/*  Distributed under the GNU General Public License as part of        */
/*  Liquidsoap (http://savonet.sourceforge.net).                       */
/***********************************************************************/

/* $Id: mmap_anon.c,v 1.1 2004/12/12 00:38:46 jcristau Exp $ */

#include <stddef.h>
#include <string.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/config.h>
#include <caml/alloc.h>

extern int bigarray_element_size[];  /* from bigarray_stubs.c */

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#endif

#if defined(HAS_MMAP)

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

CAMLprim value liquidsoap_map_anon(value vkind, value vlayout,
                                 value vshared, value vdim)
{
  int flags, major_dim, shared;
  long num_dims, i;
  long dim[MAX_NUM_DIMS];
  unsigned long array_size;
  void * addr;

  flags = Int_val(vkind) | Int_val(vlayout);
  num_dims = Wosize_val(vdim);
  major_dim = flags & BIGARRAY_FORTRAN_LAYOUT ? num_dims - 1 : 0;
  /* Extract dimensions from Caml array */
  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > MAX_NUM_DIMS)
    invalid_argument("mmap_anon: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0 || dim[i] > 0x7FFFFFFFL)
      invalid_argument("mmap_anon: negative dimension");
  }
  /* Determine array size in bytes */
  /*(or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = bigarray_element_size[flags & BIGARRAY_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    array_size *= dim[i];
  /* Do the mmap */
  shared = Bool_val(vshared) ? MAP_SHARED : MAP_PRIVATE;
  addr = mmap(NULL, array_size, PROT_READ | PROT_WRITE,
	      shared | MAP_ANONYMOUS, 0, 0);
  if (addr == (void *) MAP_FAILED) caml_raise_sys_error(copy_string(""));
  /* Build and return the Caml bigarray */
  return alloc_bigarray(flags | BIGARRAY_MAPPED_FILE, num_dims, addr, dim);
}

#else

value liquidsoap_map_anon(value vkind, value vlayout,
                        value vshared, value vdim)
{
  invalid_argument("mmap_anon: not supported");
  return Val_unit;
}

#endif
