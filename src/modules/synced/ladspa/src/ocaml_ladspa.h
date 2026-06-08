/*
 * Copyright 2007-2010 Savonet team
 *
 * This file is part of ocaml-ladspa.
 *
 * ocaml-ladspa is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ladspa is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ladspa; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

#include <caml/misc.h>
#include <caml/mlvalues.h>

#include "ocaml_ladspa_config.h"

#ifdef HAS_LADSPA
#include <ladspa.h>
#else
#include "ladspa.h"
#endif

typedef struct {
  const LADSPA_Descriptor *descr;
  LADSPA_Handle handle;
  LADSPA_Data *control_data;
  value *vbuf;
} ladspa_instance;

#define Instance_val(v) (*((ladspa_instance **)Data_custom_val(v)))

#define LADSPA_descr_val(v)                                                    \
  (*((const LADSPA_Descriptor **)Data_abstract_val(v)))

static inline value value_of_ladspa_descr(value ret,
                                          const LADSPA_Descriptor *d) {
  ret = caml_alloc(1, Abstract_tag);
  LADSPA_descr_val(ret) = d;
  return ret;
}
