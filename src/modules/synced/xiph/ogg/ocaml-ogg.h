/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-ogg.
 *
 * ocaml-ogg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ogg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ogg; if not, write to the Free Software
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

#include <ogg/ogg.h>

#define Sync_state_val(v) (*((ogg_sync_state **)Data_custom_val(v)))
#define Stream_state_val(v) (*((ogg_stream_state **)Data_custom_val(v)))
#define Packet_val(v) (*((ogg_packet **)Data_custom_val(v)))

value value_of_page(ogg_page *op);
value value_of_packet(ogg_packet *op);
ogg_page *page_of_value(value v, ogg_page *op);
