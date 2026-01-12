(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(* Declare general modules. *)

let debug = Lang.add_module "debug"
let list = Lang.add_module "list"
let liquidsoap = Lang.add_module "liquidsoap"
let iterator = Lang.add_module "iterator"
let os = Lang.add_module "os"
let profiler = Lang.add_module "profiler"
let profiler_stats = Lang.add_module ~base:profiler "stats"
let random = Lang.add_module "random"
let url = Lang.add_module "url"
let record = Lang.add_module "record"
