(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

 *****************************************************************************)

class virtual base title =
object (self)
  method virtual render : unit

  initializer
    (
      ignore (Glut.init [||]);
      Glut.initDisplayMode ~double_buffer:true ();
      ignore (Glut.createWindow ~title);
      GlMat.mode `modelview;
      Glut.displayFunc ~cb:(fun () -> self#render; Glut.swapBuffers ());
      Glut.idleFunc ~cb:(Some Glut.postRedisplay);
      ignore (Thread.create Glut.mainLoop ())
    )
end
