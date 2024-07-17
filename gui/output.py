#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gobject

from client import LiqClient
from widgets import View

class LiqOutput(gtk.VBox):

  def delete_event(self, widget, event, data=None):
    return False

  def destroy(self, widget, data=None):
    gtk.main_quit()

  def __init__(self,host='localhost',port=1234,op='root'):
    gtk.VBox.__init__(self)

    # Connect to liquidsoap
    self.op = op
    self.tel = LiqClient(host,port)

    # Fast metadata view: short metadata, right click to see more (TODO)
    a=self.tel.metadata(self.op+".metadatas")
    self.list = View([['rid'        , 40],
                      ['artist'     ,'120'],
                      ['title'      ,'120'],
                      # Right-align URI because the end is more informative
                      # than the beginning
                      ['uri'        ,'300', {'xalign':1.0}],
                      ['on_air_date','50']], a)
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)
    scroll.add(self.list)
    scroll.show()
    self.pack_start(scroll,padding=10)

    # an hbox containing : remaining / start-stop / skip
    self.hbox = gtk.HBox()
    self.pack_start(self.hbox,False,False)

    self.remaining = gtk.Label("...")
    self.hbox.pack_start(self.remaining)
    gobject.timeout_add(1000,self.update)

    self.skip = gtk.Button("skip")
    self.skip.connect("clicked", self.do_skip, None)
    self.hbox.pack_start(self.skip,False,False)

    self.onoff = gtk.ToggleButton("on/off")
    self.onoff.set_active(self.tel.command(self.op+".status")=="on")
    self.onoff.connect("clicked", self.do_onoff, None)
    self.hbox.pack_start(self.onoff,False,False)

    # Show everything...
    self.list.show()
    self.remaining.show()
    self.onoff.show()
    self.skip.show()
    self.show()
    self.hbox.show()

  def update(self):
    self.remaining.set_label("Remaining: "+
        (self.tel.command(self.op+".remaining")))
    self.list.setModel(self.tel.metadata(self.op+".metadatas"))
    if self.tel.command(self.op+".status")=="on":
      if not self.onoff.get_active():
        self.onoff.set_active(True)
    else:
      if self.onoff.get_active():
        self.onoff.set_active(False)
    return 1

  def do_skip(self,widget,data=None):
    self.tel.command(self.op+".skip")

  def do_onoff(self,widget,data=None):
    # The state we get is the one *after* the click
    if self.onoff.get_active():
      self.tel.command(self.op+".start")
    else:
      self.tel.command(self.op+".stop")

  def main(self):
    gtk.main()

import getopt
import sys

if __name__ == "__main__":
  opts, args = getopt.gnu_getopt(sys.argv[1:],"h:p:o:",
                                 ['host=','port=','operator='])
  op='root'
  host='localhost'
  port=1234
  for o , a in opts:
    if o in ('-p', '--port'):
      port=int(a)
    if o in ('-o', '--operator'):
      op=a
    if o in ('-h', '--host'):
      host=a
  win = gtk.Window()
  win.set_border_width(10)
  win.connect("delete_event", lambda w,e: False)
  win.connect("destroy", lambda w: gtk.main_quit ())
  win.set_title(host+':'+str(port)+' -- Output '+op)
  win.resize(700,300)
  win.add(LiqOutput(host,port,op))
  win.show()
  gtk.main()
