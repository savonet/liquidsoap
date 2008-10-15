#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gobject

import socket
import re
import urllib

from client import LiqClient
from widgets import View

class LiqQueue(gtk.VPaned):

  def __init__(self,host='localhost',port=1234,op='request'):
    gtk.VPaned.__init__(self)
    self.show()

    # Connect to liquidsoap
    self.op = op
    self.tel = LiqClient(host,port)

    # Fast metadata view: short metadata, right click to see more (TODO)
    self.list = View([['rid'        , 40],
                      ['2nd_queue_pos', 10],
                      ['skip'       , False],
                      ['artist'     ,'120'],
                      ['title'      ,'120'],
                      # Right-align URI because the end is more informative
                      # than the beginning
                      ['uri'        ,'300', {'xalign':1.0}]], self.queue())
    self.list.drag_dest_set(gtk.DEST_DEFAULT_ALL,
                            [('text/uri-list',0,0)],
                            gtk.gdk.ACTION_COPY)
    self.list.connect("drag_data_received",self.drag)
    self.list.connect("row_activated",self.row_activated)
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)
    scroll.add(self.list)
    scroll.show()
    self.list.show()
    self.pack1(scroll,resize=True)

    fsel = gtk.FileChooserWidget()
    fsel.connect("file_activated", lambda s: self.selected(s))
    exp = gtk.Expander("File chooser")
    exp.add(fsel)
    self.pack2(exp,resize=False)
    fsel.show()
    exp.show()

    gobject.timeout_add(1000,self.update)

  def selected(self,file):
    self.tel.command(self.op+".push "+file.get_filename())

  def drag(self,w,context,x,y,data,info,time):
    if data and data.format == 8:
      for e in data.data.split('\r\n')[:-1]:
        self.tel.command(self.op+".push "+urllib.unquote(e))
        context.finish(gtk.TRUE, gtk.FALSE, time)

  def row_activated(self,view,path,column):
    rid = view.get_model().get_value(view.get_model().get_iter(path),0)
    skip = view.get_model().get_value(view.get_model().get_iter(path),2)
    if skip:
      self.tel.command(self.op+".consider "+str(rid))
    else:
      self.tel.command(self.op+".ignore "+str(rid))

  def queue(self):
    a = filter(lambda x: x!='',
          re.compile('(\d+)\s*').split(self.tel.command(self.op+".queue")))
    a = [ self.tel.metadata('metadata '+e)[0] for e in a ]
    return a

  def update(self):
    self.list.setModel(self.queue())
    return 1

import getopt
import sys

if __name__ == "__main__":
  try:
    opts, args = getopt.gnu_getopt(sys.argv[1:],"h:p:o:",
                                 ['host=','port=','operator='])
  except:
    # TODO real help
    print 'Options are --operator=s --host=s --port=s and -ohp shortcuts'
    sys.exit()
  op='request'
  host='localhost'
  port=1234
  for o , a in opts:
    if o in ('-p', '--port'):
      port=int(a)
    if o in ('-o', '--operator'):
      op=a
    if o in ('-h', '--host'):
      host=a
  try:
    win = gtk.Window()
    win.set_border_width(10)
    win.connect("delete_event", lambda w,e: False)
    win.connect("destroy", lambda w: gtk.main_quit ())
    win.set_title(host+':'+str(port)+' -- Request '+op)
    win.resize(700,300)

    print "Double click on request to toggle skip flag"

    win.add(LiqQueue(host,port,op))
    win.show()
    gtk.main()
  except socket.error, x:
    print "Couln't connect to "+host+':'+str(port)+'!'
