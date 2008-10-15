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

class LiqPlaylist(gtk.ScrolledWindow):

  def __init__(self,host='localhost',port=1234,op='root'):
    gtk.ScrolledWindow.__init__(self)
    self.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)

    # Connect to liquidsoap
    self.op = op
    self.tel = LiqClient(host,port)

    self.view = View([['status','50'],['uri','300']],[])
    self.update()
    self.add(self.view)
    self.view.show()
    self.show()

    gobject.timeout_add(1000,self.update)

  def next(self):
    a = filter(lambda x: x!='',
               re.compile('\n').split(self.tel.command(self.op+".next")))
    def f(e):
      m = re.search('^\[(.*?)\] (.*)',e)
      if m:
        return { 'uri': m.group(2) , 'status': m.group(1) }
      else:
        return { 'uri': e }

    return [ f(e) for e in a ]

  def update(self):
    # TODO restore scroll position
    self.view.setModel(self.next())
    return True

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
  op='playlist'
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
    win.set_title(host+':'+str(port)+' -- Playlist '+op)
    win.resize(700,300)

    win.add(LiqPlaylist(host,port,op))
    win.show()
    gtk.main()
  except socket.error, x:
    print "Couln't connect to "+host+':'+str(port)+'!'
