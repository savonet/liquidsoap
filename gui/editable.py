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

ridformat = ("int/liquidsoap-rid", 0, 32)

class LiqEditable(gtk.VPaned):

  def __init__(self,host='localhost',port=1234,op='request'):
    gtk.VPaned.__init__(self)
    self.show()

    # Connect to liquidsoap
    self.op = op
    self.tel = LiqClient(host,port)

    self.list = View([['rid'        , 40],
                      ['status'     ,'70'],
                      ['artist'     ,'120'],
                      ['title'      ,'120'],
                      # Right-align URI because the end is more informative
                      # than the beginning
                      ['uri'        ,'300', {'xalign':1.0}]], [])
    self.update()

    # Popup menu for requests
    menu = gtk.Menu()
    item = gtk.ImageMenuItem(gtk.STOCK_REMOVE)
    def remove_request(item):
      model,path = self.list.get_selection().get_selected()
      self.tel.command(self.op+'.remove '+str(model.get_value(path,0)))
    item.connect('activate',remove_request)
    item.show()
    menu.append(item)
    def popup(w,event):
      if event.button==3:
        menu.popup(None,None,None,event.button,event.time)
    self.list.connect('button_press_event',popup)

    # Receive drag-n-drops
    self.list.enable_model_drag_dest(
        [('text/uri-list',0,0),ridformat],
        gtk.gdk.ACTION_DEFAULT)

    def dnd_receive(w,context,x,y,data,info,time):
      if data and (data.format != 8 and data.format != ridformat[2]):
        print "DnD received: Unknown data format! (%d)" % data.format
        return
      row = w.get_dest_row_at_pos(x,y)
      if row:
        # This is an insertion
        path, pos = row
        # Remove the number of resolv(ed|ing) requests to get the pos
        # in the pending queue
        pos = path[0]-(len(w.get_model())-self.plen)
        if pos<0:
          print "Cannot move outside pending queue!"
          return
        if pos >= self.plen-1:
          pos = -1
        if data and data.format == ridformat[2]:
          rid = int(data.data)
          self.tel.command('%s.move %d %d' % (self.op,rid,pos))
        if data and data.format == 8:
          for e in data.data.split('\r\n')[:-1]:
            self.tel.command(self.op+'.insert '+str(pos)+' '+e)
      else:
        # This is a push
        if data and data.format == ridformat[2]:
          rid = int(data.data)
          self.tel.command('%s.move %d %d' % (self.op,rid,-1))
        if data and data.format == 8:
          for e in data.data.split('\r\n')[:-1]:
            self.tel.command(self.op+".push "+urllib.unquote(e))
            context.finish(gtk.True, gtk.False, time)

    self.list.connect("drag_data_received",dnd_receive)

    # Emit drag-n-drops
    self.list.enable_model_drag_source(gtk.gdk.BUTTON1_MASK,[ridformat],
        gtk.DEST_DEFAULT_ALL)
    def dnd_emit(w,context,sel,info,time):
      # Check that format is RID
      if info==ridformat[2]:
        model,iter = w.get_selection().get_selected()
        if iter:
          sel.set(sel.target,info,str(model.get_value(iter,0)))
    self.list.connect("drag_data_get",dnd_emit)

    # Put the list in a scroll
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)
    scroll.add(self.list)
    scroll.show()
    self.list.show()
    self.pack1(scroll,resize=True)

    # A file selector in the other side of the pane
    fsel = gtk.FileChooserWidget()
    fsel.connect("file_activated", lambda s: self.selected(s))
    exp = gtk.Expander("File chooser")
    exp.add(fsel)
    self.pack2(exp,resize=False)
    fsel.show()
    exp.show()

    # And the update callback
    gobject.timeout_add(1000,self.update)

  def selected(self,file):
    self.tel.command(self.op+".push "+file.get_filename())

  def queue(self):
    a = filter(lambda x: x!='',
          re.compile('(\d+)\s*').split(self.tel.command(self.op+".queue")))
    a = [ self.tel.metadata('metadata '+e)[0] for e in a ]
    return a

  def update(self):
    newq = self.queue()
    if newq != self.list.getModel():
      # The test is useless for setModel but needed for plen update
      self.list.setModel(newq)
      plen = int(self.tel.command(self.op+'.pending_length'))
      self.plen = plen
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

    win.add(LiqEditable(host,port,op))
    win.show()
    gtk.main()
  except socket.error, x:
    print "Couln't connect to "+host+':'+str(port)+'!'
