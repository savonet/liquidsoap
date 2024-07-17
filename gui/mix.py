#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gobject

import socket
import re
import urllib
from threading import Timer

from client import LiqClient
from widgets import View


def strbool(b):
  if b:
    return "true"
  else:
    return "false"

class LiqMix(gtk.HBox):

  def __init__(self,host='localhost',port=1234,op='mix'):
    gtk.HBox.__init__(self)
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_NEVER)
    self.add(scroll)
    scroll.show()

    self.box = gtk.HBox()
    scroll.add_with_viewport(self.box)
    self.box.show()
    fadebttn = gtk.Button("Fade")
    self.box.pack_start(fadebttn, False, False)
    fadebttn.show()
    # Connect to liquidsoap
    self.tel = LiqClient(host,port)

    # Get info about the mixing table
    self.inputs = filter(lambda x: x!='',
        re.compile('\s*(\S+)\s*').split(self.tel.command(op+".inputs")))
    self.n = len(self.inputs)
    self.status = [ {} for e in range(self.n) ]

    # Create widgets
    remaining,skip,ready,volume,single,fadein,fadeout,selected=[ [] for i in range(8) ]
    def play_stop(i):
      self.tel.command(op+".select "+str(i)+" "+
        (strbool(not self.status[i]['selected'])))  
    for i in range(self.n):
      # Add a controller
      scroll = gtk.ScrolledWindow()
      scroll.set_policy(gtk.POLICY_NEVER,gtk.POLICY_AUTOMATIC)
      self.box.pack_start(scroll)
      vbox = gtk.VBox()
      scroll.add_with_viewport(vbox)
      scroll.show()
      vbox.show()

      # Status line
      ready.append(gtk.Label())
      vbox.pack_start(ready[i],False,False,10)
      ready[i].set_markup('<b>'+self.inputs[i]+'</b>')
      ready[i].set_justify(gtk.JUSTIFY_CENTER)
      ready[i].show()

      # Two next buttons will be packed horizontally
      hbox = gtk.HBox()
      vbox.pack_start(hbox,False,False)
      hbox.show()

      # Play/stop button
      selected.append(gtk.Button('play/stop'))
      hbox.pack_start(selected[i])
      selected[i].show()
      selected[i].connect("clicked",lambda b,i:play_stop(i),i)

      # Skip
      skip.append(gtk.Button("skip"))
      skip[i].connect("clicked",
                   lambda button,i:
                     self.tel.command(op+".skip "+str(i)), i)
      hbox.pack_start(skip[i])
      skip[i].show()

      # Single mode
      single.append(gtk.CheckButton("Stop at end of track"))
      vbox.pack_start(single[i],False,False)
      single[i].show()
      single[i].connect("clicked",
          lambda button,i:
            self.tel.command(op+".single "+str(i)+" "+
              (strbool(single[i].get_active()))),i)

      # Fade in check box
      fade = gtk.HBox()
      vbox.pack_start(fade,False,False)
      lbl = gtk.Label("Fade: ")
      fade.pack_start(lbl)

      fadein.append(gtk.CheckButton("in"))
      fadeout.append(gtk.CheckButton("out"))
      fade.pack_start(fadein[i])
      fade.pack_start(fadeout[i])
      
      lbl.show()
      fadein[i].show()
      fadeout[i].show()
      fade.show()
      
      # Volume
      volume.append(gtk.Adjustment())
      vol=gtk.VScale(volume[i])
      vbox.pack_start(vol,True,True)
      vol.set_inverted(True)
      vol.set_increments(1,10)
      vol.set_range(0,100)
      vol.connect("value-changed",
          lambda l,i: self.tel.command(op+".volume "+str(i)+" "+
            str(volume[i].get_value())),i)
      vol.set_update_policy(gtk.UPDATE_CONTINUOUS)
      vol.show()

      # Remaining time
      remaining.append(gtk.Label("..."))
      vbox.pack_start(remaining[i],False,False)
      remaining[i].show()

    # Fade out all the fadeout_Checkbutton and fade in all the fadein_CB
    # during duration
    def fade_in(i):
      volume[i].set_value(volume[i].get_value() + 1)
      if volume[i].get_value() == 100:
        return False #No need to redo
      else:
        return True #Need to redo

    def fade_out(i):
      volume[i].set_value(volume[i].get_value() - 1)
      if volume[i].get_value() == 0:
        return False
      else:
        return True
        
    def fade_in_out():
      duration = 10000 # in ms
      for i in range(self.n):
        if fadein[i].get_active() and not fadeout[i].get_active():
          sleep_in = duration/(101 - volume[i].get_value())
          print sleep_in
          gobject.timeout_add(int(sleep_in), lambda i:fade_in(i),i)
        elif fadeout[i].get_active() and not fadein[i].get_active():
          sleep_out = duration/(volume[i].get_value()+1)
          print sleep_out
          gobject.timeout_add(int(sleep_out), lambda i:fade_out(i),i)
        elif fadeout[i].get_active() and fadein[i].get_active():
          pass
      # TODO

    fadebttn.connect("clicked", lambda y:fade_in_out())

    # TODO charmap dependent
    keys = list('aqAQwzsZSxedEDcrfRFvtgTGbyhYHn')
    def keypress(vol,ev):
      try:
        print ev.string, ev.keyval, ev.hardware_keycode
        i = keys.index(ev.string)
        if i%5==4:
          play_stop(i/5)
        elif i%5==2:
          fadein[i/5].set_active(not fadein[i/5].get_active())
        elif i%5==3:
          fadeout[i/5].set_active(not fadeout[i/5].get_active())
        else:
          volume[i/4].set_value(volume[i/4].get_value()+((i%4==0 and 1) or -1))
      except:
        pass
    self.set_events(gtk.gdk.KEY_PRESS_MASK)
    self.connect("key_press_event",keypress)

    def update():
      for i in range(self.n):
        a=filter(lambda x: x!='',
            re.compile('(\S+)=(\S+)\s*').split(
              self.tel.command(op+".status "+str(i))))
        self.status[i]={}
        for j in range(len(a)/2):
          self.status[i][a[2*j]]=a[2*j+1]
        self.status[i]['selected'] = (self.status[i]['selected']=='true')
        self.status[i]['ready'] = (self.status[i]['ready']=='true')

        selected[i].set_label(
            (self.status[i]['selected'] and "stop") or 'start')
        remaining[i].set_label('End of track: '+self.status[i]['remaining'])
        volume[i].set_value(int(re.sub('%','',self.status[i]['volume'])))
        if self.status[i]['selected']:
          ready[i].set_markup('<b>'+self.inputs[i]+'</b>\n'+
            ((self.status[i]['ready'] and 'Currently playing') or
              'Waiting for source'))
        else:
          ready[i].set_label('<b>'+self.inputs[i]+'</b>\nStopped, '+
              ((self.status[i]['ready'] and 'ready') or 'not ready'))
      return True

    gobject.timeout_add(1000,update)
    self.show()

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
  op='mix'
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
    win.set_title(host+':'+str(port)+' -- Mixing table '+op)
    win.resize(700,300)

    print "Key bindings:"
    print "source volume up down play/stop"
    print " 0             a    q    w"
    print " 1             z    s    x"
    print " 2             e    d    c"
    print "..."
    win.add(LiqMix(host,port,op))
    win.show()
    gtk.main()
  except socket.error, x:
    print "Couln't connect to "+host+':'+str(port)+'!'
