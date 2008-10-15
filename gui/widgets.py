#!/usr/bin/python

import pygtk
pygtk.require('2.0')
import gobject
import gtk
import re

class Model(gtk.ListStore):
  def __init__(self,model,a):
    def gtypeof(e):
      return { type(1)   : gobject.TYPE_INT,
               type('a') : gobject.TYPE_STRING,
               type(True): gobject.TYPE_BOOLEAN }[type(e[1])]

    gtk.ListStore.__init__(self,*[ gtypeof(e) for e in model ])
    def get(e,k):
      try:
        return e[k[0]]
      except:
        return {type(1):0, type(''):'', type(True):k[1]}[type(k[1])]
    for e in a:
      self.append([ get(e,k) for k in model ])

# Creates a listview for hashes. A model is given at the beginning.
# The model is an array (for controlling order):
# [ k, v, p ]
# k is a string, column name
# v type (int,string,bool) defines the column type
#   its value defines the width of the column (100 or '100' for example)
# p is optionnal, is a property hash for the renderer
# There is currently no way to control column attributes.
class View(gtk.TreeView):
  def __init__(self,model,a):
    gtk.TreeView.__init__(self)
    self.model=model
    self.complete_model=[]
    self.setModel(a)

    for i in range(len(model)):
      k=model[i]
      column = gtk.TreeViewColumn(re.sub('_',' ',k[0]))
      renderer = {type('str'): gtk.CellRendererText(),
                  type(0): gtk.CellRendererText(),
                  type(True): gtk.CellRendererText()}[type(k[1])]
      # Optionnal third argument for rendering properties
      if len(model[i])>2:
        for p in model[i][2].keys():
          renderer.set_property(p,model[i][2][p])
      column.pack_start(renderer, True)
      column.add_attribute(renderer, 'text', i)
      column.set_resizable(True)
      column.set_expand(False)
      column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
      # Size is described by the example element of the column
      column.set_fixed_width({
          type(1): k[1],
          type(True): 40,
          type('s'): int(k[1])}[type(k[1])])
      self.append_column(column)

  def setModel(self,a):
    if a!=self.complete_model:
      self.set_model(Model(self.model,a))
      self.complete_model = a
  def getModel(self):
    return self.complete_model
