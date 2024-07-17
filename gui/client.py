#!/usr/bin/python

from telnetlib import Telnet
import re

class LiqClient:

  def __init__(self,host,port):
    self.tel = Telnet(host,port)

  def command(self,s):
    self.tel.write(s+"\n")
    ans = self.tel.read_until("END")
    ans = re.sub("\nEND$","",ans)
    ans = re.sub("^\n","",ans)
    return ans

  def metadata(self,s):
    def dohash(a):
      h={}
      for i in range(len(a)/2):
        a[2*i+1] = re.sub('^"','',re.sub('"$','',a[2*i+1]))
        if a[2*i] in ('2nd_queue_pos','rid','source_id'):
          h[a[2*i]]=int(a[2*i+1])
        else:
          if a[2*i] in ('skip'):
            if a[2*i+1]=='true':
              h[a[2*i]]=True
            else:
              h[a[2*i]]=False
          else:
            h[a[2*i]]=unicode(a[2*i+1],'latin1')
      return h

    def noblank(a):
      return filter(lambda x: x!='',a)

    # Avoid empty matched groups, otherwise there will be no element in
    # the array. To do so, we include the ", and remove them later.
    return [ dohash(noblank(re.compile('(.+)=(".*")\n').split(e))) for e in
             noblank(re.compile('--- \d+ ---\n').split(self.command(s))) ]

if __name__ == "__main__":
  c = LiqClient('localhost',1234)
  print "Has been running for "+c.command("uptime")
  print c.metadata("root.metadatas")
