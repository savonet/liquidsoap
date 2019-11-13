Bubble
======
Bubble is a simple program which scans your audio files and stores their metadata in a SQLite database. It can rewrite paths into URI so that you can index remote files mounted locally and rewrite the local path into the general URI before storing it in the database. For example if you mount your Samba workground in `/mnt/samba/workgroup` using `fusesmb`, you'll ask bubble to rewrite `/mnt/samba/workgroup` into `smb://`.

Bubble has been designed to be interfaced with liquidsoap to provide a protocol for selecting files by queries on metadata. URI rewriting makes it possible to query from another machine than the one where the indexer runs, and also makes sure that the file will appear as a remote one to liquidsoap, so that it will be fully downloaded to a safe local place before being played.

To add the bubble protocol to liquidsoap, we use the following code:

```liquidsoap
bubble = "/home/dbaelde/savonet/bubble/src/bubble-query " ^
         "-d /var/local/cache/bubble/bubble.sql "
add_protocol(
  "bubble",
  fun (arg,delay) -> get_process_lines(bubble^quote(arg)))
```

You could then have an IRC bot which accepts queries like play ``Alabama song'' and transforms it into the URI `bubble:title="Alabama song"` before queueing it in a liquidsoap instance. The bubble protocol in liquidsoap will call the `bubble-query` script which will translate the query from Bubble to SQLite and return a list of ten random matches, which liquidsoap will try.

Although it has been used for months as distributed on our old [SVN 
repository](http://savonet.svn.sourceforge.net/viewvc/savonet/oldies/bubble/src/),
bubble is mostly a proof-of-concept tool. It is very concise and can be tailored to custom needs.


