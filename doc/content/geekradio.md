Geek Radio
==========

The historical webradio, founded by David Baelde and Samuel Mimram at the ENS
Lyon.

The very first version was, as many other radios, a Perl function called by
Ices. It played files, one by one. On the campus, there was plenty of audio
files available, so they soon wanted to index them and be able to ask easily for
one file to be streamed. Samuel made a dirty campus indexer in OCaml, and David
made an ugly Perl hack for adding user requests to the original system. It
probably kind of worked for a while. Then they wanted something more, and
realized it was all too ugly.

So they made the binding of libshout for OCaml and built the first streamer in
pure OCaml. It had a simple telnet interface so an IRC bot could send user
requests easily to it, same for the website. There were two request queues, one
for users, one for admins. But it was still not so nicely designed, and they
felt it when they needed more. They wanted scheduling, especially techno music
at night.

Around that time students had to set up a project for one of their
courses. David and Samuel proposed to build a complete flexible webradio system,
that's Savonet. To give jobs to everybody, they had planned a complete rewriting
of every part, with grand goals. A new website with so much features, a new
intelligent multilingual bot, a new network libraries for glueing that,
etc. Most died. But still, Liquidsoap was born, and they had plenty of new
libraries for OCaml. Since then, Liquidsoap has been greatly enhanced, and is
now spreading outside the ENS Lyon.

Features
--------

The liquidsoap script schedules several static (but periodically reloaded)
playlists played on different times, adds jingle to the usual stream every hour,
adds short live interventions, or completely switches to live shows when
available. It accepts user requests, which have priority over static playlists
but not live shows, and adds speech-synthetized metadata information at the end
of requests.

Geek Radio used to have a Strider daemon running to fill our database. Since
that project is now dead, a simple hack is now used instead: bubble.

The usual way of sending a request is via an IRC bot, which queries the database
and sends the chosen URI to liquidsoap.
