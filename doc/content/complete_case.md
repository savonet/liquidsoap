# A complete case analysis

We will develop here a more complex example, according to the following specifications:

- play different playlists during the day;
- play user requests -- done via the telnet server;
- insert about 1 jingle every 5 songs;
- add one special jingle at the beginning of every hour, mixed on top of the normal stream;
- relay live shows as soon as one is available;
- and set up several outputs.

Once you can describe what you want in such a modular way, you're halfway there. Think of the diagram below as a graph through which the audio stream flows, following the arrows. The nodes modify the stream using basic operators — switching and mixing in our case. The final nodes are outputs: they pull data out of the graph and send it to the world. In our case, we have two Icecast outputs using different formats.

![Graph for 'radio.liq'](/assets/img/liqgraph.png)

Now here is how to write that in [Liquidsoap](index.html).

```{.liquidsoap include="complete-case.liq"}

```

To try this example, you'll need to edit the file names. To see the playlist switch in action, adjust the time intervals — if it's currently 16:42, try `0h-16h45` and `16h45-24h` instead of `6h-22h` and `22h-6h`. To test the hourly jingle, you can trigger it every minute by using the `0s` interval instead of `0m0s`.

To test the transition to a live show, start a new stream on the `live.ogg` mount of your server. You can use the examples from the [quickstart](quick_start.html) to stream a playlist to it. To start a real live show from soundcard input, use `darkice`, or simply liquidsoap if you have a working ALSA input:

```liquidsoap
liquidsoap 'output.icecast(%vorbis, \
  mount="live.ogg",host="...",password="...",input.alsa())'
```
