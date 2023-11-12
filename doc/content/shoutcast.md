# Streaming to Shoutcast

Although Liquidsoap is primarily aimed at streaming to Icecast servers (that provide
much more features than Shoutcast), it is also able to stream to Shoutcast.

## Shoutcast output

Shoutcast server accept streams encoded with the MP3 or AAC/AAC+ codec. You to compile Liquidsoap with
`lame` support, so it can encode in MP3. Liquidsoap also has support for AAC+ encoding
using FDK-AAC or using an [external encoder](external_encoders.html). The recommended format is MP3.

Shoutcast output are done using the `output.shoutcast` operator with the appropriate parameters.
An example is:

```{.liquidsoap include="content/liq/shoutcast.liq" from=1}

```

As usual, `liquidsoap -h output.shoutcast` gives you the full list of options for this operator.

## Shoutcast as relay

A side note for those of you who feel they ``need'' to use Shoutcast for non-technical reasons (such as their stream
directory service...): you can still benefit from Icecast's power by streaming to an Icecast server, and then relaying
it through a shoutcast server.

In order to do that, you have to alias the root mountpoint ("`/`") to your MP3 mountpoint in your icecast server
configuration, like this:

```
<alias source="/" dest="/mystream.mp3" />
```

Be careful that icecast often aliases the status page (`/status.xsl`) with the `/`. In this case, comment
out the status page alias before inserting yours.
