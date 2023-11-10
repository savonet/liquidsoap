# Split and re-encode a CUE sheet.

CUE sheets are sometimes distributed along with a single audio file containing a whole CD.
Liquidsoap can parse CUE sheets as playlists and use them in your request-based sources.

Here's for instance an example of a simple code to split a CUE sheet into several mp3 files
with `id3v2` tags:

```{.liquidsoap include="content/liq/split-cue.liq"}

```
