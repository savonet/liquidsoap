# Liquidsoap scripting language reference

The **Source / ...** categories contain all functions that return sources.
The **Source / Input** functions are those which build elementary sources
(playing files, synthesizing sound, etc.).
The **Source / Output** functions are those which take a source and register it
for being streamed to the outside (file, soundcard, audio server, etc.).
The **Source / Visualization** functions let you
visualize in real time some aspects of the audio stream.
The **Source / Audio processing** functions are those which basically work on the source
as a continuous audio stream. They would typically be mixers of streams,
audio effects or analysis.
Finally, **Source / Track processing** functions are basically all
others, often having a behavior that depends on or affects the extra
information that liquidsoap puts in streams: track limits and metadata.
The **Track / ...** categories contain the equivalent functions operating on
individual tracks of a source.
