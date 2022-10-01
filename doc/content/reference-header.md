Liquidsoap scripting language reference
=======================================

The **Source / ...** categories contain all functions that return sources.
The **Input** functions are those which build elementary sources
(playing files, synthesizing sound, etc.).
The **Output** functions are those which take a source and register it
for being streamed to the outside (file, soundcard, audio server, etc.).
The **Visualization** functions are experimental ones that let you
visualize in real-time some aspects of the audio stream.
The **Sound Processing** functions are those which basically work on the source
as a continuous audio stream. They would typically be mixers of streams,
audio effects or analysis.
Finally, **Track Processing** functions are basically all
others, often having a behaviour that depends on or affects the extra
information that liquidsoap puts in streams: track limits and metadata.
