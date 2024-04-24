This part is pretty open. We describe below a few advanced features of
liquidsoap, including video and midi. We provide some examples
or simply propose things to do, to give you an idea of what's in
the near future of liquidsoap and hopefully draw you into
contributing to shaping that future,
if only through discussions.

# Visualization

A liquidsoap script is like any other program: you can in principle
predict what it does, but there's always a point where you miss something.
In such cases, you need to debug it: control all the relevant parameters
until you spot what's wrong.

In liquidsoap, the most important parameter is probably the availability
of a source. You'll end up creating devices dealing with various sources
going on and off. To test them you can create sources whose availability
is controlled by you, and you should also be able to monitor their
availability.

As an exercise, you can try to write a simple logging function that
periodically displays `source.is_ready(s)` for some source `s`.
The `mix` operator can be useful too: it is a mixing table for
liquidsoap, allowing you to monitor and cancel the availability
of its inputs; the best way to use it is through liGuidsoap.

## Audio volume

You can visualize the audio volume on screen using `visu.volume`:

```{.liquidsoap include="on2-volume.liq"}

```

# Video streams

Video may be used very simply in liquidsoap: common operators
such as `single` and `playlist` will attempt to decode their files
as video if their content type is appropriate, which is dictated by
the output operator.
For example you can do `output.sdl(single("video.ogg"))`
to display the video part of an audio+video stream;
to hear the audio part, insert your favorite audio output operator
together with `drop_audio` and `drop_video` at the right places
(most outputs do not silently drop irrelevant data).

## Slideshow

The following script displays a slideshow of images,
while playing a playlist of audio files.
Pass it the images playlist/directory as the first argument on the command
line (after `--`) and (optionally) the audio playlist/directory as the
second argument.

```{.liquidsoap include="on2-slideshow.liq"}

```

If you experience transparency problems... it's a known bug
(see notably [#393](http://savonet.rastageeks.org/ticket/393)).
You may also experience segfaults... it does not seem to happen
with a selected list avoiding too large sizes or too exotic formats.

## Audio volume

You can render the audio volume visualization as a video stream,
that you can then process as any other video stream:

```{.liquidsoap include="on2-volume2.liq"}

```

## Static image over audio track

TODO: the youtube encoder

## Transitions

Playing a video file `video.ogv` is simply achieved by

```liquidsoap
s = single("video.ogv")
output.sdl(s)
```

There are many useful (or not) effects in Liquidsoap which can be used to modify
the video. These should be inserted between the first and the second line of
the script above. For example, the image can be converted to sepia by adding

```liquidsoap
s = video.sepia(s)
```

Common operations include adding a logo (stored in a PPM image file
`image.ppm`):

```liquidsoap
s = video.add_image(width=30, height=30, x=10, y=10, file="image.ppm", s)
```

and displaying a scrolling text:

```liquidsoap
s = video.add_text("Hello people!", s)
```

Try modifying the scrolling text example so that you can modify the contents of
the text over the telnet interface.

Very similarly to audio transitions (fade in, fade out, etc.) there are some
video transitions implemented. For example fading in the video is simply done using

```liquidsoap
s = video.fade.in(s)
```

The kind of transition that should be used is controlled by the `transition`
parameter of `video.fade.in`. Try `disc` for example, as well as the others
that you can find in the documentation.

Of course, the `add` operator of liquidsoap also works on video streams. So, in
order to add a rotating image on a video you could use

```liquidsoap
s = add([s,
         video.rotate(
           video.add_image(
             width=50, height=50, x=150, y=150,
             file="image.ppm",
             blank()))])
```

## Overloaded demo

```{.liquidsoap include="on2-overloaded.liq" from="BEGIN" to="END"}

```

# Manipulating MIDI data

## Playing with the keyboard

MIDI is a format for describing streams of notes, scores, etc. Liquidsoap has
a basic support for such streams.

In order to generate a MIDI stream, the `input.keyboard.sdl` operator can be
used. It will convert typing onto the keyboard of your operator into notes. In
order to be able to hear the notes of a stream, you have to _synthesize_ them,
which means to convert them to wave sound. Various operators can be used for
this, each corresponding to a different instrument. For example, a synthesizer
with sawtooth waves is provided by the operator `synth.saw`. A mini-keyboard
synthesizer can thus be programmed using the following script

```liquidsoap
s = input.keyboard.sdl()
s = synth.saw(s)
out(s)
```

You can also test `synth.sine` or `synth.square` for other kinds of simple
sounds.

In order to check the MIDI data contained in streams, the `midimeter` operator
is very convenient: it prints on the standard output the notes currently being
played.

## Playing MIDI files

Liquidsoap comes with built-in support for MIDI files: when such a file is
played it is detected as such and decoded as a MIDI stream. Usually, MIDI files
contain multiple channels of notes (typically one for each instrument). In order
to use the sawtooth synthesizer on all channels, the `synth.all.saw` operator
should be used (with the `synth.saw` operator, only the first channel will be
synthesized).

So, a MIDI file named `file.mid` can be played using the following script

```liquidsoap
s = single("file.mid")
s = midi.remove([9],s)
s = source.mux.audio(audio=blank(),s)
s = synth.all.saw(s)
s = drop_midi(s)
out(s)
```

The second line removes all the notes from the channel 9 which is usually used
for drums (and would thus sound bad with our basic synthesizer). The third line
adds an audio channel to the stream `s`, in which the sound will be synthesized.

## Playing chords

The implementation of MIDI-related operators in Liquidsoap is still in early
stage and their implementation gives us the possibility to simply test new
ideas... we would be glad hear yours too!

For example, we thought it would be nice to be able to play chords in
Liquidsoap. This is actually pretty simple using metadata. First, describe your
sequence of chords in a file named `chords.txt` with the following contents:

```
1 "chord" "C"
2 "chord" "Am"
3 "chord" "F"
4 "chord" "G"
```

This file just contains a list of metadata: on each line, the first number
indicate when (in seconds) the metadata should occur, the string in second
indicates the name of the metadata (`chord` here) and the string in third
position indicates the value of the metadata (the chord to be played here). This
file format for metadata is supported natively by Liquidsoap. Now, the metadata
containing the chord names can be converted to MIDI notes by using the `chord`
operator. The sequence of chords above can thus be heard using the following
script:

```liquidsoap
s = single("chord.txt")
s = midi.chord(s)
s = source.mux.audio(audio=blank(),s)
s = synth.saw(s)
s = drop_midi(s)
out(s)
```

# Open problems

If you feel like hacking seriously, here are some tasks
from the Savonet community.

## Listener-sensitive radio

Write a script that checks whether an icecast mount point is being listened
to, and use it to switch a radio to some dummy source when nobody is
listening, and switch back to normal when listeners come back.
This can be useful to avoid using the hard drive when unnecessary
-- it is noisy, calorific, and simply not so long-lived.

## Liquidsoap script generator

Write a script (even better, a web page)
that generates a simple liquidsoap script with a few options:
input from playlist, live relay, output to icecast and/or soundcard.

## Re-usable tools for radios

The open-source radio community needs re-usable tools that can be interfaced
with existing streamers: indexer, database generator, scheduler,
crossfading editor, etc.
