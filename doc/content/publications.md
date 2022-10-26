---
header-includes: |
  \DeclareUnicodeCharacter{03BB}{$\lambda$}
...
The theory behind Liquidsoap
============================

Publications
------------

### Liquidsoap: a High-Level Programming Language for Multimedia Streaming

Many of the advanced features of the Liquidsoap language are described in
[Liquidsoap: a High-Level Programming Language for Multimedia Streaming](/assets/docs/bbm10.pdf).
The article details in particular Liquidsoap's handling of heterogeneous stream
contents (e.g. audio and video), as well as the model for clocks in the
language.

### De la webradio lambda à la &lambda;-webradio

The first published presentation of Liquidsoap was made in
[De la webradio lambda à la &lambda;-webradio](/assets/docs/bm08.pdf)
(*Baelde D. and Mimram S. in proceedings of Journées Francophnes des Langages Applicatifs (JFLA), pages 47-61, 2008*)
-- yes, it's in French, sorry. It gives a broad description of the Liquidsoap
tool and explains the theory behind the language, which is formalized as a
variant of the typed &lambda;-calculus with labels and optional arguments. The
article describes the typing inference algorithm as well as some properties of
the language (confluence) and of typing (subject reduction, admissible rules,
termination of typed terms).
