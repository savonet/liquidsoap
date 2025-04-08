# 0.3.1 (2024-04-01) 🃏

- Add support for `ogg/flac` metadata.

  # 0.3.0 (2024-03-24)

- Add basic example.
- Add optional custom parser argument to override the default parsing mechanism.
- Add binary format for encoding frames.
- Update default metadata mappings to follow musicbrainz's picard mapping.
- Add `MIME` module to guess MIME type of files (#4).
- Generic RIFF format parser, adds support for wave files (#6).

  # 0.2.0 (2023-07-01)

- Add support for FLAC.
- id3v2: use "bpm" instead of "tempo".
- id3v2: convert "tlen" to "duration".
- id3v2: implement partial support for rendering.
- Fix charset conversion from utf16.
- Fixed MP4 parsing.

  # 0.1.0 (2023-02-08)

- Initial release.
