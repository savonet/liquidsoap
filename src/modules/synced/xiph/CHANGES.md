# 1.0.0 (2025-01-30)

- Initial release of the combined modules
- `ogg`: Add optional `fill` argument to decoders, used to pull more data when needed.
- `flac`: cleanup implementation and global roots, add `fill` argument to decoder to prevent use of exception to refill data.
- Others: adapted to new `fill` API from `ogg`.
