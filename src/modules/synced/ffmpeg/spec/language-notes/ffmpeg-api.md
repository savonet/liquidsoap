# Language notes: FFmpeg API behaviour worth knowing

Facts about FFmpeg's C API that the normative rules rest on and that its
headers do not all state. Each was read in FFmpeg's sources or headers at
release tags of the 7.1, 8.1 or 9.0 series unless marked **unverified**; not
every fact was read at all three. It is advice, not contract: check each
against the release in hand.

## Ownership and failure

- `av_dict_set` on failure frees the dictionary only when it has no entry. A
  dictionary with at least one entry must be freed by the caller.
- `av_packet_ref` on failure leaves the destination blank and returns the
  error.
- `av_packet_add_side_data` on failure leaves the data buffer with the caller.
  An entry of a type the packet already carries is replaced.
- `av_interleaved_write_frame` takes ownership of the packet's reference and
  returns it blank, on success and on error. `av_write_frame` works on a copy
  and leaves the packet as it was.
- `av_bsf_send_packet` moves the packet's content into the filter. A packet
  with no data and no side data signals end of stream.
- `avformat_open_input` on failure frees the context and nulls the pointer. It
  leaves the entries it did not consume in the dictionary.
- A context given a preset I/O context is marked as using custom I/O by
  `avformat_open_input`, and closing it then leaves that I/O context alone.
  Before that call the mark is absent. **unverified**
- There is no public call that removes a stream from a muxer once
  `avformat_new_stream` returned.
- `avfilter_graph_create_filter` frees the filter context itself when its
  initialisation fails.
- `avfilter_graph_parse_ptr` on failure frees every filter of the graph,
  including those created before the call.
- `av_hwframe_get_buffer` returns a frame that always carries its frames
  context. `av_hwframe_transfer_data` copies no frame property in either
  direction: timestamps and metadata are the caller's to copy.
- `av_hwdevice_ctx_create` leaves its option dictionary untouched: it reports
  nothing about which entries it used.
- `av_channel_layout_copy` and `av_opt_get_chlayout` uninitialise their
  destination before writing it. A destination must be initialised, zeroed at
  least, before the call. A layout owns heap memory only when its order is
  custom.
- `avsubtitle_free` reads `rects[i]` for every `i` below `num_rects` with no
  null test.

## Sentinels and absent values

- Unknown timestamps and durations are `AV_NOPTS_VALUE`. An unknown aspect
  ratio has a zero numerator. A packet's unknown duration is 0 and its unknown
  position is -1.
- A codec's supported lists end with different sentinels: sample formats and
  pixel formats with -1, sample rates with 0, frame rates with a zero
  numerator, channel layouts with a zero channel count, colour spaces and
  ranges with their "unspecified" member.
  `avcodec_get_supported_config` also reports the count, which avoids walking
  to a sentinel.
- Text fields can be null: a codec's long name, a filter's description in a
  build configured for small size, a pad's name, the name lookups of colour
  properties for a value the loaded library does not know.
- `av_channel_layout_default` never fails: for a channel count with no
  standard layout it yields a layout of unspecified order.
- `av_pix_fmt_count_planes` returns a negative code for a format with no
  descriptor. A paletted format has one plane and a palette buffer that is
  not a plane; `av_image_fill_plane_sizes` reports a size for it.
- FFmpeg uses "none" as an ordinary answer for formats and codec identifiers.
- `enum AVCodecID` has no negative member, so compilers make it unsigned.

## Codecs

- libavcodec runs a codec on one thread unless told otherwise. FFmpeg's tools
  set the thread count to automatic before opening.
- `avcodec_open2` applies its dictionary with child search, private options
  before generic ones, and after any field the caller assigned directly.
- The generic codec context options include `ar`, `time_base`, `pixel_format`,
  `video_size`, `colorspace`, `color_range` and `ch_layout`. They do not
  include `r`, `framerate`, `channel_layout`, `sample_fmt` or `ac`: the frame
  rate and the sample format are fields to assign.
- Once draining has started, `avcodec_send_packet` and `avcodec_send_frame`
  return end of stream. `avcodec_flush_buffers` returns a decoder to its
  initial state.
- A bitstream filter's options live in its private data. Setting a dictionary
  on the filter context reaches them only with child search.
- A receive that has nothing yet is the most frequent outcome and is reported
  as an error code.

## Containers and I/O

- The buffered I/O layer treats a custom write function's result as success or
  failure. A count smaller than requested is taken as a full write.
- `avio_tell` answers from the buffer position, returns 64 bits and does not
  call the seek function.
- `avio_open2` with a null URL dereferences it.
- Setting a dictionary on the format context does not reach the muxer's
  private options without child search. **unverified**
- A demuxer without a header, MPEG-PS for one, adds streams while packets are
  read.
- After end of file `av_read_frame` keeps returning end of file.
  **unverified**
- The interrupt function is polled during any blocking I/O, including the I/O
  done while a context is being closed.
- The SubRip encoder writes carriage return and line feed for a line break
  inside a cue. **unverified**

## Filters

- In the argument string of a filter, a value with no key is bound to the
  options in declaration order, and is rejected once a `key=value` pair has
  appeared.
- An array-typed option whose declaration gives no separator uses `,`.
- A buffer source given a null frame is closed; every later frame returns end
  of stream.
- `av_buffersink_set_frame_size` takes effect when called after the graph is
  configured.
- No filter of libavfilter declares a pad that is neither audio nor video.
- `av_opt_next` reads only the class pointer of the object it is given, so the
  address of a class pointer can stand for an object when listing the options
  of a class.

## Resampling and scaling

- `swr_convert` drains its buffered samples only when the input pointer is
  null. A non-null pointer with a zero count returns what is already
  converted and leaves the tail inside.
- `swr_get_out_samples` is an upper bound for the next call. **unverified**
- `av_samples_alloc` and `av_frame_get_buffer` round allocations up, which
  hides small overruns from tools.
- A video buffer from `av_frame_get_buffer` holds every plane in one
  allocation sized for the padded height; a chroma plane is shorter than the
  luma plane by the format's vertical subsampling.
- Slice threading in libswscale is reachable only through the frame-based
  entry point, which wants reference-counted buffers. Planes that belong to
  someone else can be wrapped in buffers whose release does nothing.

## Devices

- `avdevice_app_to_dev_control_message` returns "not implemented" for a
  context that is not an output device with a message handler.
- Device-to-application messages are emitted by PulseAudio output, from its
  own threads and from inside a write. The header documents the payload of
  each message type; typed payloads are never null.
- `avdevice_register_all` may be called any number of times.
- One device format has a name that is a comma-separated list of aliases:
  `video4linux2,v4l2`.

## Logging

- The log callback is called from any thread, including FFmpeg's worker
  threads and threads that hold FFmpeg's internal locks, and from inside calls
  the binding made with the runtime lock held.
- One line may arrive in several calls.
