(* Conformance checks of spec/playlists.md §2 and §4: every byte binding. *)

open Hls_output

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let entry ?init ?(discontinuous = false) ?(tags = []) uri =
  { Playlist.uri; duration = 1.; discontinuous; init_uri = init; tags }

(* The spec's example. *)
let () =
  let bytes =
    Playlist.media
      {
        target = 1;
        independent = true;
        media_sequence = 6;
        discontinuity_sequence = 0;
        extra_tags = [];
        entries =
          [
            entry ~init:"m_0.mp4" "m_7.m4s";
            entry ~init:"m_1.mp4" ~discontinuous:true "m_8.m4s";
            entry ~init:"m_1.mp4" "m_9.m4s";
          ];
      }
  in
  check "spec example"
    (bytes
    = String.concat "\r\n"
        [
          "#EXTM3U";
          "#EXT-X-TARGETDURATION:1";
          "#EXT-X-VERSION:6";
          "#EXT-X-INDEPENDENT-SEGMENTS";
          "#EXT-X-MEDIA-SEQUENCE:6";
          "#EXT-X-DISCONTINUITY-SEQUENCE:0";
          "#EXT-X-MAP:URI=\"m_0.mp4\"";
          "#EXTINF:1.000,";
          "m_7.m4s";
          "#EXT-X-DISCONTINUITY";
          "#EXT-X-MAP:URI=\"m_1.mp4\"";
          "#EXTINF:1.000,";
          "m_8.m4s";
          "#EXTINF:1.000,";
          "m_9.m4s";
          "";
        ])

(* KC8: the first listed segment carries no discontinuity; inserted tags
   precede their EXTINF; variant extra tags follow the header. *)
let () =
  let bytes =
    Playlist.media
      {
        target = 6;
        independent = false;
        media_sequence = 0;
        discontinuity_sequence = 1;
        extra_tags = ["#EXT-X-FOO"];
        entries =
          [entry ~discontinuous:true "a.ts"; entry ~tags:["#EXT-X-CUE"] "b.ts"];
      }
  in
  check "no marker on first, tags before EXTINF"
    (bytes
   = "#EXTM3U\r\n\
      #EXT-X-TARGETDURATION:6\r\n\
      #EXT-X-VERSION:6\r\n\
      #EXT-X-MEDIA-SEQUENCE:0\r\n\
      #EXT-X-DISCONTINUITY-SEQUENCE:1\r\n\
      #EXT-X-FOO\r\n\
      #EXTINF:1.000,\r\n\
      a.ts\r\n\
      #EXT-X-CUE\r\n\
      #EXTINF:1.000,\r\n\
      b.ts\r\n")

(* KC10: URIs. *)
let () =
  check "plain" (Playlist.uri ~prefix:"" "a_1.ts" = "a_1.ts");
  check "sub-directory"
    (Playlist.uri ~prefix:"" "2026/11/a b.ts" = "2026/11/a%20b.ts");
  check "prefix without slash"
    (Playlist.uri ~prefix:"http://x/hls" "a.ts" = "http://x/hls/a.ts");
  check "prefix with slash"
    (Playlist.uri ~prefix:"http://x/" "a.ts" = "http://x/a.ts");
  check "quote, brace, control characters"
    (Playlist.uri ~prefix:"" "a\"{$\n.ts" = "a%22%7B$%0A.ts");
  check "version" (Playlist.version = 6);
  Printf.printf "playlist: %d checks passed\n" !checks;
  if !checks < 8 then failwith "too few checks ran"
