module T = Content_base.MkContent (struct
  include Content_timed.Specs

  type kind = [ `T ]
  type params = unit
  type data = (params, int) content

  let kind = `T
  let string_of_kind _ = "test"
  let kind_of_string = function "test" -> Some `T | _ -> None
  let string_of_params () = "test"
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let blit = blit ~copy:(fun x -> x)
  let copy = copy ~copy:(fun x -> x)
end)

let () =
  (* Make sure consolidate_chunks consolidates a single chunks
     with non-null offset and/or size smaller than data length in
     a single chunk with size equal to data length and offset
     equal to zero. *)
  let d = Content_base.make ~size:1024 (T.lift_params ()) in
  let d = Content_base.sub d 12 34 in
  let chunked = T.get_chunked_data d in
  assert (List.length chunked.Content_base.chunks = 1);
  let { Content_base.size; offset; data } =
    List.hd chunked.Content_base.chunks
  in
  assert (size = 34);
  assert (offset = 12);
  assert (T.length data = 1024);
  let chunked = T.consolidate_chunks chunked in
  assert (List.length chunked.Content_base.chunks = 1);
  let { Content_base.size; offset; data } =
    List.hd chunked.Content_base.chunks
  in
  assert (size = 34);
  assert (offset = 0);
  assert (T.length data = 34);

  (* Make sure sub works as expected. *)
  let d =
    T.lift_data
      {
        Content_timed.Specs.params = ();
        size = 10;
        data =
          [
            (1, 1);
            (2, 2);
            (3, 3);
            (4, 4);
            (5, 5);
            (6, 6);
            (7, 7);
            (8, 8);
            (9, 9);
          ];
      }
  in
  let d = Content_base.sub d 3 4 in
  let c = (T.get_data d).Content_timed.Specs.data in
  assert (c = [(0, 3); (1, 4); (2, 5); (3, 6)])
