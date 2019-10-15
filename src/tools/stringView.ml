type t =
  {
    string : string;
    offset : int;
    length : int
  }

let of_string s =
  {
    string = s;
    offset = 0;
    length = String.length s;
  }

let of_substring s o l =
  assert (0 <= o && 0 <= l && o + l <= String.length s);
  {
    string = s;
    offset = o;
    length = l;
  }

let to_string s =
  String.sub s.string s.offset s.length

let to_substring s =
  s.string, s.offset, s.length

let length s = s.length

let is_empty s = s.length = 0

let sub s o l =
  of_substring s.string (s.offset + o) l

let blit s b o =
  String.blit s.string s.offset b o s.length
