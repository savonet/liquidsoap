let () =
  assert (Metadata.ID3v2.trim_eos 2 "\000ab\000de\000\000" = "\000ab\000de")
