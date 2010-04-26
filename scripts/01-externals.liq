# Decoders, enabled when the binary is detected

# Generic mime test. First try
# to use file.mime if it exist.
# Otherwise try to get the value 
# using the file binary.
def mime_test(file,pattern) = 
  def file_method(file,pattern) = 
    if test_process("which file") then
      ret = list.hd(get_process_lines("file -b --mime-type \
                                       #{quote(file)}"))
      string.match(pattern=pattern,ret)
    else
      false
    end
  end
  def mime_method(file,pattern) = 
    ret = false
    %ifdef file.mime
      ret = string.match(pattern=pattern,file.mime(file))
    %endif
    ret
  end
  # First try mime method
  mime_method(file,pattern) or
  # Now try file method
  file_method(file,pattern)
end

if test_process("which flac") then
  log(level=3,"Found flac binary: enabling flac external decoder.")
  flac_p = "flac -d -c - 2>/dev/null"
  def test_flac(file) =
    if test_process("which metaflac") then
      channels = list.hd(get_process_lines("metaflac \
                                            --show-channels #{quote(file)} \
                                            2>/dev/null"))
      # If the value is not an int, this returns 0 and we are ok :)
      int_of_string(channels)
    else
      if mime_test(file,"flac") then
        # We do not know the number of audio channels
        # so setting to -1
        (-1)
      else
        # All tests failed: no audio decodable using flac..
        0
      end
    end
  end
  add_decoder(name="FLAC",description="Decode files using the flac \
              decoder binary.", test=test_flac,flac_p)
else
  log(level=3,"flac binary not found: flac decoder disabled.")
end

if test_process("which metaflac") then
 log(level=3,"Found metaflac binary: enabling flac external metadata \
              resolver.")
 def flac_meta(file)
   ret = get_process_lines("metaflac --export-tags-to=- \
                #{quote(file)} 2>/dev/null")
   ret = list.map(string.split(separator="="),ret)
   # Could be made better..
   def f(l',l)=
     if list.length(l) >= 2 then
       list.append([(list.hd(l),list.nth(l,1))],l')
     else
       if list.length(l) >= 1 then
         list.append([(list.hd(l),"")],l')
       else
         l'
       end
     end
   end
   list.fold(f,[],ret)
 end
 add_metadata_resolver("FLAC",flac_meta)
else
 log(level=3,"metaflac binary not found: flac metadata resolver disabled.")
end

if test_process("which faad") then
  log(level=3,"Found faad binary: enabling external faad decoder and \
               metadata resolver.")
  faad_p = (fun (f) -> "faad -w #{quote(f)} 2>/dev/null")
  def test_faad(file) = 
    channels = list.hd(get_process_lines("faad -i #{quote(file)} 2>&1 | \
                                          grep 'ch,'"))
    ret = string.extract(pattern=", (\d) ch,",channels)
    ret = if list.length(ret) == 0 then
      "0"
    else
      ret["1"]
    end
    # If the value is not an int, this returns 0 and we are ok :)
    int_of_string(ret)
  end
  add_oblivious_decoder(name="FAAD",description="Decode files using \
                        the faad binary.", test=test_faad, faad_p)
  def faad_meta(file) =
    ret = get_process_lines("faad -i \
                 #{quote(file)} 2>&1")
    # Yea, this is tuff programming (again) !
    def get_meta(l,s)=
      ret = string.extract(pattern="^(\w+):\s(.+)$",s)
      if list.length(ret) > 0 then
        list.append([(ret["1"],ret["2"])],l)
      else
        l
      end
    end
    list.fold(get_meta,[],ret)
  end
  add_metadata_resolver("FAAD",faad_meta)
else
  log(level=3,"faad binary not found: faad decoder disabled.")
end

