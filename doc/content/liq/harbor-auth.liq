def auth(args) =
  # Call an external process to check the credentials: The script will return
  # the string "true" of "false".
  #
  # First call the script. Make sure to apply proper escaping of the arguments
  # to prevent command injection!
  ret =
    process.read.lines(
      "/path/to/script --user=#{args.user} --password=#{args.password}"
    )

  # Then get the first line of its output.
  ret = list.hd(default="", ret)

  # Finally returns the boolean represented by the output (bool_of_string can
  # also be used).
  if ret == "true" then true else false end
end
