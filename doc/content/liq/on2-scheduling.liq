# BEGIN
# Our custom request function
def get_request() =
  # Get the URI
  uri =
    list.hd(
      default="",
      process.read.lines(
        "cat /tmp/request"
      )
    )

  # Create a request
  request.create(uri)
end

# END
# BEGIN2
s = request.dynamic(id="s", get_request)

# END2
output.dummy(fallible=true, s)
