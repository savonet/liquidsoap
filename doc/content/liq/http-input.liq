url = "http://radiopi.org:8080/reggae"

# BEGIN
# url is a HTTP location, like http://radiopi.org:8080/reggae
source = input.http(url)

# END
output.dummy(fallible=true, source)
