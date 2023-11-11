url = "http://radiopi.org:8080/reggae"

# url is a HTTP location, like http://radiopi.org:8080/reggae
source = input.http(url)
output.dummy(fallible=true, source)
