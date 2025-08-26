# Redirect all files other than /admin.* to icecast, located at localhost:8000.
def redirect_icecast(request, response) =
  response.redirect("http://localhost:8000#{request.path}")
end

# Register this handler at port 8005 (provided harbor sources are also served
# from this port).
harbor.http.register.regexp(
  port=8005,
  method="GET",
  r/^\/admin/,
  redirect_icecast
)
