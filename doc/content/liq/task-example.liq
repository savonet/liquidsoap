def connect_callback() =
  ignore(http.post("http://host/on_connect"))
end

thread.run(connect_callback)
