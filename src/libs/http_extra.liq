# Harbor middleware to add CORS headers
# @category Internet
# @flag extra
# @param ~origin Configures the Access-Control-Allow-Origin CORS header
# @param ~origin_callback Origin callback for advanced uses. If passed, overrides `origin` argument. Takes the request as input and returns the allowed origin. Return `null` to skip all CORS headers.
# @param ~methods Configures the Access-Control-Allow-Methods CORS header.
# @param ~allowed_headers Configures the Access-Control-Allow-Headers CORS header. If not specified, defaults to reflecting the headers specified in the request's Access-Control-Request-Headers header.
# @param ~exposed_headers Configures the Access-Control-Expose-Headers CORS header. If not specified, no custom headers are exposed.
# @param ~credentials Configures the Access-Control-Allow-Credentials CORS header. Set to true to pass the header, otherwise it is omitted.
# @param ~max_age Configures the Access-Control-Max-Age CORS header. Set to an integer to pass the header, otherwise it is omitted.
# @param ~preflight_continue Pass the CORS preflight response to the nexnhandler.
# @param ~options_status_code Provides a status code to use for successful OPTIONS requests, since some legacy browsers (IE11, various SmartTVs) choke on 204.
def harbor.http.middleware.cors(
  ~origin=null("*"),
  ~origin_callback=null(),
  ~methods=["GET", "HEAD", "PUT", "PATCH", "POST", "DELETE"],
  ~allowed_headers=null(),
  ~exposed_headers=[],
  ~credentials=false,
  ~max_age=null(),
  ~preflight_continue=false,
  ~options_status_code=204) =
  fun (req, res, next) -> begin
    # This is for typing purposes
    res = if false then http.response() else res end
    if false then
      harbor.http.register("/foo", fun (r, _) -> ignore(if false then r else req end))
    end

    vary = ref([])

    def add_vary() =
      if vary() != [] then
        res.header("Vary", string.concat(separator=",", vary()))
      end
    end

    def vary(v) =
      vary := v::vary()
    end

    def add_origin(origin) =
      res.header("Access-Control-Allow-Origin", origin)
      if origin != "*" then
        vary("Origin")
      end
    end

    def add_methods() =
      if methods != [] then
        res.header("Access-Control-Allow-Methods", string.concat(separator=",", methods))
      end
    end

    def add_credentials() =
      if credentials then
        res.header("Access-Control-Allow-Credentials", "true")
      end
    end

    def add_allowed_headers() =
      allowed_headers =
        if null.defined(allowed_headers) then
          string.concat(separator=",", null.get(allowed_headers))
        elsif list.assoc.mem("access-control-request-headers", req.headers) then
          req.headers["access-control-request-headers"]
        else
          null()
        end

      if null.defined(allowed_headers) then
        res.header("Access-Control-Allow-Headers", null.get(allowed_headers))
        vary("Access-Control-Request-Headers")
      end
    end

    def add_exposed_headers() =
      if exposed_headers != [] then
        res.header("Access-Control-Expose-Headers", string.concat(separator=",", exposed_headers))
      end
    end

    def add_max_age() =
       if null.defined(max_age) then
         res.header("Access-Control-Max-Age", "#{(null.get(max_age):int)}")
       end
    end

    origin =
      if null.defined(origin_callback) then
        fn = null.get(origin_callback)
        fn(req)
      else
        getter.get(origin)
      end

    if not null.defined(origin) then
      next(req, res)
    else
      add_origin(null.get(origin))

      if req.method == "OPTIONS" then
        add_credentials()
        add_methods()
        add_allowed_headers()
        add_max_age()
        add_exposed_headers()
        add_vary()

        if preflight_continue then
          next(req, res)
        else
          res.status_code(options_status_code)
          res.header("Content-length", "0")
        end
      else
        add_credentials()
        add_allowed_headers()
        add_vary()
        next(req, res)
      end
    end
  end
end

# This is for typing purposes
if false then
  harbor.http.middleware.register(harbor.http.middleware.cors())
end
