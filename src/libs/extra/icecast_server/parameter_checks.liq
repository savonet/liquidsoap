# Validation helpers of icecast.server parameters (parameters.md §7), used by
# parameters.liq and mount_parameters.liq: `check(field)` gives the validators
# of one field, each raising `error.icecast.server.invalid_parameters` with the
# field's name, `checked` applies one to a value and `use` turns a getter into
# a `() -> T` validated at each call.
let icecast.server.parameters = ()

# @flag hidden
let icecast.server.compatibilities =
  ["icecast-2.5", "icecast-2.4", "rsas", "kh"]

# @flag hidden
let icecast.server.events =
  [
    "source_connect",
    "source_disconnect",
    "listener_connect",
    "listener_disconnect",
    "metadata"
  ]

# @flag hidden
def icecast.server.parameters.invalid(field, why) =
  error.raise(
    error.icecast.server.invalid_parameters,
    "#{field}: #{why}"
  )
end

# Validators of a field's value, raising `invalid_parameters` with `field`.
# @flag hidden
def icecast.server.parameters.check(field) =
  def range(low, high) =
    fun (value) ->
      if
        value < low or value > high
      then
        icecast.server.parameters.invalid(
          field,
          "#{value} is not in #{low} to #{high}"
        )
      end
  end

  def count(value) =
    if
      value < 0
    then
      icecast.server.parameters.invalid(
        field,
        "#{value} is negative"
      )
    end
  end

  def duration(value) =
    if
      value < 0. or float.is_nan(value) or value == infinity
    then
      icecast.server.parameters.invalid(
        field,
        "#{value} is not a finite duration"
      )
    end
  end

  def headers(entries) =
    list.iter(
      fun (entry) ->
        begin
          why = icecast.server.headers.invalid_entry(entry)
          if
            null.defined(why)
          then
            icecast.server.parameters.invalid(field, null.get(why))
          end
        end,
      entries
    )
  end

  def bindings(bindings) =
    list.iter(
      fun (binding) ->
        if
          not list.mem(binding.event, icecast.server.events)
        then
          icecast.server.parameters.invalid(
            field,
            "unknown event #{string.quote(binding.event)}"
          )
        end,
      bindings
    )
  end

  def ranges(entries) =
    list.iter(
      fun (entry) ->
        if
          not icecast.server.addresses.is_range(entry)
        then
          icecast.server.parameters.invalid(
            field,
            "#{string.quote(entry)} is not an address or a CIDR range"
          )
        end,
      entries
    )
  end

  def optional(check) =
    fun (value) -> if null.defined(value) then check(null.get(value)) end
  end

  {
    range = range,
    count = count,
    duration = duration,
    optional_count = optional(count),
    optional_duration = optional(duration),
    optional_ranges = optional(ranges),
    headers = headers,
    bindings = bindings
  }
end

# `value`, once `check` accepts it.
# @flag hidden
def icecast.server.parameters.checked(check, value) =
  check(value)
  value
end

# A validator that accepts every value.
# @flag hidden
def icecast.server.parameters.any(_) =
  ()
end

# Function evaluating the getter `value` and validating it, called once now so
# that an invalid value fails here.
# @flag hidden
def icecast.server.parameters.use(check, value) =
  def read_value() =
    icecast.server.parameters.checked(check, getter.get(value))
  end
  ignore(read_value())
  read_value
end
