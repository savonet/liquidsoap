# Instantiate a new setting.
# @category Settings
# @flag hidden
def settings.make(~comments="", ~(description:string), v) =
  current_value = ref(v)
  current_value.{
    description = description,
    comments    = comments
  }
end

# Instantiate a new empty setting.
# @category Settings
# @flag hidden
def settings.make.void(~comments="", (description:string)) =
  { description = description, comments = comments }
end

let frame = ()

# Duration of a frame.
# @category Settings
def frame.duration =
  settings.frame.duration
end

# Top-level init module for convenience.
# @category Settings
# @flag hidden
init = settings.init
