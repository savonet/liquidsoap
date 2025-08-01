# Instantiate a new setting.
# @category Settings
# @flag hidden
def settings.make(~comments="", ~(description:string), v) =
  current_value = ref(v)
  current_value.{description=description, comments=comments}
end

# Instantiate a new empty setting.
# @category Settings
# @flag hidden
def settings.make.void(~comments="", (description:string)) =
  {description=description, comments=comments}
end

let frame = ()

# Duration of a frame.
# @category Settings
def frame.duration =
  settings.frame.duration
end

let settings.init.compact_before_start =
  settings.make(
    description=
      "Run the OCaml memory compaction algorithm before starting your script. \
       This is useful when script caching is not possible but initial memory \
       consumption is a concern. This will result in a large chunk of memory \
       being freed right before starting the script. This also increases the \
       script's initial startup time.",
    true
  )

# Top-level init module for convenience.
# @category Settings
# @flag hidden
init = settings.init

on_start(
  {if settings.init.compact_before_start() then runtime.gc.compact() end}
)
