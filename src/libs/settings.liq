%ifdef settings
# Instantiate a new setting
# @flag hidden
def settings.make(~comments="", ~(description:string), v) =
  current_value = ref(v)
  {!current_value}.{
    description = description,
    comments    = comments,
    set         = (fun (v) -> current_value := v)
  }
end

# Instantiate a new empty setting
# @flag hidden
def settings.make.void(~comments="", (description:string)) =
  { description = description, comments = comments }
end
%endif
