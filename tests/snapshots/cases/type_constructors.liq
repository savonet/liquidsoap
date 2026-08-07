# `ref` and `getter` name types but are ordinary functions too, so they are
# recognized by name in the grammar rather than lexed as keywords.
r = ref(1)
ignore((r : ref(int)))
ignore((1 : getter(int)))

# Still ordinary identifiers.
ref = "a variable"
getter = "another"
(ref, getter)
