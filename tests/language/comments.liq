#-
# This is
a
multiline
comment -#

#-this is another -
multiline -
comment
-#

#- this is also
a multiline
comment -#

#-
Maybe the
more standard form
of multiline
comment?
-#

#------#
and this is also
a multiline comment
#------#

#---------------#
# Yet another
# form of multiline comment
#----------------#

#-
Multiline comment
# leading dash
  # and leading dash with spaces around them
are trimmed
-#

def f() =
  test.pass()
end

test.check(f)
