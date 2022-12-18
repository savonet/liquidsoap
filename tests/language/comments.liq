#------#
This is
a multiline comment
#------#

#---------------#
# Yet another
# form of multiline comment
#----------------#

   #-----#
multiline comment can start and end
in the middle of a line
   #----#

#-----#
Anything following them
is executed
#----#x = 1

#-----#y = x + 2.
Anything after the mark
is comment
#----#

def f() =
  test.equals(x, 1)

  try
    let eval _ = "#----#
This comment is not terminated"
    test.fail()
  catch _ do
    ()
  end

  let eval _ = "# single line comment can be followed by EOF"

  test.pass()
end

test.check(f)
