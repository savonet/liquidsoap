#<------#
This is
a multiline comment
with > some # of
these characters in it.
#------>#

#<---------------
# Yet another
# form of multiline comment
 ---------------->#

   #<
multiline comment can start and end
in the middle of a line
      >#

#<-----#
Anything following them
is executed
#---->#x = 1

#<-----#y = x + 2.
Anything after the mark
is comment
#---->#

#<---------------
# Nested comments

   #<
     Are accepted
   >#

#---------------->#

s = "foo
#<-----
this is a comment inside a string,
it is not a code comment!
#---->
bar
"
ignore(s)

r = r/foo
#<-----
this is a comment inside a regexp,
it is not a code comment!
#---->
bar/
ignore(r)

def f() =
  test.equals(x, 1)

  try
    let eval _ = "#<----#
This comment is not terminated"
    test.fail()
  catch _ do
    ()
  end

  let eval _ = "# single line comment can be followed by EOF"

  test.equals("a#b", string.base64.decode("YSNi"))
  test.equals(string(r/a#b/), "r/a#b/")
  test.equals("a#<b>#c", string.base64.decode("YSM8Yj4jYw=="))
  test.equals(string(r/a#<b>#c/), "r/a#<b>#c/")

  test.pass()
end

test.check(f)
