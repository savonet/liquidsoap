# Check these examples with: liquidsoap --no-libs -i -c typing.liq

# TODO Throughout this file, parsing locations displayed in error messages
#      are often much too inaccurate.

# Check that some polymorphism is allowed.
# id : (string,'a)->'a
def id(a,b)
  log(a)
  b
end
ignore("bla"==id("bla","bla"))
ignore(0==id("zero",0))

# Reporting locations for the next error is non-trivial, because it is about
# an instantiation of the type of id. The deep error doesn't have relevant 
# information about why the int should be a string, the outer one has.
# id(0,0)

# Polymorphism is limited to outer generalizations, this is not system F.
# apply : ((string)->'a)->'a
def apply(f)
  f("bla")
  # f is not polymorphic, the following is forbidden:
  # f(0)
  # f(f)
end

# The level checks forbid abusive generalization.
# id' : ('a)->'a
def id'(x)
  # If one isn't careful about levels/scoping, f2 gets the type ('a)->'b
  # and so does twisted_id.
  def f2(y)
    x
  end
  f2(x)
end

# More errors...
# 0=="0"
# [3,""]

# Subtyping, functions and lists.
f1 = fun () -> 3
f2 = fun (a=1) -> a

# This is OK, l1 is a list of elements of type f1.
l1 = [f1,f2]
list.iter(fun (f) -> log(string_of(f())), l1)
# Forbidden. Indeed, f1 doesn't accept any argument -- although f2 does.
# Here the error message may even be too detailed since it goes back to the 
# definition of l1 and requires that f1 has type (int)->int.
# list.iter(fun (f) -> log(string_of(f(42))), l1)

# Actually, this is forbidden too, but the reason is more complex...
# The infered type for the function is ((int)->int)->unit,
# and (int)->int is not a subtype of (?int)->int.
# There's no most general answer here since (?int)->int is not a 
# subtype of (int)->int either.
# list.iter(fun (f) -> log(string_of(f(42))), [f2])

# Unlike l1, this is not OK, since we don't leave open subtyping constraints
# while infering types.
# I hope we can make the inference smarter in the future, without obfuscating
# the error messages too much.
# The type error here shows the use of all the displayed positions:
#  f1 has type t1, f2 has type t2, t1 should be <: t2
# l2 = [ f2, f1 ]

# An error where contravariance flips the roles of both sides..
# [fun (x) -> x+1, fun (y) -> y^"."]

# An error without much locations..
# TODO An explaination about the missing label would help a lot here.
# def f(f)
#   f(output.icecast.vorbis)
#   f(output.icecast.mp3)
# end

# This causes an occur-check error.
# TODO The printing of the types breaks the sharing of one EVAR
#  across two types. Here the sharing is actually the origin of the occur-check
#  error. And it's not easy to understand..
# omega = fun (x) -> x(x)

# Now let's test ad-hoc polymorphism.

echo = fun(x) -> system("echo #{quote(string_of(x))}")

ignore("bla")
ignore((1,3.12))
ignore(1 + 1)
ignore(1. + 2.14)

# string is not a Num
# echo("bl"+"a")

ignore(1 <= 2)
ignore((1,2) == (1,3))

# float <> int
# echo(1 == 2.)

# source is not an Ord
# echo(blank()==blank())

def sum_eq(a,b)
  a+b == a
end

print("TEST PASSED")
