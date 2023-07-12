#!../../liquidsoap ../test.liq

# TODO Throughout this file, parsing locations displayed in error messages
#      are often much too inaccurate.

def f() =
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
  ignore(apply)

  # The level checks forbid abusive generalization.
  # id' : ('a)->'a
  def id'(x)
    # If one isn't careful about levels/scoping, f2 gets the type ('a)->'b and
    # so does twisted_id.
    def f2(y)
      x
    end
    f2(x)
  end
  ignore(id')

  # Generalization and aliasing (see #941).
  def id (x) = x end
  _ = id(3)
  _ = id("a")

  id' = id
  _ = id'(3)
  _ = id'("a")

  # Testing generalization of recursive functions.
  def rec recid(x)
    x
  end
  ignore(recid(3) == 3)
  ignore(recid("a") == "a")

  # More errors...
  # 0=="0"
  # [3,""]

  # Subtyping, functions and lists.
  f1 = fun () -> 3
  f2 = fun (a=1) -> a

  # This is OK, l1 is a list of elements of type f1.
  l1 = [f1,f2]
  list.iter(fun (f) -> log(string(f())), l1)

  # Forbidden. Indeed, f1 doesn't accept any argument -- although f2 does.  Here
  # the error message may even be too detailed since it goes back to the
  # definition of l1 and requires that f1 has type (int)->int.
  # list.iter(fun (f) -> log(string(f(42))), l1)

  # This used to be forbidden, but is allowed now that we have dropped currying:
  # we can now subtype a (?int)->int into an (int)->int (which is the type
  # inferred in the iter).
  list.iter(fun (f) -> log(string(f(42))), [f2])

  # Unlike l1, this is not OK, since we don't leave open subtyping constraints
  # while inferring types. I hope we can make the inference smarter in the
  # future, without obfuscating the error messages too much. The type error here
  # shows the use of all the displayed positions:
  #  f1 has type t1, f2 has type t2, t1 should be <: t2
  # l2 = [ f2, f1 ]

  # An error where contravariance flips the roles of both sides.
  # [fun (x) -> x+1, fun (y) -> y^"."]

  # An error without much locations.
  # TODO An explanation about the missing label would help a lot here.
  # def f(f)
  #   f(output.icecast.vorbis)
  #   f(output.icecast.mp3)
  # end

  # This causes an occur-check error.
  # TODO The printing of the types breaks the sharing of one EVAR
  #  across two types. Here the sharing is actually the origin of the occur-check
  #  error. And it's not easy to understand..
  # omega = fun (x) -> x(x)

  # The argument f of the function g is inferred as taking a non-optional
  # parameter x, whereas f has an optional parameter x. Subtyping should however
  # accept this now that we have disable partial application.
  g = fun(f) -> f(x=3)
  f = fun(~x=3) -> x+5
  ignore(g(f))

  # Now let's test ad-hoc polymorphism.

  echo = fun(x) -> process.run(process.quote.command("echo", args=[(x:string)]))
  ignore(echo)

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

  # Test record subtyping.
  def f(x) =
    "" ^ x
  end
  ignore(f("a".{n = 3}))
  # Ensure that we have correct subtyping for arguments, which should be
  # contravariant, see #1465.
  # We have string.{l : int} < string
  # Thus    (string) -> int  < (string.{l : int}) -> int
  def g(f) =
    f("a".{l = 3})
  end
  def f(x) =
    0
  end
  ignore(g(f))

  def sum_eq(a,b)
    a+b == a
  end
  ignore(sum_eq)

  (noise():source(audio=pcm,video=canvas))
  (noise():source(audio=pcm(mono)))
  (noise():source(audio=pcm("5.1")))
  (noise():source)

  (single("annotate:foo=\"bla\":/nonexistent"):source(audio=ffmpeg.copy,video=ffmpeg.copy,midi=midi))
  (single("annotate:foo=\"bla\":/nonexistent"):source(audio=ffmpeg.audio.raw,video=ffmpeg.video.raw,midi=midi))
  (single("annotate:foo=\"bla\":/nonexistent"):source(audio=ffmpeg.audio.raw(sample_rate=44100, channel_layout="5.1"),video=ffmpeg.video.raw(pixel_format=yuva420p),midi=midi))

  let ([x]:[int]) = [123]

  ([]:[(string * int)] as json.object)
  ([("bla", 3.14)]:[(string * float)] as json.object)

  ({foo = 123}:{foo:int})
  ({foo = 123}:{"✨ name ✨" as foo: int})
  (123.{foo=3.14,gni="aabbcc"}:int.{foo:float,gni:string})
  (123.{foo=3.14,gni="aabbcc"}:int.{foo:float}.{gni:string})
  (123.{foo=3.14,gni="aabbcc"}:int.{foo:float}.{gni:int}.{gni:string})
  (source.tracks(single("")).audio.{foo=3.14,gni="aabbcc"}:ffmpeg.copy.{foo:float}.{gni:int}.{gni:string})
  (blank().{foo=3.14,gni="aabbcc"}:source(audio=pcm).{foo:float}.{gni:int}.{gni:string})
  (blank().{foo=3.14,gni="aabbcc"}:source.{foo:float}.{gni:int}.{gni:string})
  (123:{foo:int}.foo)
  (123.{foo=3.14,gni="aabbcc"}:int.{"✨ name ✨" as foo:float,gni:string})
  (():{})
  (():unit.{})

  ({foo = 123}:{foo?:int})
  ({}:{foo?:int})
  ({foo = null()}:{foo?:int})
  ({foo = 123}:{foo?:int})
  ({foo = null(null())}:{foo?:int?})

  ([({}:{foo?:int}), (), {foo = 123}]:[{foo?:int}])
  ([(), {foo = 123}]:[{foo?:int}])

  # This one is more subtle than it seems because we have no field video on the
  # left and a video:never field on the right when testing for the subtyping,
  # see #3210.
  s = (sine():source(video=none))
  s = (sine():source(video=none,...))

  test.pass()
end

test.check(f)
