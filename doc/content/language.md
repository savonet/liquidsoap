# Liquidsoap's scripting language

_The following is adapted from the [Liquidsoap book](book.html). The reader is avised to check out the whole
chapter in the book for more details about the liquidsoap language_

## General features

Liquidsoap is a novel language which was designed from scratch to handle media stream. It takes some inspiration
from functional languages such as [OCaml](https://ocaml.org/) but features a syntax that is more intuitive to the
general purpose programmer, similar to Ruby or Javascript.

### Typing

One of the main features of the language is that it is _typed_. This means that
every expression belongs to some type which indicates what it is. For instance,
`"hello"` is a _string_ whereas `23` is an _integer_, and, when presenting a
construction of the language, we will always indicate the associated
type. Liquidsoap implements a _typechecking_ algorithm which ensures that
whenever a string is expected a string will actually be given, and similarly for
other types. This is done without running the program, so that it does not
depend on some dynamic tests, but is rather enforced by theoretical
considerations. Another distinguishing feature of this algorithm is that it also
performs _type inference_: you never actually have to write a type, those are
guessed automatically by Liquidsoap. This makes the language very safe, while
remaining very easy to use.

### Functional programming

The language is _functional_, which means that you can very easily define
functions, and that functions can be passed as arguments of other
functions. This might look like a crazy thing at first, but it is actually quite
common in some language communities (such as OCaml). It also might look quite
useless: why should we need such functions when describing webradios? You will
soon discover that it happens to be quite convenient in many places: for
handlers (we can specify the function which describes what to do when some event
occurs such as when a DJ connects to the radio), for transitions (we pass a
function which describes the shape we want for the transition) and so on.

### Streams

The unique feature of Liquidsoap is that it allows the manipulation of _sources_
which are functions which will generate streams. These streams typically consist
of stereo audio data, but we do restrict to this: they can contain audio with
arbitrary number of channels, they can also contain an arbitrary number of video
channels, and also MIDI channels (there is limited support for sound synthesis).

### Standard library

Although the core of Liquidsoap is written in OCaml, many of the functions of
Liquidsoap are written in the Liquidsoap language itself. Those are defined in
the `stdlib.liq` script, which is loaded by default and includes all the
libraries. You should not be frightened to have a look at the standard library,
it is often useful to better grasp the language, learn design patterns and
tricks, and add functionalities. Its location on your system is indicated in the
variable `configure.libdir` and can be obtained by typing

## Basic values

### Integers and floats

The _integers_, such as `3` or `42`, are of type
`int`. Depending on the current architecture of the
computer on which we are executing the script (32 or 64 bits, the latter being
the most common nowadays) they are stored on 31 or 63 bits. The minimal
(resp. maximal) representable integer can be obtained as the constant
`min_int`
(resp. `max_int`); typically, on a 64 bits
architecture, they range from -4611686018427387904 to 4611686018427387903.

The _floating point numbers_, such as `2.45`, are of type
`float`, and are in double precision, meaning that
they are always stored on 64 bits. We always write a decimal point in them,
so that `3` and `3.` are not the same thing: the former is an integer and the
latter is a float. This is a source of errors for beginners, but is necessary for
typing to work well.

### Strings

Strings are written between double or single quotes,
e.g. `"hello!"` or `'hello!'`, and are of type `string`.

In order to write the character "`"`" in a string, one cannot simply type "`"`"
since this is already used to indicate the boundaries of a string: this
character should be _escaped_, which
means that the character "`\`" should be typed first so that

```liquidsoap
print("My name is \"Sam\"!")
```

will actually display "`My name is "Sam"!`". Other commonly used escaped
characters are "`\\`" for backslash and "`\n`" for new line. Alternatively, one
can use the single quote notation, so that previous example can also be written
as

```liquidsoap
print('My name is "Sam"!')
```

This is most often used when testing JSON data which can contain many quotes or for
command line arguments when calling external scripts. The character "`\`" can also
be used at the end of the string to break long strings in scripts without
actually inserting newlines in the strings. For instance, the script

```liquidsoap
print("His name is \
       Romain.")
```

will actually print

```
His name is Romain.
```

Note that there is no line change between "is" and "Romain", and the indentation
before "Romain" is not shown either.

The concatenation of two strings is achieved by the infix operator "`^`", as in

```liquidsoap
user = "dj"
print("Current user is " ^ user)
```

Instead of using concatenation, it is often rather convenient to use _string
interpolation_: in a string, `#{e}` is replaced by the string representation of
the result of the evaluation of the expression `e`:

```liquidsoap
user = "admin"
print("The user #{user} has just logged.")
```

will print `The user admin has just logged.` or

```liquidsoap
print("The number #{random.float()} is random.")
```

will print `The number 0.663455738438 is random.` (at least it did last time I
tried).

#### Escaping strings

Liquidsoap strings follow the most common lexical conventions from `C` and `javascript` and `JSON`, in particular,
`string.unescape` recognizes the same escape sequences as `C` (except for `UTF-16` characters) and javascript.

The following sequences are recognized:

| Escape sequence | Hex value in ASCII | Character represented                                                                 |
| --------------- | ------------------ | ------------------------------------------------------------------------------------- |
| `\a`            | `\x07`             | Alert (Beep, Bell)                                                                    |
| `\b`            | `\x08`             | Backspace                                                                             |
| `\e`            | `\x1B`             | Escape character                                                                      |
| `\f`            | `\x0C`             | Formfeed, Page Break                                                                  |
| `\n`            | `\x0A`             | Newline (Line Feed)                                                                   |
| `\r`            | `\x0D`             | Carriage Return                                                                       |
| `\t`            | `\x09`             | Horizontal Tab                                                                        |
| `\v`            | `\x0B`             | Vertical Tab                                                                          |
| `\\`            | `\x5C`             | Backslash                                                                             |
| `\/`            | `\x2f`             | Forward slash                                                                         |
| `\'`            | `\x27`             | Apostrophe or single quotation mark                                                   |
| `\"`            | `\x22`             | Double quotation mark                                                                 |
| `\?`            | `\x3F`             | Question mark (used to avoid Digraphs and trigraphs)                                  |
| `\nnn`          | any                | The byte whose numerical value is given by _nnn_ interpreted as an _octal_ number     |
| `\xhh`          | any                | The byte whose numerical value is given by _hh_ interpreted as a _hexadecimal_ number |
| `\uhhhh`        | none               | UTF8-8 code point given by _hhhh_ interpreted as an _hexadecimal_ number              |

This convention has been decided to follow the most common practices. In particular, `\nnn` is an _octal_ escape sequence in most languages
including C, Ruby, Javascript, Python and more. This differs from OCaml where `\nnn` is considered a _digital_ escape sequence.

These lexical conventions are used in the default `string.escape` and `string.unescape`.

Here's an example of an escaped string:

```
# "\" \t \045 \x2f \u4f32";;
- : string = "\" \t % / 2"
```

The function `string.quote` returns [JSON-compatible](https://www.json.org/json-en.html) strings.

### Regular expressions

_This feature was introduced in liquidsoap version 2.1.0_

Regular expressions can be created using the `regexp` operator or the syntactic sugar: `r/.../<flags>`. For instance:

```liquidsoap
# Using the regexp operator:
r = regexp(flags=["g","i"], "foo([\\w])+bar")

# Using the r/../ syntactic sugar:
r = r/foo([\w])bar/gi
```

Using the `r/../` syntactic sugar makes it possible to write regular expressions without having to escape `\` characters,
which makes them more easily readable.

Regular expression flags are:

- `i`: perform case-insensitive match
- `g`: substitute all matched sub-strings, not just the first one
- `s`: match all characters, including `\n` when using the `.` pattern
- `m`: `^` and `$` match before/after newlines, not just at the beginning/end of a string

Regular expressions have the following methods:

- `replace(fn, s)`: replace matched substrings of `s` using function `fn`. If the `g` flag is not passed, only the first match is replaced otherwise, all matches are replaced
- `split(s)`: split the given string on all substrings matching the regular expression.
- `test(s)`: returns `true` if the given string matches the regular expression.
- `exec(s)`: execute the regular expression and return a of list matches of the form: `[(<match index>, <match>), ..]`. Named matches are also supported and returned as property `groups` of type `[string * string]`:

```liquidsoap
r/(foo)(?<gno>gni)?/g.exec("foogni")
- : [int * string].{groups : [string * string]} =
[
    (2, "gni"),
    (1, "foo"),
    (0, "foogni")
].{
    groups = [
      ("gno", "gni")
    ]
}
```

### Booleans

The _booleans_ are either `true` or
`false` and are of type `bool`. They can be combined
using the usual boolean operations

- `and`: conjunction,
- `or`: disjunction,
- `not`: negation.

Booleans typically originate from comparison operators, which take two values
and return booleans:

- `==`: compares for equality,
- `!=`: compares for inequality,
- `<=`: compares for inequality,

and so on (`<`, `>=`, `>`). For instance, the following is a boolean expression:

```liquidsoap
(n < 3) and not (s == "hello")
```

_Conditional branchings_ execute code depending on whether a condition is true
or not. For instance, the code

```liquidsoap
if (1 <= x and x <= 12) or (not 10h-15h) then
  print("The condition is satisfied")
else
  print("The condition ain't satisfied")
end
```

will print that the condition is satisfied when either `x` is between 1 and 12
or the current time is not between 10h and 15h. A conditional branching might
return a value, which is the last computed value in the chosen branch. For
instance,

```liquidsoap
y = if x < 3 then "A" else "B" end
```

will assign `"A"` or `"B"` to `y` depending on whether `x` is below 3 or
not. The two branches of a conditional should always have the same return type:

```liquidsoap
x = if 1 == 2 then "A" else 5 end
```

will result in

```
At line 1, char 19-21:
Error 5: this value has type string
but it should be a subtype of int
```

meaning that `"A"` is a string but is expected to be an integer because the
second branch returns an integer, and the two should be of same nature. The
`else` branch is optional, in which case the `then` branch should be of type
`unit`:

```liquidsoap
if x == "admin" then print("Welcome admin") end
```

In the case where you want to perform a conditional branching in the
`else` branch, the `elsif`{.liquidsoap} keyword should be used, as
in the following example, which assigns 0, 1, 2 or 3 to `s` depending on whether
`x` is `"a"`, `"b"`, `"c"` or something else:

```liquidsoap
s = if x == "a" then 0
    elsif x == "b" then 1
    elsif x == "c" then 2
    else 3 end
```

This is equivalent (but shorter to write) to the following sequence of
imbricated conditional branchings:

```liquidsoap
s = if x == "a" then 0
    else
      if x == "b" then 1
      else
        if x == "c" then 2
        else 3 end
      end
    end
```

Finally, we should mention that the notation `c?a:b` is also available as a
shorthand for `if c then a else b end`, so that the expression

```liquidsoap
y = if x < 3 then "A" else "B" end
```

can be shortened to

```liquidsoap
y = (x<3)?"A":"B"
```

(and people will think that you are a cool guy).

#### Time predicates

Time predicates are special boolean values such as `{0h-7h}`. These values are
`true` or `false` depending on the current time. Some examples of time
predicates are

---

`{11h15-13h}` between 11h15 and 13h
`{12h}` between 12h00 and 12h59
`{12h00}` at 12h00
`{00m}` on the first minute of every hour
`{00m-09m}` on the first 10 minutes of every hour
`{2w}` on Tuesday
`{6w-7w}` on weekends

---

Above, `w` stands for weekday: 1 is Monday, 2 is Tuesday, and so on. Sunday is
both 0 and 7.

Time predicate can also be parsed at runtime, for instance if you want to create
them dynamically. The syntax is:

```liquidsoap
# f = time.predicate("00m-30m");;
f : () -> bool = <fun>
```

Be aware that, if parsing fails, it will raise `error.string`:

```liquidsoap
# f = time.predicate("foo")
Error 14: Uncaught runtime error:
type: string, message: "Failed to parse foo as time predicate"
```

### Unit

Some functions, such as `print`, do not return a meaningful value: we are
interested in what they are doing (e.g. printing on the standard output) and not
in their result. However, since typing requires that everything returns
something of some type, there is a particular type for the return of such
functions: `unit`. Just as there are only two values in the booleans (`true` and
`false`), there is only one value in the unit type, which is written `()`. This
value can be thought of as the result of the expression saying "I'm done".

### Lists

Some more elaborate values can be constructed by combining the previous ones. A
first kind is _lists_ which are finite sequences of values, being all of the
same type. They are constructed by square bracketing the sequence whose elements
are separated by commas. For instance, the list

```liquidsoap
[1, 4, 5]
```

is a list of three integers (1, 4 and 5), and its type is `[int]`, and the type
of `["A", "B"]` would obviously be `[string]`. Note that a list can
be empty: `[]`.

You can extract list elements through _splats_ such as

```liquidsoap
l = [1, 5, 7, 8, 9]
let [x, _, z, ...t] = l
```

In this example, the value of `x` is `1`, the value of `z` is `7` and the value of `t`
is [`8, 9]`.

You can also combine lists in a similar way

```liquidsoap
x = [1, ...[2, 3, 4], 5, ...[6, 7]]
```

In this example, the value of `x` is `[1, 2, 3, 4, 5, 6 ,7]`

### Tuples

Another construction present in Liquidsoap is _tuples_ of values, which are
finite sequences of values which, contrarily to lists, might have different
types. For instance,

```
(3, 4.2, "hello")
```

is a triple (a tuple with three elements) of type

```
int * float * string
```

which indicate that the first element is an integer, the second a float and the
third a string.

Similarly to lists, there is a special syntax in order to access
tuple elements. For instance, if `t` is the above tuple `(3, 4.2, "hello")`, we can write

```liquidsoap
let (n, x, s) = t
```

which will assign the first element to the variable `n`, the second element to
the variable `x` and the third element to the variable `s`.

## Programming primitives

### Variables

We have already seen many examples of uses of _variables_: we use

```liquidsoap
x = e
```

in order to assign the result of evaluating an expression `e` to a
variable `x`, which can later on be referred to as `x`. Variables can be masked:
we can define two variables with the same name, and at any point in the program the
last defined value for the variable is used:

```liquidsoap
n = 3
print(n)
n = n + 2
print(n)
```

will print `3` and `5`. Contrarily to most languages, the value for a variable
cannot be changed (unless we explicitly require this by using references, see
below), so the above program does not modify the value of `n`, it is simply that
a new `n` is defined.

There is an alternative syntax for declaring variables which is

```liquidsoap
def x =
  e
end
```

It has the advantage that the expression `e` can spread over multiple lines and
thus consist of multiple expressions, in which case the value of the last one
will be assigned to `x`. This is particularly useful to use local variables when defining a value.

### References

As indicated above, by default, the value of a variable cannot be
changed. However, one can use a _reference_ in order to be able to do this.
Those can be seen as memory cells, containing values of a given fixed type,
which can be modified during the execution of the program. They are created with
the `ref` keyword, with the initial value of the cell as argument. For instance,

```liquidsoap
r = ref(5)
```

declares that `r` is a reference which contains `5` as initial value. Since `5`
is an integer (of type `int`), the type of the reference `r` will be

```
ref(int)
```

meaning that its a memory cell containing integers. On such a reference, two
operations are available.

- One can obtain the value of the reference by applying the reference to `()`,
  so that `r()` denotes the value contained in the reference `r`, for instance

  ```liquidsoap
  x = r() + 4
  ```

  declares the variable `x` as being 9 (which is 5+4).

- One can change the value of the reference by using the `:=` keyword, e.g.

  ```liquidsoap
  r := 2
  ```

  will assign the value 2 to `r`. Internally, this is done by calling the `set`
  method of the reference, so that the above is equivalent to writing

  ```liquidsoap
  r.set(2)
  ```

  which used to be the syntax for some reference manipulations.

### Loops

The usual looping constructions are available in Liquidsoap. The `for` loop
repeatedly executes a portion of code with an integer variable varying between
two bounds, being increased by one each time. For instance, the following code
will print the integers `1`, `2`, `3`, `4` and `5`, which are the values
successively taken by the variable `i`:

```liquidsoap
for i = 1 to 5 do
  print(i)
end
```

In practice, such loops could be used to add a bunch of numbered files
(e.g. `music1.mp3`, `music2.mp3`, `music3.mp3`, etc.) in a request queue for
instance.

The `while` loop repeatedly executes a portion of code, as long a condition is
satisfied. For instance, the following code doubles the contents of the
reference `n` as long as its value is below `10`:

```liquidsoap
n = ref(1)
while n() < 10 do
  n := n() * 2
end
print(n())
```

The variable `n` will thus successively take the values `1`, `2`, `4`, `8` and
`16`, at which point the looping condition `n() < 10` is not satisfied anymore
and the loop is exited. The printed value is thus `16`.

## Functions

Liquidsoap is built around the notion of function: most operations are performed
by those. For some reason, we sometimes call _operators_ the functions acting on
sources. Liquidsoap includes a standard library which consists of functions
defined in the Liquidsoap language, including fairly complex operators such as
`playlist` which plays a playlist or `crossfade` which takes care of fading
between songs.

### Basics

A function is a construction which takes a bunch of arguments and produces a
result. For instance, we can define a function `f` taking two float arguments,
prints the first and returns the result of adding twice the first to the second:

```liquidsoap
def f(x, y)
  print(x)
  2*x+y
end
```

This function can also be written on one line if we use semicolons (`;`) to
separate the instructions instead of changing line:

```liquidsoap
def f(x, y) = print(x); 2*x+y end
```

The type of this function is

```
(int, int) -> int
```

The arrow `->` means that it is a function, on the left are the types of the
arguments (here, two arguments of type `int`) and on the right is the type of
the returned value of the function (here, `int`). In order to use this function,
we have to apply it to arguments, as in

```
f(3, 4)
```

This will trigger the evaluation of the function, where the argument `x`
(resp. `y`) is replaced by `3` (resp. `4`), i.e., it will print `3` and return
the evaluation of `2*3+4`, which is `10`.

### Anonymous functions

For concision in scripts, it is possible define a function without giving it a
name, using the syntax

```liquidsoap
fun (x) -> ...
```

This is called an _anonymous function_, and it is typically used in order to
specify short handlers in arguments.

#### Anonymous function with no arguments

You will see that it is quite common to use anonymous functions with no
arguments. For this reason, we have introduced a special convenient syntax for
those and allow writing

```liquidsoap
{...}
```

instead of

```liquidsoap
fun () -> ...
```

### Labeled arguments

A function can have an arbitrary number of arguments, and when there are many of them it
becomes difficult to keep track of their order and their order matter! For
instance, the following function computes the sample rate given a number of
samples in a given period of time:

```liquidsoap
def samplerate(samples, duration) = samples / duration end
```

which is of type

```
(float, float) -> float
```

For instance, if you have 110250 samples over 2.5 seconds the samplerate will be
`samplerate(110250., 2.5)` which is 44100. However, if you mix the
order of the arguments and type `samplerate(2.5, 110250.)`, you
will get quite a different result and this will not be detected by
the typing system because both arguments have the same type. Fortunately, we can
give _labels_ to arguments in order to prevent this, which forces explicitly
naming the arguments. This is indicated by prefixing the arguments with a tilde
"`~`":

```liquidsoap
def samplerate(~samples, ~duration) = samples / duration end
```

The labels will be indicated as follows in the type:

```
(samples : float, duration : float) -> float
```

Namely, in the above type, we read that the argument labeled `samples` is a
float and similarly for the one labeled `duration`. For those arguments, we have
to give the name of the argument when calling the function:

```liquidsoap
samplerate(samples=110250., duration=2.5)
```

The nice byproduct is that the order of the arguments does not matter anymore, the
following will give the same result:

```liquidsoap
samplerate(duration=2.5, samples=110250.)
```

Of course, a function can have both labeled and non-labeled arguments.

### Optional arguments

Another useful feature is that we can give _default values_ to arguments, which
thus become _optional_: if, when calling the function, a value is not specified
for such arguments, the default value will be used. For instance, if for some
reason we tend to generally measure samples over a period of 2.5 seconds, we can
make this become the value for the `duration` parameter:

```{.liquidsoap include="samplerate3.liq" from="BEGIN" to="END"}

```

In this way, if we do not specify a value for the duration, its value will
implicitly be assumed to be 2.5, so that the expression:

```liquidsoap
samplerate(samples=110250.)
```

will still evaluate to 44100. Of course, if we want to use another value for the
duration, we can still specify it, in which case the default value will be
ignored:

```liquidsoap
samplerate(samples=132300., duration=3.)
```

The presence of an optional argument is indicated in the type by prefixing the
corresponding label with "`?`", so that the type of the above function is

```
(samples : float, ?duration : float) -> float
```

### Advanced argument syntax

Arguments can be ignored, typed, named (and renamed) and given default values
and all these possibilities can be combined.

Here is the syntax to do it:

```liquidsoap
# Ignored anonymous argument
def f(_) = 123 end
# f : ('a) -> int = fun (_) -> 123

# Typed anonymous argument
def f((foo:int)) = foo end
# f : (int) -> int = <fun>

# Anonymous argument with default value
def f(foo = 123) = foo end
# f : (?int) -> int = <fun>

# Typed ignored anonymous argument
def f((_:int)) = 123 end
# f : (int) -> int = fun (_) -> 123

# Typed anonymous argument with default value
def f((foo:int) = 123) = foo end
# f : (?int) -> int = <fun>

# Typed ignored anonymous argument with default value
def f((_:int) = 123) = 456 end
# f : (?int) -> int = fun (_=123) -> 456

# Typed named argument
def f(~(foo:int)) = foo end
# f : (foo : int) -> int = <fun>

# Named argument with rename
def f(~foo:bla) = bla end
# f : (foo : 'a) -> 'a = <fun>

# Ignored named argument
def f(~foo:_) = 123 end
# f : (foo : 'a) -> int = fun (~foo=_) -> 123

# Named argument with default value
def f(~foo=123) = foo end
# f : (?foo : int) -> int = <fun>

# Typed named argument with rename
def f(~foo:(bla:int)) = bla end
# f : (foo : int) -> int = <fun>

# Typed named argument with default value
def f(~(foo:int)=123) = foo end
# f : (?foo : int) -> int = <fun>

# Typed named argument with rename and default value
def f(~foo:(bla:int)=123) = bla end
# f : (?foo : int) -> int = <fun>

# Typed ignored named argument with default value
def f(~foo:(_:int)=123) = 456 end
# f : (?foo : int) -> int = fun (~foo=123) -> 456

# Ignored argument with default value
def f(~foo:_=123) = 456 end
# f : (?foo : int) -> int = fun (~foo=123) -> 456
```

### Getters

We often want to be able to dynamically modify some parameters in a script. For
instance, consider the operator `amplify`, which takes a float and an audio
source and returns the audio amplified by the given volume factor: we can expect
its type to be

```
(float, source('a)) -> source('a)
```

so that we can use it to have a radio consisting of a microphone input amplified
by a factor 1.2 by

```liquidsoap
mic   = input.alsa()
radio = amplify(1.2, mic)
```

In the above example, the volume 1.2 was supposedly chosen because the sound
delivered by the microphone is not loud enough, but this loudness can vary from
time to time, depending on the speaker for instance, and we would like to be
able to dynamically update it. The problem with the current operator is that the
volume is of type `float` and a float cannot change over time: it has a fixed
value.

In order for the volume to have the possibility to vary over time, instead of
having a `float` argument for `amplify`, we have decided to have instead an
argument of type

```
() -> float
```

This is a function which takes no argument and returns a float (remember that a
function can take an arbitrary number of arguments, which includes zero arguments). It is
very close to a float excepting that each time it is called the returned value
can change: we now have the possibility of having something like a float which
varies over time. We like to call such a function a _float getter_, since it can
be seen as some kind of object on which the only operation we can perform is get
the value. For instance, we can define a float getter by

```liquidsoap
n = ref(0.)
def f ()
  n := n() + 1.
  n()
end
```

Each time we call `f`, by writing `f()` in our script, the resulting float
will be increased by one compared to the previous one: if we try it in an
interactive session, we obtain

```
# f();;
- : float = 1.0
# f();;
- : float = 2.0
# f();;
- : float = 3.0
```

Since defining such arguments often involves expressions of the form

```liquidsoap
fun () -> e
```

which is somewhat heavy, we have introduced the alternative syntax

```liquidsoap
{e}
```

for it.

Finally, in order to simplify things a bit, you will see that the type of
amplify is actually

```
({float}, source('a)) -> source('a)
```

where the type `{float}` means that both `float` and `() -> float` are accepted,
so that you can still write constant floats where float getters are
expected. What we actually call a _getter_ is generally an element of such a
type, which is either a constant or a function with no argument.

In order to work with such types, the standard library often uses the following
functions:

- `getter`, of type `({'a}) -> {'a}`, creates a getter,
- `getter.get`, of type `({'a}) -> 'a`, retrieves the current value of a getter,
- `getter.function`, of type `({'a}) -> () -> 'a`, creates a function from a
  getter.

### Recursive functions

Liquidsoap supports functions which are _recursive_, i.e., that can call
themselves. For instance, in mathematics, the factorial function on natural
numbers is defined as fact(n)=1×2×3×...×n, but it can also be defined
recursively as the function such that fact(0)=1 and fact(n)=n×fact(n-1) when
n>0: you can easily check by hand that the two functions agree on small values
of n (and prove that they agree on all values of n). This last formulation has
the advantage of immediately translating to the following implementation of
factorial:

```liquidsoap
def rec fact(n) =
  if n == 0 then 1
  else n * fact(n-1) end
end
```

for which you can check that `fact(5)` gives 120, the expected result. As
another example, the `list.length` function, which computes the length of a
list, can be programmed in the following way in Liquidsoap:

```liquidsoap
def rec length(l)
  if l == [] then 0
  else 1 + length(list.tl(l)) end
end
```

We do not detail much further this trait since it is unlikely to be used for
radios, but you can see a few occurrences of it in the standard library.

## Records and modules

### Records

Suppose that we want to store and manipulate structured data. For instance, a
list of songs together with their duration and tempo. One way to store each song
is as a tuple of type `string * float * float`, but there is a risk of confusion
between the duration and the length which are both floats, and the situation
would of course be worse if there were more fields. In order to overcome this,
one can use a _record_ which is basically the same as a tuple, excepting that
fields are named. In our case, we can store a song as

```liquidsoap
song = { filename = "song.mp3", duration = 257., bpm = 132. }
```

which is a record with three fields respectively named `filename`, `duration`
and `bpm`. The type of such a record is

```
{filename : string, duration : float, bpm : float}
```

which indicates the fields and their respective type. In order to access a field
of a record, we can use the syntax `record.field`. For instance, we can print
the duration with

```liquidsoap
print("The duration of the song is #{song.duration} seconds")
```

Records can be re-used using _spreads_:

```liquidsoap
song = { filename = "song.mp3", duration = 257., bpm = 132. }

# This is a fresh value with all the fields from `song` and
# a new `id` field:
song_with_id = { id = 1234, ...song }
```

Alternatively, you can also extend a record using the explicit `v.{...}` syntax:

```liquidsoap
song = { filename = "song.mp3", duration = 257., bpm = 132. }

# This is a fresh value with all the fields from `song` and
# a new `id` field:
song_with_id = song.{id = 1234}
```

### Modules

Records are heavily used in Liquidsoap in order to structure the functions of
the standard library. We tend to call _module_ a record with only functions, but
this is really the same as a record. For instance, all the functions related to
lists are in the `list` module and functions such as `list.hd` are fields of
this record. For this reason, the `def` construction allows adding
fields in record. For instance, the definition

```liquidsoap
def list.last(l)
  list.nth(l, list.length(l)-1)
end
```

adds, in the module `list`, a new field named `last`, which is a function which
computes the last element of a list. Another shorter syntax to perform
definitions consists in using the `let` keyword which allows assigning a value
to a field, so that the previous example can be rewritten as

```liquidasoap
let list.last = fun(l) -> list.nth(l, list.length(l)-1)
```

If you often use the functions of a specific module, the `open` keyword allows
using its fields without having to prefix them by the module name. For instance,
in the following example

```liquidsoap
l = [1,2,3]
open list
x = nth(l, length(l)-1)
```

the `open list` directive allows directly using the functions in this module: we
can simply write `nth` and `length` instead of `list.nth` and `list.length`.

### Values with fields

A unique feature of the Liquidsoap language is that it allows adding fields to
any value. We also call them _methods_ by analogy with object-oriented
programming. For instance, we can write

```liquidsoap
song = "test.mp3".{duration = 123., bpm = 120.}
```

which defines a string (`"test.mp3"`) with two methods (`duration` and
`bpm`). This value has type

```
string.{duration : float, bpm : float}
```

and behaves like a string, e.g. we can concatenate it with other strings:

```liquidsoap
print("the song is " ^ song)
```

but we can also invoke its methods like a record or a module:

```liquidsoap
print("the duration is #{song.duration}")
```

The construction `def replaces` allows changing the main value
while keeping the methods unchanged, so that

```liquidsoap
def replaces song = "newfile.mp3" end
print(song)
```

will print

```
"newfile.mp3".{duration = 123., bpm = 120.}
```

(note that the string is modified but not the fields `duration` and `bpm`).

### Optional fields

During the execution of your script, it can be useful to allow functions
to receive records that may or may not have a specific field. This can
be used, for instance, to model optional arguments.

This can be achieved in two ways:

1. Using the `x.foo ?? default` syntax

Here's an example:

```liquidsoap
# This functions adds 1 to x unless options has a
# add field in which case it adds this value
def f(x, options) =
  x + (options.add ?? 1)
end
```

The type of this function is:

```liquidsoap
f : (int, 'a.{add? : int}) -> int = <fun>
```

which denotes that the `options` argument can be any value that may or may not have
a `add` field. However, if this field is present, it must be of type `int`.

2. Using the `x?.foo` syntax

Given a variable `x`, `x?.foo` returns the field value `foo`, if present, or `null`
otherwise.

The `?.` syntax can be chained and works with functions, which make it a very convenient
way to drill deep inside nested records:

```liquidsoap
x?.fn(123, "aabb")?.field
```

## Patterns

As explained earlier, you can use several constructions to extract data from structured values such
as `let [x, y] = l` and etc. These constructions are called _patterns_.

Patterns allows to quickly access values nested deeply inside structured data in a way that remains pretty intuitive when
reading the code.

Patterns are constructed using _variable placeholders_, which are either a variable name such as: `x`, `foo`, etc. or
the special symbol `_` for any ignored value.

### Tuple patterns

Tuple patterns are pretty straight forward and consist of any sequence of variable captures:

```liquidsoap
let (x, y, _, z) = (123, "aabbcc", true, 3.14)
# x = 1, y = "aabbcc", z = 3.14
```

### List patterns

List patterns are composed of variable placeholders, etc. and spreads of the form:
`...<variable placeholder>` such as: `...z`. The spread `..._`
can be simply written `...`. See below for an example.

You can use any combination of:

- Forward variable names: these capture the first elements of the list.
- One spread: this captures any remaining element as a list.
- Backward variable names: these capture the last elements of a the list.

Here are some examples:

```liquidsoap
# Forward capture:
let [x, y, z] = [1, 2, 3]
# x = 1, y = 2, z = 3

# Forward capture with spread:
let [x, y, ...z] = [1, 2, 3, 4]
# x = 1, y = 2, z = [3, 4]

# Forward capture with ignored values:
let [_, x, ...z] = [1, 2, 3, 4]
# x = 2, z = [3, 4]

# Full capture:
let [x, y, ...z, t, u, v] = [1, 2, 3, 4, 5, 6, 7, 8, 9]
# x = 1, y = 2, z = [3, 4, 5, 6, 7], t = 7, u = 8, v = 9

# Backward capture only.
let [..., t, u, v] = [1, 2, 3, 4, 5]
# t = 3, u = 4, v = 5
```

### Record and module patterns

Record and module patterns consist of either variable names (not variable capture!), which capture method values
or variable names with an associated pattern.

Record patterns are of the form: `{<captured methods>}` while module patterns are of the form: `<variable capture>.{<captured methods>}`

Here are some examples:

```liquidsoap
# Record capture
let {foo, bar} = {foo = 123, bar = "baz", gni = true}
# foo = 123, bar = "baz"

# Record capture with spread
let {foo, bar, ...x} = {foo = 123, bar = "baz", gni = true}
# foo = 123, bar = "baz", x = {gni = true}

# Module capture
let v.{foo, bar} = "aabbcc".{foo = 123, bar = "baz", gni = true}
# v = "aabbcc", foo = 123, bar = "baz"

# Module capture with ignored value
let _.{foo, bar} = "aabbcc".{foo = 123, bar = "baz", gni = true}
# foo = 123, bar = "baz"

# Record capture with sub-patterns. Same works for module!
let {foo = [x, y, z], gni} = {foo = [1, 2, 3], gni = "baz"}
# foo = [1, 2, 3], x = 1, y = 2, z = 3, gni = "baz"

# Record capture with optional methods:
let { foo? } = ()
# foo = null

let { foo? } = { foo = 123 }
# foo = 123
```

## Combining patterns

As seen with record and modules, patterns can be combined at will, for instance, these
are all valid patterns:

```liquidsoap
let [{foo}, {gni}, ..., {baz}] = l

let (_.{ bla = [..., z] }, t, _, u) = x
```

## Advanced values

In this section, we detail some more advanced values than the ones presented in. You are not expected to be understanding
those in details for basic uses of Liquidsoap.

### Errors

In the case where a function does not have a sensible result to return, it can raise an
_error_. Typically, if we try to take the head of the empty list without
specifying a default value (with the optional parameter `default`), an error will be raised.
By default, this error will stop the script, which is usually not a desirable
behavior. For instance, if you try to run a script containing

```liquidsoap
list.hd([])
```

the program will exit printing

```
Error 14: Uncaught runtime error:
type: not_found, message: "no default value for list.hd"
```

This means that the error named "`not_found`" was raised, with a message
explaining that the function did not have a reasonable default value of the head
to provide.

In order to avoid this, one can _catch_ exceptions with the syntax

```liquidsoap
try
  code
catch err do
  handler
end
```

This will execute the instructions `code`: if an error is raised at some point
during this, the code `handler` is executed, with `err` being the error. For
instance, instead of writing

```liquidsaop
l = []
x = list.hd(default=0, l)
```

we could equivalently write

```liquidsoap
l = []
x =
  try
    list.hd(l)
  catch err do
    0
  end
```

The name and message associated to an error can respectively be retrieved using
the error `kind` and `message` attributes, e.g. we can write

```liquidsoap
try
  ...
catch err do
  print("the error #{err.kind} was raised")
  print("the error message is #{err.message}")
end
```

Typically, when reading from or writing to a file, errors will be raised when a
problem occurs (such as reading from a non-existent file or writing a file in a
non-existent directory) and one should always check for those and log the
corresponding message:

```liquidsoap
data = "bla"
try
  file.write(data=data, "/non/existent/path")
catch err do
  log.important("Could not write to file: #{error.message(err)}")
end
```

Specific errors can be caught with the syntax

```liquidsoap
try
  ...
catch err : l do
  ...
end
```

where `l` is a list of error names that we want to handle here.

Errors can be raised from Liquidsoap with the function `error.raise`, which
takes as arguments the error to raise and the error message. For instance:

```liquidsoap
error.raise(error.not_found, "we could not find your result")
```

We should also mention that all the errors should be declared in advance
with the function `error.register`, which takes as argument the name of the new
error to register:

```liquidsoap
myerr = error.register("my_error")
error.raise(myerr, "testing my own error")
```

Lastly, if you need to make sure that a certain piece of code is executed
whether or not there is an exception raised, you can use _finally_:

```liquidsoap
# Without a catch block
try
  ...
finally
  ...
end

# With a catch block
try
  ...
catch ... do
  ...
finally
  ...
end
```

This is roughly equivalent to:

```liquidsoap
finally_called = ref(false)
def finally() = ... end
try
  let ret = ...
  finally_called := true
  finally()
  ret
# If specified:
catch ... do
  let ret = ...
  if not finally_called() then finally() end
  ret
end
```

The biggest different is that `finally` is called on all errors, including internal errors that cannot
be caught by the runtime code.

Errors raised in a `finally` block do override any previously raised errors.

### Nullable values

It is sometimes useful to have a default value for a type. In Liquidsoap, there
is a special value for this, which is called `null`. Given a type `t`, we write
`t?` for the type of values which can be either of type `t` or be `null`: such a
value is said to be _nullable_. For instance, we could redefine the `list.hd`
function in order to return null (instead of raising an error) when the list is
empty:

```liquidsoap
def list.hd(l)
  if l == [] then null else list.hd(l) end
end
```

whose type would be

```
(['a]) -> 'a?
```

since it takes as argument a list whose elements are of type `'a` and returns a
list whose elements are `'a` or `null`. As it can be observed above, the null
value is created with `null`.

In order to use a nullable value, one typically uses the construction `x ?? d`
which is the value `x` excepting when it is null, in which case it is the
default value `d`. For instance, with the above head function:

```liquidsoap
x = list.hd(l)
print("the head is " ^ (x ?? "not defined"))
```

Some other useful functions include

- `null.defined`: test whether a value is null or not,
- `null.get`: obtain the value of a nullable value supposed to be distinct from `null`,
- `null.case`: execute a function or another, depending on whether a value is
  null or not.

### Runtime evaluation of scripting values

Similarly to how JSON is [parsed](json.html), you can evaluate string into values at runtime
using the `eval` decorator. As with JSON, too, the recommended way to use it is by adding an
explicit type annotation:

```liquidsoap
let eval (x: {foo: int, bla: string}) = "{foo = 123, bla = \"gni\"}"
print("x.foo = #{x.foo}, x.bla = #{x.bla}")
```

### Including other files

It is often useful to split your script over multiple files, either because it
has become quite large, or because you want to be able to reuse common functions
between different scripts. You can include a file `file.liq` in a script by
writing

```liquidsoap
%include "file.liq"
```

which will be evaluated as if you had pasted the contents of the file in place
of the command.

For instance, this is useful in order to store passwords out of the main file, in
order to avoid risking leaking those when handing the script to some other
people. Typically, one would have a file `passwords.liq` defining the passwords
in variables, e.g.

```liquidsoap
radio_pass = "secretpassword"
```

and would then use it by including it:

```liquidsoap
%include "passwords.liq"

radio = ...
output.icecast(%mp3, host="localhost", port=8000,
               password=radio_pass, mount="my-radio.mp3", radio)
```

so that passwords are not shown in the main script.

### Code comments

Comments can be added to your code in two ways:

_Multi-line comments_ are comments that can span multiple lines. They are delimitated
by the sequence of characters `#<` at the beginning and `>#` at the end. Anything
in between those two sequences is considered code comment.

Here are some examples:

Simple multiline comments:

```liquidsoap
#< This is a comment >#
```

Multiline comments can be nested:

```liquidsoap
#<
This is a top-level comment

  # This is also a comment

  #<
    This is a nested code comment
  >#
>#
```

Fancy looking multiline comment

```liquidsoap
#<------- BEGIN CODE COMMENT ----#
Comments can also look like this
#--------- END CODE COMMENT ----->#
```

_Single-line comments_ are comments that are limited to the current line. Such comments
are started with the character `#` without a following `<`. Anything after the initial
`#` character and until the end of the line is considered code comment:

```liquidsoap
def f(x) = # This is a single line comment.
  123
end
```

## Caching

Type-checking scripts can take a lot of time and consume memory. To optimize things, this step can be cached.

During the first execution, the script is parsed, type checked and evaluated. On second and any following execution, a cache of the script is used, reducing the typechecking phase, sometimes by a `100x` factor!

Here's a log without caching on a M3 macbook pro:

```
2024/07/03 14:31:41 [startup:3] main script hash computation: 0.03s
2024/07/03 14:31:41 [startup:3] main script cache retrieval: 0.03s
2024/07/03 14:31:41 [startup:3] stdlib hash computation: 0.03s
2024/07/03 14:31:41 [startup:3] stdlib cache retrieval: 0.03s
2024/07/03 14:31:41 [startup:3] Typechecking stdlib: 3.37s
2024/07/03 14:31:41 [startup:3] Typechecking main script: 0.00s
```

And the same log after caching:

```
2024/07/03 14:32:59 [startup:3] main script hash computation: 0.02s
2024/07/03 14:32:59 [startup:3] Loading main script from cache!
2024/07/03 14:32:59 [startup:3] main script cache retrieval: 0.05s
```

Scripts can be cached ahead of time without executing them, for instance while compiling a docker image, using `--cache-only`. Caching can also be disabled using `--no-cache`.

Caching happens at two different time:

- First the standard library is cached
- Then the script itself is cached

Caching the standard library makes it possible to run the type-checker faster on new scripts. Here's an example of a log from running a new script with
a cached standard library:

```
2024/07/03 14:33:27 [startup:3] main script hash computation: 0.02s
2024/07/03 14:33:27 [startup:3] main script cache retrieval: 0.02s
2024/07/03 14:33:27 [startup:3] stdlib hash computation: 0.03s
2024/07/03 14:33:27 [startup:3] Loading stdlib from cache!
2024/07/03 14:33:27 [startup:3] stdlib cache retrieval: 0.10s
2024/07/03 14:33:27 [startup:3] Typechecking main script: 0.00s
```

Caching can be disabled by setting `LIQ_CACHE` to anything else than `"true"`.

### Cache locations

Cache files can accumulate and also take up disk space so it is important to know where they are located!

There are two type of cache locations:

- System cache for cached files that should be shared with all liquidsoap scripts. This is where the standard library cache is located. This location is a system-wide path on unix system such as `/var/cache/liquidsoap`.
- User cache for cached files that are specific to the user running liquidsoap scripts. On unix systems, this location is at `$HOME/.cache/liquidsoap`.

On windows, the default cache directory for both type of cache locations is in the same directory as the binary.

At runtime, `liquidsoap.cache(mode=<mode>)` returns the cache directory. `mode` should be one of: `"user"` or `"system"`.

### Cache maintenance

There is a cache maintenance routine which deletes unused cache files after `10` days and keeps the cache to a maximum of `200` files.

You can run the cache maintenance routing by calling `liquidsoap.cache.maintenance(mode=<mode>)` manually. Here, too, `mode` should be one of: `"user"` or `"system"`.

### Cache security

Please be aware that the cache does _not_ encrypt its values. As such, user cache files should be considered sensitive as they may contain password and other runtime secrets
that are available through your scripts. We recommend to:

- Use environment variables as much as possible when passing secrets
- Secure your user script and cache files.

The default creation permissions for user cache files is: `0o600` so only the user creating them should be able to read them. You should make sure that your script permissions are also similarly restricted.

### Cache and memory usage

One side-benefit from loading a script from cache is that the entire typechecking process is skipped.

This leads to a significant reduction in initial memory consumption, typically down from about `375MB` to about `80MB`!

If memory consumption is a concern but you are not sure you can cache your script, you can also set the environment variable
`settings.init.compact_before_start` to `true`:

```liquidsoap
settings.init.compact_before_start := true
```

This will run the OCaml memory compaction algorithm after typechecking your script but before running it. This will result
in a similar memory footprint when running the script but will delay its initial startup time.

### Cache environment variables

The following environment variables control the cache behavior:

- `LIQ_CACHE`: disable the cache when set to anything else than `1` or `true`
- `LIQ_CACHE_SYSTEM_DIR`: set the cache system directory
- `LIQ_CACHE_SYSTEM_DIR_PERMS`: set the permission used when creating cache system directory (and its parents when needed). Default: `0o755`
- `LIQ_CACHE_SYSTEM_FILE_PERMS`: set the permissions used when creating a system cache file. Default: `0o644`
- `LIQ_CACHE_USER_DIR`: set the cache user directory
- `LIQ_CACHE_USER_DIR_PERMS`: set the permission used when creating cache user directory (and its parents when needed). Default: `0o700`.
- `LIQ_CACHE_USER_FILE_PERMS`: set the permissions used when creating a user cache file. Default: `0o600`
- `LIQ_CACHE_MAX_DAYS`: set the maximum days a cache file can be stored before it is eligible to be deleted during the next cache maintenance pass.
- `LIQ_CACHE_MAX_FILES`: set the maximum number of files in each cache directory. Older files are removed first.
