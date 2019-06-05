The Liquidsoap scripting language
=================================

## Introduction

The Liquidsoap scripting language is a functional language with static, inferred types.
Confused? Let's have a quick look at what this means.

Before following, perhaps you should have an interactive console around so you
can try the examples we're gonna go through. We recommend that you install `liquidsoap` and `ledit` via `opam`.
Once done, just do:

```sh
% ledit liquidsoap --interactive

Welcome to the liquidsoap interactive loop.

You may enter any sequence of expressions, terminated by ";;".
Each input will be fully processed: parsing, type-checking,
evaluation (forces default types), output startup (forces default clock).

Logs can be found in "/path/to/liquidsoap.log".

#
```

We can now get started!

### The typing system

#### Static types

The Liquidsoap language has **static types**. This means that all variables have a defined types, which
describe the type of value that they hold. For instance:

```
# x = 1;;
x : int = 1
# y = "hullo!";;
y : string = "hullo!"
# z = 1.23;;
z : float = 1.23
```

We have define 3 different variables. `x` holds the value `1` and is of type `int`. `y` hold the value `"hullo!"` of type `string` and, finally, `z` holds the value `1.23`, of type `float`.

Note that `int` and `float` are two different type of numbers in this language!

Now, if variables have types, then some operations will be limited to certain types. Check this out:

```
# 1 + 2;;
- : int = 3
# x + 4;;
- : int = 5
# x + y;;
At line 13, char 3:
  this value has type
    string (inferred at line 3, char 3)
  but it should be a subtype of
    int (inferred at line 3, char 3)
```

We can add two `int` values but adding an `int` and a `string` does not work. What happen if we try to add an `int` and a `float`? Well:

```
# 1 + 2.3;;
At line 16, char 3:
  this value has type
    float
  but it should be a subtype of
    int (inferred at line 1, char 2-3)
```

Fortunately, the language provides functions to convert between types, when possible. These functions are usually named `type1_of_type2`. So, here, we can do:

```
# float_of_int(1) + 2.3;;
- : float = 3.3
```

#### Inferred types

### Functional language

