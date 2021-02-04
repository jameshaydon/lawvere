# Lawvere

> Programming in categories

Lawvere is a categorical programming language. It allows you to program morphisms in arbitrary cartesian closed categories, allows you to define locally finitely presentable categories, and allows you to define functors between these categories. Lawvere does not have lambdas, just morphisms.

## Tutorial

This is a small tutorial introducing the basics of the Lawvere programming language. The full program, along with other examples, is [here](/examples).

### Basic types

Lawvere has support for some basic types. They are written as you would expect, e.g. `"hello"` for the string of characters spelling "hello", and `42` for a special interger. However these actually denote _morphisms_ (in some category with has objects `String` and `Int`). For example `42` denotes the morphism which which is constantly 42:

```lawvere
ar someNumber : {:} --> Int = 42
```

The above code defined an _arrow_ (using the `ar` keyword) in the category `Base`. The arrow has source `{:}` (which is notation for the unit type) and target `Int`. The definition of the arrow is simply `42`.

### Composition

The main way to build up larger programs from smaller ones if by using _composition_. The syntax for this is very lightweight, it is simply whitespace: `f g` denotes the composition of `f` and `g`. If you are coming from Haskell, note that this is _not_ `.`, its `>>>`, that is, `f` comes first, then `g`. The identity morphisms is written with no characters at all ` `.

To illustrate this we can use the built-in function `incr`, which increments an integer:

``` lawvere
ar Base main : {:} --> Int = 42 incr incr incr
```
will output `45`.

To run this, save this code to a file and call `bill`:

```
$ bill test.law
Check OK!

input:
  {=}
output:
  45
```

### Products

Product types in `Lawvere` are defined using braces:

``` lawvere
ob Base Point = { x : Float, y : Flaot }
```

The morphism which projects out the `x` component from `Point` is written `.x`. This is supposed to remind one of the `foo.x` notation, except without anything preceding the dot.

``` lawvere
ar Base horizontal : Point --> Float = .x
```

The result of `main` is `2.3`.



---

## TODO

- Implement more general diagrams and limits/colimits thereof.
- Make example showing extensible data programming, e.g. let-desugaring as a cartesian retract.
- Defining via sketches more pure categories, finitely presentable caregories, etc.
- Make a small (but not just a few lines) "real program".
- Allow one to define morphisms via curry/uncurry.
- Think about if diagrams (which are used for e.g. (co)limits) can be
  represented as functors directly (from discrete categories).
