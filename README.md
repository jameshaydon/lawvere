# Lawvere

> Programming in categories

Lawvere is a categorical programming language. It allows you to program morphisms in arbitrary cartesian closed categories, allows you to define locally finitely presentable categories, and allows you to define functors between these categories. Lawvere does not have lambdas, just morphisms.

## Tutorial

This is a small tutorial introducing the basics of the Lawvere programming language. These can all be found in the [example](/examples).

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

To run this, create a file with contents:

``` lawvere
ar Base someNumber : {:} --> Int = 42

ar Base main : {:} --> Int =
  someNumber incr incr incr
```

same this to a file, and use `bill`:

```
Lawvere v0.0.0
-------
checking..
Check OK!

input:
  {=}
output:
  45
```

A lawvere file should always have a `main` morphism, whose source is `{=}` (the terminal object).

### Products

We define a new object `Point` with the keyword `ob`, and specify a product using braces:

``` lawvere
ob Base Point = { x : Float, y : Flaot }
```

The morphism which projects out the `x` component from `Point` is written `.x`. This is supposed to remind one of the `foo.x` notation that is usual in other programming languages, except without anything preceding the dot.

To map _to_ a product we need to specify a _cone_. This specifies a morphism to each component of the product. For example,

``` lawvere
ar Base somePoint : {=} --> Point =
  { x = 2.3, y = 4.6 }
```

This works because `2.3` and `4.6` are syntax for morphisms. In this case they have type `{:} --> Float`.

A complete program would be:

``` lawvere
ar Base horizontal : Point --> Float = .x

ar Base somePoint : {:} --> Point =
  { x = 2.3, y = 4.6 }

ar Base main : {:} --> Int =
  somePoint horizontal
```

whose result is `2.3`.

### Sums

We can define sum-types too. Let's define the booleans:



---

## TODO

- Implement more general diagrams and limits/colimits thereof.
- Make example showing extensible data programming, e.g. let-desugaring as a cartesian retract.
- Defining via sketches more pure categories, finitely presentable caregories, etc.
- Make a small (but not just a few lines) "real program".
- Allow one to define morphisms via curry/uncurry.
- Think about if diagrams (which are used for e.g. (co)limits) can be
  represented as functors directly (from discrete categories).
