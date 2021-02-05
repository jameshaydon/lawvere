# Lawvere

> Programming in categories

Lawvere is a categorical programming language. It allows you to program morphisms in arbitrary cartesian closed categories, allows you (soon) to define locally finitely presentable categories, and allows you to define functors between these categories. Lawvere is completely pointfree; it does not have lambdas or variables, just morphisms.

## Build/Installation

First install stack (`curl -sSL https://get.haskellstack.org/ | sh
`) and then use `stack build`. To install the `bill` executable (to `~/.local/bin`) run `stack install`.

## Tutorial

This is a small tutorial introducing the basics of the Lawvere programming language. These can all be found in the [examples](/examples).

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
$ bill test.law
--------------
Lawvere v0.0.0
--------------
checking..
Check OK!

input:
  {=}
output:
  45
```

(Or run `stack exec bill -- test.law`.)

A lawvere file should always have a `main` morphism, whose source is `{=}` (the terminal object).

### Products

We define a new object `Point` with the keyword `ob`, and specify a product using braces:

``` lawvere
ob Base Point = { x: Float, y: Flaot }
```

The morphism which projects out the `x` component from `Point` is written `.x`. This is supposed to remind one of the `foo.x` notation that is usual in other programming languages, except without anything preceding the dot.

To map _to_ a product we need to specify a _cone_. This specifies a morphism to each component of the product. For example,

``` lawvere
ar Base somePoint : {:} --> Point =
  { x = 2.3, y = 4.6 }
```

This works because `2.3` and `4.6` are syntax for morphisms. In this case they have type `{:} --> Float`.

In general, to specify a morphism `X --> { a: A, b: B, c: C, ... }`, one uses a cone:

``` lawvere
{ a = f, b = g, ... }
```
where `f : X --> A`, `g : X --> B`, `h : X --> C`, etc.

A complete program would be:

``` lawvere
ar Base horizontal : Point --> Float = .x

ar Base somePoint : {:} --> Point =
  { x = 2.3, y = 4.6 }

ar Base main : {:} --> Int =
  somePoint horizontal
```

whose result is `2.3`.

When there are no components one still uses the seperator symbol. So the empty product object is denoted `{:}`, and the unique morphism to is it denoted `{=}`.

By using parentheses instead of braces, the components are positional rather than named. In this case the projections are `.1`, `.2`, etc. Using a positional product for `Point` the previous program would be:

``` lawvere
ob Base Point = (Float, Float)

ar Base horizontal : Point --> Float = .1

ar Base somePoint : {:} --> Point =
  ( 2.3, 4.6 )

ar Base main : {:} --> Float =
  somePoint horizontal
```

### Sums

We can define sum-types too. Let's define the booleans:

``` lawvere
ob Base Bool = [ true: {:}, false: {:} ]
```

Using square brackets we define a sumtype with two summands, `true` and `false`, each with the terminal object `{:}` as payload.

Sum types come equipped with canonical injections. The canonical injection into the component with name `foo` is denoted `foo.`, simply mirroring the notation for canonical projections.

In order to define some simple boolean functions, we'll need to learn how map _from_ sums. This is done with a cocone. This is simply the specification of a morphism for each component of the sum. This is exacly like a cone except using square brackets instead of braces. To illustrate this let's define the negation function:

``` lawvere
ar Base not : Bool --> Bool
  = [ true  = false.,
      false = true. ]
```

In words, we split the morphism into two cases. In the first case, on the `true` component, we use the canonical injection `false.`. On the other component, `false`, we use `true.`.

In general, to specify a morphism

``` lawvere
[ a: A, b: B, c: C, ... ] --> X 
```
one uses a cocone
``` lawvere
[ a = f, b = g, c = h, ... ]
```
where `f : A --> X`, `g : B --> X`, `h : C --> X`, etc.

### Distribution

Continuing with boolean functions, let's try to define the `and` function:

``` lawvere
ar Base and : {x: Bool, y: Bool} --> Bool = ?
```

This is a morphism _to_ a sum (`Bool`), so we can't use a cocone, and _from_ a product (`{x : Bool, y: Bool }`), so we can't use a cone---are we stuck? Intuitively we want to inspect one of the two arguments `x`, `y` in order to continue. For this we will use a _distributor_. To distribute the product at the `x` component, we use `@x`. To understand what this does, first let's re-write `{x : Bool, y : Bool}` by expanding the definition of `Bool` at the `x` summand:

``` lawvere
{ x: [ true: {:}, false: {:}], y: Bool }
```

The type of `@x` is:

``` lawvere
@x : { x: [ true: {:}, false: {:}], y: Bool } --> [ true: { x: {:}, y: Bool}, false: { x: {:}, y: Bool } ] 
```

The morphism `@x` transforms the product into a sum; a sum with the same summand names as the sum in the component it targets. So in this case we end up with a sum with summands `true` and `false`, and the `x` component contains the unwrapped payload for the original sum at `x` (in this case they are both `{:}`).

Using this we can define `and` as follows:

``` lawvere
ar Base and : {x : Bool, y : Bool} --> Bool =
  @x [ true  = .y,
       false = {=} false. ]
```

In words: "Perform a case analysis on `x`, if `x` is true, then return `y`, otherwise return `false`". Note the similarity with the equivalent Elm program (Haskell doesn't have anonymous records, making the comparison less clear), even though lawvere has no variables or lamdas:

```elm
and : { x : Bool, y : Bool } -> Bool
and input =
  case input.x of
    True  -> input.y
    False -> False 
```

### Summing over a list

In this example we'll sum up a list of values.

First we'll define lists of `Int`s (we'll learn how to define the list _functor_ later, which means we don't need to define a new object for each possible object of elements):

``` lawvere
ob Base ListI =
  [ empty: {:},
    cons:  { head: Int, tail: ListI }
  ]
```

In order to sum up the elements of a list we'll use the built-in function `plus`:

``` lawvere
plus : (Int, Int) --> Int
```

as follow:

``` lawvere
ar Base sumList : ListI --> Int =
  [ empty = 0,
    cons  = (.head, .tail sumList) plus ]
```

In words: If the list is `empty`, then return `0`. Otherwise take the `head`, and the `sumList` of the `.tail`, and `plus` them together.

## Editor support

For the moment there is only an [emacs mode](/tools/emacs). The syntax looks much better if you use a font with programming ligatures, e.g. [FiraCode](https://github.com/tonsky/FiraCode) or [alternatives](https://github.com/tonsky/FiraCode#alternatives).

---

## TODO

- Implement more general diagrams and limits/colimits thereof.
- Make example showing extensible data programming, e.g. let-desugaring as a cartesian retract.
- Defining via sketches more pure categories, finitely presentable caregories, etc.
- Make a small (but not just a few lines) "real program".
- Allow one to define morphisms via curry/uncurry.
- Think about if diagrams (which are used for e.g. (co)limits) can be
  represented as functors directly (from discrete categories).
