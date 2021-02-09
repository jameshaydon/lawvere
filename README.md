<div align="center">

# Lawvere

A categorical programming language

[Install](#buildinstallation) • [Tutorial](#tutorial) • [Editor support](#editor-support)

`<"What is your name?"> putLine getLine <"Hello {}!"> putLine`

</div>

- Program morphisms in any category with enough structure (e.g. [cartesian closed](https://ncatlab.org/nlab/show/cartesian+closed+category), [distributive](https://ncatlab.org/nlab/show/distributive+category), etc.). New compilers will be added, for the moment there is an evaluator in Haskell and a compiler to JavaScript.
- Effect system via free Freyd categories.
- Define (soon) locally finitely presentable categories and functors between these categories.
- Pointfree functional programming (no lambdas).

(Still very work-in-progress.)

The Lawvere language (and the executable `bill`) is named after [William Lawvere](https://en.wikipedia.org/wiki/William_Lawvere).

## Build/Installation

You can build the project with stack or nix.

### Stack

First install stack (`curl -sSL https://get.haskellstack.org/ | sh
`) and then use `stack build`. To install the `bill` executable (to `~/.local/bin`) run `stack install`.

### Nix

Make sure you have [nix](https://nixos.org/) and optionally [direnv](https://direnv.net/) installed.

_Optional:_ (but faster) install cachix (`nix-env -iA cachix -f https://cachix.org/api/v1/install`) and use the `lawvere` cache: `cachix use lawvere`.

To build project dependencies and tooling the first time enter a nix shell either using direnv (recommended):

```
$ echo "use nix" > .envrc
$ direnv allow
```

or manually:

```
$ nix-shell
```

Once in the nix shell, to build a release and run it:

```
$ nix-build nix/release.nix
$ result/bin/bill <file>
```

## Tutorial

### Basic types

Lawvere has support for some basic types. They are written as you would expect, e.g. `"hello"` for a string of character, and `42` for a special integer. However these actually denote _morphisms_ (in some category with has objects `String` and `Int`). For example `42` denotes the morphism which is constantly 42:

```lawvere
ar someNumber : {:} --> Int = 42
```

The above code defines an _arrow_ (using the `ar` keyword) in the category `Base`. The arrow has source `{:}` (which is notation for the unit type) and target `Int`. The definition of the arrow is simply `42`.

### Composition

The main way to build up larger programs from smaller ones is by using _composition_. The syntax for this is very lightweight, simply whitespace: `f g` denotes the composition of `f` and `g`. If you are coming from Haskell, note that this is _not_ `.`, it's `>>>`, that is, `f` comes first, then `g`. The identity morphism is written with no characters at all: ` `.

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

save this to a file, and use `bill`:

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

(Or run `stack exec bill -- test.law` or `cabal run bill -- test.law` if you haven't installed `bill`.)

A Lawvere file should always have a `main` morphism, whose source is `{:}` (the terminal object).

_Note:_ The checker is a work-in-progress and is far from complete.

### Compiling to JavaScript

To compile to JavaScript, use the `--js` option:

```
$ bill --js test.law
```

This will output a JavaScript program that logs the output. You can pipe this directly to `node`:

```
$ bill --js test.law | node
45
```

The JavaScript compiler isn't very well maintained and will just error out on the anything but the most basic features of the language.

### Products

We define a new object `Point` with the keyword `ob`, and specify a product using braces:

``` lawvere
ob Base Point = { x: Float, y: Float }
```

The morphism which projects out the `x` component from `Point` is written `.x`. This is supposed to remind one of the `foo.x` notation that is usual in other programming languages, except without anything preceding the dot.

To map _to_ a product we need to specify a [_cone_](https://ncatlab.org/nlab/show/limit#definition_in_terms_of_universal_cones). This specifies a morphism to each component of the product. For example,

``` lawvere
ar Base somePoint : {:} --> Point =
  { x = 2.3, y = 4.6 }
```

This works because, as mentioned above, `2.3` and `4.6` are morphisms of type `{:} --> Float`.

In general, to specify a morphism `X --> { a: A, b: B, c: C, ... }`, one uses a cone:

``` lawvere
{ a = f, b = g, ... }
```
where `f : X --> A`, `g : X --> B`, `h : X --> C`, etc.

A complete program would be:

``` lawvere
ob Base Point = { x: Float, y: Float }

ar Base horizontal : Point --> Float = .x

ar Base somePoint : {:} --> Point =
  { x = 2.3, y = 4.6 }

ar Base main : {:} --> Float =
  somePoint horizontal
```

whose result is `2.3`.

When there are no components one still uses the separator symbol. So the empty product object (the terminal object) is denoted `{:}`, and the unique morphism to is it denoted `{=}`.

By using parentheses instead of braces, the components are positional rather than named. In this case the projections are `.1`, `.2`, etc. Using a positional product for `Point` the previous program would be:

``` lawvere
ob Base Point = (Float, Float)

ar Base horizontal : Point --> Float = .1

ar Base somePoint : {:} --> Point =
  ( 2.3, 4.6 )

ar Base main : {:} --> Float =
  somePoint horizontal
```

### String interpolation

A string can contain interpolated expressions. For example, `"Name: {f}, Age: {g}"` denotes a morphism `A --> String` as long as both `f` and `g` are also morphisms `A --> String`.

The program:
``` lawvere
ar Base main : {:} --> String =
  { name= "James", hobby= "playing Go" } "{.name} likes {.hobby}."
```
will print `"James likes playing Go."`

### Sums

We can define sum-types too. Let's define the booleans:

``` lawvere
ob Base Bool = [ true: {:}, false: {:} ]
```

Using square brackets we define a sum type with two summands, `true` and `false`, each with the terminal object `{:}` as payload.

Sum types come equipped with canonical injections. The canonical injection into the component with name `foo` is denoted `foo.`, simply mirroring the notation for canonical projections.

In order to define some simple boolean functions, we'll need to learn how to map _from_ sums. This is done with a _cocone_, which specifies a morphism for each summand. This is exactly like a cone except using square brackets instead of braces. To illustrate this let's define the negation function:

``` lawvere
ar Base not : Bool --> Bool
  = [ true  = false.,
      false = true. ]
```

In words, we split the morphism into two cases. In the first case (on the `true` component) we use the canonical injection `false.`, on the other component we use `true.`.

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

This is a morphism _to_ a sum (`Bool`), so we can't use a cocone, and _from_ a product (`{x : Bool, y: Bool }`), so we can't use a cone---are we stuck? Intuitively we want to inspect one of the two arguments `(x` or `y`) in order to continue. For this we will use the _distributor_ `@x`. To understand what this does, first let's re-write `{x : Bool, y : Bool}` by expanding the definition of `Bool` at the `x` summand:

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

In words: "Perform a case analysis on `x`, if `x` is true, then return `y`, otherwise return `false`". Note the similarity with the equivalent Elm program (Haskell doesn't have anonymous records, making the comparison less clear), even though Lawvere has no variables or λs:

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

### Effects

(Very WIP)

Lawvere is is a pure language but allows programming with effects using free [Freyd categories](https://ncatlab.org/nlab/show/Freyd+category), much like Haskell is pure but allows programming with effects using monads or arrows. In fact Freyd categories and arrows are very similar, e.g. see [Categorical semantics for arrows](http://homepages.inf.ed.ac.uk/cheunen/publications/2008/arrows/arrows.pdf).

#### I/O

The `IO` effect is built-in. Here is an example of a morphism which performs I/O:

``` lawvere
ar Base[IO] hello : {:} --> String =
  <"What is your name?"> putLine
  getLine
  <"Hello {}"> putLine
```

To run this, one must use the `io` functor:

``` lawvere
ar InputOutput main : {:} --> {:} =
  io(hello)
```

This will print `What is your name`, wait for the user to input their name, and then greet them.

Cones (`{..}`) are not permitted in effectful morphisms, but one can still perform effects at a single component. Here is a program which asks for 2 pieces of user input:

``` lawvere
// Turn a question into an answer.
ar Base[IO] ask : String --> String =
  putLine getLine

// Ask some questions and then print a greeting.
ar Base[IO] greet : {:} --> {:} =
  <{name = "What is your name?", hobby = "What is your favourite hobby?"}>
  !name{ask}
  !hobby{ask}
  <"Hello {.name}, I like {.hobby} too!"> putLine

ar InputOutput main : {:} --> {:} =
  io(greet)
```

Effectful programming will be explained more in the next section. In practice the main points are:
- Cones (`{..}`) are not permitted.
- Pure computations must be wrapped in `<..>`.
- To run an effect at a single component of a product, use `!label{..}` syntax.
- To run effects you need to map to the `InputOutput` category with `io`.

#### State

As an example we define a new effect sketch for some integer state (see [here](/examples/freyd-state.law) for the full example).

``` lawvere
sketch IntState over Base = {
  ar get : {:} --> Int,
  ar put : Int --> {:}
}
```

This defines a theory for extending the `Base` category with two distinguished morphisms `get` and `put`.

We can then define morphisms in this abstract extension of `Base`. The following morphisms increments the state while returning the original value:

``` lawvere
ar Base[IntState] next : {:} --> Int =
  get <{ current = , next = }> !next{ <incr> put } <.current>
```

There are two new pieces of syntax:
- `<..>` denotes the canonical injection into the Freyd category. So this can be used for lifting any pure morphism. This performs the same role as the [`arr`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#v:arr) method of the [`Arrow`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#t:Arrow) type class in Haskell.
- `!label{...}` (where `label` can  be any component name). Effect categories do not (necessarily) have products, so using the cone syntax is prohibited. The sequencing of effects is specified by using the categorical composition. The Freyd category does have the same objects as the pure category it extends however, and an effectful morphisms can be performed at one component of a product of the base category. If `f : B --> B'` is effectful morphism and `{a : A, b : B, c : C}` is a product in the pure category, then `!b{f} : {a : A, b : B, c : C} --> {a : A, b : B', c : C}` is another effectful morphism. In other words, `!b{f}` means "perform effect `f` at component `b`". This performs the same role as [`first`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#v:first) in [`Arrow`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#t:Arrow) except for any component.

So `next` works as follows:
- `get` the current state,
- Fanout (duplicate) the current state using the pure morphism `{current = , next = }`. This is the cone which uses the identity morphism at both components.
- On the `next` component, `incr` and `put`,
- Project out (purely) the `current` component.

Next we'll specify how to map this function over a list. We can't reuse the `list` functor because that doesn't specify how to sequence the effects: should the effect be performed first on the head or the tail of the list?

``` lawvere
ar Base[IntState] mapNext : list({:}) --> list(Int) =
    [ empty = <empty.>,
      cons  = !head{ next } !tail{ mapNext } <cons.> ]
```

We explicitly sequence the effects, using composition, on first the head and then the tail of the list.

The Freyd category is still totally abstract, to actually use it we must define a functor to a category we know how to "execute":

``` lawvere
interp IntState pureState =
  over
    { state : Int, value : }
  handling
    { ar get  |->  {state = .state, value = .state},
      ar put  |->  {state = .value, value = {=}}
    }
  summing
    @value
  side
    { eff = { state = .state,
              value = .value .eff }
            eff,
      pur = .value
    }
    { state = .eff .state,
      value = { pur = .pur .pur,
                eff = .eff .value }
    }
```

This defines an interpretation of the `IntState` sketch.

- First one specifies how the interpretation acts on the pure morphisms: it maps them using the functor `{state: Int, value: }`, in other words what used to be an object `X` is now interpreted as `{state: Int, value: X}`, the same object but bundled with `Int`.
- Next we specify how to handle the two generators `get` and `put`. `get` simply copies the state to the value component: `{state = .state, value = .state}`, while `put` copies the value component to the state one (while setting the state to unit): `{state = .value, value = {=}}`.
- The Freyd category does have sums, and the interpretation functor should preserve them, that's what `@value` does.
- Finally we need to specify how effectful morphisms are lifted into products, in this case `!eff{...}`, lifting some effectful morphism `A --> B` into a product `{ pur: P, eff: A } --> { pur: P, eff: B }`.

We can then execute this effect on an `exampleList`:

``` lawvere
ar Base main : {:} --> Int =
  { state = 0, value = }           // set the initial state to 0
  pureState(<exampleList> mapNext) // use the freyd arrow `counting` interpreting it with `pureState` functor
  .value                           // we are jut interesting in the result, not the accumulated state
```

Checkout the [full example](/examples/freyd-state.law).

## Editor support

For the moment there is only an [emacs mode](/tools/emacs). The syntax looks much better if you use a font with programming ligatures, e.g. [FiraCode](https://github.com/tonsky/FiraCode) or [alternatives](https://github.com/tonsky/FiraCode#alternatives).

## Development

To update the nix derivation when project dependencies change:

```
$ hpack
$ cabal2nix . > nix/packages/lawvere.nix
$ direnv reload
```

Note: Cabal is also available in the nix shell so you can build with it as well if you like:

```
$ cabal build
```

---

## TODO

- Implement more general diagrams and limits/colimits thereof.
- Make example showing extensible data programming, e.g. let-desugaring as a cartesian retract.
- Defining via sketches more pure categories, finitely presentable caregories, etc.
- Make a small (but not just a few lines) "real program".
- Allow one to define morphisms via curry/uncurry.
- Think about if diagrams (which are used for e.g. (co)limits) can be represented as functors directly (from discrete categories).
- Type checker is not complete; it really needs row variables to be implemented properly.
