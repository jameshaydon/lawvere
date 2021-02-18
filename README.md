<div align="center">

# Lawvere

A categorical programming language with effects

[Install](#buildinstallation) • [Tutorial](#tutorial) • [Editor support](#editor-support) • [Development](#development)

_Very work-in-progress_

</div>

```
(.playerA .points - .playerB .points)
{ leader = (> 0) [ true = "A", false = "B"],
  delta  = abs show }
"Player {.leader} is winning by {.delta} points!"
```

- Compile to any category that has structures corresponding to the programming features you use ([cartesian closed](https://ncatlab.org/nlab/show/cartesian+closed+category), [distributive](https://ncatlab.org/nlab/show/distributive+category), etc.).
- Comes with is an evaluator in Haskell, a [compiler to JavaScript](#compiling-to-javascript), and a "bytecode" compiler to a [categorical abstract machine](#the-categorical-abstract-machine).
- Effect system based on free effect categories.
- Point-free functional programming (no lambdas); a categorical take on concatenative programming.

The Lawvere language (and the executable `bill`) is named after [William Lawvere](https://en.wikipedia.org/wiki/William_Lawvere).

## Tutorial

### REPL

Once [installed](#buildinstallation), start a Lawvere REPL (Read-Eval-Print-Loop) with `bill -i`:

```
$ bill -i
--------------
Lawvere v0.0.0
--------------
> 40 + 2
42
> "hello"
"hello"
```

(Or `cabal run bill -- -i` or `stack exec bill -- -i` if `bill` isn't install.)

`bill` can also be given a file: `bill -i example.law`. By omitting the `-i` flag, the `main` arrow is executed directly and the REPL is not started. In the REPL, `:r` will reload the loaded file, and `:q` will terminate the session. This README file is a literate Lawvere script, so you can load it and try out the examples:

```
$ bill -i README.md
Check OK!
> answer + 1
43
```

### Basic types

Values of basic types are written as in other programming languages, e.g. `42` and `"hello world"`. But in Lawvere, everything is an arrow (Lawvere's equivalent of a function), so that these actually denote constant arrows. For example, `42` denotes the arrow which is constantly 42:

```lawvere
ar answer : {} --> Int = 42
```

The above code defines an arrow using the `ar` keyword. The arrow has source `{}` (which is the syntax for the unit type) and target `Int`.

When the REPL accepts an input, it actually executes it (with the Haskell evaluator) on the unit input. So for example inputting `incr` (which expects an `Int`) will result in an error.

Lawvere also has support for basic arithmetic and comparisons. These are operations on arrows, for example `f + g` forms the pointwise addition of arrows `f` and `g`.

### Composition

The main way to build up larger programs from smaller ones is by using _composition_. The syntax for this is very lightweight - it's simply whitespace! That is, `f g` denotes the composition of `f` and `g`. If you are coming from Haskell, note that this does _not_ correspond to `.`, but to `>>>`, that is, `f` comes first, then `g`.

(If you know Forth composition and literals as constant functions will feel familiar.)

To illustrate this we can use the built-in arrow `incr`, which increments an integer:

``` lawvere
ar plus3 : Int --> Int = incr incr incr

ar fourtyFive : {} --> Int = 42 plus3
```
will output `45`.

To run this, create a file with the above contents and use `bill`:

```
$ bill -i test.law
--------------
Lawvere v0.0.0
--------------
checking..
Check OK!
> fourtyFive
45
> fourtyFive plus3 incr
49
```

_Note:_ The checker is a work-in-progress and is far from complete.

The identity arrow is called `identity`, but you can also write it with nothing at all (or whitespace). So the  mathematical function [x ↦ x * x + 1] can be written `identity * identity + 1`, or simply `* + 1`:

``` lawvere
ar squarePlusOne : Int --> Int = identity * identity + 1

ar squarePlusOne' : Int --> Int = * + 1
```

```
$ bill -i test.law
> 2 squarePlusOne'
5
```

### Products

Datatypes in `lawvere` are called _objects_ (since they correspond to objects in category theory -- as arrows, as you might have guessed, correspond to morphisms).  We define a new object `Point` with the keyword `ob`. If the object is a product type (or 'struct', or 'record'), specify it using braces:

``` lawvere
ob Base Point = { x: Float, y: Float }
```

The arrow which projects out the `x` component from `Point` is written `.x`. (Think of the `foo.x` notation that is usual in other programming languages, except without anything preceding the dot.)

```
$ bill -i
> { user = { name = "Mina", age = 2 }, req = { format = "json" } } .user .name
"Mina"
```

To create arrow _to_ a product, we specify, again using braces, arrows to each component of the product (in categorical terms, a [_cone_](https://ncatlab.org/nlab/show/limit#definition_in_terms_of_universal_cones)) . For example,

``` lawvere
ar somePoint : {} --> Point =
  { x = 2.3, y = 4.6 }
```

This works because `2.3` and `4.6` are arrows of type `{} --> Float`, and the braces syntax uses arrows which all have the same source.

In general, arrows of type `X --> { a: A, b: B, c: C, ... }` can be written as

```
{ a = f, b = g, ... }
```
if `f : X --> A`, `g : X --> B`, `h : X --> C`, etc.

Here's a fuller example of using products:

``` lawvere
ob Base Point = { x: Float, y: Float }

ar linFun : Point --> Float = 2.0 * .x + 3.0 * .y

ar someNum : {} --> Float =
  { x = 2.3, y = 4.6 } linFun
```

`someNum` is then `18.4`.

The empty product object (in categorical terms, the terminal object) is written as `{}`, and the unique arrow to it is also `{}`. If there is any ambiguity (which occurs when defining functors) then you can use `{:}` and `{=}`.

By using parentheses instead of braces, the components are positional rather than named. In this case the projections are `.1`, `.2`, etc. Using a positional product for `Point` the previous program would be:

``` lawvere
ob Base PointPos = (Float, Float)

ar linFunPos : PointPos --> Float = 2.0 * .1 + 3.0 * .2

ar someNumPos : {} --> Float =
  (2.3, 4.6) linFunPos
```

### String interpolation

A string can contain interpolated expressions. For example, `"Name: {f}, Age: {g}"` denotes an arrow `A --> String` as long as both `f` and `g` are also morphisms `A --> String`.

The program:
``` lawvere
ar james : {} --> String =
  { name= "James", hobby= "playing Go" } "{.name} likes {.hobby}."
```
will result in `"James likes playing Go."`

### Sums

We can define sum types too. For instance, booleans:

``` lawvere
ob Base Bool = [ true: {}, false: {} ]
```

Using square brackets we define a sum type with two summands, `true` and `false`, each with `{}` as payload.

Sum types come equipped with constructors (injection). The constructor into the component with name `foo` is denoted `foo.`, simply mirroring the notation for projections.

In order to define some simple boolean functions, we'll need to learn how to map _from_ sums. This is like pattern matching, specifying an arrow for each summand (and thus, in categorical language, a cocone). This is similar to cones, except using square brackets instead of braces. To illustrate this let's define the negation function:

``` lawvere
ar not : Bool --> Bool
  = [ true  = false.,
      false = true. ]
```

In words, we split the arrow into two cases. In the first case (on the `true` component) we use `false.` constructor, on the other component we use `true.`.

In general, to specify an arrow:

```
[ a: A, b: B, c: C, ... ] --> X
```
one uses a cocone
```
[ a = f, b = g, c = h, ... ]
```
where `f : A --> X`, `g : B --> X`, `h : C --> X`, etc.

### Distribution

Continuing with boolean functions, let's try to define the `and` function:

```
ar and : {x: Bool, y: Bool} --> Bool = ?
```

This is an arrow _to_ a sum (`Bool`), so we can't use a cocone, and _from_ a product (`{x : Bool, y: Bool }`), so we can't use a cone---are we stuck? Intuitively we want to inspect one of the two arguments `(x` or `y`) in order to continue. For this we will use the _distributor_ `@x`. To understand what this does, first let's re-write `{x : Bool, y : Bool}` by expanding the definition of `Bool` at the `x` summand:

```
{ x: [ true: {}, false: {}], y: Bool }
```

The type of `@x` is:

```
@x : { x: [ true: {}, false: {}], y: Bool } --> [ true: { x: {}, y: Bool}, false: { x: {}, y: Bool } ]
```

The morphism `@x` transforms the product into a sum; a sum with the same summand names as the sum in the component it targets. So in this case we end up with a sum with summands `true` and `false`, and the `x` component contains the unwrapped payload for the original sum at `x` (in this case they are both `{}`).

Using this we can define `and` as follows:

``` lawvere
ar and : {x : Bool, y : Bool} --> Bool =
  @x [ true  = .y,
       false = {} false. ]
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
  [ empty: {},
    cons:  { head: Int, tail: ListI }
  ]
```

An example list can be built up by composing morphisms together:

``` lawvere
ar aFewPrimes : {} --> ListI =
  empty.
  { head = 2, tail = } cons.
  { head = 3, tail = } cons.
  { head = 5, tail = } cons.
```

Note that in `{ head = 2, tail = }`, the arrow being used at the `tail` component is the identity. This could also be written `{ head = 2, tail = identity}`. Another thing to note is that the `2` being used here doesn't have source `{}`, indeed all integer literals actually have type `forall a. a --> Int` (and similarly for other scalars). This saves one from having to write `{} 2`. Like Haskell, Lawvere has some syntactic sugar for list-building:

``` lawvere
ar morePrimes : {} --> ListI =
  #(2, 3, 5, 7, 11)
```

We can sum over a list using a cocone:

``` lawvere
ar sum : ListI --> Int =
  [ empty = 0,
    cons  = .head + .tail sum ]
```

In words: If the list is `empty`, then return `0`. Otherwise take the `head`, and the `sum` of the `.tail`, and `+` them together.

```
> morePrimes sum
28
```

### Effects

(Very WIP)

Lawvere is is a pure language but allows programming with effects using free [Freyd categories](https://ncatlab.org/nlab/show/Freyd+category), much like Haskell is pure but allows programming with effects using monads or arrows. In fact Freyd categories and arrows are very similar, e.g. see [Categorical semantics for arrows](http://homepages.inf.ed.ac.uk/cheunen/publications/2008/arrows/arrows.pdf).

#### I/O

The `IO` effect is built-in. Here is an example of a morphism which performs I/O:

``` lawvere
ar Base[IO] hello : {} --> String =
  ~"What is your name?" putLine
  getLine
  ~"Hello {}" putLine
```

To run this, one must use the `io` functor:

``` lawvere
ar InputOutput main : {} --> {} =
  io(hello)
```

This will print `What is your name`, wait for the user to input their name, and then greet them.

Cones (`{..}`) are not permitted in effectful morphisms, but one can still perform effects at a single component. Here is a program which asks for 2 pieces of user input:

``` lawvere
// Turn a question into an answer.
ar Base[IO] ask : String --> String =
  putLine getLine

// Ask some questions and then print a greeting.
ar Base[IO] greet : {} --> {} =
  ~{name = "What is your name?", hobby = "What is your favourite hobby?"}
  !name(ask)
  !hobby(ask)
  ~"Hello {.name}, I like {.hobby} too!" putLine

ar InputOutput main : {} --> {} =
  io(greet)
```

Effectful programming will be explained more in the next section. In practice the main points are:
- Cones (`{..}`) are not permitted.
- Pure computations must be lifted with `~`.
- To run an effect at a single component of a product, use `!label(..)` syntax.
- To run effects you need to map to the `InputOutput` category with `io`.

#### State

In this example we'll define two sorts of effects: integer state and throwing a string error. See [here](/examples/partial-state.law) 

``` lawvere
effect IntState over Base {
  get : {} --> Int,
  put : Int --> {}
}

effect Err over Base {
  err : String --> []
}
```

This defines a theory `IntState` for extending the `Base` category with two distinguished morphisms for state manipulation: `get` and `put`. Similarly `Err` is a theory with a distinguished morphism `err : String -> []`. Note that `[]` is the empty sum, i.e. the initial object of `Base`. Therefore `err` can be used to map from `String` to _any_ object, by composing with `[]`, the empty cocone.

We can then define morphisms in this abstract extension of `Base`. The following morphism increments the state while returning the original value:

``` lawvere
ar Base[IntState] next : {} --> Int =
  get ~{ current = , next = incr} !next(put) ~.current
```

There are two new pieces of syntax:
- `~` denotes the canonical injection into the effect-category. So this can be used for lifting any pure morphism. This performs the same role as the [`arr`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#v:arr) method of the [`Arrow`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#t:Arrow) type class in Haskell.
- `!label(..)` (where `label` can  be any component name). Effect categories do not (necessarily) have products, so using the cone syntax is prohibited. The sequencing of effects is specified by using the categorical composition. The effect-category has the same objects as the pure category it extends however, and an effectful morphisms can be performed at one component of a product of the base category. If `f : A --> B'` is an effectful morphism and `{a : A, b : B, c : C}` is a product in the pure category, then `!b(f) : {a : A, b : B, c : C} --> {a : A, b : B', c : C}` is another effectful morphism. In other words, `!b(f)` means "perform effect `f` at component `b`". This performs the same role as [`first`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#v:first) in [`Arrow`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#t:Arrow) except for any component.

So `next` works as follows:
- `get` the current state,
- Create two versions of the current state (the one we want to return, and the one we want to `put`) using the pure morphism `{current = , next = incr}`,
- Do a `put` on the `next` component,
- Project out (purely) the `current` component.

For testing the error effect, we'll make a version which throws an error if the next number is greater than 3:

``` lawvere
ar Base[IntState, Err] nextSub3 : {} --> Int =
  next ~( { sub3 = < 3, ok = } @sub3 )
  [ true  = ~.ok,
    false = ~"Was not under 3!" err []]
```

Next we'll specify how to map this function over a list. We can't reuse the `list` functor because that doesn't specify how to sequence the effects: should the effect be performed first on the head or the tail of the list?

``` lawvere
ar Base[IntState, Err] mapNextSub3 : list({}) --> list(Int) =
    [ empty = ~empty.,
      cons  = !head(nextSub3) !tail(mapNextSub3) ~cons. ]
```

We explicitly sequence the effects, using composition, on first the head and then the tail of the list.

The effect-category is still abstract, to actually use the above we must define an effect-category over base and interpret the effects:

``` lawvere
category ErrIntState {
  ob             = ob Base,
  ar A --> B     = ar Base :
                   { state: Int, value: A } -->
                   [ err: String, suc: { state: Int, value: B } ],
  identity       = suc.,
  f g            = f [ err = err., suc = g ],
  SumOb(idx)     = SumOb(idx),
  sumInj(label)  = { state, value = .value sumInj(label) } suc.,
  sumUni(cocone) = @value sumUni(cocone)
}
```

This defines a new category with the same objects as `Base`, but with a different composition, identity and sum. Next we make this into an effect category over `Base`:

``` lawvere
effect_category pureErrIntState ErrIntState over Base {
  ~f      = { state, value = .value f } suc.,
  side(f) =
    { runeff = { state = .state,
                 value = .value .eff } f,
      onside = .value .pur }
    @runeff
    [ err = .runeff err.,
      suc = { state = .runeff .state,
              value = { eff = .runeff .value,
                        pur = .onside } } suc. ]
}
```

This is done by defining the canoncial injection `~` and `side`, the _action_ of the effect-category at product components. This is done by interpreting `!eff{..}`, lifting some effectful morphism `A --> B` into a product `{ pur: P, eff: A } --> { pur: P, eff: B }`.

Then we provide interpretations for the effects we want to use:

``` lawvere
interpret IntState in ErrIntState
  { get = { state = .state, value = .state} suc.,
    put = { state = .value, value = {}    } suc.
  }

interpret Err in ErrIntState
  { err = .value err. }
```

Finally, we can execute this effect:

``` lawvere
ar Base count : {} --> Int =
  { state = 0,                 // initialise the state to 0
    value = #({}, {}, {}) }    // we'll map over a list of size 3
  pureErrIntState(mapNextSub3)
```

This returns the list `#(0, 1, 2)`, but if one increases the size of the length of the list to 4, then it will instead throw an error.

Checkout the [full example](/examples/partial-state.law).

### The Categorical Abstract Machine

Lawvere has a compiler to a [Categorical Abstract Machine](https://www.sciencedirect.com/science/article/pii/0167642387900207). Again not all features are supported yet.

Compiling the `sum` function above (called on an empty list) produces:

```
$ bill --target vmcode examples/sum.law
#main:
  empty.
  call #sum
#sum:
  cocone empty:#sum_0 cons:#sum_1
#sum_0:
  scal 0
#sum_1:
  push
  scal 1
  push_cone
  pop
  push
  .tail
  call #sum
  push_cone
  pop
  end_cone 1 2
  prim plus
```

And you can execute the code on the virtual machine with:

```
bill --target vm examples/sum.law
--------------
Lawvere v0.0.0
--------------
Checking..
Check OK!
Running on categorical machine..
Result: 0
```

### Compiling to JavaScript

To compile to JavaScript, use the `--target js` option:

```
$ bill --target js test.law
```

This will output a JavaScript program that logs the output. You can pipe this directly to `node`:

```
$ bill --target js test.law | node
45
```

The JavaScript compiler isn't well maintained and will just error out on the anything but the most basic language features.

## Build/Installation

You can build the project with stack or nix.

### Stack

First install stack (`curl -sSL https://get.haskellstack.org/ | sh
`) and then use `stack build`. To install the `bill` executable (to `~/.local/bin`) run `stack install`.

On linux you may also need to install `tinfo`, e.g. `sudo apt-get install libtinfo-dev` on Ubuntu.

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

## Editor support

For the moment there is only an [emacs mode](/tools/emacs). The syntax looks much better if you use a font with programming ligatures, e.g. [FiraCode](https://github.com/tonsky/FiraCode) or [alternatives](https://github.com/tonsky/FiraCode#alternatives).

## Development

To update the nix derivation when project dependencies change:

```
$ hpack
$ cabal2nix . > nix/packages/lawvere.nix
$ direnv reload
```

_Note:_ Cabal is also available in the nix shell so you can build with it as well if you like:

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
