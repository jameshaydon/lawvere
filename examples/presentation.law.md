# Lawvere

## Warning

I will present and demo some of the features of the language.

But much is "aspirational" ~> not implemented yet.

## λ-calculus interpreter

```haskell
data Expr
  = Lit Scalar
  | Lamda (Expr -> Expr)
  | App (Expr, Expr)

eval :: Expr -> Val
eval = todo

-- .. lots more
```

## Add let-bindings:

```haskell
data Expr'
  = Lit' Int
  | Lambda' (Expr -> Expr)
  | App' (Expr, Expr)
  | Let' (Expr, Expr -> Expr) -- new!
  
eval' :: Expr' -> Val
eval' = annoying

-- .. more annoyance
```

## "Desugar" `let` into λ

`let x = v in e`    |-> `(λ x. e) v`

`Let' (v, binding)` |-> `App (Lambda binding, v)`

```haskell
desugar :: Expr' -> Expr
desugar = \case
  Lit' i      -> Lit i             -- 🗑
  Lambda' f   -> Lambda f          -- 🗑
  App' (f, x) -> App (f, x)        -- 🗑
  Let' (v, b) -> App (Lambda b, v) -- 🍖

eval' = eval . desugar
```

## Some sketches

```
sketch Lam where
  ob Expr
  ar Lit    : Scalar         -> Expr
  ar Lambda : (Expr => Expr) -> Expr
  ar App    : (Expr, Expr)   -> Expr

sketch Let extends Lam where
  ar Let : (Expr, Expr => Expr) -> Expr
```

## Theories of sketches

- `T(S)` := _theory_ of sketch `S`
  `T(S) -> Set` ~ models of `S` in `Set`

- `Lam >-> Let` |-> `i : T(Lam) >-> T(Let)`

- initial models:
  `I(Lam) : T(Lam) -> Set`
  `I(Let) : T(Let) -> Set`
  `Core := I(Lam)(Expr)`
  `Rich := I(Let)(Expr)`
  
- `I(Let) . i : T(Lam) >-> T(Let) -> Set`
  ∴ `I(Lam) -> I(Let) . i`
  ∴ `I(Lam)(Expr) -> I(Let)(i(Expr))`
  ∴ `Core -> Rich` in `Set`

- Other way?

## Desugar

Assume `d : T(Let) -> T(Lam)`,
       `d(Expr) = Expr`

∴ `I(Lam) . d : T(Let) -> T(Lam) -> Set`
∴ `I(Let) -> I(Lam) . d`
∴ `I(Let)(Expr) -> I(Lam)(d(Expr))`
∴ `Rich -> Core` ✔

## Cartesian retract

**Goal:** `d : T(Let) -> T(Lam)`

- Need only define on the sketch `Let`

- _Retract_ of `i : T(Lam) >-> T(Let)`
  * `d(Expr) = Expr`
  * no 🗑
  
- _Cartesian_ ~~>

Recall: `Let : (Expr, Expr => Expr) -> Expr` (last case)

```
  d( (Expr,    Expr  =>   Expr)) -> d(Expr)

~> (  Expr,    Expr  =>   Expr ) ->   Expr
```

`Let |-> App . (Lambda . π_2, π_1)`

## Quote

> Desugarings are cartesian retracts

## Dream

```
desugar : T(Let) --> T(Lam) =
  generators {
    Let |-> (π_2, Lambda . π_1) . App 
  }
  using (cartesian, retracts i)
```

> 💭 I should make a categorical programming language

## Other design considerations

- Source code ~  `String = [Char]` monoid
- Compilation ~> `[Instr]`         monoid

- `denot : String --> Prog`
  `compi : Prog   --> [Instr]`

- `Prog` a category ~> `denot`/`compi` functors
  * identity: `""`
  * composition `G . F` written: `"F G"`

- "Concatenative" PLs already had this idea (not very categorically though)

- Clean/minimal semantics.

## No λ, but lots of names

data <-(Church encoding)-> code

> Bad programmers worry about the code. Good programmers worry about data structures and their relationships.
Linus Torvalds

> Data dominates. If you've chosen the right data structures and organized things well, the algorithms will almost always be self-evident. Data structures, not algorithms, are central to programming.
Rob Pike

_Church encoding of data:_
Easy transform good use of data-structures into a mess of variables and lambdas.

## Scalars

Scalars are written as normal, but represent points:

```lawvere
ar favInt : {} --> Int =
  42

ar myName : {} --> String =
  "James Haydon" 
```

## Composition

```lawvere
ar plus2 : Int --> Int =
   incr

ar plus4 : Int --> Int =
  plus2 plus2

ar plus10 : Int --> Int =
  plus4 plus4 plus2
```

## Usual operations

All operations operate at the morphism level:

```lawvere
ar inc : Int --> Int = + 1

ar foo : {} --> Bool =
  43 > 40 + 2

// x -> 2 x^2 + 1
ar bar : Int --> Int =
  2 * * + 1

ar baz : { x: Int, y: Int } --> Int =
  2 * .x + 3 * .y + 6
```

Assumes you want to compile to a category with "an `Int` object".

## Products

If your target category has products:

```lawvere
ob Base User = { name: String, points: Int }

// A cone:
ar newPlayer : String --> User =
  { name = , points = 0 }

// Another cone:
ar score : User --> User =
  { name = .name, points = .points (+ 40) }

// field punning:
ar score' : User --> User =
  { name, points = .points (+ 40) }

ar isPowerPlayer : User --> Bool =
  .points >= 100
```

## Sums

If your target category has sums:

```lawvere
ob Base Bool = [ true: {}, false: {} ]

ar worldIsFlat : {} --> Bool = false.

// A cocone:
ar not : Bool --> Bool =
  [ true  = false.,
    false = true. ]
```

## Distributivity

If your target category is _distributive_:

```lawvere
ar and : { x: Bool, y: Bool } --> Bool =
  @x [ true  = .y,
       false = {} false. ]
```

If the category is _extensive_, then could have case analysis at any morphism.

But `Hask` isn't extensive (no refinement types).

## Sum a list

```lawvere
ob Base ListInt =
  [ empty: {}, cons: { head: Int, tail: ListInt }] 

ar exampleList : {} --> ListInt =
  empty.
  { head = 2, tail = } cons.
  { head = 1, tail = } cons.

ar exampleList2 : {} --> ListInt =
  #(1, 2, 3) // sugar

ar sum : ListInt --> Int =
  [ empty = 0,
    cons  = .head + .tail sum ]
```

## Case splitting

```lawvere
ar gameHeadline1 : { userA: User, userB: User } --> String =
  { users = ,
    delta = .userA .points - .userB .points }
  { users, delta,
    sign = .delta (>= 0) }
  { delta,
    leader = @sign [ true  = .users .userA,
                     false = .users .userB ] }
  "Player {.leader .name} is winning by {.delta abs show} points!"
```

3 cones because of data dependencies:
`users -> delta -> sign -> leader`

## Case splitting v2

But not really interested in `sign` per-se, just want to split on it:

```lawvere
ar gameHeadline2 : { userA: User, userB: User } --> String =
  { userA, userB,
    delta = .userA .points - .userB .points }
  { winningBy = .delta abs show,
    leader    = {v = , case = .delta (>= 0)}
                @case [ true  = .v .userA,
                        false = .v .userB ]
  }
  "Player {.leader .name} is winning by {.winningBy} points!"
```

## Case on a projection

This is a pattern:
```
{v = , case = .delta (>= 0)}
@case [ true  = .v .userA,
        false = .v .userB ]
```

More generally, when `f` targets a sum `[a : A, b : B, ...]`
```
{v = , case = f }
@case [ a = ..., b = ..., ... ]
```


## Case splitting v3

Sugar for `Bool` case:

```lawvere
ar gameHeadline3 : { userA: User, userB: User } --> String =
  { userA, userB,
    delta = .userA .points - .userB .points }
  { winningBy = .delta abs show,
    leader    = if .delta (>= 0) then .userA else .userB
  }
  "Player {.leader .name} is winning by {.winningBy} points!"
```

Sugar for general case too?

## Case splitting v4

Not yet implemented. Note the `;`.

```
ar gameHeadline4 : { userA: User, userB: User } --> String =
  { userA, userB,
    delta     = .userA .points - .userB .points;
    winningBy = .delta abs show,
    leader    = if .delta (>= 0) then .userA else .userB }
  "Player {.leader .name} is winning by {.winningBy} points!"
```

## How to define if-then-else in the language?

```
   -->
 A --> B  ~~>  A --> B
 ↓ 
Bool
```

Need more ideas to make it easy.

## Effects!

_Idea:_
- Sketches over a base category, which can be combined, produce free Freyd categories.
- These can be interpreted in various ways.

_Syntax:_
- `~`: canonical injection
- `!label(..)`: Freyd "action" at a product component

## Example

```lawvere
effect IO over Base {
  putLine : String --> {},
  getLine : {} --> String
}
```

```lawvere
ar Base[IO] ask : String --> String =
  putLine getLine
```

```lawvere
ar Base[IO] hello : {} --> String =
  ~"What is your name?" ask
  ~"Hello {}!" putLine

ar InputOutput helloIO : {} --> {} =
  io(hello)
```

## The action

```lawvere
ar Base[IO] twoQuestions : {} --> { name: String, hobby: String } =
  ~{ name = "What is your name?",
     hobby = "What's your hobby?" }
  !name(ask)
  !hobby(ask)
```

## State

```lawvere
effect IntState over Base {
  get : {} --> Int,
  put : Int --> {}
}

ar Base[IntState] next : {} --> Int =
  get ~{ current = , next = incr}
  !next(put) ~.current
```

## Errors

```lawvere
effect Err over Base {
  err : String --> []
}

ar Base[IntState, Err] nextSub3 : {} --> Int =
  next
  ~( { sub3 = < 3, ok = } @sub3 )
  [ true  = ~.ok,
    false = ~"Was not under 3!" err [] ]
```

## Map effects over a list

```lawvere
ar Base[IntState, Err] mapNextSub3 : list({}) --> list(Int) =
  [ empty = ~empty.,
    cons  = !head(nextSub3)
            !tail(mapNextSub3)
            ~cons. ]

ar ErrIntState count : {} --> Int =
  { state = 0, value = #({}, {}, {}, {}, {}) }
  pureErrIntState(mapNextSub3)
```

What is `pureErrIntState`?

## A new category

```lawvere
category ErrIntState {
  ob = ob Base,
  ar A --> B = ar Base :
    { state: Int, value: A } -->
    [ err: String,
      suc: { state: Int, value: B } ],
  identity = suc.,
  f g = f [ err = err., suc = g ],
  SumOb(idx) = SumOb(idx),
  sumInj(label) =
    { state, value = .value sumInj(label) } suc.,
  sumUni(cocone) = @value sumUni(cocone)
}
```

## An effect category

```lawvere
effect_category pureErrIntState ErrIntState over Base {
  ~f      = { state = .state, value = .value f } suc.,
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

## Interpretations

```lawvere
interpret Err in ErrIntState
  { err = "state: {.state show}, ERROR: {.value}." err. }

interpret IntState in ErrIntState
  { get = { state = .state, value = .state } suc.,
    put = { state = .value, value = {}     } suc.
  }
```
