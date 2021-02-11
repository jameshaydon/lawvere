# Lawvere

## Warning

I will present and demo some of the features of the language.

But the talk is also "aspirational" (i.e. not implemented yet).

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
  ar Lambda : (Expr ~> Expr) -> Expr
  ar App    : (Expr, Expr)   -> Expr

sketch Let extends Lam where
  ar Let : (Expr, Expr ~> Expr) -> Expr
```

## Theories of sketches

- `T(S)` the theory of the sketch `S`

- `T(S) -> Set` ~ models of `S`

- `Lam -> Let` |-> `i : T(Lam) -> T(Let)`

- initial models:
  `I(Lam) : T(Lam) -> Set`
  `I(Let) : T(Let) -> Set`
  `Expr  := I(Lam)(Expr)`
  `Expr' := I(Let)(Expr)`
  
- `I(Let) . i : T(Lam) -> T(Let) -> Set`
  ∴ `I(Lam) -> I(Let) . i`
  ∴ `I(Lam)(Expr) -> I(Let)(i(Expr))`
  ∴ `Expr -> Expr'`

- We get the natural inclusion of the concrete expression types.

- Other way?

## Desugar

Assume `d : T(Let) -> T(Lam)`, `d(Expr) = Expr`:

`I(Lam) . d : T(Let) -> T(Lam) -> Set`
∴ `I(Let) -> I(Lam) . d`
∴ `I(Let)(Expr) -> I(Lam)(d(Expr))`
∴ `Expr' -> Expr` ✔

## Cartesian retract

*Goal:* `d : T(Let) -> T(Lam)`

- Retract of `i`  ~~> `d(Expr) = Expr` + 🗑
- Cartesian ~~>

```
d(Let) :
d( (Expr,    Expr  =>   Expr)) -> d(Expr)
 (d(Expr), d(Expr  =>   Expr)) ->   Expr
 (  Expr,  d(Expr) => d(Expr)) ->   Expr
 (  Expr,    Expr  =>   Expr ) ->   Expr
```

`(v, bind) |-> App (Lambda bind, v)` _what we wanted to write_

## Dream

```
desugar : T(Let) --> T(Lam) =
  generators {
    Let |-> ((v, bind) |-> App (Lambda bind, v))
  }
  using (cartesian, retracts i)
```

> 💭 I should make a categorical programming language

## Other design considerations

A programming language is a text-format.

- `print : Prog   --> String`
  `parse : String --> Maybe Prog`

- `String` ~ free monoid on `Char`.

- If `Prog` is a category, `print` should be a functor.

- "Concatenative" PLs already had this idea.

- Clean/minimal semantics (e.g. for blockchain, "universal scripts", etc.)

## No λ, but lots of names

An experiment.

data <-> code

> Bad programmers worry about the code. Good programmers worry about data
> structures and their relationships.
Linus Torvalds

> Data dominates. If you've chosen the right data structures and organized
> things well, the algorithms will almost always be self-evident. Data
> structures, not algorithms, are central to programming.
Rob Pike

_Church encoding of data:_
Easy transform good use of data-structures into a mess of variables and lambdas.

## Scalars

Scalars are written as normal, but represent constant morphisms.

```lawvere
ar Base favInt : {:} --> Int =
  42

ar Base myName : {:} --> String =
  "James Haydon" 
```

## Composition

```lawvere
ar Base plus2 : Int --> Int =
   incr incr

ar Base plus4 : Int --> Int =
  plus2 plus2

ar Base plus10 : Int --> Int =
  plus4 plus4 plus2
```

## Usual operations

All operations operate at the morphism level:

```lawvere
ar Base inc : Int --> Int = + 1

// Explained later
ob Base Bool = [ true: {:}, false: {:}]

ar Base foo : {:} --> Bool =
  43 > 40 + 2
```

Assumes you want to compile to a category with "an `Int` object".

## Products

If your target category has products:

```lawvere
ob Base User = { name: String, points: Int }

// A cone:
ar Base newPlayer : String --> User =
  { name = , points = 0 }

// Another cone:
ar Base score : User --> User =
  { name = .name, points = .points (+ 40) }

// field punning:
ar Base score' : User --> User =
  { name, points = .points (+ 40) }

ar Base isPowerPlayer : User --> Bool =
  .points >= 100
```

## Sums

If your target category has sums:

```lawvere
// Already defined above:
// ob Base Bool = [ true: {:}, false: {:}]

ar Base worldIsFlat : {:} --> Bool = false.

// A cocone:
ar Base not : Bool --> Bool =
  [ true  = false.,
    false = true. ]
```

## Distributivity

If your target category is _distributive_:

```lawvere
ar Base and : { x: Bool, y: Bool } --> Bool =
  @x [ true  = .y,
       false = {=} false. ]
```

If the category is _extensive_, then could have case analysis at any morphism.

But `Hask` isn't extensive, e.g. would need refinement types.

## If-then-else

```lawvere
ar Base ifThenElse : { cond : Bool, tt : Int, ff : Int} --> Int =
  @cond [ true = .tt, false = .ff ]
```

## Sum a list

```lawvere
ob Base ListInt =
  [ empty: {:}, cons: { head: Int, tail: ListInt }] 

ar Base sum : ListInt --> Int =
  [ empty = 0,
    cons  = .head + .tail sum ]
    
ar Base exampleList : {:} --> ListInt =
  empty.
  { head = 3, tail = } cons.
  { head = 2, tail = } cons.
  { head = 1, tail = } cons.

ar Base exampleList2 : {:} --> ListInt =
  #(1, 2, 3) // sugar

ar Base sixIsSix : {:} --> Bool =
  exampleList sum == exampleList2 sum
```

## Case splitting v1

```lawvere
ar Base gameHeadline1 : { userA: User, userB: User } --> String =
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
ar Base gameHeadline2 : { userA: User, userB: User } --> String =
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
ar Base gameHeadline3 : { userA: User, userB: User } --> String =
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
ar Base gameHeadline4 : { userA: User, userB: User } --> String =
  { userA, userB,
    delta  = .userA .points - .userB .points;
    sign   = .delta (>= 0);
    leader = @sign [ true  = .users .userA,
                     false = .users .userB ] }
  "Player {.leader .name} is winning by {.delta abs show} points!"
```

## Case splitting v5

```
ar Base gameHeadline3 : { userA: User, userB: User } --> String =
  { userA, userB,
    delta     = .userA .points - .userB .points;
    winningBy = .delta abs show,
    leader    = if .delta (>= 0) then .userA else .userB
  }
  "Player {.leader .name} is winning by {.winningBy} points!"
```

## How to define if-then-else in the language?

_draw some stuff_

## Effects!

_Idea:_
- Sketches (which can be combined) produce free Freyd categories.
- These can be interpreted in various ways.

_Syntax:_
- `<..>`: canonical injection
- `!label{..}`: Freyd "action" at a product component

## Example

```lawvere
// Turn a question into an answer.
ar Base[IO] ask : String --> String =
  putLine getLine
```
