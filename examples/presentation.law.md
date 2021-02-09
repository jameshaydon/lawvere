# Lawvere

## Warning

I will present and demo some of the features of the language.

But some of what I say is "aspirational" (i.e. does not work yet).

## The ideas

There were two ideas that led me to creating _Lawvere_:

- Wanted to use sketches for functional programming.

- `f(g(x))` syntax is wrong.

## Example: a λ-calc interpreter

```haskell
data Expr
  = Lit Scalar
  | Lamda (Expr -> Expr)
  | App (Expr, Expr)

eval :: Expr -> Val
eval = undefined

-- .. lots more
```

## Add let-bindings:

```haskell
data ExprLam
  = Lit' Int
  | Lambda' (Expr -> Expr)
  | App' (Expr, Expr)
  | Let' (Expr, Expr -> Expr) -- new!
```

## "Desugar" lets into λ

`let x = v in e` ~~> `(λx.e)v`

```haskell
desugar :: ExprLam -> Expr
desugar = \case
  Lit' i      -> Lit i
  Lambda' f   -> Lambda f
  App' (f, x) -> App (f, x)
  Let' (v, b) -> App (Lambda b, v)
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

- `T(S) -> Set` ~~> _explain_

- `Let -> Lam` ~~> `i : T(Lam) -> T(Let)`

## Cartesian retract

*Goal:* `d : T(Let) -> T(Lam)`

- Retract of `i`  ~~>  kills boilerplate
- Cartesian  ~~>

```
d(Let) :
d( (Expr,    Expr  =>   Expr)) -> d(Expr)
 (d(Expr), d(Expr  =>   Expr)) ->   Expr
 (  Expr,  d(Expr) => d(Expr)) ->   Expr
 (  Expr,    Expr  =>   Expr ) ->   Expr
✅
```

## Foo

```lawvere
ar Base plus3 : Int --> Int =
   incr incr incr

ar Base main : {:} --> Int =
  10 plus3
```
