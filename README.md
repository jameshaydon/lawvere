# Lawvere

> Effectfully pointless programming made easy

Lawvere is a categorical programming language with sketches and effects. It allows you to program morhisms in arbitrary cartesian closed categories.

---

Programming in categories, or in point-free style, etc. has a reputation for being obtuse, even _pointless_. We hope to show that pointfree programming need not be _nameless_, and aim to produce a use-friendly language for this style of programming.

The main contributions making Lawvere what it is a nice syntax for representing cones and cocones in an intuitive way, and also a formalism for specifying category extensions and retrations, which can accomodate a extensible programming style (e.g. for defining ASTs), or interpreting effects.

---

## Some random notes:

```
((a -> b) * a) -> b


trash : a -> {}
= <{}


List a = Empty {}
       | Cons {head: a, tail: List a}

List a = colim{ empty: {},
}

map : lim{ f: Int -> Int, xs : colim{empty: {}, cons: {head: a, tail: List a}} } -> List Int

{f} * (empty{} + cons{head, tail})


empty{f} + cons{f, head, tail}

F * (1 + X)

A * B * C(X + Y + Z) * D    

distr C

  A * B * D * X
+ A * B * D * Y
+ A * B * D * Z

lim{A:, B:, C: colim{X:, Y:, Z:}, D:}

|
v

colim
{X: lim{A:, B:, D:, C:}
 Y: lim{A:, B:, D:, C:}
 Z: lim{A:, B:, D:, C:}
}

colim
{empty: {f: a -> b, xs: {} }
 cons:  {f: a -> b, xs: {head: a, tail: List a} }
}                                                      -> List Int


>{ empty: empty.
   cons: _ cons.
  }


{f: Int -> Int, cons: {head: a, tail: List a} } -> List a




<{ head: .xs .head .f,
   tail: {f: .f, xs: .xs .tail} map}
 .cons



map(f, xs) =
  case xs of
    [] -> []
    (head:tail) -> cons (f head) (map(f,tail))

---

{f: a -> b, xs: {head: a, tail: List a} } -> {head: _, tail: _}

<{ head: .xs .head .f,
   tail: {f: .f, xs: .xs .tail} map}
 .cons


get : {} -> Int
set : Int -> {}


get <{ current : id, next : incr set } current


extension
  get : {} -> Int

  get : {} -> IO Int


Hask

get : String -> Maybe Int
```

```
<{ m = isEven }
```


```
extension CanFail
  error : String -> Void

absurd : Void -> a = >{}

isEven Int -> Int =
  <{_1 = , _2 = 2}
  divRem
  <{div, isZeroRem = .rem <{ _1 = , _2 = 0} eq}
  @isZeroRem
  >{ true = .div,
     false = "was not even" error absurd }

foo : {m : Int, n : Int} -> Int =
  <{ m = isEven, n = isEven } <{ _1 = .m, _2 = .n } plus


```



```
<{ m = }
```

