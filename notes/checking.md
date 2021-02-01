# Some notes on checking

When checking a cocone `[x: ..., y: ...]` we know that `x`, `y` are _all_ the cases of the source.

When checking a cone `{x: ..., y: ...}` we know that `x`, `y` are _all_ the cases of the target.

So maybe we don't need row polymorphism?

Inferring `@xs`, i.e. a distr, seems quite hard. Maybe we only allow checking it.

So we only have checking, which alternates to inferring once in a while.

But all toplevels must have signatures.
