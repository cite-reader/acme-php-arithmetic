There is an interesting meme going around, that there are some things that
cannot be expressed in static languages.

To which I say: really? Perhaps in a language like Java you'd have to cheat the
type system to express yourself, but Java does not provide a fine example of
static typing. So what is the situation in a humane static language?

Let's find out.

> module Acme.PHP.Arithmetic where
> import qualified Prelude as Hask
> import Prelude (undefined)

That's right. We're going to take the arithmetic semantics of PHP, the most
insane of the dynlangs, and embed them in Haskell, the most aggressively pure
static language in common use.

(I've imported the Prelude qualified so it's clear what comes from where.)

And I'm going to put it in the Acme namespace because GOOD GOD this is a bad
idea.


Let's start with types. We won't try to represent absolutely everything a PHP
program could use, because then we would end up wandering into the object and
resource systems; but we can emulate their booleans, integers, floats, and
strings. Maybe if I still feel like doing this at the end I'll also implement
their arrays.

Anyway, PHP strings are interesting. For reasons of history, these aren't
Unicode codepoint sequences, but rather run-length-encoded byte arrays. So we
need an import:

> import qualified Data.ByteString as B

With that available to us, we can roll the interesting subset of PHP values
into a single sum type:

> data PVal = PBool Hask.Bool | PInt Hask.Int | PFloat Hask.Float |
>             PString B.ByteString | PNull deriving (Hask.Show)

Let's start with the most basic of operations: equality testing. PHP has a
notion of so-called "strict equality" using the operator `(===)`, which is easy
enough to implement:

> (===) :: PVal -> PVal -> PVal
> PBool a === PBool b = PBool ((Hask.==) a b)
> PInt a === PInt b = PBool ((Hask.==) a b)
> PFloat a === PFloat b = PBool ((Hask.==) a b)
> PString a === PString b = PBool ((Hask.==) a b)
> PNull === PNull = PBool Hask.True
> _ === _ = PBool Hask.False
>
> (!==) :: PVal -> PVal -> PVal
> a !== b = not (a === b)

We'll get to `not` later.

There's also loose eqality with the `==` operator. There's a table at
<http://us2.php.net/manual/en/language.operators.comparison.php> which explains
the rules. But wait... in order to implement this, we need type juggling first!
Let's just get this over with:

> toBool :: PVal -> PVal
> toBool (PBool x) = PBool x
> toBool (PInt x) = PBool (x Hask./= 0)
> toBool (PFloat x) = PBool (x Hask./= 0.0)
> toBool (PString x) = PBool (x Hask./= (B.pack []) Hask.&& x Hask./= (B.pack [48]))
> toBool PNull = PBool Hask.False

> toInt :: PVal -> PVal
> toInt (PBool Hask.False) = PInt 0
> toInt (PBool Hask.True) = PInt 1
> toInt (PInt x) = PInt x
> toInt (PFloat x) = PInt (Hask.truncate x)
> toInt (PString x) = PInt undefined -- TODO

> toFloat :: PVal -> PVal
> toFloat (PString x) = PFloat undefined -- TODO
> toFloat (PInt x) = PFloat (Hask.fromIntegral x)
> toFloat x = toFloat (toInt x)

> not :: PVal -> PVal
> not (PBool b) = PBool (Hask.not b)
> not b = not (toBool b)
