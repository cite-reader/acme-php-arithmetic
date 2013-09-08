There is an interesting meme going around, that there are some things that
cannot be expressed in static languages.

To which I say: really? Perhaps in a language like Java you'd have to cheat the
type system to express yourself, but Java does not provide a fine example of
static typing. So what is the situation in a humane static language?

Let's find out.

> module Acme.PHP.Arithmetic where

That's right. We're going to take the arithmetic semantics of PHP, the most
insane of the dynlangs, and embed them in Haskell, the most aggressively pure
static language in common use.

And I'm going to put it in the Acme namespace because *good God* this is a bad
idea.


Let's start with types. We won't try to represent absolutely everything a PHP
program could use, because then we would end up wandering into the object and
resource systems; but we can emulate their booleans, integers, floats, and
strings. Maybe if I still feel like doing this at the end I'll also implement
their arrays.

Anyway, PHP strings are interesting. For reasons of history, these aren't
Unicode codepoint sequences, but rather run-length-encoded byte arrays. So we
need a couple imports (the second so we can later turn things into strings):

> import qualified Data.ByteString as B
> import Data.ByteString.Char8 (pack)

With that available to us, we can roll the interesting subset of PHP values
into a single sum type:

> data PVal = PBool Bool | PInt Int | PFloat Double |
>             PString B.ByteString | PNull deriving (Show)

(Potential future work: define a custom `Show` instance that mimics the PHP
`var_dump` function.)

Let's start with the most basic of operations: equality testing. PHP has a
notion of so-called "strict equality" using the operator `(===)`, which is easy
enough to implement:

> (===) :: PVal -> PVal -> PVal
> PBool a === PBool b = PBool (a == b)
> PInt a === PInt b = PBool (a == b)
> PFloat a === PFloat b = PBool (a == b)
> PString a === PString b = PBool (a == b)
> PNull === PNull = PBool True
> _ === _ = PBool False

> (!==) :: PVal -> PVal -> PVal
> a !== b = pnot (a === b)

We'll get to `pnot` later.

There's also loose eqality with the `==` operator. There's a table at
<http://us2.php.net/manual/en/language.operators.comparison.php> which explains
the rules. But wait... in order to implement this, we need type juggling first!
Let's just get this over with:

> toBool :: PVal -> PVal
> toBool (PBool x) = PBool x
> toBool (PInt x) = PBool (x /= 0)
> toBool (PFloat x) = PBool (x /= 0.0)
> toBool (PString x) = PBool (x /= (pack "") && x /= (pack "0"))
> toBool PNull = PBool False

> toInt :: PVal -> PVal
> toInt (PBool False) = PInt 0
> toInt (PBool True) = PInt 1
> toInt (PInt x) = PInt x
> toInt (PFloat x) = PInt (truncate x)
> toInt (PString x) = PInt undefined -- TODO

> toFloat :: PVal -> PVal
> toFloat (PString x) = PFloat undefined -- TODO
> toFloat (PInt x) = PFloat (fromIntegral x)
> toFloat x = toFloat (toInt x)

> toString :: PVal -> PVal
> toString (PBool False) = PString (pack "")
> toString (PBool True) = PString (pack "1")
> toString (PInt x) = PString (pack . show $ x)
> toString (PFloat x) = PString (pack . show $ x)
> toString (PString x) = PString x

`pnot` is short for "PHP not". It would be nice to have prefix-`!` for this, but
I don't think that's possible.

> pnot :: PVal -> PVal
> pnot (PBool b) = PBool (not b)
> pnot b = pnot (toBool b)
