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

Let's move on to the most basic of operations: equality testing. PHP has a
notion of so-called "strict equality" using the operator `(===)`, which is easy
enough to implement:

> (===) :: PVal -> PVal -> PVal
> PBool a === PBool b = PBool (a == b)
> PInt a === PInt b = PBool (a == b)
> PFloat a === PFloat b = PBool (a == b)
> PString a === PString b = PBool (a == b)
> PNull === PNull = PBool True
> _ === _ = PBool False

And, of course, its inverse:

> (!==) :: PVal -> PVal -> PVal
> a !== b = pnot (a === b)

`pnot` is simply PHP's negation, "PHP not". It would be nice to have this as
prefix-`!`, but I don't think that I can make Haskell do that.

> pnot :: PVal -> PVal
> pnot (PBool b) = PBool (not b)
> pnot b = pnot (toBool b)

This uses `toBool`, which I will define in a moment. It coerces any `PVal` to
the appropriate `PBool`.

PHP also has "loose" eqality with the `==` operator. There's a table at
<http://us2.php.net/manual/en/language.operators.comparison.php> which explains
the rules. But wait... in order to implement this, we need type juggling first!
Let's just get this nonsense over with:

> toBool :: PVal -> PVal
> toBool (PBool x) = PBool x
> toBool (PInt x) = PBool (x /= 0)
> toBool (PFloat x) = PBool (x /= 0.0)
> toBool (PString x) = PBool (x /= (pack "") && x /= (pack "0"))
> toBool PNull = PBool False

That really is the rule for strings, by the way: `(bool) "0"` is `false`, but
`(bool) "00"` is `true`. Okay, back to juggling:

> toInt :: PVal -> PVal
> toInt (PBool False) = PInt 0
> toInt (PBool True) = PInt 1
> toInt (PInt x) = PInt x
> toInt (PFloat x) = PInt (truncate x)
> toInt (PString x) = PInt undefined -- TODO
> toInt (PNull) = PInt 0

> toFloat :: PVal -> PVal
> toFloat (PString x) = PFloat undefined -- TODO
> toFloat (PInt x) = PFloat (fromIntegral x)
> toFloat x = toFloat (toInt x)

> toString :: PVal -> PVal
> toString (PBool False) = PString (pack "") -- ORLY?
> toString (PBool True) = PString (pack "1") -- YARLY
> toString (PInt x) = PString (pack . show $ x)
> toString (PFloat x) = PString (pack . show $ x)
> toString (PString x) = PString x
> toString PNull = PString (pack "")

Now that we can juggle types, are we ready to implement `(==)` for `PVal`s?
Nope. Because when we're comparing two strings, we might have to juggle them
both to numbers. When both are "numeric" strings, each is converted to a number:
an integer if it will fit, a float if it won't, *unless* "both overflow and
appear equal numerically." I have no idea what that actually means, so I guess
I'm going to go read some C at some point.

Also, what is considered a "numeric" string is somewhat difficult. The manual
at
<http://us2.php.net/manual/en/language.types.string.php#language.types.string.conversion>
claims that "Valid numeric data is an optional sign, followed by one or more
digits (optionally containing a decimal point), followed by an optional
exponent. The exponent is an 'e' or 'E' followed by one or more digits." This
is a lie; `"0xFF" == 255` evaluates to `true`. *But,* by the way,
`intval("0xFF") === 0` and `floatval("0xFF") === 0.0`. And yes, we could say
`intval("0xFF", 0)` and get our `255`, but then the problem shifts:
`"010" == 10`, but `intval("010", 0) === 8`. I don't even know how this is
*possible.*

…

Okay, I'm back from the Land of PHP Internals. I learned how to tell if a string
is numeric! To nobody's surprise, the process is complicated.

The relevant function has three possible return values: one for when the string
was not numeric, one for if it fits in a `long`, and one for when it's a double.
No word on what happens when it doesn't fit in a double.

It also uses a pair of output parameters to say what the actual numeric value
is, but with a sufficiently-nice type system that becomes unnecessarry.

> data Numberkind = Nokind | Longkind Int | Doublekind Double

(C's type system is actually nearly nice enough for this:

```C
    typedef enum { NOKIND, LONGKIND, DOUBLEKIND } numberkind;
    typedef union { long lng, double dbl } number_u;
    typedef struct { numberkind kind, number_u number } number_t;
```

but don't tell anyone. We wouldn't want them to catch on.)
