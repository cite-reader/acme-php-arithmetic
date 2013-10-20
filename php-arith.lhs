As those in the Haskell community may be aware, there is an `acme-php` package
[available on Hackage][acme-php]. And I don't like it.

Not because I'm a web dev offended that someone would poke fun at my favorite
language. I *do* get paid to write PHP, but I hate every minute of it and will
happily mock PHP until I run out of things to mock (I never will). I don't like
`acme-php` because it's the *Scary Movie* of PHP mockery―a parody so shallow
that it ceases to have the slightest resemblance to its inspiration. The
thought process of whoever wrote it was apparently "PHP is dumb so I'm just
going to throw all the dumb things I can think of into this library." Which is
dissatisfying, because there are so very many *completely legitimate*
complaints we can level at PHP without needing to make up our own.

> module Acme.PHP where

So let's do this *right.*

* * *

I'm going to focus on the insanity of PHP's type system. As you may be aware,
PHP is unityped: all values are of the same static type, and are only classified
at runtime. PHP's interpreter refers to this single übertype as a `ZVal`; I
will use that name to refer to the genuine article and `PVal` to refer to my
own reimplementation.

For the moment, I'll be ignoring PHP's arrays and object system. (Both are
entirely deserving of a dissertation on their own; I just can't be bothered
with the arrays right now. And I'm not sure I can embed the object system in
Haskell anyway.) So this leaves booleans, integers, floats, strings and null.

Yes, they have a null. Of *course* they have a null.

Anyway, to simulate the strings in most modern, high-level languages you'd
probaby import `Data.Text`. But, well. This is PHP. And in PHP, the atomic unit
of strings is the byte.

> import qualified Data.ByteString as B
> import qualified Data.ByteString.Char8 as BC
> import Data.ByteString.Char8 (pack)

SO WE'RE OFF TO A FANTASTIC START. Here's a `PVal`:

> data PVal = PBool !Bool | PInt !Int | PFloat !Double |
>             PString !B.ByteString | PNull deriving (Show, Read)

The derived `Show` and `Read` instances are for my own debugging use.

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
is, but with a sufficiently-nice type system that becomes unnecessarry:

> data Numberclass = Noclass | Longclass Int | Doubleclass Double
>   deriving (Show, Read, Eq)

(C's type system is actually *almost* nice here, if you let it be:

```C
typedef enum { NOCLASS, LONGCLASS, DOUBLECLASS } numberclass_tag;
typedef union { long lng, double dbl } number_u;
typedef struct { numberclass_tag class, number_u number } numberclass;
```

but don't tell anyone. We wouldn't want them to catch on.)

I'm not sure what the algorithm is. The function spans 134 lines of
Zend-flavored C, the author has no idea what loop invariants are for, and `goto`
is used four times. I'm just going to leave it here for now.

> isNumericString :: B.ByteString -> Numberclass
> isNumericString str | B.null str = Noclass
>                     | otherwise =
>   let str' = BC.dropWhile (`elem` " \t\n\r\v\f") str in
>   if B.take 2 str' == pack "0x" then goHex (B.drop 2 str')
>   else goDec str'
>   where
>     goHex str = undefined
>     goDec str = undefined

[acme-php]: http://hackage.haskell.org/package/acme-php "acme-php on Hackage"
