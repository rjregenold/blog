---
title: Of Arrows and State Monads
---

I\'m working on a [project](http://github.com/rjregenold/timer) and found
a practical use for
[arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) and the
[state monad](http://www.haskell.org/haskellwiki/State_Monad) (this actually
makes the second time in a week arrows have come in handy!).

<!--more-->

The problem was this: given a duration in seconds, produce the number of hours,
minutes and seconds that duration represents. For example, `7654.23s` should be
converted to `02h 07m 34s` (milliseconds don\'t matter for this project).

I started with a brute-force solution:

```haskell
hourInSec, minInSec, secInSec :: Integer
hourInSec = 3600
minInSec  = 60
secInSec  = 1

data Duration = Duration Integer Integer Integer

toDuration :: NominalDiffTime -> Duration
toDuration diffTime =
  let duration :: Double
      duration = toSeconds diffTime
      hour = floor $ duration / (fromIntegral hourInSec)
      duration' = duration - (fromIntegral (hour * hourInSec))
      min = floor $ duration' / (fromIntegral minInSec)
      duration'' = duration' - (fromIntegral (min * minInSec))
      sec = floor duration''
  in  Duration hour min sec
```

That worked but was ugly, repetitve and error-prone (nothing was preventing me
from accidentally using `duration` when I meant `duration'`, etc). I saw
a common pattern in this code:

1. Divide the current duration by a given divisor.
2. Multiply the amount from step 1 by the divisor and subtract that from the
   current duration.
3. Set the value from step 2 as the remaining duration.

This sounded like a good use for the state monad. My first refactor looked like
this:

```haskell
toDuration :: NominalDiffTime -> Duration
toDuration diffTime = evalState go (toSeconds diffTime)
  where
    go = do
      hour <- splitDuration hourInSec
      min  <- splitDuration minInSec
      sec  <- splitDuration secInSec
      return $ Duration hour min sec

splitDuration :: Integer -> State Double Integer
splitDuration divisor = do
  duration <- get
  let amount = floor $ duration / (fromIntegral divisor)
  put $ duration - (fromIntegral (amount * divisor))
  return amount
```

As you can see, `splitDuration` uses the state monad to execute the steps
above. This worked fine and is probably an acceptable solution. Just for fun
(this is a personal project, after all) I decided to see if I could implement
`splitDuration` without using do notation.

The
[state](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Lazy.html)
function from the mtl `Control.Monad.State` module looked promising. It has
a type of:

```haskell
state :: (s -> (a, s)) -> m a
```

It takes a function which takes the current state and returns a tuple with
a return value in the first spot and the new state in the second. Lately,
anytime I see a tuple I think of arrows. I don\'t know much about arrows, but
I did find this cool operator that allows you to apply two functions to a given
argument and return a tuple of the results:

```haskell
(&&&) :: a b c -> a b c' -> a b (c, c')
```

That plays out like this:

```bash
$ ghci
λ ➜ import Control.Arrow
λ ➜ let isGreaterThan10 = (>10)
λ ➜ let isEven = ((==0) . flip mod 2)
λ ➜ (filter isGreaterThan10 &&& filter isEven) [1..20]
([11,12,13,14,15,16,17,18,19,20],[2,4,6,8,10,12,14,16,18,20])
```

So `(&&&)` takes the argument `[1..20]` and applies it to both `filter
isGreaterThan10` and `filter isEven` and gathers the results in a tuple. Cool.

Using those two bits of juicy info, I refactored `splitDuration` to look like
this:

```haskell
splitDuration :: Integer -> State Double Integer
splitDuration divisor = state (amount &&& newDuration)
  where
    amount = floor . (/ fromIntegral divisor)
    newDuration duration = duration - (fromIntegral (amount duration * divisor))
```

Step-by-step, it works like this:

```haskell
state (amount &&& newDuration)
```

Uncurried, that line looks like this:

```haskell
state $ \duration -> (amount &&& newDuration) duration
```

So that calls `amount` with the argument of the current `duration`. It also
calls `newDuraution` with that same argument and returns the results as
a tuple. If you remember from above, `state` wants a function that takes the
current state and returns a tuple with a value and the new state. This does
just that.

```haskell
amount = floor . (/ fromIntegral divisor)
```

Is just a curried version of:

```haskell
amount duration = floor $ (duration / fromIntegral divisor)
```

The same as before. The final line:

```haskell
newDuration duration = duration - (fromIntegral (amount duration * divisor))
```

This function stayed the same.

So there it is. Arrows and state monads in real-life. If this were a project
with other programmers, I probably would stick with the first refactored
version. This, however, was a fun experiment with arrows.
