---
title: Control.Applicative Alternative
---

If you provide an API that allows users to log in with either an email address 
or a username, `(<|>)` can provide an elegant solution.

<!--more-->

Here is an example of `(<|>)` in action:

``` {.bash}
ghci> let f = foldl (<|>) Nothing
ghci> f [Nothing :: Maybe Int, Just 1, Just 2]
Just 1
ghci> f [Nothing, Nothing]
Nothing
ghci> f [Just 1, Just 2]
Just 1
ghci> f [Just 1, Nothing :: Maybe Int]
Just 1
```

For the `Maybe` applicative functor, `(<|>)` will take either the first `Just`
it finds, or `Nothing` if both arguments are `Nothing`.

We can use this in _\"real life\"_ to solve the multiple authentication problem.

``` {.haskell}
import Control.Applicative (<|>)
import Data.Text (Text)
 
authByEmail :: Text -- ^ email
            -> Text -- ^ password
            -> Maybe User
 
authByUsername :: Text -- ^ username
               -> Text -- ^ password
               -> Maybe User
 
authUser :: Text -- ^ ident
         -> Text -- ^ password
         -> Maybe User
authUser ident password =
  foldl (<|>) Nothing $ 
    [ authByEmail ident password
    , authByUsername ident password
    ]
```

If `authByEmail` or `authByUsername` succeeds, `authUser` will succeed.
