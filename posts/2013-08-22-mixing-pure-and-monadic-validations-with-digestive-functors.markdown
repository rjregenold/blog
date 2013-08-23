---
title: Mixing pure and monadic validations with Digestive Functors
---

There comes a point in every developer\'s life when he or she needs to mix pure
and monadic validations. Fortunately, with [Digestive
Functors](https://github.com/jaspervdj/digestive-functors), it is very straight
forward.

<!--more-->

Note: The stack for this example is [Happstack](http://happstack.com/),
[acid-state](http://acid-state.seize.it/), and Digestive Functors. Email
validation is done using
[email-validate](http://hackage.haskell.org/package/email-validate).

``` {.haskell .numberLines}
-- a few of the relevant'ish imports
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Text.Digestive
import qualified Text.Email.Validate as EV

newtype UserId { unUserId :: Integer }

data User
  { userId :: UserId
  , name   :: Text
  , email  :: Text
  }

data AppState
  { nextUserId :: UserId
  , users      :: IxSet User
  }

emailExists :: Text -> Query AppState Bool
emailExists email = do
  AppState{..} <- ask
  return $ isJust $ getOne $ users @= (UserEmail email)

emptyUserId :: UserId
emptyUserId = UserId 0

userForm :: Form Text App User
userForm = User
  <$> pure emptyUserId
  <*> "name"  .: check "Name is required" (not . T.null) (text Nothing)
  <*> "email" .: checkEmailUnique (checkEmailValid (text Nothing))
  where
    checkEmailValid = check "Email is not valid" $ EV.isValid . E.encodeUtf8
    checkEmailUnique = checkM "Email already taken" $ \email -> do
      exists <- query $ EmailExists email
      return $ not exists
```

The magic happens on lines 33-38. The `check` and `checkM` functions return
`Form` instances. This allows you to chain validators. The nice thing about
this approach is that it allows you to have pure validators where you can, but
gives you the chance to perform monadic validations (ie: checking something in the
database) when you need to. Yay, Haskell.

Final note: In the example above, `App` is just a monad that wraps the acid-state 
handle in a `ReaderT` as described in the [acid-state crash
course](http://happstack.com/docs/crashcourse/AcidState.html#acid-state-advanced). 
