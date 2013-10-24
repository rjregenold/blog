---
title: Aeson Happstack Response
---

Happstack makes it simple to return a JSON response with the correct
`Content-Type` header using Aeson. 

<!--more-->

You only need to create a `ToMessage` instance like this:

```haskell
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Happstack.Server

instance (ToJSON a) => ToMessage a where
  toContentType _ = B.pack "application/json"
  toMessage       = encode
```

Now you can respond with JSON like this:

```haskell
data MyFancyRes = MyFancyRes
  { username :: Text
  , about    :: Text
  }

instance ToJSON MyFancyRes where
  toJSON res = object
    [ "username" .= username res
    , "about"    .= about res
    ]

someHandler :: ServerPart Response
someHandler = ok $ toResponse $ MyFancyRes "RJ" "Some guy"
```

The `MyFancyRes` ADT will be encoded and the appropriate `Content-Type` will be
set.
