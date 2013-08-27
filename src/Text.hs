module Text where

import Prelude
import Fay.Text (Text)
import FFI (ffi)

-- Utility functions on Text.

lower :: Text -> Text
lower = ffi "%1.toLowerCase()"

split :: Text -> Text -> [Text]
split = ffi "%2.split(%1)"

join :: Text -> [Text] -> Text
join = ffi "%2.join(%1)"

textLength :: Text -> Int
textLength = ffi "%1.length"

(<>) :: Text -> Text -> Text
(<>) = ffi "%1 + %2"

