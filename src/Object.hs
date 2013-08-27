{-# LANGUAGE TypeOperators #-}
module Object where

import Fay.Text
import FFI (ffi)

infixr :*:
type (:*:) a b = (a, b)

data Object a

emptyObj :: Object ()
emptyObj = ffi "{}"

get :: Text -> Object (a, o) -> a
get = ffi "%2[%1]"

set :: Text -> a -> Object o -> Object (a, o)
set = ffi "(%3[%1] = /*YEAH*/ %2, %3)"

cast :: Object (a, o) -> Object o
cast = ffi "%1"

