{-# LANGUAGE TypeOperators #-}
module Pickler.Object where

import Prelude (Bool)
import Fay.Text
import FFI (ffi, Automatic)

infixr :*:
type (:*:) a b = (a, b)

data Object a

emptyObj :: Object ()
emptyObj = ffi "{}"

get :: Text -> Object (a, o) -> Automatic a
get = ffi "%2[%1]"

has :: Text -> Object o -> Bool
has = ffi "%2[%1] !== undefined"

set :: Text -> Automatic a -> Object o -> Object (a, o)
set = ffi "(function (k, v, i)\n { var o = {};\n for (var p in i) o[p] = i[p];\n o[k] = v;\n return o; })\n(%1, %2, %3)"

cast :: Object (a, o) -> Object o
cast = ffi "%1"

