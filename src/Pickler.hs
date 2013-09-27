{-# LANGUAGE
    GADTs
  , EmptyDataDecls
  , TypeOperators
  , RebindableSyntax
  , OverloadedStrings
  #-}
module Pickler where

import Prelude hiding (any)
import Fay.Text (Text, fromString)

import Pickler.Text
import Pickler.Object

-------------------------------------------------------------------------------

data Expect
  = Prim Text
  | And  Expect Expect
  | Or   Expect Expect
  | Many Expect
  | Some Expect

showError :: Expect -> Text
showError = join " " . go
  where
    go e = case e of
             Prim  t -> [t]
             And a b -> paren ( go a ++ ["and"] ++ go b )
             Or  a b -> paren ( go a ++ ["or" ] ++ go b )
             Many  a -> paren ( ["many of"] ++ go a )
             Some  a -> paren ( ["some of"] ++ go a )
    paren x = ["("] ++ x ++ [")"]

type Parser  i o = [i] -> Either Expect (o, [i])
type Printer i o =  o  -> Either Expect [i]

data Point i j o = Pickler
  { _parser  :: [i] -> Either Expect (o, [i])
  , _printer ::  j  -> Either Expect [i]
  , _label   :: Expect
  }

type Pickler i o = Point i o o

parser :: Point i j o -> [i] -> (Expect -> t) -> (o -> t) -> t
parser p i err ok =
  case _parser p i of
    Left e       -> err e
    Right (r, _) -> ok  r

printer :: Point i j o -> j -> (Expect -> t) -> ([i] -> t) -> t
printer p o err ok =
  case _printer p o of
    Left e  -> err e
    Right r -> ok r

-------------------------------------------------------------------------------
-- Helper functions to simplify writing pickler compositions.

parseAnd :: Point i j o -> (o -> [i] -> Either Expect b) -> [i] -> Either Expect b
parseAnd p cont xs =
  case _parser p xs of
    Left e        -> Left e
    Right (o, ys) -> cont o ys

parseOr :: Point i j o -> (Expect -> [i] -> Either a (o, [i])) -> [i] -> Either a (o, [i])
parseOr p cont xs =
  case _parser p xs of
    Left e  -> cont e xs
    Right o -> Right o

printAnd :: Point i j o -> ([i] -> j -> Either Expect b) -> j -> Either Expect b
printAnd p cont xs =
  case _printer p xs of
    Left e  -> Left e
    Right i -> cont i xs

printOr :: Point i j o -> (Expect -> j -> Either a [i]) -> j -> Either a [i]
printOr p cont xs =
  case _printer p xs of
    Left e  -> cont e xs
    Right i -> Right i

-------------------------------------------------------------------------------

label :: Expect -> Point i j o -> Point i j o
label l a = Pickler p q l
  where p = parseOr a e
        q = printOr a e
        e :: Expect -> b -> Either Expect c
        e _ _ = Left l

-------------------------------------------------------------------------------

for :: (j -> o) -> Pickler i o -> Point i j o
for = (>-)

fmap :: (b -> o) -> Point i j b -> Point i j o
fmap = (<$>)

app :: Point i j (b -> o) -> Point i j b -> Point i j o
app = (<*>)

alt :: Pickler i o -> Pickler i o -> Pickler i o
alt = (<|>)

-------------------------------------------------------------------------------

-- Functor and Applicative.

infix  5 >-
infixl 4 <$>
infixl 4 <*>
infixl 4 <*
infixl 4  *>
infixl 2 <|>
infixl 3 `guards`
infix  5 `prints`

(<$>) :: (b -> o) -> Point i j b -> Point i j o
(<$>) f a = Pickler p q w
  where p = parseAnd a $ \o ->
            Right . (,) (f o)
        q = printAnd a $ \i -> const $
            Right i
        w = _label a

bimap :: (o -> p) -> (p -> o) -> Pickler i o -> Pickler i p
bimap f g a = Pickler p q w
  where p = parseAnd a $ \o ->
            Right . (,) (f o)
        q = printAnd a ( \i -> const $
            Right (i) ) . g
        w = _label a

(>-) :: (j -> o) -> Pickler i o -> Point i j o
(>-) f (Pickler p q w) = Pickler p (q . f) w

pure :: o -> Pickler i o
pure o = Pickler p q w
  where p = Right . (,) o
        q = const (Right [])
        w = Prim "pure"

prints :: Pickler i o -> [i] -> Point i j o
prints a i = Pickler p q w
  where p = _parser a
        q = const (Right i)
        w = Prim "prints"

(<*>) :: Point i j (b -> o) -> Point i j b -> Point i j o
a <*> b = Pickler p q w
  where p = parseAnd a $ \f ->
            parseAnd b $ \o ->
              Right . (,) (f o)
        q = printAnd a $ \i ->
            printAnd b $ \j -> const $
              Right (i ++ j)
        w = _label a `And` _label b

(<*) :: Pickler i o -> Point i o b -> Pickler i o
p <* q = const <$> p <*> q

(*>) :: Point i o b -> Pickler i o -> Pickler i o
p *> q = flip const <$> p <*> q

(<|>) :: Pickler i o -> Pickler i o -> Pickler i o
(<|>) a b = Pickler p q w
  where p = parseOr a $ \e ->
            parseOr b $ \f ->
              const (Left (Or e f))
        q = printOr a $ \e ->
            printOr b $ \f ->
              const (Left (Or e f))
        w = _label a `Or` _label b

guards :: (o -> Bool) -> Pickler i o -> Pickler i o
guards g a = Pickler (_parser a) q (_label a)
  where q i | g i = _printer a i
        q _       = Left (_label a)

msum :: [Pickler i o] -> Pickler i o
msum = foldl1 (<|>)

option :: Eq o => o -> Pickler i o -> Pickler i o
option d = (<|> pure d)

many :: Eq o => Pickler i o -> Pickler i [o]
many p = label (Many (_label p)) $
         cons `guards`
           (:) <$> head >- p
               <*> tail >- many p
     <|> (nil `guards` pure [])

some :: Eq o => Pickler i o -> Pickler i [o]
some p = label (Some (_label p)) $
         cons `guards`
           (:) <$> head >- p
               <*> tail >- many p

cons :: [a] -> Bool
cons [] = False
cons _  = True

nil :: [a] -> Bool
nil [] = True
nil _  = True

tuple :: Pickler i a -> Pickler i b -> Pickler i (a, b)
tuple a b = (,) <$> fst >- a
                <*> snd >- b

withDefault :: (o -> Bool) -> Pickler i o -> Pickler i o
withDefault g a = Pickler (_parser a) q (_label a)
  where q i | g i = Right []
        q i       = _printer a i

----------------------------------------------------------------
-- Primitive token picklers.

any :: Pickler b b
any = Pickler p q w
  where p (x:xs) = Right (x, xs)
        p []     = Left w
        q x = Right [x]
        w = Prim "anything"

satisfy :: (b -> Bool) -> Pickler b b
satisfy f = Pickler p q w
  where p (x:xs) | f x = Right (x, xs)
        p _            = Left w
        q x | f x = Right [x]
        q _       = Left w
        w = Prim "unsatisfied"

token :: Text -> Pickler Text Text
token t = Pickler p q w
  where p (x:xs) | x == t = Right (t, xs)
        p _               = Left w
        q x | x == t = Right [t]
        q _          = Left w
        w = Prim ("token '" <> t <> "'")

tokenI :: Text -> Pickler Text Text
tokenI t = Pickler p q w
  where p (x:xs) | lower x == l = Right (x, xs)
        p _                     = Left w
        q x | lower x == l = Right [x]
        q _                = Left w
        l = lower t
        w = Prim ("token '" <> t <> "' (case insensitive)")

oneOf :: [Text] -> Pickler Text Text
oneOf = msum . map token

oneOfI :: [Text] -> Pickler Text Text
oneOfI = msum . map tokenI

----------------------------------------------------------------

prefix :: Text -> Pickler Text o -> Pickler Text o
prefix t p = tokenI t `prints` [t] *> p

prefixI :: Text -> Pickler Text o -> Pickler Text o
prefixI t p = tokenI t `prints` [t] *> p

sep1I :: Eq a => Text -> Pickler Text a -> Pickler Text [a]
sep1I s p = cons `guards`
  (:) <$> head >- p
      <*> tail >- many (prefixI s p)

named
  :: Text
  -> Pickler i b
  -> Pickler i (Object o)
  -> Pickler i (Object (b, o))
named key a b = has key `guards`
  set key <$> get key >- a
          <*> cast    >- b

obj :: Pickler i (Object ())
obj = pure emptyObj

