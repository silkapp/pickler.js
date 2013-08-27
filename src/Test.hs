{-# LANGUAGE
    TypeOperators
  , RebindableSyntax
  , OverloadedStrings
  #-}
module Test where

import Prelude hiding (any)
import Fay.Text (Text, fromString)

import Pickler
import Object
import Text

data Fix f = In { out :: f (Fix f) }

pRelations :: Pickler Text [Text]
pRelations = many (prefixI "tag" any)

infixr :-
type a :- b = (a, b)

namedP :: Text `Pickler` Object (Text :- User :- [Text] :- ())
namedP =
  ( object "mode"       (tokenI "expLore")
  . object "view"       userP
  . object "collection" (prefix "collection" pRelations)
  $ pure emptyObj
  )

data User = User
  { username  :: Text
  , passwords :: Text
  , age       :: Text
  }

userP :: Pickler Text User
userP =
  User <$> username  -< (prefix "user" any)
       <*> passwords -< (prefix "pass" any)
       <*> age       -< (oneOfI ["rage", "cage", "age"] `prints` ["age"] *> any)

main :: Fay ()
main = parsePrint namedP "EXPLORE/user/sebas/pass/b/age/10/collection/tag/a/tag/b"

  where
    parsePrint p i =
      do let inp = split "/" i
         print ("-----------------------------------------" :: Text)
         case parser p inp of
           Left  e       -> print ("expected while parsing:\n" <> showError e)
           Right (r, rs) ->
             do print r
                print rs
                case printer p r of
                  Left  e -> print ("expected while printing:\n" <> showError e)
                  Right s -> print ("'" <> join "/" s <> "'")

