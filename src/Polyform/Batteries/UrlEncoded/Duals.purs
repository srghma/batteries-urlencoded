-- | You can use `Batteries.Integer.dual` or `Batteries.Number.dual` or any
-- | other `String` based dual as value dual here.
module Polyform.Batteries.UrlEncoded.Duals
  ( array
  , boolean
  , enum
  , enum'
  , Field
  , optional
  , required
  , value
  ) where

import Prelude

import Data.Array (singleton) as Array
import Data.Enum (class BoundedEnum)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..))
import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex)
import Polyform.Batteries.Generic.Enum (dual, dual') as Enum
import Polyform.Batteries.Int (IntExpected)
import Polyform.Batteries.Int (dual) as Int
import Polyform.Batteries.UrlEncoded.Query (Key, Value) as Query
import Polyform.Batteries.UrlEncoded.Query (Query(..))
import Polyform.Batteries.UrlEncoded.Types (Dual)
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, MissingValue)
import Polyform.Batteries.UrlEncoded.Validators (array, boolean, optional, required, value) as Validators
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Type.Prelude (Proxy)
import Type.Row (type (+))

type Field m e b
  = Batteries.Dual m e (Maybe Query.Value) b

type SingleField m e b
  = Batteries.Dual m e String b

type MultiField m e b
  = Batteries.Dual m e (Array String) b

optional ∷
  ∀ a e m.
  Monad m ⇒
  Query.Key →
  SingleField m e a →
  Dual m e Query (Maybe a)
optional name d = dual validator serializer
  where
  validator = Validators.optional name (Dual.parser d)

  serializer = case _ of
    Just v → do
      i ← Dual.serializer d v
      pure (Query (Map.singleton name [ i ]))
    Nothing → pure (Query (Map.singleton name []))

required ∷
  ∀ a e m.
  Monad m ⇒
  Query.Key →
  SingleField m (MissingValue + e) a →
  Dual m (MissingValue + e) Query a
required name d = dual validator serializer
  where
  validator = Validators.required name (Dual.parser d)

  serializer =
    map (Query <<< Map.singleton name <<< Array.singleton)
      <<< Dual.serializer d

value ∷ ∀ e m. Monad m ⇒ Field m (MissingValue + e) String
value = dual Validators.value (pure <<< Just <<< Array.singleton)

boolean ∷ ∀ e m. Monad m ⇒ Field m (BooleanExpected + e) Boolean
boolean =
  dual
    Validators.boolean
    (pure <<< if _ then Just [ "on" ] else Just [ "off" ])

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array =
  dual
    Validators.array
    (pure <<< Just)

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → SingleField m (IntExpected + InvalidEnumIndex + e) a
enum p = Enum.dual p <<< Int.dual

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ SingleField m (IntExpected + InvalidEnumIndex + e) a
enum' = Enum.dual' <<< Int.dual
