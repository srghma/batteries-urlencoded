-- | You can use `Batteries.Integer.dual` or `Batteries.Number.dual` or any
-- | other `String` based dual as value dual here.
module Polyform.Batteries.UrlEncoded.Duals
  ( boolean
  , enum
  , enum'
  , FieldDual
  , MultiValueFieldDual
  , optional
  , required
  , required'
  , SingleValueFieldDual
  , value
  , valueArray
  ) where

import Prelude

import Data.Array (singleton) as Array
import Data.Enum (class BoundedEnum)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FormURLEncoded.Query (Value, singleton) as Query
import Data.Maybe (Maybe(..))
import Polyform.Batteries as Batteries
import Polyform.Batteries.Generic.Enum (dual, dual') as Enum
import Polyform.Batteries.Int (dual) as Int
import Polyform.Batteries.UrlEncoded.Types (Dual', Dual)
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, InvalidEnumValue, MissingValue, flattenEnumErr, missingValue)
import Polyform.Batteries.UrlEncoded.Validators (valueArray, boolean, optional, required, value) as Validators
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Polyform.Validator.Dual (lmapDual)
import Type.Prelude (Proxy)
import Type.Row (type (+))

type FieldDual m e b = Batteries.Dual' m e (Maybe Query.Value) b

type SingleValueFieldDual m e b = Batteries.Dual' m e String b

type MultiValueFieldDual m e b = Batteries.Dual' m e (Array String) b

optional
  ∷ ∀ a e m
  . Monad m
  ⇒ FieldId
  → SingleValueFieldDual m e a
  → Dual' m e Query (Maybe a)
optional name d = dual validator serializer
  where
  validator = Validators.optional name (Dual.parser d)

  serializer = case _ of
    Just v → do
      i ← Dual.serializer d v
      pure $ Query.singleton name [ i ]
    Nothing → pure (Query.singleton name [])

required
  ∷ ∀ a err m
  . Monad m
  ⇒ FieldId
  → err
  → Batteries.Dual m err String a
  → Dual m err Query a
required name err d = dual validator serializer
  where
  validator = Validators.required name err (Dual.parser d)

  serializer o = do
    v <- Dual.serializer d o
    pure (Query.singleton name [ v ])

required'
  ∷ ∀ a e m
  . Monad m
  ⇒ FieldId
  → SingleValueFieldDual m (MissingValue + e) a
  → Dual' m (MissingValue + e) Query a
required' name d = required name (missingValue "Missing value") d

value ∷ ∀ err m. Monad m ⇒ err -> Batteries.Dual m err (Maybe Query.Value) String
value err = dual (Validators.value err) (pure <<< Just <<< Array.singleton)

boolean ∷ ∀ e m. Monad m ⇒ FieldDual m (BooleanExpected + e) Boolean
boolean =
  dual
    Validators.boolean
    (pure <<< if _ then Just [ "on" ] else Just [ "off" ])

valueArray ∷ ∀ e m. Monad m ⇒ FieldDual m e (Array String)
valueArray =
  dual
    Validators.valueArray
    (pure <<< Just)

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → SingleValueFieldDual m (InvalidEnumValue + e) a
enum p = lmapDual (map flattenEnumErr) $ Enum.dual p <<< Int.dual

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ SingleValueFieldDual m (InvalidEnumValue + e) a
enum' = lmapDual (map flattenEnumErr) $ Enum.dual' <<< Int.dual
