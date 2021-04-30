-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of HTML form validation.
-- |
-- | You can use `Batteries.Integer.validator` or `Batteries.Number.validator`
-- | directly as field value validator.
module Polyform.Batteries.UrlEncoded.Validators
  ( BooleanExpected
  , Field
  , MissingValue
  , SingleField
  , module Query
  , _booleanExpected
  , _missingValue
  , array
  , boolean
  , enum
  , enum'
  , optional
  , optValidator
  , required
  , value
  ) where

import Prelude
import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (Validator, invalid) as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex)
import Polyform.Batteries.Generic.Enum (validator, validator') as Enum
import Polyform.Batteries.Int (IntExpected)
import Polyform.Batteries.Int (validator) as Int
import Polyform.Batteries.UrlEncoded.Query (Query(..), Key, Value, lookup) as Query
import Polyform.Batteries.UrlEncoded.Query (Query)
import Polyform.Batteries.UrlEncoded.Types (Validator, fromValidator)
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (liftFn, liftFnMV, liftFnV, runValidator)
import Type.Prelude (Proxy, SProxy(..))
import Type.Row (type (+))

type Field m e b
  = Batteries.Validator m e (Maybe Query.Value) b

type SingleField m e b
  = Batteries.Validator m e String b

type MultiField m e b
  = Batteries.Validator m e (Array String) b

required ∷
  ∀ a m errs.
  Monad m ⇒
  Query.Key →
  SingleField m (MissingValue + errs) a →
  Validator m (MissingValue + errs) Query a
required name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value <<< Validator.liftFn (Query.lookup name))

optional ∷
  ∀ a m errs.
  Monad m ⇒
  Query.Key →
  SingleField m (errs) a →
  Validator m (errs) Query (Maybe a)
optional name fieldValidator = fromValidator name (optValidator fieldValidator <<< Validator.liftFn (Query.lookup name))

_missingValue = SProxy ∷ SProxy "missingValue"

type MissingValue e
  = ( missingValue ∷ Unit | e )

value ∷ ∀ e m. Applicative m ⇒ Field m (MissingValue + e) String
value =
  liftFnV
    $ \qv → case qv >>= Array.head of
        Just "" → Batteries.invalid _missingValue msg unit
        Just v → pure v
        Nothing → Batteries.invalid _missingValue msg unit
  where
  msg _ = "Missing value"

-- | We could do a bit of dance with `Choice.first` etc.
-- | but this seems simpler and a bit more efficient
optValidator ∷ ∀ b e m. Monad m ⇒ SingleField m e b → Field m e (Maybe b)
optValidator fieldValidator =
  liftFnMV \v → case v >>= Array.head of
    Nothing → pure (V (Right Nothing))
    Just "" → pure (V (Right Nothing))
    Just h → runValidator (Just <$> fieldValidator) h

-- | TODO: This is probably useless. Drop this.
-- |
-- | Encodes default browser behavior which sets `checkbox` value to "on"
-- | when checked and skips it completely when it is not.
-- | We consider also "off" value because we want to be more consistent when
-- | building API comunication layer - if you have any objections please fill
-- | an issue with description.
_booleanExpected = SProxy ∷ SProxy "booleanExpected"

type BooleanExpected e
  = ( booleanExpected ∷ Query.Value | e )

-- | TODO: It seems that in the context of form validation we don't need this
-- | and it is enough to use `Maybe String` + `isJust` to get the same result.
boolean ∷ ∀ e m. Applicative m ⇒ Field m ( booleanExpected ∷ Query.Value | e ) Boolean
boolean =
  liftFnV case _ of
    Just [ "on" ] → pure true
    Nothing → pure false
    Just v → Batteries.invalid _booleanExpected msg v
  where
  msg _ = "Boolean expected"

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array =
  liftFn
    $ case _ of
        Just s → s
        Nothing → []

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → SingleField m (IntExpected + InvalidEnumIndex + e) a
enum p = Enum.validator p <<< Int.validator

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ SingleField m (IntExpected + InvalidEnumIndex + e) a
enum' = Enum.validator' <<< Int.validator
