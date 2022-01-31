-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of HTML form validation.
-- |
-- | You can use `Batteries.Integer.validator` or `Batteries.Number.validator`
-- | directly as field value validator.
module Polyform.Batteries.UrlEncoded.Validators
  ( BooleanExpected
  , FieldValidator
  , MissingValue
  , InvalidEnumValue
  , SingleValueFieldValidatorValidator
  , _booleanExpected
  , _missingValue
  , boolean
  , enum
  , enum'
  , flattenEnumErr
  , optional
  , optValidator
  , required
  , value
  , valueArray
  ) where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FormURLEncoded.Query (Value, lookup) as Query
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (Msg, msg', onErr)
import Polyform.Batteries (Validator', invalid) as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex, _invalidEnumIndex)
import Polyform.Batteries.Generic.Enum (validator, validator') as Enum
import Polyform.Batteries.Int (IntExpected, _intExpected)
import Polyform.Batteries.Int (validator) as Int
import Polyform.Batteries.UrlEncoded.Types (Validator', fromValidator)
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (liftFn, liftFnMV, liftFnV, lmapValidator, runValidator)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type FieldValidator m e b
  = Batteries.Validator' m e (Maybe Query.Value) b

type SingleValueFieldValidatorValidator m e b
  = Batteries.Validator' m e String b

type MultiValueFieldValidatorValidator m e b
  = Batteries.Validator' m e (Array String) b

required ∷
  ∀ a m errs.
  Monad m ⇒
  FieldId →
  SingleValueFieldValidatorValidator m (MissingValue + errs) a →
  Validator' m (MissingValue + errs) Query a
required name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value <<< Validator.liftFn (Query.lookup name))

optional ∷
  ∀ a m errs.
  Monad m ⇒
  FieldId →
  SingleValueFieldValidatorValidator m (errs) a →
  Validator' m errs Query (Maybe a)
optional name fieldValidator = fromValidator name (optValidator fieldValidator <<< Validator.liftFn (Query.lookup name))

_missingValue = Proxy ∷ Proxy "missingValue"

type MissingValue e
  = ( missingValue ∷ {} | e )

value ∷ ∀ e m. Applicative m ⇒ FieldValidator m (MissingValue + e) String
value =
  liftFnV
    $ \qv → case qv >>= Array.head of
        Just "" → Batteries.invalid _missingValue msg {}
        Just v → pure v
        Nothing → Batteries.invalid _missingValue msg {}
  where
  msg _ = "Missing value"

valueArray ∷ ∀ e m. Monad m ⇒ FieldValidator m e (Array String)
valueArray =
  liftFn
    $ case _ of
        Just s → s
        Nothing → []


-- | We could do a bit of dance with `Choice.first` etc.
-- | but this seems simpler and a bit more efficient
optValidator ∷ ∀ b e m. Monad m ⇒ SingleValueFieldValidatorValidator m e b → FieldValidator m e (Maybe b)
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
_booleanExpected = Proxy ∷ Proxy "booleanExpected"

type BooleanExpected e
  = ( booleanExpected ∷ Query.Value | e )

-- | TODO: It seems that in the context of form validation we don't need this
-- | and it is enough to use `Maybe String` + `isJust` to get the same result.
boolean ∷ ∀ e m. Applicative m ⇒ FieldValidator m ( booleanExpected ∷ Query.Value | e ) Boolean
boolean =
  liftFnV case _ of
    Just [ "on" ] → pure true
    Nothing → pure false
    Just v → Batteries.invalid _booleanExpected msg v
  where
  msg _ = "Boolean expected"

_invalidEnumValue = Proxy ∷ Proxy "invalidEnumValue"

type InvalidEnumValue e = ( invalidEnumValue ∷ String | e )

flattenEnumErr
  :: forall err
   . Msg (InvalidEnumIndex + IntExpected + InvalidEnumValue + err)
  -> Msg (InvalidEnumValue + err)
flattenEnumErr = do
  let
    reMsg = msg' "Invalid choice" _invalidEnumValue
  identity
    # onErr _invalidEnumIndex (reMsg <<< show <<< _.info)
    # onErr _intExpected (reMsg <<< _.info)

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → SingleValueFieldValidatorValidator m (InvalidEnumValue + e) a
enum p = lmapValidator (map flattenEnumErr) $ Enum.validator p <<< Int.validator

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ SingleValueFieldValidatorValidator m (InvalidEnumValue + e) a
enum' = lmapValidator (map flattenEnumErr) $ Enum.validator' <<< Int.validator
