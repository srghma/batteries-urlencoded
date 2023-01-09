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
  , MultiValueFieldValidator
  , SingleValueFieldValidator
  , _booleanExpected
  , _missingValue
  , missingValue
  , boolean
  , enum
  , enum'
  , flattenEnumErr
  , optional
  , optionalMulti
  , optValidator
  , required
  , required'
  , requiredMulti
  , requiredMulti'
  , value
  , valueArray
  ) where

import Prelude

import Data.Array (head) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FormURLEncoded.Query (Value, lookup) as Query
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup as V
import Polyform.Batteries (Msg, msg', onErr)
import Polyform.Batteries as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex, _invalidEnumIndex)
import Polyform.Batteries.Generic.Enum (validator, validator') as Enum
import Polyform.Batteries.Int (IntExpected, _intExpected)
import Polyform.Batteries.Int (validator) as Int
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded.Types (Validator', Validator, fromValidator)
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (liftFn, liftFnMV, liftFnV, lmapValidator, runValidator)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type FieldValidator m e b
  = Batteries.Validator m e (Maybe Query.Value) b

type SingleValueFieldValidator m e b
  = Batteries.Validator m e String b

type MultiValueFieldValidator m e b
  = Batteries.Validator m e (Array String) b

-- | Generic over error type.
required ∷
  ∀ a m err.
  Monad m ⇒
  FieldId →
  err ->
  Batteries.Validator m err String a →
  Validator m err Query a
required name err fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value err <<< Validator.liftFn (Query.lookup name))

-- | Using `Batteries.Msg` for error type.
required' ∷
  ∀ a m err.
  Monad m ⇒
  FieldId →
  Batteries.Validator m (Msg (MissingValue + err)) String a →
  Validator m (Msg (MissingValue + err)) Query a
required' name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value (missingValue "Missing value") <<< Validator.liftFn (Query.lookup name))


optional ∷
  ∀ a m errs.
  Monad m ⇒
  FieldId →
  Batteries.Validator m errs String a →
  UrlEncoded.Validator m errs Query (Maybe a)
optional name fieldValidator = fromValidator name (optValidator fieldValidator <<< Validator.liftFn (Query.lookup name))

requiredMulti ∷
  ∀ a m err.
  Monad m ⇒
  FieldId →
  err →
  Batteries.Validator m err (NonEmptyArray String) (NonEmptyArray a) →
  Validator m err Query (NonEmptyArray a)
requiredMulti name err fieldValidator =
  fromValidator
    name
    (fieldValidator <<< nonEmptyArray <<< valueArray <<< Validator.liftFn (Query.lookup name))
  where
  nonEmptyArray = liftFnV
    $ Array.NonEmpty.fromArray >>> case _ of
      Just arr -> pure arr
      Nothing → V.invalid [err] -- Batteries.invalid _missingValue (const "Missing value") {}

requiredMulti' ∷
  ∀ a m errs.
  Monad m ⇒
  FieldId →
  Batteries.Validator m (Msg (MissingValue + errs)) (NonEmptyArray String) (NonEmptyArray a) →
  Validator m (Msg (MissingValue + errs)) Query (NonEmptyArray a)
requiredMulti' name fieldValidator = requiredMulti name (missingValue "Missing value") fieldValidator

optionalMulti ∷
  ∀ a m errs.
  Monad m ⇒
  FieldId →
  Batteries.Validator' m errs (Array String) (Array a) →
  Validator' m errs Query (Array a)
optionalMulti name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< valueArray <<< Validator.liftFn (Query.lookup name))

_missingValue = Proxy ∷ Proxy "missingValue"

type MissingValue e
  = ( missingValue ∷ {} | e )

missingValue ∷ ∀ err. String -> Msg (MissingValue err)
missingValue msg = Batteries.msg' msg _missingValue {}

value ∷ ∀ err m. Applicative m ⇒ err -> Batteries.Validator m err (Maybe Query.Value) String -- (Msg (MissingValue + e)) String
value err =
  liftFnV
    $ \qv → case qv >>= Array.head of
        Just "" → V.invalid [err]
        Just v → pure v
        Nothing → V.invalid [err]

valueArray ∷ ∀ e m. Monad m ⇒ FieldValidator m e (Array String)
valueArray =
  liftFn
    $ case _ of
        Just s → s
        Nothing → []

-- | We could do a bit of dance with `Choice.first` etc.
-- | but this seems simpler and a bit more efficient
optValidator ∷ ∀ b e m. Monad m ⇒ Batteries.Validator m e String b → Batteries.Validator m e (Maybe Query.Value) (Maybe b)
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
boolean ∷ ∀ e m. Applicative m ⇒ FieldValidator m (Msg (booleanExpected ∷ Query.Value | e )) Boolean
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

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → Batteries.Validator m (Msg (InvalidEnumValue + e)) String a
enum p = lmapValidator (map flattenEnumErr) $ Enum.validator p <<< Int.validator

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Batteries.Validator m (Msg (InvalidEnumValue + e)) String a
enum' = lmapValidator (map flattenEnumErr) $ Enum.validator' <<< Int.validator
