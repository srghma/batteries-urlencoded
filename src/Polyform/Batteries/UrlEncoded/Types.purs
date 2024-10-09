module Polyform.Batteries.UrlEncoded.Types where

import Prelude

import Data.FormURLEncoded.Query (FieldId(..))
import Data.Lazy as Lazy
import Polyform (Validator) as Polyform
import Polyform.Batteries (Dual, Validator) as Batteries
import Polyform.Batteries (Msg)
import Polyform.Batteries.UrlEncoded.Types.Errors (ErrorId(..), Errors)
import Polyform.Batteries.UrlEncoded.Types.Errors (singleton) as Errors
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Type.Prelude (Proxy(..))

type Validator m errs i o = Polyform.Validator m (Errors errs) i o

type Validator' m (errs :: Row Type) i o = Validator m (Msg errs) i o

stringifyValidator ∷ ∀ m errs i o. Monad m ⇒ Validator' m errs i o → Validator m String i o
stringifyValidator = lmapValidator (map (Lazy.force <<< _.msg))

_urlEncoded = Proxy ∷ Proxy "urlEncoded"

-- | Lifts base `Batteries.Validator` by enhancing possible error structure with `name` info.
fromValidator ∷ ∀ errs m i. Monad m ⇒ FieldId → Batteries.Validator m errs i ~> Validator m errs i
fromValidator (FieldId name) = lmapValidator (Errors.singleton (ErrorId name))

type Dual m errs i o = Validator.Dual.Dual m (Errors errs) i o

type Dual' m (errs :: Row Type) i o = Dual m (Msg errs) i o

-- | Lifts base `Batteries.Dual` by enhancing possilbe errors structure with `name` info.
fromDual ∷ ∀ errs i m. Monad m ⇒ FieldId → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)

