module Polyform.Batteries.UrlEncoded.Types where

import Prelude

import Data.FormURLEncoded.Query (FieldId)
import Data.Map (SemigroupMap(..))
import Data.Map (singleton) as Map
import Polyform (Validator) as Polyform
import Polyform.Batteries (Dual, Errors, Validator) as Batteries
import Polyform.Batteries (Msg)
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Type.Prelude (SProxy(..))

type Name
  = String

type Errors errs
  = SemigroupMap FieldId (Batteries.Errors errs)

type Errors' (errs :: Row Type) = Errors (Msg errs)

type Validator m errs i o
  = Polyform.Validator m (Errors errs) i o

type Validator' m (errs :: Row Type) i o = Validator m (Msg errs) i o

_urlEncoded = SProxy ∷ SProxy "urlEncoded"

-- | Lifts base `Batteries.Validator` by enhancing possible error structure with `name` info.
fromValidator ∷ ∀ errs m i. Monad m ⇒ FieldId → Batteries.Validator m errs i ~> Validator m errs i
fromValidator name = lmapValidator (SemigroupMap <<< Map.singleton name)

type Dual m errs i o
  = Validator.Dual.Dual m (Errors errs) i o

type Dual' m (errs :: Row Type) i o = Dual m (Msg errs) i o

-- | Lifts base `Batteries.Dual` by enhancing possilbe errors structure with `name` info.
fromDual ∷ ∀ errs i m. Monad m ⇒ FieldId → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)


