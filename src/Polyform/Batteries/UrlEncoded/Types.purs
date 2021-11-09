module Polyform.Batteries.UrlEncoded.Types where

import Prelude
import Data.Array (singleton) as Array
import Polyform (Validator) as Polyform
import Polyform.Batteries (Dual, Errors, Validator) as Batteries
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Type.Prelude (SProxy(..))

type Name
  = String

type Errors (errs ∷ # Type)
  = Array { name ∷ Name, errors ∷ Batteries.Errors errs }

type Validator m (errs ∷ # Type) i o
  = Polyform.Validator m (Errors errs) i o

_urlEncoded = SProxy ∷ SProxy "urlEncoded"

-- | Lifts base `Batteries.Validator` by enhancing possible error structure with `name` info.
fromValidator ∷ ∀ errs m i. Monad m ⇒ String → Batteries.Validator m errs i ~> Validator m errs i
fromValidator name = lmapValidator (Array.singleton <<< { name, errors: _ })

type Dual m (errs ∷ # Type) i o
  = Validator.Dual.Dual m (Errors errs) i o

-- | Lifts base `Batteries.Dual` by enhancing possilbe errors structure with `name` info.
fromDual ∷ ∀ errs i m. Monad m ⇒ String → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)
