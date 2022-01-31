module Polyform.Batteries.UrlEncoded.Types.Errors where

import Prelude

import Data.FormURLEncoded.Query (FieldId)
import Data.Map (Map, SemigroupMap(..))
import Data.Map (alter, insert, lookup, singleton, update) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Newtype (over) as Newtype
import Polyform.Batteries (Errors) as Batteries
import Polyform.Batteries (Msg)

newtype Errors errs
  = Errors (SemigroupMap FieldId (Batteries.Errors errs))

type Errors' (errs :: Row Type) = Errors (Msg errs)

derive instance Newtype (Errors errs) _
derive newtype instance Eq errs => Eq (Errors errs)
derive newtype instance Semigroup (Errors errs)
derive newtype instance Monoid (Errors errs)

-- Internal
overMap :: forall errs. (Map FieldId (Array errs) -> Map FieldId (Array errs)) -> Errors errs -> Errors errs
overMap f = Newtype.over Errors (Newtype.over SemigroupMap f)

lookup ∷ forall errs. FieldId → Errors errs → Maybe (Array errs)
lookup name (Errors (SemigroupMap q)) = Map.lookup name q

insert :: forall errs. FieldId -> Array errs -> Errors errs -> Errors errs
insert k v = overMap (Map.insert k v)

update ∷ forall errs. (Array errs → Maybe (Array errs)) → FieldId → Errors errs → Errors errs
update u k = overMap (Map.update u k)

alter :: forall errs. (Maybe (Array errs) -> Maybe (Array errs)) -> FieldId -> Errors errs -> Errors errs
alter f name = overMap (Map.alter f name)

singleton :: forall errs. FieldId -> Array errs -> Errors errs
singleton name value = Errors (SemigroupMap (Map.singleton name value))

