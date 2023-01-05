module Polyform.Batteries.UrlEncoded.Types.Errors where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (alter, insert, lookup, singleton, update) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Newtype (over) as Newtype
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Polyform.Batteries (Errors) as Batteries
import Polyform.Batteries (Msg)

newtype ErrorId = ErrorId String
derive instance Newtype ErrorId _
derive instance Generic ErrorId _
derive newtype instance Eq ErrorId
derive newtype instance Ord ErrorId
instance Show ErrorId where
  show = genericShow

newtype Errors errs
  = Errors (Map ErrorId (Array errs))

type Errors' (errs :: Row Type) = Errors (Msg errs)

derive instance Newtype (Errors errs) _
derive newtype instance Eq errs => Eq (Errors errs)
derive instance Functor Errors
derive instance Foldable Errors
derive instance Traversable Errors
derive newtype instance Semigroup (Errors errs)
derive newtype instance Monoid (Errors errs)

-- Internal
overMap :: forall errs. (Map ErrorId (Array errs) -> Map ErrorId (Array errs)) -> Errors errs -> Errors errs
overMap f = Newtype.over Errors f

lookup ∷ forall errs. ErrorId → Errors errs → Array errs
lookup name (Errors q) = fromMaybe [] $ Map.lookup name q

insert :: forall errs. ErrorId -> Array errs -> Errors errs -> Errors errs
insert k v = overMap (Map.insert k v)

update ∷ forall errs. (Array errs → Maybe (Array errs)) → ErrorId → Errors errs → Errors errs
update u k = overMap (Map.update u k)

alter :: forall errs. (Maybe (Array errs) -> Maybe (Array errs)) -> ErrorId -> Errors errs -> Errors errs
alter f name = overMap (Map.alter f name)

singleton :: forall errs. ErrorId -> Array errs -> Errors errs
singleton name value = Errors (Map.singleton name value)

