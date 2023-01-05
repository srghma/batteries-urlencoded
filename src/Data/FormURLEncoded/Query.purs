-- TODO: Rename to `Dict`
module Data.FormURLEncoded.Query where
-- | `Map` based search params representation.
-- |  `FormURLEncoded` is based on `Array (String /\ Maybe String)`.

import Prelude

import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode, encode) as FormURLEncoded
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (over) as Newtype
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (fromHomogeneous, toArrayWithKey) as Object
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList)
import Safe.Coerce (coerce)
import Type.Row.Homogeneous (class HomogeneousRowList)

newtype FieldId = FieldId String
derive instance Newtype FieldId _
derive instance Generic FieldId _
derive newtype instance Eq FieldId
derive newtype instance Ord FieldId
instance Show FieldId where
  show = genericShow

type Value
  = Array String

-- | We need a map representation of query with
-- | appending semigroup so we can use it when
-- | composing serializers.
-- | TODO: rename to `Dict`
newtype Query
  = Query (Map FieldId Value)

derive instance Newtype Query _
derive newtype instance Eq Query
derive newtype instance Semigroup Query
derive newtype instance Monoid Query

-- Internal
overMap :: (Map FieldId Value -> Map FieldId Value) -> Query -> Query
overMap f = Newtype.over Query f

lookup ∷ FieldId → Query → Maybe (Array String)
lookup name (Query q) = Map.lookup name q

insert :: FieldId -> Value -> Query -> Query
insert k v = overMap (Map.insert k v)

update ∷ (Value → Maybe Value) → FieldId → Query → Query
update u k = overMap (Map.update u k)

alter :: (Maybe Value -> Maybe Value) -> FieldId -> Query -> Query
alter f name = overMap (Map.alter f name)

singleton :: FieldId -> Value -> Query
singleton name value = Query (Map.singleton name value)

toFormURLEncoded :: Query → FormURLEncoded
toFormURLEncoded (Query query) = FormURLEncoded $ foldMapWithIndex step query
  where
    step (FieldId k) v = map (Tuple k <<< Just) v

decode ∷ String → Maybe Query
decode query = do
  FormURLEncoded decoded ← FormURLEncoded.decode query
  pure
    <<< Query
    <<< Map.fromFoldableWith (<>)
    <<< (coerce :: Array (String /\ Value) -> Array (FieldId /\ Value))
    <<< map (map fromMaybe)
    $ decoded

encode ∷ Query → Maybe String
encode = FormURLEncoded.encode <<< toFormURLEncoded

-- | TODO: Make this safe by dropping unsupported codeunits:
-- |
-- | * https://stackoverflow.com/questions/16868415/encodeuricomponent-throws-an-exception
-- |
-- | * urlPart = urlPart.replace(/[\ud800-\udfff]/g, '');
-- |
unsafeEncode :: Query -> String
unsafeEncode = unsafePartial $ fromJust <<< encode

fromHomogeneous :: forall r rl. RowToList r rl => HomogeneousRowList rl Value => Record r -> Query
fromHomogeneous = Query <<< Map.fromFoldable <<< coerceFieldIds <<< Object.toArrayWithKey (/\) <<< Object.fromHomogeneous
  where
    coerceFieldIds :: Array (String /\ Value) -> Array (FieldId /\ Value)
    coerceFieldIds = coerce

fromFoldable :: forall f. Foldable f => f (FieldId /\ Array String) -> Query
fromFoldable = Query <<< Map.fromFoldable
