module Data.FormURLEncoded.Query where
-- | `Map` based search params representation.
-- |  `FormURLEncoded` is based on `Array (String /\ Maybe String)`.

import Prelude

import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode, encode) as FormURLEncoded
import Data.Generic.Rep (class Generic)
import Data.Map (Map, SemigroupMap(..))
import Data.Map (alter, fromFoldableWith, fromFoldableWithIndex, insert, lookup, singleton, update) as Map
import Data.Maybe (fromJust, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (over) as Newtype
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (fromHomogeneous) as Object
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList)
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
newtype Query
  = Query (SemigroupMap String Value)

derive instance Newtype Query _
derive newtype instance Eq Query
derive newtype instance Semigroup Query
derive newtype instance Monoid Query

-- Internal
overMap :: (Map String Value -> Map String Value) -> Query -> Query
overMap f = Newtype.over Query (Newtype.over SemigroupMap f)

lookup ∷ FieldId → Query → Maybe (Array String)
lookup (FieldId name) (Query (SemigroupMap q)) = Map.lookup name q

insert :: FieldId -> Value -> Query -> Query
insert (FieldId k) v = overMap (Map.insert k v)

update ∷ (Value → Maybe Value) → FieldId → Query → Query
update u (FieldId k) = overMap (Map.update u k)

alter :: (Maybe Value -> Maybe Value) -> FieldId -> Query -> Query
alter f (FieldId name) = overMap (Map.alter f name)

singleton :: FieldId -> Value -> Query
singleton (FieldId name) value = Query (SemigroupMap (Map.singleton name value))

toFormURLEncoded :: Query → FormURLEncoded
toFormURLEncoded (Query query) = FormURLEncoded $ foldMapWithIndex step query
  where
    step k v = map (Tuple k <<< Just) v

decode ∷ String → Maybe Query
decode query = do
  FormURLEncoded decoded ← FormURLEncoded.decode query
  pure <<< Query <<< SemigroupMap <<< Map.fromFoldableWith (<>) <<<  map (map fromMaybe) $ decoded

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
fromHomogeneous = Query <<< SemigroupMap <<< Map.fromFoldableWithIndex <<< Object.fromHomogeneous

