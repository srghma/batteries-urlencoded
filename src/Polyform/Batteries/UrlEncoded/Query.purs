module Polyform.Batteries.UrlEncoded.Query where

import Prelude

import Data.FoldableWithIndex (foldMapWithIndex, foldrWithIndex)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode) as FormURLEncoded
import Data.List (List(..), intercalate) as List
import Data.List (foldr)
import Data.Map (Map)
import Data.Map (empty, fromFoldableWith, fromFoldableWithIndex, insert, lookup, unionWith, update) as Map
import Data.Maybe (fromJust, Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Newtype (over) as Newtype
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Foreign.Object (fromHomogeneous) as Object
import JSURI (encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList)
import Type.Row.Homogeneous (class HomogeneousRowList)

type Key
  = String

type Value
  = Array String

-- | We need a map representation of query with
-- | appending semigroup so we can use it when
-- | composing serializers.
newtype Query
  = Query (Map Key Value)

fromHomogeneous :: forall r rl. RowToList r rl => HomogeneousRowList rl Value => Record r -> Query
fromHomogeneous = Query <<< Map.fromFoldableWithIndex <<< Object.fromHomogeneous

derive instance newtypeQuery ∷ Newtype Query _

derive newtype instance showQuery ∷ Show Query

derive newtype instance eqQuery ∷ Eq Query

instance semigroupQuery ∷ Semigroup Query where
  append (Query d1) (Query d2) = Query (Map.unionWith append d1 d2)

instance monoidQuery ∷ Monoid Query where
  mempty = Query Map.empty

lookup ∷ Key → Query → Maybe (Array String)
lookup name (Query q) = Map.lookup name q

insert :: Key -> Value -> Query -> Query
insert k v = Newtype.over Query (Map.insert k v)

update ∷ (Value → Maybe Value) → Key → Query → Query
update u k = Newtype.over Query (Map.update u k)


-- | Browsers serialize space as `+` character
-- | which is incorrect according to the RFC 3986
-- | but it is spread behavior accross tested engines.
-- |
-- | I've written about this issue extensively:
-- | https://github.com/owickstrom/hyper/pull/62
-- |
-- | If we want to be able to optionally distinct this `+`
-- | on the server side we have to convert it to `%2b` before
-- | decoding phase (as it is done in all investigated
-- | libraries - please check first post in the above thread).
type Options
  = { replacePlus ∷ Boolean }

defaultOptions ∷ Options
defaultOptions = { replacePlus: true }

parse ∷ Options → String → Maybe Query
parse { replacePlus } query = do
  let
    query' =
      if replacePlus then
        replaceAll (Pattern "+") (Replacement " ") query
      else
        query
  FormURLEncoded decoded ← FormURLEncoded.decode query'
  pure <<< Query <<< Map.fromFoldableWith (<>) <<< map (map fromMaybe) $ decoded

-- | TODO: Make this safe by dropping unsupported codeunits:
-- |
-- | * https://stackoverflow.com/questions/16868415/encodeuricomponent-throws-an-exception
-- |
-- | * urlPart = urlPart.replace(/[\ud800-\udfff]/g, '');
unsafeEncode ∷ Query → String
unsafeEncode = List.intercalate "&" <<< foldrWithIndex encodePart List.Nil <<< un Query
  where
  unsafeEncodeURIComponent k = unsafePartial $ fromJust $ encodeURIComponent k
  encodePart k varr l = case varr of
    [] -> List.Cons (unsafeEncodeURIComponent k) l
    vs -> foldr (step k) l vs

  step k v r = List.Cons (unsafeEncodeURIComponent k <> "=" <> unsafeEncodeURIComponent v) r

toFormURLEncoded :: Query → FormURLEncoded
toFormURLEncoded (Query query) = FormURLEncoded $ foldMapWithIndex step query
  where
    step k v = map (Tuple k <<< Just) v

-- | Parser
-- type Error fieldErrs errs =
--   ( urlEncodedQueryParseError ∷ String
--   , fieldError ∷ FieldError fieldErrs
--   | errs
--   )
-- _queryParseError = SProxy ∷ SProxy "queryParseError"
-- 
-- query ∷ ∀ m e errs. Monad m ⇒ Query.Options → Batteries.Validator m (Error e errs) String Query.Query
-- query opts = Validator.liftFnMaybe (Batteries.error _queryParseError) (Query.parse opts)
-- 
-- _fieldError = SProxy ∷ SProxy "fieldError"
-- | Dual
-- query ∷ ∀ errs fieldErrs m s
--   . Monad m
--   ⇒ Applicative s
--   ⇒ Query.Options
--   → Batteries.Dual m s (Error fieldErrs errs) String Query.Query
-- query opts = dual (Validators.query opts) serializer
--   where
--     serializer (Query.Query m) =
--       pure <<< unsafeEncode <<< FormURLEncoded <<< Array.Builder.build <<< foldMapWithIndex step $ m
--       where
--         step key [] = Array.Builder.cons (Tuple key Nothing)
--         step key values = foldMap (\v → Array.Builder.cons (Tuple key (Just v))) values
