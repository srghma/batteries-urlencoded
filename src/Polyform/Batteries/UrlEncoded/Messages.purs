module Polyform.Batteries.UrlEncoded.Messages where

import Prelude
import Data.Foldable (intercalate)
import Polyform.Batteries.Number (NumberExpected)
import Polyform.Batteries.UrlEncoded.Query (Value) as Query
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, MissingValue)
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs
  = ( BooleanExpected
        + MissingValue
        + NumberExpected
        + msgs
    )

type Printers
  = ( booleanExpected ∷ Query.Value → String
    , intExpected ∷ String → String
    , numberExpected ∷ String → String
    , missingValue ∷ Unit → String
    )

