module Test.Main where

import Prelude

import Data.FormURLEncoded.Query (FieldId(..))
import Data.FormURLEncoded.Query (fromHomogeneous) as Query
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Polyform (Dual(..))
import Polyform.Batteries.UrlEncoded.Duals (required')
import Polyform.Dual ((~))
import Polyform.Reporter.Dual (liftValidatorDualWith, runReporter)
import Prim.RowList (class RowToList)
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test.Unit
import Test.Unit.Assert (equal) as Assert
import Test.Unit.Main (runTest)
import Type.Row.Homogeneous (class HomogeneousRowList)

suite :: TestSuite
suite = do
  let
    -- | We are testing here dual lifted to `Reporter.Dual`.
    -- | This should be probably a separate test in polyform
    -- | itself.
    testForm
      :: forall r res rl
       . Show res
      => Eq res
      => RowToList r rl
      => HomogeneousRowList rl (Array String)
      => Show { | r }
      => Dual _ _ _ res
      -> { | r }
      -> Array String
      -> Maybe res
      -> _
    testForm form queryRecord expectedReport expectedResult = do
      let
        query = Query.fromHomogeneous queryRecord
      Test.Unit.test (show queryRecord) do
        Tuple givenResult givenReport ← runReporter form query
        Assert.equal expectedReport givenReport
        Assert.equal expectedResult givenResult

  Test.Unit.suite "Test.Polyform.Batteries.UrlEncoded.Duals"
    $ do
        Test.Unit.suite "Error accumulation"
          $ do
              Test.Unit.suite " object" $ do
                let
                  -- | Let's create a smiple form layout (`Array [String]`)
                  field name = liftValidatorDualWith (const $ [ name ]) (const $ [ name ]) (required' (FieldId name) identity)
                  form = Dual $ { foo: _, bar: _ } <$> _.foo ~ field "foo" <*> _.bar ~ field "bar"
                  -- | We expect that our layout is constructed correctly all the time
                  expectedReport = [ "foo", "bar" ]

                testForm form { _ignore: [] } expectedReport Nothing

                testForm form { foo: [ "value" ] } expectedReport Nothing

                testForm
                  form
                  { foo: [ "value" ], bar: [ "value" ] }
                  expectedReport
                  (Just { foo: "value", bar: "value" })

main ∷ Effect Unit
main = runTest suite
