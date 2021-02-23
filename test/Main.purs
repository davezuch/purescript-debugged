module Test.Main where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Array (range)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Debug (class Debug, debug, diff, genericDebug, prettyPrintDelta)
import Data.Debug.Eval (eval)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.List.Lazy as LL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time (Time)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Console (log)

superbAssertEqual :: forall a. Eq a => Debug a => a -> a -> Effect Unit
superbAssertEqual x y =
  if x == y
    then do
      log "(Pretend) test succeeded:"
      log (prettyPrintDelta (diff x y))
    else do
       log "(Pretend) test failed:"
       log (prettyPrintDelta (diff x y))

data Example a b
  = None
  | PairA a a
  | PairB b b
  | Loads (Array a) (Either a b)

derive instance genericExample :: Generic (Example a b) _
derive instance eqExample :: (Eq a, Eq b) => Eq (Example a b)

type Eg = Example Int (Array String)

instance debugExample :: (Debug a, Debug b) => Debug (Example a b) where
  debug = genericDebug

main :: Effect Unit
main = do
  let p = eval

  p 24
  p 1.4e10
  p 1.4e+30
  p 0
  p '\x1e0'
  p (-3)
  p (-3.0)
  p (Tuple 1 (-1))
  p (Tuple 1.0 (-1.0))
  p unit
  p [unit]
  p [[[[unit]]]]
  p [[1,2,3], [4,5,6], [7,8,9] ]
  p [Tuple "a" 1, Tuple "b" 2]
  p eg
  p (debug eg)
  p {foo: 1, bar: "hi"}
  p {foo: 1, bar: "hi", baz: {quux: 3, aah: Tuple "AAH" "AAAAH"}}
  p {type: {data: 0}}
  p {"0": {"1": 2}}
  p (inj (SProxy :: SProxy "foo") 1 :: Variant (foo :: Int, bar :: String))
  p (inj (SProxy :: SProxy "bar") "hi" :: Variant (foo :: Int, bar :: String))
  p (Map.fromFoldable [Tuple "a" 1, Tuple "b" 2])
  p (L.fromFoldable (range 1 10))
  p (LL.fromFoldable (range 1 10))
  p (NonEmptyArray.cons' 1 (range 2 10))
  p (lift3 canonicalDate (toEnum 2000) (toEnum 1) (toEnum 2))
  p (top :: Time)
  p (lift2 DateTime (lift3 canonicalDate (toEnum 2020) (toEnum 12) (toEnum 31)) (pure bottom))
  p (pure unit :: Effect Unit)

  p (None :: Eg)
  p (PairA 3 3 :: Eg)
  p (Loads [1,2,3] (Right ["hi"]))

  superbAssertEqual ([] :: Array Int) []

  do
    let x = Loads [1,2,3,4,5] (Right ["hi", "world"])
        y = Loads [1,2,4,3,5] (Right [])
    superbAssertEqual x y

  do
    let x = Map.fromFoldable [Tuple "a" 1, Tuple "b" 2]
        y = Map.fromFoldable [Tuple "a" 2, Tuple "c" 3]
    superbAssertEqual x y

  do
    let x = NonEmptyArray.cons' 1 [2, 3, 4, 5]
        y = NonEmptyArray.cons' 1 [2, 4, 3, 5]
    superbAssertEqual x y

  do
    let x = { first: { second: { third: { str: "x", bool: true, arr: [1, 2, 3] } } } }
        y = { first: { second: { third: { str: "y", bool: true, arr: [1, 2, 3] } } } }
    superbAssertEqual x y

-- note: the type signature is needed here for instance selection
eg :: forall a. Tuple (a -> a) (Tuple (Either Void (Maybe Unit)) (Either (Either Int Int) Int))
eg = Tuple identity (Tuple (Right Nothing) (Left (Left 3)))

