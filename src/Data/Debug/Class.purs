-- | This module provides the `Debug` type class, for converting values into
-- | their `Debug` representations.
module Data.Debug.Class
  ( class Debug
  , debug
  , diff
  , class DebugRowList
  , debugRowList
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Date (Date, day, month, year)
import Data.DateTime (DateTime, date, time)
import Data.Debug.Type as D
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Set (Set)
import Data.Set as Set
import Data.Time (Time, hour, minute, second)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, on)
import Effect (Effect)
import Prim.Row as Row
import Record (get, delete)
import Prim.RowList (class RowToList, kind RowList, Nil, Cons)
import Type.Data.RowList (RLProxy(..))

-- | Ideally, all types of kind `Type` should have an instance of this class.
-- | If you are defining a type where it's difficult/impossible to do anything
-- | useful here (e.g. `Ref` or `(->)`) then you should use the `opaque`
-- | constructor.
-- |
-- | If a type has an `Eq` instance, then the `debug` function in its `Debug`
-- | instance should be *injective*, that is:
-- |
-- | ```purescript
-- | x /= y `implies` debug x /= debug y
-- | ```
class Debug a where
  debug :: a -> D.Repr

-- | Compare two values using the specified options, and record the results as
-- | a `ReprDelta` structure.
diffWith :: forall a. Debug a => D.DiffOptions -> a -> a -> D.ReprDelta
diffWith opts x y = D.diffReprWith opts (debug x) (debug y)

-- | Compare two values using the default options.
diff :: forall a. Debug a => a -> a -> D.ReprDelta
diff = diffWith D.defaultDiffOptions

-------------------------------------------------------------------------------
-- Prim

instance debugInt :: Debug Int where
  debug = D.int

instance debugNumber :: Debug Number where
  debug = D.number

instance debugBoolean :: Debug Boolean where
  debug = D.boolean

instance debugString :: Debug String where
  debug = D.string

instance debugChar :: Debug Char where
  debug = D.char

instance debugArray :: Debug a => Debug (Array a) where
  debug = D.array <<< map debug

instance debugFunction :: Debug (a -> b) where
  debug _ = D.opaque_ "function"

-- | This class is part of the machinery for the `Debug (Record r)` instance;
-- | it is not intended to be used directly.
class DebugRowList wrapper (list :: RowList Type) (row :: # Type) | list -> row where
  debugRowList :: RLProxy list -> wrapper row -> List (Tuple String D.Repr)

instance debugRowListNilRecord :: DebugRowList Record Nil () where
  debugRowList _ _ = Nil

instance debugRowListNilVariant :: DebugRowList Variant Nil () where
  debugRowList _ _ = Nil

instance debugRowListConsRecord ::
  ( Debug a
  , DebugRowList Record listRest rowRest
  , Row.Cons key a rowRest rowFull
  , Row.Lacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => DebugRowList Record (Cons key a listRest) rowFull where
  debugRowList _ rec =
    Tuple (reflectSymbol key) (debug val) : rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = debugRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance debugRowListConsVariant ::
  ( Debug a
  , DebugRowList Variant listRest rowRest
  , Row.Cons key a rowRest rowFull
  , Row.Lacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => DebugRowList Variant (Cons key a listRest) rowFull where
  debugRowList _ var = 
    on key match rest var
    where
    key = SProxy :: SProxy key
    match val = Tuple (reflectSymbol key) (debug val) : Nil
    rest = debugRowList (RLProxy :: RLProxy listRest)

instance debugRecord ::
  ( RowToList row list
  , DebugRowList Record list row
  ) => Debug (Record row) where
  debug r =
    D.record (Array.fromFoldable (debugRowList prx r))
    where
    prx = RLProxy :: RLProxy list

instance debugVariant ::
  ( RowToList row list
  , DebugRowList Variant list row
  ) => Debug (Variant row) where
  debug v =
    D.variant (Array.fromFoldable (debugRowList prx v))
    where
    prx = RLProxy :: RLProxy list

-------------------------------------------------------------------------------
-- Prelude

instance debugOrdering :: Debug Ordering where
  debug LT = D.constructor "LT" []
  debug EQ = D.constructor "EQ" []
  debug GT = D.constructor "GT" []

instance debugUnit :: Debug Unit where
  debug _ = D.constructor "unit" []

instance debugVoid :: Debug Void where
  debug = absurd

-------------------------------------------------------------------------------
-- Core

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debug (Just x) = D.constructor "Just" [debug x]
  debug Nothing = D.constructor "Nothing" []

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debug (Right x) = D.constructor "Right" [debug x]
  debug (Left x) = D.constructor "Left" [debug x]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debug (Tuple x y) = D.constructor "Tuple" [debug x, debug y]

instance debugNonEmptyArray :: Debug a => Debug (NonEmptyArray.NonEmptyArray a) where
  debug xs = D.collection "NonEmptyArray" (map debug (NonEmptyArray.toArray xs))

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debug m =
    D.assoc "Map"
      (map (bimap debug debug) (Map.toUnfoldable m))

instance debugEffect :: Debug (Effect a) where
  debug _ = D.opaque_ "Effect"

instance debugList :: Debug a => Debug (List a) where
  debug xs = D.collection "List" (map debug (List.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debug xs = D.collection "List.Lazy" (map debug (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debug s = D.collection "Set" (map debug (Set.toUnfoldable s))

instance debugDate :: Debug Date where
  debug d = D.opaqueLiteral "Date" (debugDate' d)

instance debugDateTime :: Debug DateTime where
  debug dt = D.opaqueLiteral "DateTime" (debugDate' (date dt) <> " " <> debugTime' (time dt))

instance debugTime :: Debug Time where
  debug t = D.opaqueLiteral "Time" (debugTime' t)

debugDate' :: Date -> String
debugDate' d =
    (ljust0 4 (show (fromEnum (year d))) <> "-" <>
     ljust0 2 (show (fromEnum (month d))) <> "-" <>
     ljust0 2 (show (fromEnum (day d))))

debugTime' :: Time -> String
debugTime' t =
  (ljust0 2 (show (fromEnum (hour t))) <> ":" <>
   ljust0 2 (show (fromEnum (minute t))) <> ":" <>
   ljust0 2 (show (fromEnum (second t))))

ljust0 :: Int -> String -> String
ljust0 n str =
  power "0" (n - String.length str) <> str

instance debugRepr :: Debug D.Repr where
  debug r = D.opaque "Repr" r

instance debugReprDelta :: Debug D.ReprDelta where
  debug _ = D.opaque_ "ReprDelta"
