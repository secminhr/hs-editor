{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TextSpec where 

import Test.Hspec (Spec, shouldBe)
import Test.QuickCheck 
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.Natural
import Text
import Test.Hspec.QuickCheck (prop)
import Data.Maybe (fromJust, isJust)
import GHC.TypeLits (Nat)
import GHC.Num.Natural (naturalZero)
import Text (LowerBounded)
import TextEq

instance {-# INCOHERENT #-} Show (a -> b) where
  show _ = "<fn>"

instance Arbitrary String' where
  arbitrary = do
    n <- choose (1, 5)
    str <- vectorOf n (elements "abcde")
    pure . fromJust . singleLineString $ str

instance CoArbitrary String' where
  coarbitrary s = coarbitrary (string s)

instance Arbitrary (Text Nat Nat) where
  arbitrary = sized $ \n ->
    if n <= 1
      then pure empty
      else frequency
        [ (1, pure empty)
        , (5, do
            t <- decayArbitrary 2
            -- 讓 r 有很高的機率落在有效範圍內，偶爾才越界測試邊角案例
            r <- controlledRowIndex t
            f <- arbitrary
            pure $ updateRow r f t
        )
        , (3, do
            t <- decayArbitrary 2
            r <- controlledRowIndex t
            pure $ merge r t
        )
        , (3, do
            t <- decayArbitrary 2
            r <- controlledRowIndex t
            let rowStr = case row r t of
                           Just s -> string s
                           Nothing -> ""
            let len = length rowStr
            c <- fromIntegral <$> choose (0, len)
            pure $ split r c t
          )
        ]

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

instance LowerBounded Nat where 
    lowerbound = 0

controlledRowIndex :: Text Nat Nat -> Gen Nat
controlledRowIndex t = do
  frequency
    [ (5, chooseEnum (0, lastRowAvailable t)) -- 83% 機率在有效範圍內
    , (1, arbitrary)                                          -- 17% 機率完全隨機(測試越界)
    ]


tEmpty :: Text Nat Nat 
tEmpty = empty

infix 4 ~=
(~=) :: Text Nat Nat -> (Nat, Nat -> Maybe String') -> Property
t ~= (maxR, f) = 
    (forAll (chooseEnum (0, maxR)) $ \r -> row r t === f r) .&&.
    lastRowAvailable t === maxR


-------------- row observation
prop_row_gt_lastRowAvailable_is_nothing :: Nat -> Text Nat Nat -> Property 
prop_row_gt_lastRowAvailable_is_nothing r t = 
    r > lastRowAvailable t ==> row r t `shouldBe` Nothing

prop_row_le_lastRowAvailable_is_just :: Text Nat Nat -> Property 
prop_row_le_lastRowAvailable_is_just t = forAll (chooseEnum (0, lastRowAvailable t)) $
                                            \r -> isJust (row r t) === True

prop_0th_row_of_empty_is_empty_string :: Property
prop_0th_row_of_empty_is_empty_string = (string <$> (row 0 tEmpty)) === Just ""

prop_row_updateRow_at_le_lastRowAvailable :: Text Nat Nat -> String -> Property
prop_row_updateRow_at_le_lastRowAvailable t s = 
    forAll (chooseEnum (0, lastRowAvailable t)) $ \r -> 
        let updated = updateRow r (const updatedS) t 
            updatedS = fromJust $ singleLineString $ filter (/= '\n') s in
        updated ~= (lastRowAvailable updated, \obsR -> 
                if obsR == r then Just updatedS
                else row obsR t
            )
            

prop_updateRow_at_gt_lastRowAvailable :: Text Nat Nat -> Nat -> String -> Property
prop_updateRow_at_gt_lastRowAvailable t r s = 
    r > lastRowAvailable t ==> 
        updateRow r (const $ fromJust $ singleLineString $ filter (/= '\n') s) t ==== t 


prop_row_merge_le_lastRowAvailable :: Text Nat Nat -> Property
prop_row_merge_le_lastRowAvailable t =
    forAll (chooseEnum (0, lastRowAvailable t)) $ \r -> let merged = merge r t in 
        merged ~= (lastRowAvailable merged, \obsR -> 
                if obsR < r then row obsR t 
                else if obsR > r then row (obsR + 1) t 
                else do 
                    let rowAtR = string $ fromJust $ row r t
                    let rowAtSuccR = string <$> row (r+1) t
                    singleLineString $ case rowAtSuccR of 
                        Nothing -> rowAtR
                        Just sr -> rowAtR ++ sr
            )

prop_merge_at_lastRowAvailable :: Text Nat Nat -> Property 
prop_merge_at_lastRowAvailable t = merge (lastRowAvailable t) t ==== t 

prop_merge_at_gt_lastRowAvailable :: Text Nat Nat -> Nat -> Property 
prop_merge_at_gt_lastRowAvailable t r = 
    r > lastRowAvailable t ==> merge r t ==== t


prop_row_split_le_lastRowAvailable :: Text Nat Nat -> Property
prop_row_split_le_lastRowAvailable t = 
    forAll (chooseEnum (0, lastRowAvailable t)) $ \r -> 
        forAll (chooseEnum (naturalZero, fromIntegral $ length $ string $ fromJust $ row r t)) $ \c -> 
            let splitted = split r c t in 
                splitted ~= (lastRowAvailable splitted, \obsR -> 
                        if obsR < r then row obsR t 
                        else if obsR == r then singleLineString $ take (fromIntegral c) $ string $ fromJust $ row r t
                        else if obsR == r + 1 then singleLineString $ drop (fromIntegral c) $ string $ fromJust $ row r t 
                        else row (obsR - 1) t
                    )

prop_split_at_gt_lastRowAvailable :: Text Nat Nat -> Nat -> Nat -> Property
prop_split_at_gt_lastRowAvailable t r c = 
    r > lastRowAvailable t ==> split r c t ==== t  

---------------------- lastRowAvailable observation
prop_lastRowAvaialble_empty_is_0 :: Property 
prop_lastRowAvaialble_empty_is_0 = lastRowAvailable tEmpty === 0

prop_lastRowAvailable_update :: Text Nat Nat -> Nat -> String -> Property
prop_lastRowAvailable_update t r s = 
    lastRowAvailable (updateRow r (const (fromJust $ singleLineString (filter (/= '\n') s))) t) === lastRowAvailable t 

prop_lastRowAvailable_merge_eq :: Text Nat Nat -> Property
prop_lastRowAvailable_merge_eq t = 
    lastRowAvailable (merge (lastRowAvailable t) t) === lastRowAvailable t 

prop_lastRowAvailable_merge_lt :: Text Nat Nat -> Property
prop_lastRowAvailable_merge_lt t = lastRowAvailable t /= 0 ==> 
    forAll (chooseEnum (0, lastRowAvailable t - 1)) $ \mr -> 
        lastRowAvailable (merge mr t) === lastRowAvailable t - 1

prop_lastRowAvailable_merge_gt :: Text Nat Nat -> Nat -> Property
prop_lastRowAvailable_merge_gt t r = 
    r > lastRowAvailable t ==> lastRowAvailable (merge r t) === lastRowAvailable t

prop_lastRowAvailable_split_le :: Text Nat Nat -> Nat -> Property
prop_lastRowAvailable_split_le t c = 
    forAll (chooseEnum (0, lastRowAvailable t)) $ \sr -> 
        lastRowAvailable (split sr c t) === lastRowAvailable t + 1

prop_lastRowAvailable_split_gt :: Text Nat Nat -> Nat -> Nat -> Property
prop_lastRowAvailable_split_gt t r c = 
    r > lastRowAvailable t ==> lastRowAvailable (split r c t) === lastRowAvailable t

---------------------------------- flatten
prop_fromString_flatten_id :: Text Nat Nat -> Property 
prop_fromString_flatten_id t = 
    fromString (flatten t) ==== t 

prop_flatten_fromString_id :: [String] -> Property 
prop_flatten_fromString_id ss = let s = unlines ss in 
    flatten ((fromString s) :: Text Nat Nat) === s

prop_flatten_row :: Text Nat Nat -> Property
prop_flatten_row t = 
    conjoin $ map (\(r, str) -> singleLineString str === row r t) $ zip [0..] $ lines (flatten t)



------------ non-observation
prop_merge_split :: Text Nat Nat -> Nat -> Nat -> Property
prop_merge_split t r c = 
    (merge r . split r c) t ==== t 

prop_updateRow_updateRow :: Text Nat Nat -> Nat -> (String' -> String') -> (String' -> String') -> Property
prop_updateRow_updateRow t r f g = 
    (updateRow r f . updateRow r g) t ==== updateRow r (f . g) t 
    


spec :: Spec 
spec = do 
    prop "prop_row_gt_lastRowAvailable_is_nothing" prop_row_gt_lastRowAvailable_is_nothing
    prop "prop_row_le_lastRowAvailable_is_just" prop_row_le_lastRowAvailable_is_just
    prop "prop_0th_row_of_empty_is_empty_string" prop_0th_row_of_empty_is_empty_string
    prop "prop_row_updateRow_at_le_lastRowAvailable" prop_row_updateRow_at_le_lastRowAvailable
    prop "prop_updateRow_at_gt_lastRowAvailable" prop_updateRow_at_gt_lastRowAvailable
    prop "prop_row_merge_le_lastRowAvailable" prop_row_merge_le_lastRowAvailable
    prop "prop_merge_at_lastRowAvailable" prop_merge_at_lastRowAvailable
    prop "prop_merge_at_gt_lastRowAvailable" prop_merge_at_gt_lastRowAvailable
    prop "prop_row_split_le_lastRowAvailable" prop_row_split_le_lastRowAvailable
    prop "prop_split_at_gt_lastRowAvailable" prop_split_at_gt_lastRowAvailable
    prop "prop_lastRowAvaialble_empty_is_0" prop_lastRowAvaialble_empty_is_0
    prop "prop_lastRowAvailable_update" prop_lastRowAvailable_update
    prop "prop_lastRowAvailable_merge_eq" prop_lastRowAvailable_merge_eq
    prop "prop_lastRowAvailable_merge_lt" prop_lastRowAvailable_merge_lt
    prop "prop_lastRowAvailable_merge_gt" prop_lastRowAvailable_merge_gt
    prop "prop_lastRowAvailable_split_le" prop_lastRowAvailable_split_le
    prop "prop_lastRowAvailable_split_gt" prop_lastRowAvailable_split_gt
    prop "prop_fromString_flatten_id" prop_fromString_flatten_id
    prop "prop_flatten_fromString_id" prop_flatten_fromString_id
    prop "prop_flatten_row" prop_flatten_row
    prop "prop_merge_split" prop_merge_split
    prop "prop_updateRow_updateRow" prop_updateRow_updateRow