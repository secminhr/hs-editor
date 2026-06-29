{-# LANGUAGE FlexibleInstances #-}

module ItemSelectorSpec where 

import ItemSelector
import Test.QuickCheck
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances.Semigroup

instance Arbitrary (ItemSelector Int) where 
    arbitrary = frequency 
        [ (5, newItemSelector <$> arbitrary)
        , (3, select <$> arbitrary <*> arbitrary)
        , (3, setItems <$> arbitrary <*> arbitrary)
        ]

----------- getItems
prop_getItems_newItemSelector :: NonEmpty Int -> Property 
prop_getItems_newItemSelector list = 
    (getItems $ newItemSelector list) === list 

prop_getItems_select :: ItemSelector Int -> Int -> Property 
prop_getItems_select selector pos = 
    (getItems $ select pos $ selector) === getItems selector 

prop_getItems_setItems :: ItemSelector Int -> NonEmpty Int -> Property 
prop_getItems_setItems selector list = 
    (getItems $ setItems list $ selector) === list 

---------------- selectedIndex
prop_selectedIndex_newItemSelector :: NonEmpty Int -> Property
prop_selectedIndex_newItemSelector list = 
    (selectedIndex $ newItemSelector list) === 0 

prop_selectedIndex_select :: ItemSelector Int -> Int -> Property 
prop_selectedIndex_select selector pos = 
    fromIntegral (selectedIndex $ select pos $ selector) ===
        if pos >= 0 then pos `rem` length (getItems selector)
        else let negatePos = -pos 
                 l = length (getItems selector) in (l-1) - ((negatePos-1) `rem` l)


prop_selectedIndex_setItems :: ItemSelector Int -> NonEmpty Int -> Property 
prop_selectedIndex_setItems selector list = 
    let oldIndex = selectedIndex selector 
        changedSelector = setItems list selector
        newIndex = selectedIndex changedSelector in 
            newIndex === oldIndex `rem` (fromIntegral $ length (getItems changedSelector))
    
prop_selectedIndex_always_lt_length :: ItemSelector Int -> Property 
prop_selectedIndex_always_lt_length selector = 
    property $ selectedIndex selector < (fromIntegral $ length (getItems selector))

----------------- selectedItem 
prop_selectedItem :: ItemSelector Int -> Property
prop_selectedItem selector = 
    selectedItem selector === getItems selector NE.!! (fromIntegral $ selectedIndex selector)

instance Show (Int -> Int) where 
    show _ = "<Intfn>"

prop_selectedItem_mapSelectedItem :: ItemSelector Int -> (Int -> Int) -> Property 
prop_selectedItem_mapSelectedItem selector f = 
    (selectedItem $ mapSelectedItem f selector) === (f $ selectedItem selector)

spec :: Spec
spec = do 
    prop "prop_getItems_newItemSelector" prop_getItems_newItemSelector
    prop "prop_getItems_select" prop_getItems_select
    prop "prop_getItems_setItems" prop_getItems_setItems
    prop "prop_selectedIndex_newItemSelector" prop_selectedIndex_newItemSelector
    prop "prop_selectedIndex_select" prop_selectedIndex_select
    prop "prop_selectedIndex_setItems" prop_selectedIndex_setItems
    prop "prop_selectedIndex_always_lt_length" prop_selectedIndex_always_lt_length
    prop "prop_selectedItem" prop_selectedItem
    prop "prop_selectedItem_mapSelectedItem" prop_selectedItem_mapSelectedItem