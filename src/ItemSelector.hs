module ItemSelector 
    ( ItemSelector
    , newItemSelector
    , select
    , setItems
    , getItems
    , selectedItem
    , selectedIndex
    , mapSelectedItem
    ) where 
import Numeric.Natural (Natural)
import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.List.NonEmpty as NE

data ItemSelector a = ItemSelector 
    { index :: Natural
    , list :: NonEmpty a }
    deriving (Show)

newItemSelector :: NonEmpty a -> ItemSelector a 
newItemSelector = ItemSelector 0

select :: Int -> ItemSelector a -> ItemSelector a
select pos s@(ItemSelector _ oldList) = s { 
    index = let l = length oldList in fromIntegral $ (l + pos `rem` l) `rem` l 
    }

setItems :: NonEmpty a -> ItemSelector a -> ItemSelector a
setItems l (ItemSelector oldIndex _) = ItemSelector (oldIndex `rem` (fromIntegral $ length l)) l

getItems :: ItemSelector a -> NonEmpty a 
getItems = list

selectedItem :: ItemSelector a -> a
selectedItem s = getItems s NE.!! (fromIntegral $ selectedIndex s)

selectedIndex :: ItemSelector a -> Natural
selectedIndex = index

mapSelectedItem :: (a -> a) -> ItemSelector a -> ItemSelector a
mapSelectedItem f s = 
    let newValue = f (selectedItem s) 
        l = list s in 
    setItems (fromList $ NE.take (fromIntegral $ selectedIndex s) l ++ [newValue] ++ NE.drop (fromIntegral $ selectedIndex s + 1) l) s
