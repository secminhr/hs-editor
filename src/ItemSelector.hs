module ItemSelector 
    ( ItemSelector
    , newItemSelector
    , select
    , setItems
    , getItems
    , selectedItem
    , selectedIndex
    ) where 
import Numeric.Natural (Natural)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

data ItemSelector a
    = NewItemSelector (NonEmpty a)
    | Select Int (ItemSelector a)
    | SetItems (NonEmpty a) (ItemSelector a)
    deriving (Show)

newItemSelector :: NonEmpty a -> ItemSelector a 
newItemSelector = NewItemSelector

select :: Int -> ItemSelector a -> ItemSelector a
select = Select

setItems :: NonEmpty a -> ItemSelector a -> ItemSelector a
setItems = SetItems

getItems :: ItemSelector a -> NonEmpty a 
getItems (NewItemSelector l) = l 
getItems (Select _ s) = getItems s 
getItems (SetItems l _) = l

selectedItem :: ItemSelector a -> a
selectedItem s = getItems s NE.!! (fromIntegral $ selectedIndex s)

selectedIndex :: ItemSelector a -> Natural
selectedIndex (NewItemSelector _) = 0
selectedIndex (Select pos list) = let l = length (getItems list) in fromIntegral $ (l + pos `rem` l) `rem` l 
selectedIndex (SetItems l s) = let oldIndex = selectedIndex s in oldIndex `rem` (fromIntegral $ length l)