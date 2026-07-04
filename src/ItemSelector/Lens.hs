module ItemSelector.Lens where

import ItemSelector
import Lens.Micro.Platform (Lens', lens)
import qualified Data.List.NonEmpty as NE

current :: Lens' (ItemSelector a) a 
current = lens selectedItem (\selector modified -> mapSelectedItem (const modified) selector)

list :: Lens' (ItemSelector a) (NE.NonEmpty a)
list = lens getItems (flip setItems)