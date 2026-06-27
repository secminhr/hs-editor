module TextEq where 

import Text (row, lastRowAvailable, Text)
import Test.QuickCheck
import GHC.TypeLits (Nat)

infix 4 ====
(====) :: Text Nat Nat -> Text Nat Nat -> Property
t1 ==== t2 = 
    (forAll (chooseEnum (0, lastRowAvailable t2)) $ \r -> row r t1 === row r t2) .&&.
    lastRowAvailable t1 === lastRowAvailable t2