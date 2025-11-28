module CursorMovementSpec where 

import Test.Hspec
import Test.Hspec.QuickCheck

import CursorMovement
import Test.QuickCheck

instance Arbitrary CursorMovementDsl where 
    arbitrary = elements [CursorUp, CursorRight, CursorDown, CursorLeft]

spec :: Spec
spec = do
    prop "Cursor movements should never exceeds the bounds" $ 
        \totalLineNo movements -> 
            forAll (vectorOf (getPositive totalLineNo) arbitrary) $ \totalColPerLinePositive ->
            let totalColPerLine = map getPositive totalColPerLinePositive 
                movement = makeCursorMovement (getPositive totalLineNo) (totalColPerLine !!)
                (finalLineNo, finalColNo) = applyCursorMovement movement movements (1, 1) in 
                    1 <= finalLineNo && finalLineNo <= getPositive totalLineNo && 
                    1 <= finalColNo && finalColNo <= (totalColPerLine !! (finalLineNo - 1))
    describe "For a Cursor Movement with 3 lines and each line has 3 characters, the cursor starting from (2, 2)" $ do 
        let movement = makeCursorMovement 3 (const 3)
        let cursor = (2, 2)
        it "should move to (1, 2) after CursorUp" $ do 
            applyCursorMovement movement [CursorUp] cursor `shouldBe` (1, 2)
        it "should move to (2, 3) after CursorRight" $ do 
            applyCursorMovement movement [CursorRight] cursor `shouldBe` (2, 3)
        it "should move to (3, 2) after CursorDown" $ do 
            applyCursorMovement movement [CursorDown] cursor `shouldBe` (3, 2)
        it "should move to (2, 1) after CursorLeft" $ do 
            applyCursorMovement movement [CursorLeft] cursor `shouldBe` (2, 1)