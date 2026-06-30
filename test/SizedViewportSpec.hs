module SizedViewportSpec where 

import SizedViewport
import Test.QuickCheck 
import Test.QuickCheck as QC
import Test.QuickCheck.Instances.Natural
import Test.Hspec
import Test.Hspec.QuickCheck
import AbsCursorPos (Position(Position))
import Integer.Natural (Natural, strictlyIncrease)
import Integer (convert, Positive)
import qualified Integer as I
import Data.Maybe (fromJust)

instance Arbitrary I.Positive where 
    arbitrary = (fromInteger . getPositive) <$> (arbitrary :: Gen (QC.Positive Integer))

instance Arbitrary Size where 
    arbitrary = Size <$> arbitrary <*> arbitrary

instance Arbitrary Position where 
    arbitrary = Position <$> arbitrary <*> arbitrary

instance Arbitrary Padding where 
    arbitrary = Padding <$> arbitrary <*> arbitrary

instance Arbitrary SizedViewport where 
    arbitrary = sized $ \n -> 
        if n <= 1 then new <$> arbitrary <*> arbitrary 
        else frequency 
            [ (1, new <$> arbitrary <*> arbitrary )
            , (5, setSize <$> arbitrary <*> resize (n-1) arbitrary)
            , (5, setIdealPadding <$> arbitrary <*> resize (n-1) arbitrary)
            , (5, makeVisible <$> arbitrary <*> resize (n-1) arbitrary) ]

------- size
prop_size_new :: Size -> Padding -> Property
prop_size_new s p = (size $ new s p) === s 

prop_size_setSize :: Size -> SizedViewport -> Property
prop_size_setSize s vp = (size $ setSize s $ vp) === s 

prop_size_setIdealPadding :: Padding -> SizedViewport -> Property
prop_size_setIdealPadding p vp = (size $ setIdealPadding p $ vp) === size vp 

prop_size_makeVisible :: Position -> SizedViewport -> Property 
prop_size_makeVisible p vp = (size $ makeVisible p $ vp) === size vp 

implies :: Bool -> Bool -> Bool 
implies a b = not a || b

-------- padding
-------- let the size of a direction be L, padding x on that direction should always have 2x + 1 <= L
validPadding :: Size -> Padding -> Padding -> Property 
validPadding (Size w h) (Padding idealV idealH) (Padding adjustV adjustH) = 
    property (adjustV <= idealV) .&&.
    property (adjustH <= idealH) .&&.
    property (2*adjustV + 1 <= convert h) .&&.
    property (2*adjustH + 1 <= convert w) .&&.
    -- min adjustment is it's necessary
    property ((adjustV < idealV) `implies` (2*(adjustV + 1) + 1 > convert h)) .&&.  
    property ((adjustH < idealH) `implies` (2*(adjustH + 1) + 1 > convert w))

prop_padding_new :: Size -> Padding -> Property 
prop_padding_new s p = validPadding s p (padding $ new s p)

prop_padding_setSize :: Size -> SizedViewport -> Property 
prop_padding_setSize s vp = let vp' = setSize s vp in 
    validPadding (size vp') (padding vp) (padding vp')

prop_padding_setIdealPadding :: Padding -> SizedViewport -> Property
prop_padding_setIdealPadding idealPadding vp = let vp' = setIdealPadding idealPadding vp in 
    validPadding (size vp) idealPadding (padding vp')

prop_padding_makeVisible :: Position -> SizedViewport -> Property 
prop_padding_makeVisible p vp = 
    padding vp === (padding $ makeVisible p vp)

------------ startingRow 
------------ When adjusting position, we always try to do minimal movement and have padding to cursor (as possible)
isVisible :: Position -> SizedViewport -> Property
isVisible (Position row col) vp = 
    property (row >= startingRow vp) .&&. 
    property (row < strictlyIncrease (h (size vp)) (startingRow vp)) .&&. 
    property (col >= startingCol vp) .&&.
    property (col < strictlyIncrease (w (size vp)) (startingCol vp))

prop_startingRow_new :: Size -> Padding -> Property 
prop_startingRow_new s p = (startingRow $ new s p) === 0 

prop_startingRow_setSize :: Size -> SizedViewport -> Property 
prop_startingRow_setSize s vp = (startingRow $ setSize s vp) === startingRow vp 

prop_startingRow_setIdealPadding :: Padding -> SizedViewport -> Property
prop_startingRow_setIdealPadding p vp = (startingRow $ setIdealPadding p vp) === startingRow vp 

prop_startingRow_makeVisible_inside_padded :: Natural -> SizedViewport -> Property
prop_startingRow_makeVisible_inside_padded col vp = 
    let paddedRowStart = startingRow vp + vertical (padding vp)
        paddedRowEnd = strictlyIncrease (h (size vp)) (startingRow vp) - vertical (padding vp) - 1 in 
        forAll (chooseEnum (paddedRowStart, paddedRowEnd)) $ \r -> 
            let vp' = makeVisible (Position r col) vp in 
            isVisible (Position r col) vp' .&&.
            startingRow vp' === startingRow vp 

prop_startingRow_makeVisible_above_padded :: Position -> SizedViewport -> Property 
prop_startingRow_makeVisible_above_padded p@(Position row col) vp = 
    row < startingRow vp + vertical (padding vp) ==> 
        let vp' = makeVisible p vp 
            paddingSpace = row - startingRow vp' in 
            isVisible p vp' .&&.
            property (paddingSpace == vertical (padding vp) || 
                        (paddingSpace < vertical (padding vp) && startingRow vp' == 0))

prop_startingRow_makeVisible_below_padded :: Position -> SizedViewport -> Property 
prop_startingRow_makeVisible_below_padded p@(Position row col) vp = 
    row >= strictlyIncrease (h (size vp)) (startingRow vp) - vertical (padding vp) ==> 
        let vp' = makeVisible p vp in isVisible p vp' .&&. 
        let paddingSpace = strictlyIncrease (h (size vp)) (startingRow vp') - row - 1 in 
            property (paddingSpace == vertical (padding vp))

---------------- startingCol
prop_startingCol_new :: Size -> Padding -> Property 
prop_startingCol_new s p = (startingCol $ new s p) === 0 

prop_startingCol_setSize :: Size -> SizedViewport -> Property 
prop_startingCol_setSize s vp = (startingCol $ setSize s vp) === startingCol vp 

prop_startingCol_setIdealPadding :: Padding -> SizedViewport -> Property 
prop_startingCol_setIdealPadding p vp = (startingCol $ setIdealPadding p vp) === startingCol vp 

prop_startingCol_makeVisible_inside_padded :: Natural -> SizedViewport -> Property 
prop_startingCol_makeVisible_inside_padded row vp = 
    let paddedColStart = startingCol vp + horizontal (padding vp) 
        paddedColEnd = strictlyIncrease (w (size vp)) (startingCol vp) - horizontal (padding vp) - 1 in 
            forAll (chooseEnum (paddedColStart, paddedColEnd)) $ \c -> 
                let vp' = makeVisible (Position row c) vp in 
                    isVisible (Position row c) vp' .&&.
                    startingCol vp === startingCol vp' 

prop_startingCol_makeVisible_before_padded :: Position -> SizedViewport -> Property 
prop_startingCol_makeVisible_before_padded p@(Position row col) vp = 
    col < startingCol vp + horizontal (padding vp) ==> 
        let vp' = makeVisible p vp 
            paddingSpace = col - startingCol vp' in 
                isVisible p vp' .&&. 
                property (paddingSpace == horizontal (padding vp) || 
                            (paddingSpace < horizontal (padding vp) && startingCol vp' == 0))

prop_startingCol_makeVisible_after_padded :: Position -> SizedViewport -> Property 
prop_startingCol_makeVisible_after_padded p@(Position row col) vp = 
    col >= strictlyIncrease (w (size vp)) (startingCol vp) - horizontal (padding vp) ==>
        let vp' = makeVisible p vp in isVisible p vp' .&&. 
        let paddingSpace = strictlyIncrease (w (size vp)) (startingCol vp') - col - 1 in  
            property (paddingSpace == horizontal (padding vp))

spec :: Spec 
spec = do 
    prop "prop_size_new" prop_size_new
    prop "prop_size_setSize" prop_size_setSize 
    prop "prop_size_setIdealPadding" prop_size_setIdealPadding
    prop "prop_size_makeVisible" prop_size_makeVisible
    prop "prop_padding_new" prop_padding_new
    prop "prop_padding_setSize" prop_padding_setSize
    prop "prop_padding_setIdealPadding" prop_padding_setIdealPadding
    prop "prop_padding_makeVisible" prop_padding_makeVisible
    prop "prop_startingRow_new" prop_startingRow_new
    prop "prop_startingRow_setSize" prop_startingRow_setSize
    prop "prop_startingRow_setIdealPadding" prop_startingRow_setIdealPadding
    prop "prop_startingRow_makeVisible_inside_padded" prop_startingRow_makeVisible_inside_padded
    prop "prop_startingRow_makeVisible_above_padded" prop_startingRow_makeVisible_above_padded
    prop "prop_startingRow_makeVisible_below_padded" prop_startingRow_makeVisible_below_padded
    prop "prop_startingCol_new" prop_startingCol_new
    prop "prop_startingCol_setSize" prop_startingCol_setSize
    prop "prop_startingCol_setIdealPadding" prop_startingCol_setIdealPadding
    prop "prop_startingCol_makeVisible_inside_padded" prop_startingCol_makeVisible_inside_padded
    prop "prop_startingCol_makeVisible_before_padded" prop_startingCol_makeVisible_before_padded
    prop "prop_startingCol_makeVisible_after_padded" prop_startingCol_makeVisible_after_padded