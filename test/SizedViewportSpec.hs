module SizedViewportSpec where 

import SizedViewport
import Test.QuickCheck 
import Test.QuickCheck as QC
import Test.QuickCheck.Instances.Natural
import Test.Hspec
import Test.Hspec.QuickCheck
import AbsCursorPos (Position(Position))
import Integer.Natural (Natural, strictlyIncrease)
import qualified Integer.Natural as IN
import Integer.Positive (subtractOne)
import Integer.Signed (Signed(NotMinus))
import Integer (convert, Positive, absoluteDifference)
import qualified Integer as I
import Data.Maybe (fromJust)
import Control.Exception (assert)

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
            , (5, do 
                    vp <- resize (n-1) arbitrary
                    let l = arbitrary `suchThat` (<= w (size vp))
                    makeVisible <$> arbitrary <*> l <*> (pure vp)
                ) ]

---------- Utilities for makeVisible specification
isVisibleB :: Position -> I.Positive -> SizedViewport -> Bool
isVisibleB (Position row col) length vp = assert (length <= w (size vp)) $ 
    row >= startingRow vp && 
    row < strictlyIncrease (h (size vp)) (startingRow vp) &&
    col >= startingCol vp &&
    strictlyIncrease length col <= strictlyIncrease (w (size vp)) (startingCol vp)

isVisible :: Position -> I.Positive -> SizedViewport -> Property 
isVisible p l vp = property $ isVisibleB p l vp

inPaddingArea :: SizedViewport -> Position -> Bool 
inPaddingArea vp p@(Position row col) = assert (isVisibleB p 1 vp) $ 
    row < startingRow vp + vertical (padding vp) || 
    row >= strictlyIncrease (h (size vp)) (startingRow vp) - vertical (padding vp) || 
    col < startingCol vp + horizontal (padding vp) || 
    col >= strictlyIncrease (w (size vp)) (startingCol vp) - horizontal (padding vp)

remainingPadding :: Position -> I.Positive -> SizedViewport -> Natural 
remainingPadding p@(Position row col) length vp = assert (isVisibleB p length vp) $ 
    let allPositionsInVP = [Position r c | r <- [startingRow vp .. (startingRow vp + subtractOne (h (size vp)))], c <- [startingCol vp .. (startingCol vp + subtractOne (w (size vp)))]]
        indicatedPositions = [Position row c | c <- [col .. (col + subtractOne length)]] in
            IN.length (filter (inPaddingArea vp) allPositionsInVP) - IN.length (filter (inPaddingArea vp) indicatedPositions)

(~-) :: Natural -> Natural -> Natural 
n1 ~- n2 = case n1 `I.subtract` n2 of 
                NotMinus n -> n 
                _ -> 0

variantVpsThatSeeThePosition :: Position -> I.Positive -> SizedViewport -> [SizedViewport]
variantVpsThatSeeThePosition p@(Position row col) length originalVp = assert (isVisibleB p length originalVp) $ 
    let vps = [setStartingRow r (setStartingCol c originalVp)
                | r <- [row ~- subtractOne (h (size originalVp)) .. row]
                , c <- [(col + convert length) ~- convert (w (size originalVp)) .. col]] in 
                    -- sanity check
                    assert (all (isVisibleB p length) vps) $ 
                    assert (originalVp `elem` vps) $
                    vps

movementCount :: SizedViewport -> SizedViewport -> Natural 
movementCount vp1 vp2 = 
    absoluteDifference (startingRow vp1) (startingRow vp2) + 
    absoluteDifference (startingCol vp1) (startingCol vp2)


movementSpec :: Position -> I.Positive -> SizedViewport -> SizedViewport -> Property 
movementSpec p@(Position row col) length oldVp vp = 
    let vps = variantVpsThatSeeThePosition p length oldVp 
        rowValid = filter (validPositionSpec row 1 (h (size oldVp)) (vertical (padding oldVp)) . startingRow) vps 
        colValid = filter (validPositionSpec col length (w (size oldVp)) (horizontal (padding oldVp)) . startingCol) rowValid 
        --- if there's no colValid, that means it can't be valid even if it's on the very left
        --- we then accept those with col = 0 in this case
        bothValid = if colValid == [] then filter ((== 0) . startingCol) rowValid else colValid   
        minMovement = minimum $ map (movementCount oldVp) bothValid 
        positionWithMinMovement = filter (\v -> minMovement == movementCount oldVp v) bothValid in
            counterexample (show vp ++ " is not in " ++ show positionWithMinMovement) $
            property (vp `elem` positionWithMinMovement)


validPositionSpec :: Natural -> I.Positive -> I.Positive -> Natural -> Natural -> Bool 
validPositionSpec cStart cWidth vWidth padding outStart = 
    if I.increase (2*padding) cWidth <= vWidth 
    then 
        (if cStart >= padding then cStart >= outStart+padding && strictlyIncrease cWidth cStart + padding <= strictlyIncrease vWidth outStart
        else outStart == 0)
    else let allowWidthInPadding = convert cWidth - (convert vWidth - (2 * padding)) in 
        strictlyIncrease vWidth outStart >= strictlyIncrease cWidth cStart && outStart <= cStart 
        && IN.length (filter (inPadding outStart (convert vWidth) padding) [cStart .. cStart + subtractOne cWidth]) == allowWidthInPadding
    
    where inPadding outStart vWidth padding point = 
            (outStart <= point && point < outStart + padding) || 
            (outStart + vWidth - padding <= point && point < outStart + vWidth)

------- size
prop_size_new :: Size -> Padding -> Property
prop_size_new s p = (size $ new s p) === s 

prop_size_setSize :: Size -> SizedViewport -> Property
prop_size_setSize s vp = (size $ setSize s $ vp) === s 

prop_size_setIdealPadding :: Padding -> SizedViewport -> Property
prop_size_setIdealPadding p vp = (size $ setIdealPadding p $ vp) === size vp 

prop_size_setStartingRow :: Natural -> SizedViewport -> Property 
prop_size_setStartingRow r vp = (size $ setStartingRow r $ vp) === size vp 

prop_size_setStartingCol :: Natural -> SizedViewport -> Property
prop_size_setStartingCol c vp = (size $ setStartingCol c $ vp) === size vp

prop_size_makeVisible :: Position -> I.Positive -> SizedViewport -> Property 
prop_size_makeVisible p l vp = (size $ makeVisible p l $ vp) === size vp 

-------- padding
-------- let the size of a direction be L, padding x on that direction should always have 2x + 1 <= L
implies :: Bool -> Bool -> Bool 
implies a b = not a || b

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

prop_padding_setStartingRow :: Natural -> SizedViewport -> Property
prop_padding_setStartingRow r vp = (padding $ setStartingRow r $ vp) === padding vp 

prop_padding_setStartingCol :: Natural -> SizedViewport -> Property 
prop_padding_setStartingCol c vp = (padding $ setStartingCol c $ vp) === padding vp

prop_padding_makeVisible :: Position -> I.Positive -> SizedViewport -> Property 
prop_padding_makeVisible p l vp = 
    padding vp === (padding $ makeVisible p l vp)

------------ startingRow 
------------ When adjusting position, we always try to do minimal movement and have padding to cursor (as possible)
prop_startingRow_new :: Size -> Padding -> Property 
prop_startingRow_new s p = (startingRow $ new s p) === 0 

prop_startingRow_setSize :: Size -> SizedViewport -> Property 
prop_startingRow_setSize s vp = (startingRow $ setSize s vp) === startingRow vp 

prop_startingRow_setIdealPadding :: Padding -> SizedViewport -> Property
prop_startingRow_setIdealPadding p vp = (startingRow $ setIdealPadding p vp) === startingRow vp 

prop_startingRow_setStartingRow :: Natural -> SizedViewport -> Property 
prop_startingRow_setStartingRow r vp = (startingRow $ setStartingRow r $ vp) === r 

prop_startingRow_setStartingCol :: Natural -> SizedViewport -> Property 
prop_startingRow_setStartingCol c vp = (startingRow $ setStartingCol c $ vp) === startingRow vp

---------------- startingCol
prop_startingCol_new :: Size -> Padding -> Property 
prop_startingCol_new s p = (startingCol $ new s p) === 0 

prop_startingCol_setSize :: Size -> SizedViewport -> Property 
prop_startingCol_setSize s vp = (startingCol $ setSize s vp) === startingCol vp 

prop_startingCol_setIdealPadding :: Padding -> SizedViewport -> Property 
prop_startingCol_setIdealPadding p vp = (startingCol $ setIdealPadding p vp) === startingCol vp 

prop_startingCol_setStartingRow :: Natural -> SizedViewport -> Property 
prop_startingCol_setStartingRow r vp = (startingCol $ setStartingRow r vp) === startingCol vp 

prop_startingCol_setStartingCol :: Natural -> SizedViewport -> Property 
prop_startingCol_setStartingCol c vp = (startingCol $ setStartingCol c vp) === c


------ makeVisible
prop_satisfy_movementSpec :: Position -> SizedViewport -> Property
prop_satisfy_movementSpec p vp = forAll (arbitrary `suchThat` (<= convert (w (size vp)))) $ \l ->
    movementSpec p l vp $ makeVisible p l vp


spec :: Spec 
spec = do 
    prop "prop_size_new" prop_size_new
    prop "prop_size_setSize" prop_size_setSize 
    prop "prop_size_setIdealPadding" prop_size_setIdealPadding
    prop "prop_size_setStartingRow" prop_size_setStartingRow
    prop "prop_size_setStartingCol" prop_size_setStartingCol
    prop "prop_size_makeVisible" prop_size_makeVisible
    prop "prop_padding_new" prop_padding_new
    prop "prop_padding_setSize" prop_padding_setSize
    prop "prop_padding_setIdealPadding" prop_padding_setIdealPadding
    prop "prop_padding_setStartingRow" prop_padding_setStartingRow
    prop "prop_padding_setStartingCol" prop_padding_setStartingCol
    prop "prop_padding_makeVisible" prop_padding_makeVisible
    prop "prop_startingRow_new" prop_startingRow_new
    prop "prop_startingRow_setSize" prop_startingRow_setSize
    prop "prop_startingRow_setIdealPadding" prop_startingRow_setIdealPadding
    prop "prop_startingRow_setStartingRow" prop_startingRow_setStartingRow
    prop "prop_startingRow_setStartingCol" prop_startingRow_setStartingCol
    prop "prop_startingCol_new" prop_startingCol_new
    prop "prop_startingCol_setSize" prop_startingCol_setSize
    prop "prop_startingCol_setIdealPadding" prop_startingCol_setIdealPadding
    prop "prop_startingCol_setStartingRow" prop_startingCol_setStartingRow
    prop "prop_startingCol_setStartingCol" prop_startingCol_setStartingCol
    prop "prop_satisfy_movementSpec" prop_satisfy_movementSpec