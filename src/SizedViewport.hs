module SizedViewport
    ( SizedViewport
    , Size(..) 
    , Padding(..)
    , new
    , setSize
    , setIdealPadding
    , makeVisible
    , padding
    , size
    , startingRow
    , startingCol) where 

import Integer.Positive (Positive)
import Integer.Natural (Natural, addOne)
import qualified Integer.Natural as IN
import AbsCursorPos (Position(..))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Integer (IntegerConvert(convert), IntegerNarrow (narrow), StrictlyIncrease (strictlyIncrease))
import Integer.Signed (Signed(Plus))

data SizedViewport = SizedViewport 
    { size :: Size 
    , padding :: Padding
    , startingRow :: Natural 
    , startingCol :: Natural }
    deriving (Show)

data Size = Size 
    { w :: Positive 
    , h :: Positive }
    deriving (Eq, Show)

data Padding = Padding
    { vertical :: Natural
    , horizontal :: Natural }
    deriving (Eq, Show)

adjustPadding :: Size -> Padding -> Padding 
adjustPadding (Size w h) (Padding vertical horizontal) = Padding (adjust h vertical) (adjust w horizontal)
    where
        adjust :: Positive -> Natural -> Natural 
        adjust total p 
            | addOne (2 * p) <= total = p 
            | otherwise = fromJust $ find (\x -> addOne (2*x) <= total) $ reverse [0..p]

new :: Size -> Padding -> SizedViewport
new size idealPadding = SizedViewport size (adjustPadding size idealPadding) 0 0

setSize :: Size -> SizedViewport -> SizedViewport
setSize size vp = vp { size = size, padding = adjustPadding size (padding vp) }

setIdealPadding :: Padding -> SizedViewport -> SizedViewport
setIdealPadding idealPadding vp = vp { padding = adjustPadding (size vp) idealPadding }


makeVisible :: Position -> SizedViewport -> SizedViewport
makeVisible (Position row col) vp = vp 
    { startingRow = adjustStarting row (startingRow vp) (h (size vp)) (vertical (padding vp))
    , startingCol = adjustStarting col (startingCol vp) (w (size vp)) (horizontal (padding vp)) }

data Area 
    = BeforePadding Positive 
    | Within 
    | AfterPadding Positive

locateAt :: Natural -> Natural -> Positive -> Natural -> Area 
locateAt absPos currentPos total padding = case (currentPos + padding) `IN.subtract` absPos of 
    Plus offset -> BeforePadding offset
    _ -> if absPos < strictlyIncrease total currentPos - padding then Within 
                else AfterPadding $ addOne $ absPos - (strictlyIncrease total currentPos - padding)

adjustStarting :: Natural -> Natural -> Positive -> Natural -> Natural 
adjustStarting absPos currentPos total padding = case locateAt absPos currentPos total padding of 
    BeforePadding offset -> fromIntegral $ max 0 $ IN.subtract currentPos (convert offset)
    Within -> currentPos 
    AfterPadding offset -> strictlyIncrease offset currentPos
