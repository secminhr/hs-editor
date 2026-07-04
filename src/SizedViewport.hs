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
    , startingCol
    , setStartingRow
    , setStartingCol ) where 

import Integer.Positive (Positive, subtractOne)
import Integer.Natural (Natural, addOne)
import qualified Integer.Natural as IN
import AbsCursorPos (Position(..))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Integer (IntegerConvert(convert), IntegerNarrow (narrow), StrictlyIncrease (strictlyIncrease))
import Integer.Signed (Signed(NotMinus))
import Control.Exception (assert)

data SizedViewport = SizedViewport 
    { size :: Size 
    , padding :: Padding
    , startingRow :: Natural 
    , startingCol :: Natural }
    deriving (Show, Eq)

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

setStartingRow :: Natural -> SizedViewport -> SizedViewport 
setStartingRow r vp = vp { startingRow = r }

setStartingCol :: Natural -> SizedViewport -> SizedViewport 
setStartingCol c vp = vp { startingCol = c }

makeVisible :: Position -> Positive -> SizedViewport -> SizedViewport
makeVisible (Position row col) length vp = vp 
    { startingRow = adjustStarting row 1 (startingRow vp) (h (size vp)) (vertical (padding vp))
    , startingCol = adjustStarting col length (startingCol vp) (w (size vp)) (horizontal (padding vp)) }

data Area 
    = BeforePadding Positive 
    | InFrontPadding Natural
    | InIdeal 
    | InBackPadding Natural
    | AfterPadding Positive
    deriving (Show)

locateAt :: Natural -> Natural -> Positive -> Natural -> Area 
locateAt absPos currentPos total padding
    | absPos < currentPos = BeforePadding $ fromJust $ narrow $ currentPos - absPos 
    | absPos < currentPos + padding = InFrontPadding $ absPos - currentPos
    | absPos < strictlyIncrease total currentPos - padding = InIdeal 
    | absPos < strictlyIncrease total currentPos = InBackPadding $ absPos - (strictlyIncrease total currentPos - padding)
    | otherwise = AfterPadding $ addOne $ absPos - (strictlyIncrease total currentPos)

(~-) :: Natural -> Natural -> Natural 
n1 ~- n2 = case n1 `IN.subtract` n2 of 
    (NotMinus n) -> n 
    _ -> 0

adjustStarting :: Natural -> Positive -> Natural -> Positive -> Natural -> Natural 
adjustStarting absPos length currentPos total padding
    ---  give up, the length won't fit in anyway
    | length > total = absPos         
    --- length can all fit in ideal area
    | convert length <= convert total - 2*padding = 
        case locateAt absPos currentPos total padding of 
            BeforePadding _  -> absPos ~- padding
            InFrontPadding _ -> absPos ~- padding
            -- absPos is InIdeal, InBackPadding, or AfterPadding
            -- absPos + length - 1 is thus InIdeal, InBackPadding, or AfterPadding
            _ -> case locateAt (absPos + subtractOne length) currentPos total padding of 
                InIdeal -> currentPos 
                InBackPadding _ -> (strictlyIncrease length absPos + padding) ~- convert total
                AfterPadding _ -> (strictlyIncrease length absPos + padding) ~- convert total
                _ -> assert False 0   ------ should never happen
    --- some overlapping of padding is unavoidable
    | otherwise = 
        let mustOverlap = convert length - (convert total - 2*padding) in 
            let headStatus = locateAt absPos currentPos total padding
                lastStatus = locateAt (absPos + subtractOne length) currentPos total padding in 
            case (headStatus, lastStatus) of 
                (BeforePadding n, _) -> adjustStarting absPos length (currentPos ~- convert n) total padding
                (_, AfterPadding n) -> adjustStarting absPos length (strictlyIncrease n currentPos) total padding
                (InFrontPadding n, _) -> let overlapped = min (currentPos + padding - absPos) (convert length) in 
                        if overlapped <= mustOverlap then currentPos 
                        else (strictlyIncrease length absPos + padding) ~- convert total
                (_, InBackPadding n) -> let overlapped = min (addOne n) length in 
                        if convert overlapped <= mustOverlap then currentPos 
                        else absPos - padding
                _ -> assert False $ 0
