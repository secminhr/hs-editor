module Editor 
    ( CursorPos(..)
    , Size(..)
    , Editor
    , newEditor
    , frame 
    , upKey
    , downKey
    , leftKey
    , rightKey
    , visibleInput
    , enterKey
    , backspaceKey
    ) where 

import Editing (Editing, cursor, CursorMovement (..), Position, edit, insert, backspace)
import qualified Editing as E
import Viewport (Viewport, startingRow, startingCol, viewAt, scrollH, scrollV)
import Numeric.Natural (Natural)
import Text (Text, string, fromString)
import qualified Text as T

data CursorPos = CursorPos
    { row :: Natural
    , col :: Natural }
    deriving (Show)

data Size = Size 
    { w :: Natural
    , h :: Natural }
    deriving (Show)

data Editor = Editor (Text Natural Natural) Editing
newEditor :: String -> Editor 
newEditor s = Editor (fromString s) E.empty

-- throws underflow if the minus result < 0
infix 4 ~- 
(~-) :: Natural -> Int -> Natural
n ~- x = fromIntegral $ (fromIntegral n - x)

infix 4 ~< 
(~<) :: Natural -> Int -> Bool 
n ~< x = fromIntegral n < x

infix 4 ~>=
(~>=) :: Natural -> Int -> Bool 
n ~>= x = fromIntegral n >= x

makeVisibleV :: Natural -> Natural -> Viewport -> Viewport
makeVisibleV absRow h vp
    | absRow ~< startingRow vp = viewAt (fromIntegral absRow) (startingCol vp)
    | absRow ~>= startingRow vp + (fromIntegral h) = scrollV (fromIntegral $ 1 + absRow ~- (startingRow vp + (fromIntegral h))) vp 
    | otherwise = vp

makeVisibleH :: Natural -> Natural -> Viewport -> Viewport 
makeVisibleH absCol w vp 
    | absCol ~< startingCol vp = viewAt (startingRow vp) (fromIntegral absCol)
    | absCol ~>= startingCol vp + (fromIntegral w) = scrollH (fromIntegral $ 1 + absCol ~- (startingCol vp + (fromIntegral w))) vp 
    | otherwise = vp

makeVisible :: Position -> Size -> Viewport -> Viewport
makeVisible absCursor size vp = 
    makeVisibleV (E.row absCursor) (h size) $ makeVisibleH (E.col absCursor) (w size) vp


frame :: Editor -> Viewport -> Size -> (Viewport, [Maybe String], CursorPos)
frame (Editor t e) vp size = 
    let (t', absCursor) = edit e t 
        vp' = makeVisible absCursor size vp in 
    ( vp'
    , map ((take (fromIntegral $ w size) . drop (startingCol vp'). string) <$>) $ map (\r -> T.row (fromIntegral r) t') [startingRow vp' ..(startingRow vp' - 1 + fromIntegral (h size))]
    , CursorPos (E.row absCursor ~- startingRow vp') (E.col absCursor ~- startingCol vp'))

upKey :: Editor -> Editor 
upKey (Editor t e) = Editor t (cursor CUp e)

downKey :: Editor -> Editor 
downKey (Editor t e) = Editor t (cursor CDown e)

leftKey :: Editor -> Editor 
leftKey (Editor t e) = Editor t (cursor CLeft e)

rightKey :: Editor -> Editor 
rightKey (Editor t e) = Editor t (cursor CRight e)

visibleInput :: Char -> Editor -> Editor
visibleInput c (Editor t e) = Editor t (insert c e)

enterKey :: Editor -> Editor 
enterKey (Editor t e) = Editor t (insert '\n' e)

backspaceKey :: Editor -> Editor 
backspaceKey (Editor t e) = Editor t (backspace e)