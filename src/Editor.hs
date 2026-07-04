module Editor 
    ( CursorPos(..)
    , Size(..)
    , Editor(..)
    , newEditor
    , frame 
    , editedString
    , editedText
    , viewport
    , setViewport
    , upKey
    , downKey
    , leftKey
    , rightKey
    , visibleInput
    , enterKey
    , backspaceKey
    ) where 

import Editing (Editing, cursor, CursorMovement (..), edit, insert, backspace)
import qualified Editing as E
import qualified AbsCursorPos as ACP
import Text (Text, string, fromString, flatten)
import qualified Text as T
import SizedViewport (SizedViewport (size), makeVisible, Size(..), startingRow, startingCol)
import Integer.Natural (strictlyIncrease, Natural)
import Data.Maybe (fromJust)
import AbsCursorPos (Position(Position))
import Integer (Positive, IntegerConvert (convert), narrow)
import Data.List (uncons)

data CursorPos = CursorPos
    { row :: Natural
    , col :: Natural }
    deriving (Show)

data Editor = Editor (Text Natural Natural) Editing SizedViewport (String -> Natural)
newEditor :: String -> SizedViewport -> (String -> Natural) -> Editor 
newEditor s = Editor (fromString s) E.empty 

-- convert the real insertion point to the absolute position of rendering (considering text width)
renderingAbsCursor :: String -> (String -> Natural) -> Position -> Position
renderingAbsCursor line f p =
    let beforeCursorString = take (fromIntegral (ACP.col p)) line in
        Position (ACP.row p) (convert $ f beforeCursorString)

renderingCursorLength :: String -> (String -> Natural) -> Position -> Positive 
renderingCursorLength line f p = 
    let beforeCursorDropped = drop (fromIntegral (ACP.col p)) line in
        case uncons beforeCursorDropped of 
            Nothing -> 1
            Just ('\n', _) -> 1 
            Just (c, _) -> if f [c] > 0 then fromJust (narrow (f [c])) else 1

drop' :: (String -> Natural) -> Natural -> String -> String
drop' f n "" = ""
drop' f 0 s = s 
drop' f n (c:cs)
    | f [c] == 0 = drop' f n cs 
    | f [c] == 1 = drop' f (n-1) cs 
    | otherwise = drop' f n (replicate (fromIntegral (f [c])) '<' ++ cs)

take' :: (String -> Natural) -> Natural -> String -> String 
take' f n "" = ""
take' f 0 s = ""
take' f n (c:cs)
    | f [c] == 0 = c:take' f n cs 
    | f [c] == 1 = c:take' f (n-1) cs 
    | f [c] <= n = c:take' f (n - f [c]) cs 
    | otherwise = take' f n (replicate (fromIntegral (f [c])) '>' ++ cs)

frame :: Editor -> ([Maybe String], CursorPos)
frame (Editor t e vp f) = 
    let (t', absCursor) = edit e t 
        rowString = string $ fromJust $ T.row (ACP.row absCursor) t'
        renderingCursor = renderingAbsCursor rowString f absCursor in 
    ( map ((take' f (convert $ w (size vp)) . drop' f (startingCol vp) . string) <$>) $ map (\r -> T.row r t') [startingRow vp ..(strictlyIncrease (h (size vp)) (startingRow vp)  - 1)]
    , CursorPos (ACP.row renderingCursor - startingRow vp) (ACP.col renderingCursor - startingCol vp))

editedString :: Editor -> String 
editedString e = flatten $ editedText e

editedText :: Editor -> Text Natural Natural 
editedText (Editor t e _ _) = fst $ edit e t

viewport :: Editor -> SizedViewport
viewport (Editor _ _ vp _) = vp

setViewport :: SizedViewport -> Editor -> Editor
setViewport vp' (Editor t e _ f) = makeCursorVisible (Editor t e vp' f)

makeCursorVisible :: Editor -> Editor 
makeCursorVisible (Editor t e vp f) = 
    let (t', absCursor) = edit e t 
        rowString = string $ fromJust $ T.row (ACP.row absCursor) t' in 
    Editor t e (makeVisible (renderingAbsCursor rowString f absCursor) (renderingCursorLength rowString f absCursor) vp) f

upKey :: Editor -> Editor 
upKey (Editor t e vp f) = makeCursorVisible $ Editor t (cursor CUp e) vp f

downKey :: Editor -> Editor 
downKey (Editor t e vp f) = makeCursorVisible $ Editor t (cursor CDown e) vp f

leftKey :: Editor -> Editor 
leftKey (Editor t e vp f) = makeCursorVisible $ Editor t (cursor CLeft e) vp f

rightKey :: Editor -> Editor 
rightKey (Editor t e vp f) = makeCursorVisible $ Editor t (cursor CRight e) vp f

visibleInput :: Char -> Editor -> Editor
visibleInput c (Editor t e vp f) = makeCursorVisible $ Editor t (insert c e) vp f

enterKey :: Editor -> Editor 
enterKey (Editor t e vp f) = makeCursorVisible $ Editor t (insert '\n' e) vp f

backspaceKey :: Editor -> Editor 
backspaceKey (Editor t e vp f) = makeCursorVisible $ Editor t (backspace e) vp f

