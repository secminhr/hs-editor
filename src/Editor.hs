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

data CursorPos = CursorPos
    { row :: Natural
    , col :: Natural }
    deriving (Show)

data Editor = Editor (Text Natural Natural) Editing SizedViewport
newEditor :: String -> SizedViewport -> Editor 
newEditor s vp = Editor (fromString s) E.empty vp

frame :: Editor -> ([Maybe String], CursorPos)
frame (Editor t e vp) = 
    let (t', absCursor) = edit e t in 
    ( map ((take (fromIntegral $ w (size vp)) . drop (fromIntegral (startingCol vp)) . string) <$>) $ map (\r -> T.row r t') [startingRow vp ..(strictlyIncrease (h (size vp)) (startingRow vp)  - 1)]
    , CursorPos (ACP.row absCursor - startingRow vp) (ACP.col absCursor - startingCol vp))

editedString :: Editor -> String 
editedString e = flatten $ editedText e

editedText :: Editor -> Text Natural Natural 
editedText (Editor t e _) = fst $ edit e t

viewport :: Editor -> SizedViewport
viewport (Editor _ _ vp) = vp

setViewport :: SizedViewport -> Editor -> Editor
setViewport vp' (Editor t e _) = makeCursorVisible (Editor t e vp')

makeCursorVisible :: Editor -> Editor 
makeCursorVisible (Editor t e vp) = let (_, absCursor) = edit e t in 
    Editor t e (makeVisible absCursor vp)

upKey :: Editor -> Editor 
upKey (Editor t e vp) = makeCursorVisible $ Editor t (cursor CUp e) vp

downKey :: Editor -> Editor 
downKey (Editor t e vp) = makeCursorVisible $ Editor t (cursor CDown e) vp

leftKey :: Editor -> Editor 
leftKey (Editor t e vp) = makeCursorVisible $ Editor t (cursor CLeft e) vp 

rightKey :: Editor -> Editor 
rightKey (Editor t e vp) = makeCursorVisible $ Editor t (cursor CRight e) vp

visibleInput :: Char -> Editor -> Editor
visibleInput c (Editor t e vp) = makeCursorVisible $ Editor t (insert c e) vp

enterKey :: Editor -> Editor 
enterKey (Editor t e vp) = makeCursorVisible $ Editor t (insert '\n' e) vp

backspaceKey :: Editor -> Editor 
backspaceKey (Editor t e vp) = makeCursorVisible $ Editor t (backspace e) vp

