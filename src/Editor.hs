module Editor 
    ( CursorPos(..)
    , Size(..)
    , Editor(..)
    , newEditor
    , frame 
    , editedString
    , editedText
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

data Editor = Editor (Text Natural Natural) Editing
newEditor :: String -> Editor 
newEditor s = Editor (fromString s) E.empty

frame :: Editor -> SizedViewport -> (SizedViewport, [Maybe String], CursorPos)
frame (Editor t e) vp = 
    let (t', absCursor) = edit e t 
        vp' = makeVisible absCursor vp in 
    ( vp'
    , map ((take (fromIntegral $ w (size vp)) . drop (fromIntegral (startingCol vp')) . string) <$>) $ map (\r -> T.row r t') [startingRow vp' ..(strictlyIncrease (h (size vp)) (startingRow vp')  - 1)]
    , CursorPos (ACP.row absCursor - startingRow vp') (ACP.col absCursor - startingCol vp'))

editedString :: Editor -> String 
editedString e = flatten $ editedText e

editedText :: Editor -> Text Natural Natural 
editedText (Editor t e) = fst $ edit e t

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

