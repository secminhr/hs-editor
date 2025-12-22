module Editor where 

import CursorMovement
import ViewPort

data Editor = Editor 
    { cursor :: Cursor 
    , viewPortTopLeft :: Position
    , viewPort :: ViewPort
    , cursorMovement :: CursorMovement
    }

openEditor :: Int -> Int -> Buffer -> Editor 
openEditor width height buffer = 
    Editor (1, 1) (1, 1)
            (openViewPort width height buffer)  
            (makeCursorMovement (length buffer) ((map length buffer) !!))

editorView :: Editor -> String 
editorView e = getContent (viewPortTopLeft e) (viewPort e)

editorCursorUp :: Editor -> Editor 
editorCursorUp (Editor cursor topLeft vp mv) = 
    let newCursor = applyCursorMovement mv [CursorUp] cursor
        newTopLeft = (min (rowNo newCursor) (rowNo topLeft), colNo topLeft) in
            Editor newCursor newTopLeft vp mv

editorCursorRight :: Editor -> Editor 
editorCursorRight (Editor cursor topLeft vp mv) = 
    let newCursor = applyCursorMovement mv [CursorRight] cursor
        newTopLeft = (rowNo topLeft, max (colNo newCursor) (colNo topLeft)) in 
            Editor newCursor newTopLeft vp mv

editorCursorDown :: Editor -> Editor 
editorCursorDown (Editor cursor topLeft vp mv) = 
    let newCursor = applyCursorMovement mv [CursorDown] cursor 
        newTopLeft = (max (rowNo newCursor) (rowNo topLeft), colNo topLeft) in 
            Editor newCursor newTopLeft vp mv 

editorCursorLeft :: Editor -> Editor 
editorCursorLeft (Editor cursor topLeft vp mv) = 
    let newCursor = applyCursorMovement mv [CursorLeft] cursor 
        newTopLeft = (rowNo topLeft, min (colNo newCursor) (colNo topLeft)) in 
            Editor newCursor newTopLeft vp mv


-- applyCursorMovement :: [Cursor -> Cursor] -> Editor -> Editor
-- applyCursorMovement [] e = e 
-- applyCursorMovement (m:ms) e = applyCursorMovement ms $ e { cursor = (m.cursor) e }