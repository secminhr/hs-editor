module CursorMovement
    ( makeCursorMovement
    , applyCursorMovement
    , CursorMovement    
    , CursorMovementDsl(..)
    , Cursor 
    ) where 

import ViewPort

type Cursor = Position
data CursorMovement = CursorMovement 
    { cursorUp :: Cursor -> Cursor
    , cursorRight :: Cursor -> Cursor
    , cursorDown :: Cursor -> Cursor
    , cursorLeft :: Cursor -> Cursor
    }

data CursorMovementDsl = CursorUp | CursorRight | CursorDown | CursorLeft deriving (Show)

dslToMovement :: CursorMovementDsl -> CursorMovement -> Cursor -> Cursor
dslToMovement CursorUp = cursorUp
dslToMovement CursorRight = cursorRight
dslToMovement CursorDown = cursorDown
dslToMovement CursorLeft = cursorLeft

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

wrapped :: Int -> Int -> Int 
wrapped upper = max 1 . min upper

wrapCursor :: Int -> [Int] -> Cursor -> Cursor
wrapCursor maxLineNo maxColNoPerLine (lineNo, colNo) = 
    let wrappedLine = wrapped maxLineNo lineNo 
        wrappedCol = wrapped (maxColNoPerLine !! (wrappedLine - 1)) colNo in 
            (wrappedLine, wrappedCol)
    
makeCursorMovement :: Int -> [Int] -> CursorMovement
makeCursorMovement maxLineNo maxColNoPerLine = CursorMovement
    (wrapCursor maxLineNo maxColNoPerLine . mapFst (-1 + ))
    (wrapCursor maxLineNo maxColNoPerLine . mapSnd (+ 1))
    (wrapCursor maxLineNo maxColNoPerLine . mapFst (+ 1))
    (wrapCursor maxLineNo maxColNoPerLine . mapSnd (-1 + ))

applyCursorMovement :: CursorMovement -> [CursorMovementDsl] -> Cursor -> Cursor
applyCursorMovement m l c = applyCursorMovement' (map (\cmd -> dslToMovement cmd m) l) c

applyCursorMovement' :: [Cursor -> Cursor] -> Cursor -> Cursor
applyCursorMovement' [] c = c 
applyCursorMovement' (m:ms) c = applyCursorMovement' ms $ m c
