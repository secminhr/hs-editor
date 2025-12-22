module CursorMovement
    ( makeCursorMovement
    , applyCursorMovement
    , CursorMovement    
    , CursorMovementDsl(..)
    , Cursor 
    , rowNo
    , colNo
    ) where 

import ViewPort

type Cursor = Position

rowNo :: Cursor -> Int
rowNo = fst 

colNo :: Cursor -> Int
colNo = snd

data CursorMovement = CursorMovement 
    { cursorUp :: Cursor -> Cursor
    , cursorRight :: Cursor -> Cursor
    , cursorDown :: Cursor -> Cursor
    , cursorLeft :: Cursor -> Cursor
    }

data CursorMovementDsl = CursorUp | CursorRight | CursorDown | CursorLeft deriving (Show)

dslToMovement :: CursorMovement -> CursorMovementDsl -> Cursor -> Cursor
dslToMovement m CursorUp = cursorUp m
dslToMovement m CursorRight = cursorRight m
dslToMovement m CursorDown = cursorDown m
dslToMovement m CursorLeft = cursorLeft m

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

wrapped :: Int -> Int -> Int 
wrapped upper = max 1 . min upper

wrapCursor :: Int -> (Int -> Int) -> Cursor -> Cursor
wrapCursor maxLineNo getMaxColNo (lineNo, colNo) = 
    let wrappedLine = wrapped maxLineNo lineNo 
        wrappedCol = wrapped (getMaxColNo (wrappedLine - 1)) colNo in 
            (wrappedLine, wrappedCol)
    
makeCursorMovement :: Int -> (Int -> Int) -> CursorMovement
makeCursorMovement maxLineNo getMaxColNo = CursorMovement
    (wrapCursor maxLineNo getMaxColNo . mapFst (-1 + ))
    (wrapCursor maxLineNo getMaxColNo . mapSnd (+ 1))
    (wrapCursor maxLineNo getMaxColNo . mapFst (+ 1))
    (wrapCursor maxLineNo getMaxColNo . mapSnd (-1 + ))

applyCursorMovement :: CursorMovement -> [CursorMovementDsl] -> Cursor -> Cursor
applyCursorMovement m l c = applyCursorMovement' (map (dslToMovement m) l) c

applyCursorMovement' :: [Cursor -> Cursor] -> Cursor -> Cursor
applyCursorMovement' = foldr (.) id
