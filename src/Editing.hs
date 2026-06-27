module Editing
    ( Position(..)
    , CursorMovement(..)
    , Editing
    , edit
    , empty
    , cursor 
    , insert
    , resetCursor
    , backspace
    , insertString
    ) where

import Numeric.Natural (Natural)
import Text (Text, LowerBounded, lowerbound, lastRowAvailable, string, split, updateRow, singleLineString, merge)
import qualified Text as T
import Data.Maybe (fromJust)

{-
    2D editing with a cursor
    Position = (row :: Natural, col :: Natural)

    observation:
        -- This observation indicates that we don't give oprational meaning 
        --     until the edit really happens on a piece of texts.
        -- 
        -- We could put the text into the constructor, so we can track the state
        --     of the cursor and the text all along, but it seems to be a complication to me.
        -- 
        -- Because we'll have to handle the movement out of the boundary no matter what, 
        --     the interaction between cursor oprations might not have simple properties 
        --         (e.g. cursorUp . cursorDown = id isn't valid, you need something like cursorDown editing \= editing => cursorUp . cursorDown = id), 
        --     so why not leave them to the observation part?
        -- Therefore the Editing is only a description of actions, 
        --     and the result may vary depending on the edit request.

        edit :: Editing -> NonEmpty [String] -> (rt :: [String], rp :: Position)

    constructor:
        empty :: Editing
        cursorUp :: Editing -> Editing
        cusorDown :: Editing -> Editing
        cursorRight :: Editing -> Editing
        cursorLeft :: Editing -> Editing

        insert :: Editing -> String -> Editing
        backspaceDelete :: Editing -> Natural -> Editing

    equations:
        edit/empty: edit empty = (, (0, 0))

        edit/insert:
        forall (editing :: Editing) (text :: NonEmpty [String]) (insertion :: String).
        edit (insert editing insertion)
        edit editing text

        edit/cursorUp-movable:
        forall (editing :: Editing) (text :: NonEmpty [String]).
            (row $ rp $ edit editing text) > 0 =>
                edit (cursorUp editing) text = mapFst (-1) $ edit editing text
        
        edit/cursorUp-unmovable:
        forall (editing :: Editing) (text :: NonEmpty [String]).
            (row $ rp $ edit editing text) = 0 =>
                edit (cursorUp editing) text p = edit editing text p
         
        edit/cursorDown-movable:
        forall (editing :: Editing) (text :: NonEmpty [String]).
            (row $ rp $ edit editing text) < (length text - 1) =>
                edit (cursorUp editing) text = mapFst (+1) $ edit editing text

        edit/cursorDown-unmovable:
        forall (editing :: Editing) (text :: NonEmpty [String]).
            (row $ rp $ edit editing text) >= (length text - 1) =>
                edit (cursorUp editing) text = edit editing text 

        edit/cursorRight-movable:
        ...

        edit/cursorRight-unmovable:
        ...

        edit/cursorLeft-movable:
        ...

        edit/cursorLeft-unmovable:
        ...

        -- The inverse operation (e.g. up/down, left/right) and be proved
        -- Example: edit/cursorDown/cursorUp-id:
        -- forall (editing :: Editing) (text :: NonEmpty [String]).
        --  (row $ rp $ edit editing text) > 0 =>
        --      edit (cursorDown (cursorUp editing)) text = edit editing text
        -- Prove:
        --     edit (cursorDown (cursorUp editing)) text 
        --   = mapFst (+1) $ edit (cursorUp editing) text
        --   = mapFst (+1) $ mapFst (-1) $ edit editing text
        --   = edit editing text



        edit:  -- should be provable
        forall (editing :: Editing) (text :: NonEmpty [String]).
            0 <= (x $ rp $ edit editing text) < length (edit editing text) &&
            (
                forall (r :: Natural). 
                0 <= r < length (edit editing text) =>
                    0 <= (y $ rp $ edit editing text) <= lenth (edit editing text !! r)
            )
-}

data Position = Position 
    { row :: Natural 
    , col :: Natural }
    deriving (Eq, Show)

data CursorMovement = CUp | CDown | CLeft | CRight deriving (Show)

data Editing 
    = Empty
    | CursorUp Editing 
    | CursorDown Editing 
    | CursorLeft Editing 
    | CursorRight Editing
    | Insert Char Editing
    | ResetCursor Editing
    | Backspace Editing
    deriving (Show)

instance LowerBounded Natural where 
    lowerbound = 0

infix 4 .-
(.-) :: Natural -> Natural -> Natural
n1 .- n2 
    | n1 < n2 = 0 
    | otherwise = n1 - n2

edit :: Editing -> Text Natural Natural -> (Text Natural Natural, Position)
edit Empty t = (t, Position 0 0)
edit (CursorUp e) t = let (text, pos) = edit e t in (text, pos { row = row pos .- 1 })
edit (CursorDown e) t = let (text, pos) = edit e t in (text, pos { row = min (lastRowAvailable text) (row pos + 1) })
edit (CursorLeft e) t = let (text, pos) = edit e t in (text, pos { col = col pos .- 1 })
edit (CursorRight e) t = 
    let (text, pos) = edit e t 
        rowLength = fromIntegral $ length $ string $ fromJust $ T.row (row pos) text in 
            (text, pos { col = min rowLength (col pos + 1)})
edit (Insert '\n' e) t = let (text, pos) = edit e t in (split (row pos) (col pos) text, Position (row pos + 1) 0)
edit (Insert c e) t = 
    let (text, pos) = edit e t in 
            (updateRow (row pos) (\content -> fromJust $ singleLineString $ insertion (col pos) (string content)) text, pos { col = col pos + 1 })
    where insertion col s = take (fromIntegral col) s ++ [c] ++ drop (fromIntegral col) s
edit (ResetCursor e) t = let (text, pos) = edit e t in (text, Position 0 0)
edit (Backspace e) t = let (text, pos) = edit e t in 
    case pos of 
        Position 0 0 -> (text, pos)
        Position row 0 -> let rowLengthBeforeMerge = fromIntegral $ length $ string $ fromJust $ T.row (row - 1) text in
            (merge (row - 1) text, Position (row - 1) rowLengthBeforeMerge)
        Position row col -> (updateRow row (\content -> fromJust $ singleLineString $ back (string content)) text, Position row (col - 1))
            where back s = take (fromIntegral col - 1) s ++ drop (fromIntegral col) s 


insertString :: String -> Editing -> Editing 
insertString s e = foldr ($) e (map insert $ reverse s)

empty :: Editing 
empty = Empty

cursor :: CursorMovement -> Editing -> Editing
cursor CUp = CursorUp
cursor CDown = CursorDown
cursor CLeft = CursorLeft
cursor CRight = CursorRight

insert :: Char -> Editing -> Editing 
insert = Insert

resetCursor :: Editing -> Editing 
resetCursor = ResetCursor

backspace :: Editing -> Editing
backspace = Backspace