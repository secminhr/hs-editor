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
import Text (Text, LowerBounded, lowerbound, lastRowAvailable, string, split, updateRow, singleLineString, merge, String')
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

data Editing = Edit ((Text Natural Natural, Position) -> (Text Natural Natural, Position))

instance {-# INCOHERENT #-} Show Editing where 
    show _ = "<fn>"

instance LowerBounded Natural where 
    lowerbound = 0

infix 4 .-
(.-) :: Natural -> Natural -> Natural
n1 .- n2 
    | n1 < n2 = 0 
    | otherwise = n1 - n2


makeValidCol :: Natural -> String' -> Natural
makeValidCol col s' = let s = string s' in min col $ fromIntegral $ length s

makeValid :: Text Natural Natural -> Position -> Position 
makeValid t (Position row col) = let onRow = min (lastRowAvailable t) row in 
        Position onRow $ makeValidCol col (fromJust $ T.row onRow t)

edit :: Editing -> Text Natural Natural -> (Text Natural Natural, Position)
edit (Edit f) t = f (t, Position 0 0)

insertString :: String -> Editing -> Editing 
insertString s e = foldr ($) e (map insert $ reverse s)

empty :: Editing 
empty = Edit $ id


mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

lift :: ((Text Natural Natural, Position) -> (Text Natural Natural, Position)) -> Editing -> Editing 
lift f (Edit g) = Edit $ f . g

cursor :: CursorMovement -> Editing -> Editing
cursor CUp = lift (\(text, p) ->  (text, makeValid text p { row = row p .- 1 }))
cursor CDown = lift (\(text, p) -> (text, makeValid text p { row = row p + 1 })) 
cursor CLeft = lift (\(text, p) -> (text, makeValid text p { col = col p .- 1 }))
cursor CRight = lift (\(text, p) -> (text, makeValid text p { col = col p + 1}))

insert :: Char -> Editing -> Editing 
insert '\n' = lift (\(text, p) -> (split (row p) (col p) text, Position (row p + 1) 0))
insert c = lift (\(text, p) -> (updateRow (row p) (insertion (col p) c) text, p { col = col p + 1 }))
    where 
        insertion :: Natural -> Char -> String' -> String'
        insertion col c s' = let s = string s' in 
            fromJust $ singleLineString $ take (fromIntegral col) s ++ [c] ++ drop (fromIntegral col) s


resetCursor :: Editing -> Editing 
resetCursor = lift $ mapSnd (const $ Position 0 0)

backspace :: Editing -> Editing
backspace = lift (\(text, pos) -> 
    case pos of 
        Position 0 0 -> (text, pos)
        Position row 0 -> 
            (merge (row - 1) text, Position (row - 1) (fromJust $ rowLengthMaybe (row - 1) text))
        Position row col -> (updateRow row (\content -> fromJust $ singleLineString $ back (string content)) text, Position row (col - 1))
            where back s = take (fromIntegral col - 1) s ++ drop (fromIntegral col) s 
    )


rowLengthMaybe :: Natural -> Text Natural Natural -> Maybe Natural 
rowLengthMaybe r t = do 
    rowContent <- T.row r t
    return $ fromIntegral $ length $ string $ rowContent