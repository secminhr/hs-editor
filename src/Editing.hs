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

data Position = Position 
    { row :: Natural 
    , col :: Natural }
    deriving (Eq, Show)

data CursorMovement = CUp | CDown | CLeft | CRight deriving (Show)
data Editing 
    = Empty 
    | Cursor CursorMovement Editing 
    | Insert Char Editing 
    | Backspace Editing 
    | ResetCursor Editing 
    deriving (Show)

type Action = (Text Natural Natural, Position) -> (Text Natural Natural, Position)

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
edit e t = edit' e id (t, Position 0 0)

edit' :: Editing -> Action -> Action 
edit' Empty f = f
edit' (Cursor CUp e) f = edit' e $ f . (\(text, p) ->  (text, makeValid text p { row = row p .- 1 }))
edit' (Cursor CDown e) f = edit' e $ f . (\(text, p) -> (text, makeValid text p { row = row p + 1 }))
edit' (Cursor CLeft e) f = edit' e $ f . (\(text, p) -> (text, makeValid text p { col = col p .- 1 }))
edit' (Cursor CRight e) f = edit' e $ f . (\(text, p) -> (text, makeValid text p { col = col p + 1}))
edit' (Insert '\n' e) f = edit' e $ f . (\(text, p) -> (split (row p) (col p) text, Position (row p + 1) 0))
edit' (Insert c e) f = edit' e $ f . (\(text, p) -> (updateRow (row p) (insertion (col p) c) text, p { col = col p + 1 }))
    where 
        insertion :: Natural -> Char -> String' -> String'
        insertion col c s' = let s = string s' in 
            fromJust $ singleLineString $ take (fromIntegral col) s ++ [c] ++ drop (fromIntegral col) s 
edit' (Backspace e) f = edit' e $ f . (\(text, pos) -> 
    case pos of 
        Position 0 0 -> (text, pos)
        Position row 0 -> 
            (merge (row - 1) text, Position (row - 1) (fromJust $ rowLengthMaybe (row - 1) text))
        Position row col -> (updateRow row (\content -> fromJust $ singleLineString $ back (string content)) text, Position row (col - 1))
            where back s = take (fromIntegral col - 1) s ++ drop (fromIntegral col) s 
    )
edit' (ResetCursor e) f = edit' e $ f . mapSnd (const $ Position 0 0)

insertString :: String -> Editing -> Editing 
insertString s e = foldr ($) e (map insert $ reverse s)

empty :: Editing 
empty = Empty

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

cursor :: CursorMovement -> Editing -> Editing
cursor = Cursor

insert :: Char -> Editing -> Editing 
insert = Insert 

resetCursor :: Editing -> Editing 
resetCursor = ResetCursor

backspace :: Editing -> Editing
backspace = Backspace


rowLengthMaybe :: Natural -> Text Natural Natural -> Maybe Natural 
rowLengthMaybe r t = do 
    rowContent <- T.row r t
    return $ fromIntegral $ length $ string $ rowContent