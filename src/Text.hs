{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text
    ( String'
    , singleLineString
    , string
    , Text
    , row 
    , lastRowAvailable
    , flatten
    , empty 
    , fromString 
    , updateRow
    , merge 
    , split 
    , LowerBounded(..)
    ) where 

import Data.Maybe (fromMaybe, fromJust)
import Data.List.Split (splitOn)
import Data.Foldable (find)

newtype String' = String' String deriving (Show, Eq, Ord)
(+++) :: String' -> String' -> String'
(String' s1) +++ (String' s2) = String' $ s1 ++ s2

singleLineString :: String -> Maybe String' 
singleLineString s = if '\n' `elem` s then Nothing else Just $ String' s

string :: String' -> String 
string (String' s) = s

data Text ri ci
    = Empty 
    | UpdateRow ri (String' -> String') (Text ri ci)
    | Merge ri (Text ri ci)
    | Split ri ci (Text ri ci)

instance (Show ri, Show ci) => Show (Text ri ci) where
  show Empty = "Empty"
  show (UpdateRow r _ t) = "UpdateRow " ++ show r ++ " <fn> (" ++ show t ++ ")"
  show (Merge r t) = "Merge " ++ show r ++ " (" ++ show t ++ ")"
  show (Split r c t) = "Split " ++ show r ++ " " ++ show c ++ " (" ++ show t ++ ")"


class LowerBounded a where 
    lowerbound :: a 

row :: (LowerBounded ri, Ord ri, Enum ri, LowerBounded ci, Enum ci, Eq ci) => ri -> Text ri ci -> Maybe String' 
row r Empty = if r == lowerbound then Just (String' "") else Nothing
row r1 (UpdateRow r2 f t)
    | r1 == r2 = f <$> row r2 t
    | otherwise = row r1 t
row r1 (Merge r2 t)
    | r1 < r2 = row r1 t 
    | r1 == r2 = do 
        row1 <- row r1 t 
        let row2 = fromMaybe (String' "") $ row (succ r2) t
        return $ row1 +++ row2
    | otherwise = row (succ r1) t 
row r1 (Split r2 c t) 
    | r1 < r2 = row r1 t 
    | r1 == r2 = take' c <$> row r2 t 
    | r1 == succ r2 = drop' c <$> row r2 t 
    | otherwise = row (pred r1) t 

take' :: (LowerBounded c, Enum c, Eq c) => c -> String' -> String' 
take' n (String' s) = String' $ take'' n s

drop' :: (LowerBounded c, Enum c, Eq c) => c -> String' -> String' 
drop' n (String' s) = String' $ drop'' n s

take'' :: (LowerBounded c, Enum c, Eq c) => c -> String -> String
take'' n s 
    | n == lowerbound || s == "" = ""
    | otherwise = head s : take'' (pred n) (drop 1 s)

drop'' :: (LowerBounded c, Enum c, Eq c) => c -> String -> String 
drop'' n s 
    | n == lowerbound || s == "" = s 
    | otherwise = drop'' (pred n) (drop 1 s)

lastRowAvailable :: (LowerBounded ri, Enum ri, Ord ri, LowerBounded ci, Enum ci, Eq ci) => Text ri ci -> ri 
lastRowAvailable t = pred $ fromJust $ find (\i -> row i t == Nothing) [lowerbound..]

flatten :: (LowerBounded ri, Enum ri, Ord ri, LowerBounded ci, Enum ci, Eq ci) => Text ri ci -> String 
flatten t = init $ unlines $ map (string . fromJust . (flip row) t) [lowerbound..(lastRowAvailable t)]

empty :: Text ri ci 
empty = Empty 

updateRow :: ri -> (String' -> String') -> Text ri ci -> Text ri ci 
updateRow = UpdateRow

merge :: ri -> Text ri ci -> Text ri ci
merge = Merge

split :: ri -> ci -> Text ri ci -> Text ri ci
split = Split


fromString :: (LowerBounded ri, Enum ri, LowerBounded ci) => String -> Text ri ci 
fromString s = foldr ($) expanded [updateRow i (const l) | (i, l) <- zip [lowerbound..] singleStringLines]
    where singleStringLines = map String' $ splitOn "\n" s
          expanded = foldr ($) empty $ replicate (length singleStringLines - 1) (split lowerbound lowerbound)