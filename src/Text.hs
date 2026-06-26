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

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (intercalate)

newtype String' = String' String deriving (Show, Eq, Ord)
(+++) :: String' -> String' -> String'
(String' s1) +++ (String' s2) = String' $ s1 ++ s2

singleLineString :: String -> Maybe String' 
singleLineString s = if '\n' `elem` s then Nothing else Just $ String' s

string :: String' -> String 
string (String' s) = s

data Text ri ci = Content (M.Map ri String')

instance (Show ri, Show ci) => Show (Text ri ci) where
    show (Content m) = show m
  
class LowerBounded a where 
    lowerbound :: a 

row :: (Ord ri) => ri -> Text ri ci -> Maybe String' 
row r (Content m) = M.lookup r m

lastRowAvailable :: Text ri ci -> ri 
lastRowAvailable (Content m) = fst $ M.findMax m

flatten :: Text ri ci -> String 
flatten (Content m) = intercalate "\n" $ map (string . snd) $ M.toAscList m

empty :: (LowerBounded ri) => Text ri ci 
empty = Content $ M.singleton lowerbound $ String' ""

updateRow :: (Ord ri) => ri -> (String' -> String') -> Text ri ci -> Text ri ci 
updateRow r f (Content m) = Content $ M.adjust f r m

merge :: (Ord ri, Enum ri) => ri -> Text ri ci -> Text ri ci
merge r (Content m) = Content $ 
    case M.splitLookup r m of 
        (_, Nothing, _) -> m
        (beforeR, Just rRow, afterR) -> 
            let newRow = rRow +++ M.findWithDefault (String' "") (succ r) afterR in
                -- union is left-biased, so newRow is preferred than the value in mapKeys
                M.unions [beforeR, M.singleton r newRow, M.mapKeys pred afterR]
        

split :: (Ord ri, Enum ri, LowerBounded ci, Enum ci, Eq ci) => ri -> ci -> Text ri ci -> Text ri ci
split r c (Content m) = Content $
    case M.splitLookup r m of 
        (_, Nothing, _) -> m 
        (beforeR, Just rRow, afterR) -> 
            let splitL1 = take' c rRow
                splitL2 = drop' c rRow in 
                    M.unions [beforeR, M.singleton r splitL1, M.singleton (succ r) splitL2, M.mapKeys succ afterR]

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


fromString :: (LowerBounded ri, Enum ri, Eq ri) => String -> Text ri ci 
fromString s = Content $ M.fromAscList $ zip [lowerbound..] $ map String' $ splitOn "\n" s