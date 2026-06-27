{-# LANGUAGE FlexibleInstances #-}

module EditingSpec where 

import Editing
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Numeric.Natural (Natural)
import qualified Text as T
import Text (lastRowAvailable, string, updateRow, singleLineString, split)
import Data.Maybe (fromJust)
import qualified TextEq as TE
import Data.List (intercalate)

---- saturating minus on natural
infix 4 .-
(.-) :: Natural -> Natural -> Natural
n1 .- n2 
    | n1 < n2 = 0 
    | otherwise = n1 - n2

instance Arbitrary CursorMovement where 
    arbitrary = elements [CUp, CDown, CLeft, CRight]

instance Arbitrary Editing where
  arbitrary = sized $ \n ->
    if n <= 1
      then pure empty
      else frequency
        -- 1. 終止條件
        [ (1, pure empty)
        
        -- 2. 游標移動 (利用公開介面 cursor)
        , (4, cursor <$> arbitrary <*> resize (n - 1) arbitrary)
        
        -- 3. 單字元輸入 (使用自訂的 genChar 確保換行符號的出現率)
        , (5, insert <$> genChar <*> resize (n - 1) arbitrary)
        
        -- 4. 重置游標與刪除
        , (2, resetCursor <$> resize (n - 1) arbitrary)
        , (4, backspace <$> resize (n - 1) arbitrary)
        
        -- 5. 【關鍵】利用 insertString 一口氣建立具代表性的多行基底狀態
        , (3, do
            numLines <- choose (1, 4)
            linesList <- vectorOf numLines (listOf (elements "abcde"))
            let str = intercalate "\n" linesList
            e <- resize (n - 1) arbitrary
            pure $ insertString str e
          )
        ]
    where
      -- 自訂的字元生成器，確保 QuickCheck 能夠經常探索多行狀態
      genChar = frequency
        [ (10, elements "abcde")
        , (2, pure '\n')  -- 強制給予 \n 較高的權重
        ]

instance Arbitrary (T.Text Natural Natural) where
  arbitrary = do
    -- 隨機決定行數 (例如 1 到 5 行)
    numLines <- choose (1, 5)
    
    -- 每行隨機產生字串 (這裡使用 "abcde" 確保字串中不含額外的 \n)
    linesList <- vectorOf numLines (listOf (elements "abcde "))
    
    -- 用 \n 將多行組合起來，然後一口氣餵給 fromString
    pure $ T.fromString (intercalate "\n" linesList)


--------- edit
prop_edit_empty :: T.Text Natural Natural -> Property 
prop_edit_empty t = 
    let (editedText, pos) = edit empty t in 
        editedText TE.==== t .&&. pos === Position 0 0

prop_edit_cursorUp :: Editing -> T.Text Natural Natural -> Property
prop_edit_cursorUp e t = 
    let (resultText, pos) = edit e t 
        (editedText, newPos) = edit (cursor CUp e) t in 
            editedText TE.==== resultText .&&. newPos === pos { row = row pos .- 1 }

prop_edit_cursorDown :: Editing -> T.Text Natural Natural -> Property 
prop_edit_cursorDown e t = 
    let (resultText, pos) = edit e t 
        (editedText, newPos) = edit (cursor CDown e) t in 
            editedText TE.==== resultText .&&. newPos === pos { row = min (lastRowAvailable resultText) (row pos + 1) }

prop_edit_cursorLeft :: Editing -> T.Text Natural Natural -> Property 
prop_edit_cursorLeft e t = 
    let (resultText, pos) = edit e t 
        (editedText, newPos) = edit (cursor CLeft e) t in 
            editedText TE.==== resultText .&&. newPos === pos { col = col pos .- 1 }

prop_edit_cursorRight :: Editing -> T.Text Natural Natural -> Property
prop_edit_cursorRight e t = 
    let (resultText, pos) = edit e t
        rowLength = fromIntegral $ length $ string $ fromJust $ T.row (row pos) resultText
        (editedText, newPos) = edit (cursor CRight e) t in
            editedText TE.==== resultText .&&. newPos === pos { col = min rowLength (col pos + 1)}

prop_edit_insert :: Editing -> Char -> T.Text Natural Natural -> Property 
prop_edit_insert e c t = 
    c /= '\n' ==> let (oldText, oldPos) = edit e t 
                      (text, pos) = edit (insert c e) t in 
                        (text TE.==== updateRow (row oldPos) (insertion (fromIntegral $ col oldPos)) oldText) .&&.
                        (pos === oldPos { col = col oldPos + 1})
    where insertion col s' = let s = string s' in 
            fromJust $ singleLineString $ take col s ++ [c] ++ drop col s

prop_edit_insert_newline :: Editing -> T.Text Natural Natural -> Property 
prop_edit_insert_newline e t = 
    let (oldText, oldPos) = edit e t 
        (text, pos) = edit (insert '\n' e) t in 
            (text TE.==== split (row oldPos) (col oldPos) oldText) .&&.
            (pos === Position (row oldPos + 1) 0)

prop_edit_resetCursor :: Editing -> T.Text Natural Natural -> Property 
prop_edit_resetCursor e t = 
    let (resultText, pos) = edit e t 
        (editedText, newPos) = edit (resetCursor e) t in 
            editedText TE.==== resultText .&&. newPos === Position 0 0


type ForallTextProperty = T.Text Natural Natural -> Property
(====) :: Editing -> Editing -> ForallTextProperty
e1 ==== e2 = \t -> 
    let (t1, p1) = edit e1 t
        (t2, p2) = edit e2 t in t1 TE.==== t2 .&&. p1 === p2


-------------- non-observation
prop_backspace_insert_id :: Editing -> Char -> ForallTextProperty
prop_backspace_insert_id e c = 
    (backspace . insert c) e ==== e 

prop_resetCursor_backspace :: Editing -> ForallTextProperty
prop_resetCursor_backspace e = 
    (backspace . resetCursor) e ==== resetCursor e 

prop_resetCursor_empty :: ForallTextProperty
prop_resetCursor_empty = 
    resetCursor empty ==== empty

prop_resetCursor_cursor :: Editing -> CursorMovement -> ForallTextProperty 
prop_resetCursor_cursor e m = 
    (resetCursor . cursor m) e ==== resetCursor e 

prop_insert_insert_insertString :: Editing -> Char -> Char -> ForallTextProperty 
prop_insert_insert_insertString e c1 c2 = 
    (insert c1 . insert c2) e ==== insertString [c2, c1] e 

prop_insertString_fold_insert :: Editing -> String -> ForallTextProperty
prop_insertString_fold_insert e s = 
    insertString s e ==== (foldr ($) e $ map insert $ reverse s)

spec :: Spec 
spec = do 
    prop "prop_edit_empty" prop_edit_empty
    prop "prop_edit_cursorUp" prop_edit_cursorUp
    prop "prop_edit_cursorDown" prop_edit_cursorDown
    prop "prop_edit_cursorLeft" prop_edit_cursorLeft
    prop "prop_edit_cursorRight" prop_edit_cursorRight
    prop "prop_edit_insert" prop_edit_insert
    prop "prop_edit_insert_newline" prop_edit_insert_newline
    prop "prop_edit_resetCursor" prop_edit_resetCursor
    prop "prop_backspace_insert_id" prop_backspace_insert_id
    prop "prop_resetCursor_backspace" prop_resetCursor_backspace
    prop "prop_resetCursor_empty" prop_resetCursor_empty
    prop "prop_resetCursor_cursor" prop_resetCursor_cursor
    prop "prop_insert_insert_insertString" prop_insert_insert_insertString
    prop "prop_insertString_fold_insert" prop_insertString_fold_insert