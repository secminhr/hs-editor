{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Lib
import Viewport
import Editor
import Lens.Micro.Platform (makeLenses, (%=), (.=), use, Lens', lens)
import Brick.Widgets.Core (str, reportExtent, (<=>), vLimit, fill, (<+>), hLimit)
import qualified Brick.Widgets.Core as B
import Brick.Types (Widget(..), BrickEvent, EventM, BrickEvent(VtyEvent), getContext, availWidth, availHeight, ViewportType(Horizontal), Extent(Extent))
import qualified Brick.Types as BT
import Brick.Main (customMain, neverShowCursor, showFirstCursor, resizeOrQuit, App(App), lookupExtent, continueWithoutRedraw, halt)
import Brick.AttrMap (forceAttrMap, AttrMap)
import Graphics.Vty (outputIface, Modifier (..), withBackColor, withForeColor, yellow)
import qualified Graphics.Vty.CrossPlatform as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Attributes (defAttr, withStyle, reverseVideo)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Graphics.Vty.Output (displayBounds)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceM, trace, traceShowId, traceShowWith, traceShow)
import Editor (newEditor, editedString, editedText)
import Brick (Location(Location), AttrName, attrName, attrMap, withAttr, getVtyHandle, withBorderStyle)
import Editing (Editing)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Text (Text, lastRowAvailable)
import Numeric.Natural (Natural)
import ItemSelector (ItemSelector, newItemSelector, selectedItem, mapSelectedItem, getItems, setItems, select, selectedIndex)
import qualified Data.List.NonEmpty as NE

data TabState = TabState 
    { _editor :: Editor
    , _viewport :: Viewport
    , _size :: Size
    , _text :: [Maybe String]
    , _cursorPos :: CursorPos
    , _tabType :: TabType
    , _maxRowNo :: Natural
    }

data TabType 
    = File String 
    | TmpBuffer

data AppState = AppState
    { _itemSelector :: ItemSelector TabState 
    }

makeLenses ''AppState
makeLenses ''TabState

currentTab :: Lens' AppState TabState
currentTab = lens 
    (selectedItem . _itemSelector) 
    (\appState modT -> AppState $ mapSelectedItem (const modT) $ _itemSelector appState)

data Name 
    = MainEditor
    | MainEditorCursor
    deriving (Eq, Ord, Show)

reverseAttr :: AttrName
reverseAttr = attrName "reverseText"

lineNoAttr :: AttrName 
lineNoAttr = attrName "lineNo"

theMap :: AttrMap
theMap = attrMap defAttr 
    [ (reverseAttr, defAttr `withStyle` reverseVideo)
    , (lineNoAttr, defAttr `withForeColor` yellow)
    ]

maxRowNoWidth :: Natural -> Int 
maxRowNoWidth maxRowNo = max 3 $ length (show maxRowNo)  

editorSize :: Int -> Int -> Natural -> Size
editorSize termW termH maxRowNo = 
    Size (fromIntegral termW - fromIntegral (maxRowNoWidth maxRowNo) - 1) (fromIntegral termH - 3) 

main :: IO ()
main = do 
    filename <- getLine
    content <- readFile filename
    
    initialVty <- V.mkVty defaultConfig
    (termW, termH) <- displayBounds (outputIface initialVty)

    let editor@(Editor t _) = newEditor content
    let maxRowNo = 1 + lastRowAvailable t
    let initTab = TabState editor new (editorSize termW termH maxRowNo) (map Just $ lines content) (CursorPos 0 0) (File filename) maxRowNo
    let state = AppState (newItemSelector (NE.singleton initTab))

    let app = App drawUI showFirstCursor handleEvent (return ()) (const $ theMap)

    final <- customMain initialVty (return initialVty) Nothing app state
    return ()

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent (EvKey KUp [])) = (currentTab.editor %= upKey) >> updateStates
handleEvent (VtyEvent (EvKey KDown [])) = (currentTab.editor %= downKey) >> updateStates
handleEvent (VtyEvent (EvKey KRight [])) = (currentTab.editor %= rightKey) >> updateStates
handleEvent (VtyEvent (EvKey KLeft [])) = (currentTab.editor %= leftKey) >> updateStates
handleEvent (VtyEvent (EvKey KEnter [])) = (currentTab.editor %= enterKey) >> updateStates
handleEvent (VtyEvent (EvKey (KChar c) [])) = (currentTab.editor %= visibleInput c) >> updateStates
handleEvent (VtyEvent (EvKey KBS [])) = (currentTab.editor %= backspaceKey) >> updateStates
handleEvent (VtyEvent (EvKey (KChar 's') [MCtrl])) = do 
    e <- use (currentTab.editor)
    tabType <- use (currentTab.tabType)
    case tabType of 
        File filename -> liftIO $ writeFile filename $ editedString e
        _ -> halt

handleEvent (VtyEvent (EvKey (KChar 'n') [MCtrl])) = do 
    selector <- use itemSelector
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ displayBounds (outputIface vty)
    let content = " "
    let newItems = NE.append (getItems selector) $ 
                    NE.singleton $ TabState (newEditor content) new (editorSize termW termH 1) (map Just $ lines content) (CursorPos 0 0) TmpBuffer 1
    itemSelector %= select (length newItems - 1) . setItems newItems

handleEvent (VtyEvent (EvKey KBackTab [])) = itemSelector %= (\selector -> select (fromIntegral $ 1 + selectedIndex selector) selector)
handleEvent _ = halt

updateStates :: EventM Name AppState ()
updateStates = do 
    editorState <- use (currentTab.editor)
    vpState <- use (currentTab.viewport)
    sizeState <- use (currentTab.size)
    let (vp', t', cursorPos') = frame editorState vpState sizeState 
    currentTab.viewport .= vp' 
    currentTab.text .= t' 
    currentTab.cursorPos .= cursorPos'
    let newMaxRowNo = 1 + lastRowAvailable (editedText editorState)
    currentTab.maxRowNo .= newMaxRowNo
    
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ displayBounds (outputIface vty)
    currentTab.size .= editorSize termW termH newMaxRowNo

drawUI :: AppState -> [Widget Name]
drawUI appState = let s = selectedItem (_itemSelector appState) in [ 
    drawTabs appState <=>
    (drawLineNo s <+> drawSplitter s <+> drawEditor s)
    ]

drawTabs :: AppState -> Widget Name 
drawTabs (AppState selector) = 
    (foldr1 (<+>) $ NE.map (uncurry drawTab) $ 
        NE.zip (getItems selector) (NE.map (== selectedIndex selector) $ NE.fromList [0..]))
    <+> (hBorder <=> str " " <=> hBorder)

drawTab :: TabState -> Bool -> Widget Name 
drawTab ts selected = (if selected then drawSelectedTab else drawUnselectedTab) (tabLabel ts)
    where tabLabel ts = case _tabType ts of 
                            File filename -> filename 
                            TmpBuffer -> "Untitled"

drawSelectedTab :: String -> Widget Name
drawSelectedTab tabLabel = 
    let tabName = " " ++ tabLabel ++ " " in border $ str tabName 

drawUnselectedTab :: String -> Widget Name 
drawUnselectedTab tabLabel = 
    let tabName = " " ++ tabLabel ++ " "
        strWidget = str tabName
        tabNameLength = length tabName in 
        hLimit tabNameLength $ (hBorder <=> strWidget <=> hBorder) 


drawLineNo :: TabState -> Widget Name 
drawLineNo (TabState (Editor t _) vp (Size _ h) _ _ _ maxRowNo) = 
    let rowNoWidth = maxRowNoWidth maxRowNo
        minDisplayRowNo = 1 + startingRow vp
        maxDisplayRowNo = min (fromIntegral maxRowNo) $ minDisplayRowNo + fromIntegral h in 
    withAttr lineNoAttr $ str $ unlines $ map (\no -> replicate (rowNoWidth - length no) ' ' ++ no) $ map show [minDisplayRowNo..maxDisplayRowNo]

drawSplitter :: TabState -> Widget Name
drawSplitter s = 
    str $ unlines $ replicate (fromIntegral (h (_size s))) " "

drawEditor :: TabState -> Widget Name
drawEditor (TabState _ vp _ text (CursorPos row col) _ _) = reportExtent MainEditor $ B.showCursor MainEditorCursor (Location (fromIntegral col, fromIntegral row)) $ Widget BT.Greedy BT.Greedy $ do 
    render $ str $ unlines $ map (filter (/= '\n') . or "") text
    where or s ms = case ms of 
            Just string -> string 
            Nothing -> s
    