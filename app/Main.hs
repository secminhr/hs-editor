{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Lib
import SizedViewport
import Editor
import Lens.Micro.Platform (makeLenses, (%=), (.=), use, Lens', lens)
import Brick.Widgets.Core (str, reportExtent, (<=>), vLimit, fill, (<+>), hLimit)
import qualified Brick.Widgets.Core as B
import Brick.Types (Widget(..), BrickEvent, EventM, BrickEvent(VtyEvent), getContext, availWidth, availHeight, ViewportType(Horizontal), Extent(Extent))
import qualified Brick.Types as BT
import Brick.Main (customMain, neverShowCursor, showFirstCursor, resizeOrQuit, App(App), lookupExtent, continueWithoutRedraw, halt)
import Brick.AttrMap (forceAttrMap, AttrMap)
import Graphics.Vty (outputIface, Modifier (..), withBackColor, withForeColor, yellow, Vty)
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
import ItemSelector (ItemSelector, newItemSelector, selectedItem, mapSelectedItem, getItems, setItems, select, selectedIndex)
import qualified Data.List.NonEmpty as NE
import Integer (Positive, fromInt)
import Integer.Natural (Natural, addOne)
import Integer.Positive (increase)
import Data.Maybe (fromJust)

data TabState = TabState 
    { _editor :: Editor
    , _viewport :: SizedViewport
    , _text :: [Maybe String]
    , _cursorPos :: CursorPos
    , _tabType :: TabType
    }

maxRowNo :: Editor -> Positive 
maxRowNo = addOne . lastRowAvailable . editedText

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

maxRowNoWidth :: Positive -> Positive 
maxRowNoWidth maxRowNo = fromJust $ fromInt $ max 3 $ length (show maxRowNo)  

-- intended underflow, indicating a valid editor can't be created
editorSize :: Positive -> Positive -> Positive -> Size
editorSize termW termH maxRowNo = 
    Size (termW - (maxRowNoWidth maxRowNo) - 1) (termH - 3) 

vtyDisplayBounds :: Vty -> IO (Positive, Positive)
vtyDisplayBounds vty = do 
    (termW, termH) <- displayBounds (outputIface vty)
    let positiveTermW = fromJust $ fromInt termW 
    let positiveTermH = fromJust $ fromInt termH
    
    return (positiveTermW, positiveTermH)

main :: IO ()
main = do 
    filename <- getLine
    content <- readFile filename
    
    initialVty <- V.mkVty defaultConfig
    (termW, termH) <- vtyDisplayBounds initialVty

    let editor@(Editor t _) = newEditor content
    let initTab = TabState editor (new (editorSize termW termH (maxRowNo editor)) (Padding 0 0)) (map Just $ lines content) (CursorPos 0 0) (File filename)
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
    (termW, termH) <- liftIO $ vtyDisplayBounds vty


    let content = ""
    let newItems = NE.append (getItems selector) $ 
                    NE.singleton $ TabState (newEditor content) (new (editorSize termW termH 1) (Padding 0 0)) (map Just $ lines content) (CursorPos 0 0) TmpBuffer
    itemSelector %= select (length newItems - 1) . setItems newItems

handleEvent (VtyEvent (EvKey KBackTab [])) = itemSelector %= (\selector -> select (1 + fromIntegral (selectedIndex selector)) selector)
handleEvent _ = halt

updateStates :: EventM Name AppState ()
updateStates = do 
    editorState <- use (currentTab.editor)
    vpState <- use (currentTab.viewport)

    let (vp', t', cursorPos') = frame editorState vpState 
    currentTab.viewport .= vp' 
    currentTab.text .= t' 
    currentTab.cursorPos .= cursorPos'
    
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ vtyDisplayBounds vty
    currentTab.viewport %= setSize (editorSize termW termH (maxRowNo editorState)) 

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
drawLineNo (TabState e@(Editor t _) vp _ _ _) = 
    let rowNoWidth = maxRowNoWidth $ maxRowNo e
        minDisplayRowNo = addOne $ startingRow vp
        maxDisplayRowNo = min (maxRowNo e) $ minDisplayRowNo + h (size vp) in 
    withAttr lineNoAttr $ str $ unlines $ map (\no -> replicate (fromIntegral rowNoWidth - length no) ' ' ++ no) $ map show [minDisplayRowNo..maxDisplayRowNo]

drawSplitter :: TabState -> Widget Name
drawSplitter s = 
    str $ unlines $ replicate (fromIntegral (h (size (_viewport s)))) " "

drawEditor :: TabState -> Widget Name
drawEditor (TabState _ vp text (CursorPos row col) _) = reportExtent MainEditor $ B.showCursor MainEditorCursor (Location (fromIntegral col, fromIntegral row)) $ Widget BT.Greedy BT.Greedy $ do 
    render $ str $ unlines $ map (filter (/= '\n') . or "") text
    where or s ms = case ms of 
            Just string -> string 
            Nothing -> s
    