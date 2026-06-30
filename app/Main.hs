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
import Brick (Location(Location), AttrName, attrName, attrMap, withAttr, getVtyHandle, withBorderStyle, CursorLocation, cursorLocationName, nestEventM)
import Editing (Editing)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Text (Text, lastRowAvailable)
import ItemSelector (ItemSelector, newItemSelector, selectedItem, mapSelectedItem, getItems, setItems, select, selectedIndex)
import qualified Data.List.NonEmpty as NE
import Integer (Positive, fromInt)
import Integer.Natural (Natural, addOne)
import Integer.Positive (increase, subtractOne)
import Data.Maybe (fromJust)
import StatusLine (StatusLineState, newStatusLineState, renderStatusLine, handleStatusLineEvent, _editing, message, editing)
import Data.List (find, findIndex)

data TabState = TabState 
    { _editor :: Editor
    , _text :: [Maybe String]
    , _cursorPos :: CursorPos
    , _tabType :: TabType
    }

maxRowNo :: Editor -> Positive 
maxRowNo = addOne . lastRowAvailable . editedText

data TabType 
    = File String 
    | TmpBuffer

data Name 
    = MainEditor
    | StatusLine
    deriving (Eq, Ord, Show)

selectItem :: (Eq a) => a -> ItemSelector a -> ItemSelector a 
selectItem item selector = case findIndex (== item) (NE.toList (getItems selector)) of 
                                Nothing -> selector 
                                Just i -> select i selector

data AppState = AppState
    { _itemSelector :: ItemSelector TabState 
    , _statusLineState :: StatusLineState Name
    , _focusSelector :: ItemSelector Name
    }

makeLenses ''AppState
makeLenses ''TabState

currentTab :: Lens' AppState TabState
currentTab = lens 
    (selectedItem . _itemSelector) 
    (\appState modT -> appState { _itemSelector = mapSelectedItem (const modT) $ _itemSelector appState })

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
    Size (termW - (maxRowNoWidth maxRowNo) - 1) (termH - 4) 

vtyDisplayBounds :: Vty -> IO (Positive, Positive)
vtyDisplayBounds vty = do 
    (termW, termH) <- displayBounds (outputIface vty)
    let positiveTermW = fromJust $ fromInt termW 
    let positiveTermH = fromJust $ fromInt termH
    
    return (positiveTermW, positiveTermH)

showFocused :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
showFocused (AppState _ _ fSelector) = find (\loc -> case cursorLocationName loc of 
                                            Just n -> n == selectedItem fSelector 
                                            Nothing -> False)

main :: IO ()
main = do 
    filename <- getLine
    content <- readFile filename
    
    initialVty <- V.mkVty defaultConfig
    (termW, termH) <- vtyDisplayBounds initialVty

    let editor = newEditor content (new (editorSize termW termH (maxRowNo editor)) (Padding 0 0))
    let (initT, initCursorPos) = frame editor

    let initTab = TabState editor initT initCursorPos (File filename)
    let state = AppState (newItemSelector (NE.singleton initTab)) (newStatusLineState StatusLine) (newItemSelector (MainEditor NE.:| [StatusLine]))

    let app = App drawUI showFocused handleEvent (return ()) (const $ theMap)

    final <- customMain initialVty (return initialVty) Nothing app state
    return ()

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent e = do 
    fSelector <- use focusSelector  
    case selectedItem fSelector of 
        MainEditor -> handleMainEditorEvent e 
        StatusLine -> do 
            status <- use statusLineState
            (newStatusLine, input) <- nestEventM status (handleStatusLineEvent e)
            statusLineState .= newStatusLine
            if _editing newStatusLine then return ()
            else do 
                focusSelector %= selectItem MainEditor 
                case input of 
                    Nothing -> statusLineState.message .= "Canceled"
                    Just filename -> do 
                        e <- use (currentTab.editor)
                        liftIO $ writeFile filename $ editedString e
                        currentTab.tabType .= File filename


handleMainEditorEvent :: BrickEvent Name () -> EventM Name AppState ()
handleMainEditorEvent (VtyEvent (EvKey KUp [])) = (currentTab.editor %= upKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey KDown [])) = (currentTab.editor %= downKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey KRight [])) = (currentTab.editor %= rightKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey KLeft [])) = (currentTab.editor %= leftKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey KEnter [])) = (currentTab.editor %= enterKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey (KChar c) [])) = (currentTab.editor %= visibleInput c) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey KBS [])) = (currentTab.editor %= backspaceKey) >> updateStates
handleMainEditorEvent (VtyEvent (EvKey (KChar 's') [MCtrl])) = do 
    e <- use (currentTab.editor)
    tabType <- use (currentTab.tabType)
    case tabType of 
        File filename -> liftIO $ writeFile filename $ editedString e
        _ -> do 
            focusSelector %= selectItem StatusLine
            statusLineState.message .= "Save to: "
            statusLineState.editing .= True

handleMainEditorEvent (VtyEvent (EvKey (KChar 'n') [MCtrl])) = do 
    selector <- use itemSelector
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ vtyDisplayBounds vty


    let content = ""
    let newItems = NE.append (getItems selector) $ 
                    NE.singleton $ TabState (newEditor content (new (editorSize termW termH 1) (Padding 0 0))) (map Just $ lines content) (CursorPos 0 0) TmpBuffer
    itemSelector %= select (length newItems - 1) . setItems newItems

handleMainEditorEvent (VtyEvent (EvKey KBackTab [])) = itemSelector %= (\selector -> select (1 + fromIntegral (selectedIndex selector)) selector)
handleMainEditorEvent _ = halt

updateStates :: EventM Name AppState ()
updateStates = do 
    editorState <- use (currentTab.editor)

    let (t', cursorPos') = frame editorState 
    currentTab.text .= t' 
    currentTab.cursorPos .= cursorPos'
    
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ vtyDisplayBounds vty
    let vp' = setSize (editorSize termW termH (maxRowNo editorState)) (viewport editorState) 
    currentTab.editor %= setViewport vp'

drawUI :: AppState -> [Widget Name]
drawUI appState = let s = selectedItem (_itemSelector appState) in [ 
    drawTabs appState 
    <=> (drawLineNo s <+> drawSplitter s <+> drawEditor s)
    <=> renderStatusLine (_statusLineState appState)
    ]

drawTabs :: AppState -> Widget Name 
drawTabs (AppState selector _ _) = 
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
drawLineNo (TabState e@(Editor t _ vp) _ _ _) = 
    let rowNoWidth = maxRowNoWidth $ maxRowNo e
        minDisplayRowNo = addOne $ startingRow vp
        maxDisplayRowNo = min (maxRowNo e) $ increase (subtractOne (h (size vp))) minDisplayRowNo in 
    withAttr lineNoAttr $ str $ unlines $ map (\no -> replicate (fromIntegral rowNoWidth - length no) ' ' ++ no) $ map show [minDisplayRowNo..maxDisplayRowNo]

drawSplitter :: TabState -> Widget Name
drawSplitter s = 
    str $ unlines $ replicate (fromIntegral (h (size (viewport (_editor s))))) " "

drawEditor :: TabState -> Widget Name
drawEditor (TabState _ text (CursorPos row col) _) = reportExtent MainEditor $ B.showCursor MainEditor (Location (fromIntegral col, fromIntegral row)) $ Widget BT.Greedy BT.Greedy $ do 
    render $ str $ unlines $ map (filter (/= '\n') . or "") text
    where or s ms = case ms of 
            Just string -> string 
            Nothing -> s
