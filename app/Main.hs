{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import SizedViewport
import Editor
import Lens.Micro.Platform (makeLenses, (%=), (.=), use)
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
import Brick (Location(Location), AttrName, attrName, attrMap, withAttr, getVtyHandle, withBorderStyle, CursorLocation, cursorLocationName, nestEventM, visible, textWidth)
import Editing (Editing)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Text (Text, lastRowAvailable)
import ItemSelector (ItemSelector, newItemSelector, selectedItem, mapSelectedItem, getItems, setItems, select, selectedIndex)
import ItemSelector.Lens (current, list)
import qualified Data.List.NonEmpty as NE
import Integer (Positive, fromInt)
import Integer.Natural (Natural, addOne)
import Integer.Positive (increase, subtractOne)
import Data.Maybe (fromJust)
import StatusLine (StatusLineState, newStatusLineState, renderStatusLine, handleStatusLineEvent, _editing, message, editing)
import Data.List (find, findIndex)
import Control.Arrow ((***))

data TabState = TabState 
    { _editor :: Editor
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
    | TabViewport
    deriving (Eq, Ord, Show)

selectItem :: (Eq a) => a -> ItemSelector a -> ItemSelector a 
selectItem item selector = case findIndex (== item) (NE.toList (getItems selector)) of 
                                Nothing -> selector 
                                Just i -> select i selector

data AppState = AppState
    { _tabs :: ItemSelector TabState 
    , _statusLineState :: StatusLineState Name
    , _focusSelector :: ItemSelector Name
    }

makeLenses ''AppState
makeLenses ''TabState

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

    let editor = newEditor content (new (editorSize termW termH (maxRowNo editor)) (Padding 0 0)) (fromJust . fromInt . textWidth) 
    let initTab = TabState editor (File filename)
    let state = AppState (newItemSelector (NE.singleton initTab)) (newStatusLineState StatusLine) (newItemSelector (MainEditor NE.:| [StatusLine]))

    let app = App drawUI showFocused handleEvent (return ()) (const $ theMap)

    final <- customMain initialVty (return initialVty) Nothing app state
    return ()

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent e = do 
    focus <- use (focusSelector.current)
    case focus of 
        MainEditor -> handleMainEditorEvent e 
        StatusLine -> handleStatusLineEventAppState e


handleMainEditorEvent :: BrickEvent Name () -> EventM Name AppState ()
handleMainEditorEvent (VtyEvent (EvKey KUp [])) = tabs.current.editor %= upKey
handleMainEditorEvent (VtyEvent (EvKey KDown [])) = tabs.current.editor %= downKey
handleMainEditorEvent (VtyEvent (EvKey KRight [])) = tabs.current.editor %= rightKey
handleMainEditorEvent (VtyEvent (EvKey KLeft [])) = tabs.current.editor %= leftKey
handleMainEditorEvent (VtyEvent (EvKey KEnter [])) = tabs.current.editor %= enterKey
handleMainEditorEvent (VtyEvent (EvKey (KChar c) [])) = tabs.current.editor %= visibleInput c
handleMainEditorEvent (VtyEvent (EvKey KBS [])) = tabs.current.editor %= backspaceKey 
handleMainEditorEvent (VtyEvent (EvKey (KChar 's') [MCtrl])) = do 
    e <- use (tabs.current.editor)
    tabType <- use (tabs.current.tabType)
    case tabType of 
        File filename -> liftIO $ writeFile filename $ editedString e
        _ -> do 
            focusSelector %= selectItem StatusLine
            statusLineState.message .= "Save to: "
            statusLineState.editing .= True

handleMainEditorEvent (VtyEvent (EvKey (KChar 'n') [MCtrl])) = do 
    tabList <- use (tabs.list)
    vty <- getVtyHandle
    (termW, termH) <- liftIO $ vtyDisplayBounds vty

    let content = ""
    let newTabState = TabState (newEditor content (new (editorSize termW termH 1) (Padding 0 0)) (fromJust . fromInt . textWidth)) TmpBuffer          
    tabs.list %= (<> NE.singleton newTabState)
    tabs %= select (-1)

handleMainEditorEvent (VtyEvent (EvKey KBackTab [])) = tabs %= (\selector -> select (1 + fromIntegral (selectedIndex selector)) selector)
handleMainEditorEvent _ = halt

handleStatusLineEventAppState :: BrickEvent Name () -> EventM Name AppState ()
handleStatusLineEventAppState e = do 
    status <- use statusLineState
    (newStatusLine, input) <- nestEventM status (handleStatusLineEvent e)
    statusLineState .= newStatusLine
    if _editing newStatusLine then return ()
    else do 
        focusSelector %= selectItem MainEditor 
        case input of 
            Nothing -> statusLineState.message .= "Canceled"
            Just filename -> do 
                e <- use (tabs.current.editor)
                liftIO $ writeFile filename $ editedString e
                tabs.current.tabType .= File filename
                statusLineState.message .= "Saved to " ++ filename 


drawUI :: AppState -> [Widget Name]
drawUI appState = 
    let s = selectedItem (_tabs appState) 
        (tabsWidget, tabsWidgetLength) = drawTabs appState in 
        [ 
            (hLimit tabsWidgetLength $ vLimit 3 $ B.viewport TabViewport Horizontal $ tabsWidget) <+> (hBorder <=> str " " <=> hBorder)
            <=> (drawLineNo s <+> drawSplitter s <+> drawEditor s)
            <=> renderStatusLine (_statusLineState appState)
        ]

combineFold :: (a -> b -> b) -> (c -> d -> d) -> (a, c) -> (b, d) -> (b, d)
combineFold f g (x, y) (acc1, acc2) = (f x acc1, g y acc2)

drawTabs :: AppState -> (Widget Name, Int)
drawTabs (AppState selector _ _) = 
    (foldr1 (combineFold (<+>) (+)) $ NE.map (uncurry drawTab) $ 
        NE.zip (getItems selector) (NE.map (== selectedIndex selector) $ NE.fromList [0..]))

drawTab :: TabState -> Bool -> (Widget Name, Int)
drawTab ts selected = (if selected then drawSelectedTab else drawUnselectedTab) (tabLabel ts)
    where tabLabel ts = case _tabType ts of 
                            File filename -> filename 
                            TmpBuffer -> "Untitled"

drawSelectedTab :: String -> (Widget Name, Int)
drawSelectedTab tabLabel = 
    let tabName = " " ++ tabLabel ++ " " in 
        (visible $ border $ str tabName, 2 + textWidth tabName)

drawUnselectedTab :: String -> (Widget Name, Int)
drawUnselectedTab tabLabel = 
    let tabName = " " ++ tabLabel ++ " "
        strWidget = str tabName
        tabNameLength = textWidth tabName in 
        (hLimit tabNameLength $ (hBorder <=> strWidget <=> hBorder), tabNameLength)


drawLineNo :: TabState -> Widget Name 
drawLineNo (TabState e@(Editor t _ vp _) _) = 
    let rowNoWidth = maxRowNoWidth $ maxRowNo e
        minDisplayRowNo = addOne $ startingRow vp
        maxDisplayRowNo = min (maxRowNo e) $ increase (subtractOne (h (size vp))) minDisplayRowNo in 
    withAttr lineNoAttr $ str $ unlines $ map (\no -> replicate (fromIntegral rowNoWidth - length no) ' ' ++ no) $ map show [minDisplayRowNo..maxDisplayRowNo]

drawSplitter :: TabState -> Widget Name
drawSplitter s = 
    str $ unlines $ replicate (fromIntegral (h (size (viewport (_editor s))))) " "

drawEditor :: TabState -> Widget Name
drawEditor (TabState e _) =
    let (text, CursorPos row col) = frame e in
        reportExtent MainEditor $ B.showCursor MainEditor (Location (fromIntegral col, fromIntegral row)) $ Widget BT.Greedy BT.Greedy $ do 
        render $ str $ unlines $ map (filter (/= '\n') . or "") text
        where or s ms = case ms of 
                Just string -> string 
                Nothing -> s
