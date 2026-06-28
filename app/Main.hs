{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Lib
import Viewport
import Editor
import Lens.Micro.Platform (makeLenses, (%=), (.=), use)
import Brick.Widgets.Core (str, reportExtent, (<=>), vLimit, fill, (<+>))
import qualified Brick.Widgets.Core as B
import Brick.Types (Widget(..), BrickEvent, EventM, BrickEvent(VtyEvent), getContext, availWidth, availHeight, ViewportType(Horizontal), Extent(Extent))
import qualified Brick.Types as BT
import Brick.Main (customMain, neverShowCursor, showFirstCursor, resizeOrQuit, App(App), lookupExtent, continueWithoutRedraw, halt)
import Brick.AttrMap (forceAttrMap, AttrMap)
import Graphics.Vty (outputIface, Modifier (MCtrl), withBackColor)
import qualified Graphics.Vty.CrossPlatform as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Attributes (defAttr, withStyle, reverseVideo)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Graphics.Vty.Output (displayBounds)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceM, trace, traceShowId, traceShowWith, traceShow)
import Editor (newEditor, editedString)
import Brick (Location(Location), AttrName, attrName, attrMap, withAttr)
import Editing (Editing)
import Brick.Widgets.Border (border, hBorder, vBorder)

data AppState = AppState
    { _editor :: Editor
    , _viewport :: Viewport
    , _size :: Size
    , _text :: [Maybe String]
    , _cursorPos :: CursorPos
    , _filename :: String
    }
makeLenses ''AppState

data Name 
    = MainEditor
    | MainEditorCursor
    deriving (Eq, Ord, Show)

reverseAttr :: AttrName
reverseAttr = attrName "reverseText"

theMap :: AttrMap
theMap = attrMap defAttr 
    [ (reverseAttr, defAttr `withStyle` reverseVideo)
    ]

main :: IO ()
main = do 
    filename <- getLine
    content <- readFile filename
    
    initialVty <- V.mkVty defaultConfig
    (termW, termH) <- displayBounds (outputIface initialVty)

    let state = AppState (newEditor content) new (Size (fromIntegral termW) (fromIntegral termH - 3)) (map Just $ lines content) (CursorPos 0 0) filename

    let app = App drawUI showFirstCursor handleEvent (return ()) (const $ theMap)

    final <- customMain initialVty (return initialVty) Nothing app state
    return ()

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent (EvKey KUp [])) = editor %= upKey >> updateStates
handleEvent (VtyEvent (EvKey KDown [])) = editor %= downKey >> updateStates
handleEvent (VtyEvent (EvKey KRight [])) = editor %= rightKey >> updateStates
handleEvent (VtyEvent (EvKey KLeft [])) = editor %= leftKey >> updateStates
handleEvent (VtyEvent (EvKey KEnter [])) = editor %= enterKey >> updateStates
handleEvent (VtyEvent (EvKey (KChar c) [])) = editor %= visibleInput c >> updateStates
handleEvent (VtyEvent (EvKey KBS [])) = editor %= backspaceKey >> updateStates
handleEvent (VtyEvent (EvKey (KChar 's') [MCtrl])) = do 
    e <- use editor
    f <- use filename
    let t = editedString e
    liftIO $ writeFile f t

handleEvent _ = halt

updateStates :: EventM Name AppState ()
updateStates = do 
    editorState <- use editor 
    vpState <- use viewport 
    sizeState <- use size
    let (vp', t', cursorPos') = frame editorState vpState sizeState 
    viewport .= vp' 
    text .= t' 
    cursorPos .= cursorPos'

    mainEditorExtent <- lookupExtent MainEditor
    case mainEditorExtent of 
        Nothing -> return ()
        Just (Extent _ _ (w, h)) -> let _ = (Size (fromIntegral w) (fromIntegral h)) in return ()

drawUI :: AppState -> [Widget Name]
drawUI s = [ 
    (border (str (" " ++ _filename s ++ " "))) <+> (hBorder <=> str " " <=> hBorder) <=>
    drawEditor s 
    ]

drawEditor :: AppState -> Widget Name
drawEditor (AppState _ vp _ text (CursorPos row col) _) = reportExtent MainEditor $ B.showCursor MainEditorCursor (Location (fromIntegral col, fromIntegral row)) $ Widget BT.Greedy BT.Greedy $ do 
    render $ str $ unlines $ map (filter (/= '\n') . or "") text
    where or s ms = case ms of 
            Just string -> string 
            Nothing -> s
    