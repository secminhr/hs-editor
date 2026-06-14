{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Lib
import Viewport
import Lens.Micro.Platform (makeLenses, (%=))
import Brick.Widgets.Core (str, reportExtent)
import Brick.Types (Widget(..), BrickEvent, EventM, BrickEvent(VtyEvent), Size(Greedy), getContext, availWidth, availHeight)
import Brick.Main (defaultMain, neverShowCursor, resizeOrQuit, App(App), lookupExtent, continueWithoutRedraw, halt)
import Brick.AttrMap (forceAttrMap)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceM)

data AppState = AppState
    { _text :: String 
    , _viewport :: Viewport
    }
makeLenses ''AppState

data Name 
    = MainEditor
    deriving (Eq, Ord, Show)

main :: IO ()
main = do 
    filename <- getLine
    content <- readFile filename
    
    let state = AppState content new
    let app = App drawUI neverShowCursor handleEvent (return ()) (const $ forceAttrMap defAttr)

    final <- defaultMain app state
    return ()
    
handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent (EvKey KUp [])) = viewport %= scrollV (-1)
handleEvent (VtyEvent (EvKey KDown [])) = viewport %= scrollV 1
handleEvent (VtyEvent (EvKey KRight [])) = viewport %= scrollH 1
handleEvent (VtyEvent (EvKey KLeft [])) = viewport %= scrollH (-1)
handleEvent _ = halt

drawUI :: AppState -> [Widget Name]
drawUI s = [ drawEditor s ]

drawEditor :: AppState -> Widget Name
drawEditor (AppState text vp) = Widget Greedy Greedy $ do 
    context <- getContext
    let height = availHeight context
    let width = availWidth context
    let filtered = map (take width . drop (startingCol vp)) $ 
                    take height $ drop (startingRow vp) $ lines text

    render $ str $ unlines filtered
    