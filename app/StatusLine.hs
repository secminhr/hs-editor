{-# LANGUAGE TemplateHaskell #-}

module StatusLine where 
import Lens.Micro.Platform (makeLenses, use, (.=))
import Brick (Widget, vLimit, str, emptyWidget, (<+>), txt, BrickEvent (VtyEvent), EventM, Named (getName), zoom)
import Brick.Widgets.Edit (Editor, renderEditor, editorText, getEditContents, handleEditorEvent)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty (Event(EvKey), Key (KEnter, KEsc))
import Data.Maybe (listToMaybe)

data StatusLineState n = StatusLineState
    { _message :: String 
    , _editing :: Bool
    , _editorState :: Editor Text n }

makeLenses ''StatusLineState

newStatusLineState :: n -> StatusLineState n
newStatusLineState editorName = StatusLineState "" False $ editorText editorName (Just 1) T.empty

handleStatusLineEvent :: (Eq n) => BrickEvent n () -> EventM n (StatusLineState n) (Maybe String)
handleStatusLineEvent (VtyEvent (EvKey KEnter [])) = do 
    editing .= False 
    editor <- use editorState
    let content = getEditContents editor 
    editorState .= editorText (getName editor) (Just 1) T.empty
    return $ T.unpack <$> listToMaybe content

handleStatusLineEvent (VtyEvent (EvKey KEsc [])) = do 
    editing .= False 
    editor <- use editorState
    editorState .= editorText (getName editor) (Just 1) T.empty
    return Nothing 

handleStatusLineEvent e = do 
    zoom editorState $ handleEditorEvent e
    return Nothing 


renderStatusLine :: (Ord n, Show n) => StatusLineState n -> Widget n 
renderStatusLine s = vLimit 1 $ str (_message s) 
    <+> if not (_editing s) then emptyWidget 
        else renderEditor (txt . T.unlines) True (_editorState s)