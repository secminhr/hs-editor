module Editor where 

import CursorMovement


data ViewPortMovement next 
    = ViewUp 
    | ViewRight 
    | ViewDown 
    | ViewLeft 

data Editor = Editor 
    { cursor :: Cursor 
    }

openEditor :: String -> Editor 
openEditor path = Editor (1, 1)

-- applyCursorMovement :: [Cursor -> Cursor] -> Editor -> Editor
-- applyCursorMovement [] e = e 
-- applyCursorMovement (m:ms) e = applyCursorMovement ms $ e { cursor = (m.cursor) e }