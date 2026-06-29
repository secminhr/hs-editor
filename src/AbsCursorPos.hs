module AbsCursorPos where 

import Integer.Natural (Natural)

data Position = Position 
    { row :: Natural 
    , col :: Natural }
    deriving (Eq, Show)