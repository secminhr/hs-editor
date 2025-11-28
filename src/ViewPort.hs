module ViewPort(ViewPort, openViewPort, getContent, Position) where 

type Buffer = [String]
type LineNo = Int
type ColNo = Int
type Position = (LineNo, ColNo)

data ViewPort = ViewPort 
    { width :: Int 
    , height :: Int 
    , buffer :: Buffer }

openViewPort :: Int -> Int -> Buffer -> ViewPort
openViewPort width height buffer 
    | width <= 0 = error "Can't create ViewPort with no width or height"
    | height <= 0 = error "Can't create ViewPort with no width or height"
    | otherwise = ViewPort width height buffer
 
-- get the element within [start, start + length)
range :: Int -> Int -> [a] -> [a]
range start length = take length . drop start

getContent :: Position -> ViewPort -> String 
getContent (topLine, leftCol) (ViewPort width height buffer) = 
   unlines . map (range (leftCol - 1) width) . range (topLine - 1) height $ buffer