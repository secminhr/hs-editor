module Viewport
        ( Viewport
        , startingRow
        , startingCol
        , viewAt
        , new 
        , scrollV
        , scrollH
        ) where

{-
    Viewport of a piece of text

    Observation: 
        startingRow :: Viewport -> Int
        startingCol :: Viewport -> Int

    Constructor:
        new :: Viewport

        # positive offset indicates a scroll down, negative indicates a scroll up
        scrollV :: Int -> Viewport -> Viewport
        # positive offset indicates a scroll right, negative indicates a scroll left
        scrollH :: Int -> Viewport -> Viewport

        viewAt :: (vOffset :: Int) -> (hOffset :: Int) -> Viewport
    
    Equation:
        scrollV-id:
        scrollV 0 = id
        
        scrollV/scrollV:
        forall (o1 :: Int) (o2 :: Int).
            scrollV o1 . scrollV o2 = scrollV (o1 + o2)

        scrollH-id:
        scrollH 0 = id

        scrollH/scrollH:
        forall (o1 :: Int) (o2 :: Int).
            scrollH o1 . scrollH o2 = scrollH (o1 + o2)

        scrollH/scrollV:
        forall (ov :: Int) (oh :: Int).
            scrollH oh . scrollV ov = scrollV ov . scrollH oh 

        viewAt/scrollV/scrollH/new:
        forall (vOffset :: Int) (hOffset :: Int).
            viewAt vOffset hOffset = scrollV vOffset . scrollH hOffset . new

        viewAt/new:
        forall (h :: Int) (w :: Int).
            new = viewAt 0 0

        viewAt/scrollV/scrollH/viewAt:
        forall (vo :: Int) (ho :: Int) (h :: Int) (w :: Int) (initVo :: Int) (initHo :: Int).
            scrollV vo (scrollH ho (viewAt initVo initHo h w)) = 
                viewAt (initVo + vo) (initHo + ho) h w

        startingRow/viewAt:
        forall (vo :: Int) (ho :: Int).
            startingRow (viewAt vo ho) = vo

        startingCol/viewAt:
        forall (vo :: Int) (ho :: Int).
            startingCol (viewAt vo ho) = ho

-}

data Viewport 
    = ViewAt Int Int
    deriving (Show, Eq)

startingRow :: Viewport -> Int
startingRow (ViewAt vo _) = vo

startingCol :: Viewport -> Int
startingCol (ViewAt _ ho) = ho

viewAt :: Int -> Int -> Viewport
viewAt = ViewAt

new :: Viewport
new = ViewAt 0 0

scrollV :: Int -> Viewport -> Viewport
scrollV offset (ViewAt vo ho) = ViewAt (vo + offset) ho

scrollH :: Int -> Viewport -> Viewport
scrollH offset (ViewAt vo ho) = ViewAt vo (ho + offset)