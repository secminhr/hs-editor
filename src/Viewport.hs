module Viewport
        ( Viewport
        , inside
        , viewAt
        , new 
        , scrollV
        , scrollH
        ) where

{-
    Viewport of a piece of text

    Observation: 
        displayMask :: Viewport -> (row :: Int) -> (col :: Int) -> Bool

    Constructor:
        new :: (height :: Int) -> (width :: Int) -> Viewport

        # positive offset indicates a scroll down, negative indicates a scroll up
        scrollV :: Int -> Viewport -> Viewport
        # positive offset indicates a scroll right, negative indicates a scroll left
        scrollH :: Int -> Viewport -> Viewport

        viewAt :: (vOffset :: Int) -> (hOffset :: Int) -> (height :: Int) -> (width :: Int) -> Viewport
    
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

        viewAt/new":
        forall (h :: Int) (w :: Int).
                new = viewAt 0 0

        viewAt/scrollV/scrollH/viewAt:
        forall (vo :: Int) (ho :: Int) (h :: Int) (w :: Int) (initVo :: Int) (initHo :: Int).
            scrollV vo (scrollH ho (viewAt initVo initHo h w)) = 
                viewAt (initVo + vo) (initHo + ho) h w

        # Using (Positive Int) as the type of h and w would be more reasonable
        # but by this law, when h <= 0 or w <= 0 we automatically get a viewport that displays nothing
        displayMask/viewAt:
        forall (vo :: Int) (ho :: Int) (h :: Int) (w :: Int) (r :: Int) (c :: Int).
            displayMask (viewAt vo ho h w) r c = vo <= r < vo+h && ho <= c < ho+w

-}

data Viewport 
    = ViewAt Int Int Int Int
    deriving (Show, Eq)

inside :: Viewport -> Int -> Int -> Bool
inside (ViewAt vo ho h w) r c = vo <= r && r < vo+h && ho <= c && c < ho+w

viewAt :: Int -> Int -> Int -> Int -> Viewport
viewAt = ViewAt

new :: Int -> Int -> Viewport
new = ViewAt 0 0

scrollV :: Int -> Viewport -> Viewport
scrollV offset (ViewAt vo ho h w) = ViewAt (vo + offset) ho h w

scrollH :: Int -> Viewport -> Viewport
scrollH offset (ViewAt vo ho h w) = ViewAt vo (ho + offset) h w