module ViewportSpec where 

import Test.Hspec 
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Viewport

instance Arbitrary Viewport where 
    arbitrary = do 
        vo <- chooseInt (-1, maxBound)
        ho <- chooseInt (-1, maxBound)

        return $ viewAt vo ho

infix 1 ~=
(~=) :: (Eq b, Show b) => (a -> b) -> (a -> b) -> a -> Expectation
(~=) = liftA2 shouldBe

spec :: Spec 
spec = do 
    prop "scrollV with offset 0 = id" $
        scrollV 0 ~= id
    
    prop "scrollH with offset 0 = id" $
        scrollH 0 ~= id

    prop "scrollV can be combined" $
        \o1 o2 -> 
            (scrollV o1 . scrollV o2) ~= scrollV (o1 + o2)
    
    prop "scrollH can be combined" $
        \o1 o2 ->
            (scrollH o1 . scrollH o2) ~= scrollH (o1 + o2)
        
    prop "scrollH/scrollV" $
        \ho vo ->
            (scrollH ho . scrollV vo) ~= (scrollV vo . scrollH ho)
    
    prop "viewAt/new" $ 
        new `shouldBe` viewAt 0 0

    prop "viewAt/scrollV/scrollH/viewAt" $ 
        \vo ho initVo initHo ->
            scrollV vo (scrollH ho (viewAt initVo initHo)) `shouldBe` viewAt (initVo + vo) (initHo + ho)

    prop "inside/viewAt" $ 
        \vo ho h w r c ->
            inside (viewAt vo ho) h w r c `shouldBe` (vo <= r && r < vo+h && ho <= c && c < ho+w)
    
