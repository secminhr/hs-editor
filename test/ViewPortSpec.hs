module ViewPortSpec where 

import Test.Hspec 
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property 
import Test.QuickCheck.Gen
import ViewPort
import Control.Exception

spec :: Spec
spec = do 
    let buffer = ["a", "bc", "edf", "ghij"] 
    context ("When buffer is " ++ show buffer) $ do
        describe "ViewPort with width and height >= 4" $ do
            prop "should show full content when top-left position is (1, 1)" $
                \width height -> width >= 4 && height >= 4 ==> 
                    let viewPort = openViewPort width height buffer in 
                    getContent (1, 1) viewPort `shouldBe` "a\nbc\nedf\nghij\n"
        describe "ViewPort with 0 < width < 4 and height >= 4" $ do 
            prop "should cut the line wider than width when top-left is (1, 1)" $ 
                forAll (chooseInt (1, 3)) $ \width height -> height >= 4 ==>
                    let viewPort = openViewPort width height buffer in 
                        getContent (1, 1) viewPort `shouldBe` unlines (map (take width) buffer)
        describe "ViewPort with width >= 4 and 0 < height < 4" $ do 
            prop "should cut the lines after height when top-left is (1, 1)" $
                forAll (chooseInt (1, 3)) $ \height width -> width >= 4 ==>
                    let viewPort = openViewPort width height buffer in 
                        getContent (1, 1) viewPort `shouldBe` unlines (take height buffer)
        describe "ViewPort has width and height == 4" $ do 
            let viewPort = openViewPort 4 4 buffer
            describe "with top-left is (1, n)" $ do 
                prop "should cut the left (n-1) characters each line" $ 
                    \n -> n >= 1 ==> getContent (1, n) viewPort `shouldBe` unlines (map (drop (n - 1)) buffer)
            describe "with top-left is (n, 1)" $ do 
                prop "should show n-th line as the first line, skipping first n-1 lines" $
                    \n -> n >= 1 ==> getContent (n, 1) viewPort `shouldBe` unlines (drop (n-1) buffer)

    describe "Creating ViewPort with non-positive width or height" $ do 
        prop "is not allowed, causes error" $
            \width height -> width <= 0 || height == 0 ==> 
                evaluate (openViewPort width height []) `shouldThrow` errorCall "Can't create ViewPort with no width or height"
    
    describe "Creating ViewPort with empty buffer" $ do 
        it "should show nothing" $ do 
            getContent (1, 1) (openViewPort 4 4 [])  `shouldBe` ""
