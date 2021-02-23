module MorraSpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "dummy" $
        it "foo" $
            1 `shouldBe` 1