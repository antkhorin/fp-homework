module Test where

import           Data.Map
import           Lib
import           Test.Hspec

r :: Map Name Integer
r = insert "x" 1 $ insert "y" (-9) $ insert "z" 3 empty

main :: IO ()
main = hspec $
    describe "expr" $ do
        it "lit" $
            eval (Lit 42) r `shouldBe` Right 42
        it "varOk" $
            eval (Var "z") r `shouldBe` Right 3
        it "varNotOk" $
            eval (Var "a") r `shouldBe` Left UndefinedVariable
        it "+" $
            eval (Lit 2 `Add` Lit 3) r `shouldBe` Right 5
        it "-" $
            eval (Var "x" `Sub` Var "y") r `shouldBe` Right 10
        it "*" $
            eval (Lit 15 `Mul` Var "z") r `shouldBe` Right 45
        it "/ok" $
            eval (Lit 42 `Div` Lit 6) r `shouldBe` Right 7
        it "/notOk" $
            eval (Var "x" `Div` Lit 0) r `shouldBe` Left DivideByZero
        it "let" $
            eval (Let "x" (Lit 12) (Lit 5 `Add` Var "x")) r `shouldBe` Right 17
        it "test" $
            eval (Var "x" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x"))) r `shouldBe` Right 7
