module Test where

import           Data.List  (sort)
import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "expr" $ do
        it "+" $
            eval (Const 12 :+ Const 3) `shouldBe` Right 15
        it "-" $
            eval (Const 12 :- Const 3) `shouldBe` Right 9
        it "*" $
            eval (Const 12 :* Const 3) `shouldBe` Right 36
        it "/ok" $
            eval (Const 12 :/ Const 3) `shouldBe` Right 4
        it "/notok" $
            eval (Const 12 :/ Const 0) `shouldBe` Left DivideByZeroError
        it "^ok" $
            eval (Const 12 :^ Const 3) `shouldBe` Right 1728
        it "^notok" $
            eval (Const 12 :^ Const (-1)) `shouldBe` Left NegativeExponentError
    describe "bin" $ do
        it "2" $
            sort (bin 2) `shouldBe` [[0, 0], [0, 1], [1, 0], [1, 1]]
        it "3" $
            sort (bin 3) `shouldBe` [[0, 0, 0], [0, 0, 1], [0, 1, 0], [0, 1, 1],
                                     [1, 0, 0], [1, 0, 1], [1, 1, 0], [1, 1, 1]]
    describe "comb" $ do
        it "4 2" $
            sort (map sort $ combinations 4 2) `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
        it "200 0" $
            sort (map sort $ combinations 200 0) `shouldBe` [[]]
        it "1 10" $
            sort (map sort $ combinations 1 10) `shouldBe` []
        it "100 100" $
            sort (map sort $ combinations 100 100) `shouldBe` [[1..100]]
    describe "perm" $ do
        it "[1, 2, 3]" $
            sort (permutations [1, 2, 3]) `shouldBe` [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
        it "[]" $
            sort (permutations []) `shouldBe` [[]]
    describe "SExpr" $ do
        it "5" $
            runParser parseSExpr "5" `shouldBe` Right ("", A $ N 5)
        it "foo3" $
            runParser parseSExpr "foo3" `shouldBe` Right ("", A $ I "foo3")
        it "(bar (foo) 3 5 874)" $
            runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Right ("", Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)])
        it "(((lambda x (lambda y (plus x y))) 3) 5)" $
            runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` Right ("", Comb [Comb [Comb [A (I "lambda"), A (I "x"), Comb [A (I "lambda"), A (I "y"), Comb [A (I "plus"), A (I "x"), A (I "y")]]], A(N 3)], A (N 5)])
        it "   ( lots of ( spaces in ) this ( one ) )   " $
            runParser parseSExpr "   ( lots of ( spaces in ) this ( one ) )   " `shouldBe` Right ("", Comb [A (I "lots"), A (I "of"), Comb [A (I "spaces"), A (I "in")], A (I "this"), Comb [A (I "one")]])
        it "1 2 3" $
            runParser parseSExpr "1 2 3" `shouldBe` Right ("2 3", A $ N 1)
        it "() 1 2" $
            runParser parseSExpr "() 1 2" `shouldBe` Left ParseError
    describe "let" $ do
        it "let x = 2\n" $
            optLets "let x = 2" `shouldBe` "let x = 2\n"
        it "   let     x    = 2     \n" $
            optLets "   let     x    = 2     " `shouldBe` "let x = 2\n"
        it "let x = 2\nlet y = 3\n" $
            optLets "let x = 2\nlet y = 3" `shouldBe` "let x = 2\nlet y = 3\n"
        it "let x = 2 + 3 + 4\n" $
            optLets "let x = 2 + 3 + 4\n" `shouldBe` "let x = 9\n"
        it "   let     x    = 2 +   1 \n  let y = 5 + 2 + 7 \n" $
            optLets "   let     x    = 2 +   1 \n  let y = 5 + 2 + 7 \n" `shouldBe` "let x = 3\nlet y = 14\n"
        it "let x = 2\nlet y = x\n" $
            optLets "let x = 2\nlet y = x" `shouldBe` "let x = 2\nlet y = 2\n"
        it "let x = 2 + 4 + 5\nlet y = x + x + 2\nlet z = x + y + 12\n" $
            optLets "let x = 2 + 4 + 5\nlet y = x + x + 2\nlet z = x + y + 12" `shouldBe` "let x = 11\nlet y = 24\nlet z = 47\n"
        it "let x = 1 + 2 + 5\nlet   y = x+x\nlet z=0+    x   + y + 8\n" $
            optLets "let x = 1 + 2 + 5\nlet   y = x+x\nlet z=0+    x   + y + 8" `shouldBe` "let x = 8\nlet y = 16\nlet z = 32\n"
