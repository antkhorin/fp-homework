module Test where

import           Data.Foldable   (toList)
import           Data.List       (sort)
import qualified Data.List.Split (splitOn)
import           Data.Monoid
import           Lib
import           System.Random   (newStdGen, randomRs)
import           Test.Hspec

randomIntegerList :: Int -> Integer -> Integer -> IO [Integer]
randomIntegerList n from to = take n . randomRs (from, to) <$> newStdGen

tests1 :: [String]
tests1 = [ "1", "1 2 3", " 1", "\t12345\t", "010 020 030"
            , "-1 -2 -3", "\t-12345\t", "0\t\n\t\n\t\t -4 -40"
          ]
tests1res :: [Maybe Integer]
tests1res = map Just [1, 6, 1, 12345, 60, -6,-12345, -44]

-- testNat' :: (Nat -> Nat -> Bool) -> (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
-- testNat' f g a b = f (fromInteger a) (fromInteger b) == g a b

main :: IO ()
main = do
  mergeSortTests <- mapM (\x -> randomIntegerList x 1 10) [5..10]
  let mergeSortResults = map sort mergeSortTests
  natxsTests <- randomIntegerList 100 20 40
  natysTests <- randomIntegerList 100 1 20
  let natTests = zip natxsTests natysTests

  hspec $ do
    describe "order3" $ do
      it "can sort Integers" $
        order3 (7 :: Integer, -1, 4) `shouldBe` (-1 :: Integer, 4, 7)
      it "can sort strings" $
        order3 ("cc", "aa", "bb") `shouldBe` ("aa", "bb", "cc")
    describe "highestBit" $
      it "degree" $
        highestBitAdv ((2 :: Integer) ^ (3 :: Integer)) `shouldBe` (Just (8, 3) :: Maybe (Integer, Integer))
    describe "smartReplicate" $
      it "givenTest" $
        smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]
    describe "contains" $
      it "givenTest" $
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]
    describe "removeAt" $ do
      it "givenTest1" $
        removeAtAdv (1 :: Integer) ([1,2,3] :: [Integer]) `shouldBe` ((Just 2 , [1,3]) :: (Maybe Integer, [Integer]))
      it "givenTest2" $
        removeAtAdv 10 [1,2,3] `shouldBe` ((Nothing, [1,2,3]) :: (Maybe Integer, [Integer]))
    describe "collectEvery" $
      it "givenTest1" $
        collectEvery 3 ([1..8] :: [Integer]) `shouldBe` (([1,2,4,5,7,8], [3,6]) :: ([Integer], [Integer]))
    describe "collectEvery" $
      it "givenTest1" $
        collectEvery 3 ([1..8] :: [Integer]) `shouldBe` (([1,2,4,5,7,8], [3,6]) :: ([Integer], [Integer]))
    describe "stringSum" $
      it "givenTest1" $
        map stringSum tests1 `shouldBe` tests1res
    describe "mergeSort" $
      it "randomTests" $
        map mergeSort mergeSortTests `shouldBe` mergeSortResults
    {-describe "figths" $ do
      it "knight with axe vs knight with sword" $ do
        let sw = Knight 5 5 Sword
        let ax = Knight 5 5 Axe
        simulateFight ax sw `shouldBe` (sw, 1)
      it "monster vs knight" $ do
        let m = Monster 5 5 Carnivore
        let k = Knight 5 5 Axe
        simulateFight m k `shouldBe` (k, 1) -}
    describe "nat" $ do
      let checkOp f g = map (\(x, y) -> natToInteger $ fromInteger x `f` fromInteger y) natTests `shouldBe` map (uncurry g) natTests
      it "mult" $
        checkOp (*) (*)
      it "add" $
        checkOp (+) (+)
      it "minus" $
        checkOp (-) (-)
    describe "tree" $ do
      let tree = fromList [1..20] :: Tree Integer
      it "contains True" $
        all (`elem` tree) [1..20] `shouldBe` True
      it "contains False" $
        any (`elem` tree) [21..40] `shouldBe` False
      it "notEmpty" $
        null tree `shouldBe` False
      it "length" $
        length tree `shouldBe` 20
      it "toList . fromList == sort" $ do
        let testarr = ([100,99..1] ++ [120..150]) :: [Integer]
        (toList . fromList) testarr `shouldBe` sort testarr
    describe "splitOn == Data.List.Split.splitOn" $ do
      let tests = ["", "//", "ujas", "asd/asd", "/asd", "/vsd/"]
      --let wr str = splitOn str `shouldBe` Data.List.Split.splitOn str
      it "check with del = '/'" $
        map (splitOn '/') tests `shouldBe` map (Data.List.Split.splitOn "/") tests
      it "check joinWith . splitOn == id" $
        map (joinWith '/' . splitOn '/') tests `shouldBe` tests
    describe "maybeConcat" $
      it "test1" $
        maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` ([1..5] :: [Integer])
    describe "eitherConcat" $
      it "test1" $ do
        let tst = [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
        eitherConcat tst `shouldBe` ((Sum {getSum = 8}, [1,2,3,4,5]) :: (Sum Integer, [Integer]))
    describe "Monoid Tree" $
      it "(a <> b) <> c == a <> (b <> c)" $ do
        let a = fromList [1..3] :: Tree Integer
        let b = fromList [120,119..78] :: Tree Integer
        let c = fromList [10..63] :: Tree Integer
        a `mappend` b `mappend` c `shouldBe` a `mappend` (b `mappend` c)
