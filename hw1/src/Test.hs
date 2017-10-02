module Test where

import           Data.List     (sort)
import           Lib
import           System.Random (newStdGen, randomRs)
import           Test.Hspec

randomIntegerList :: Int -> Integer -> Integer -> IO [Integer]
randomIntegerList n from to = take n . randomRs (from, to) <$> newStdGen

tests1 :: [String]
tests1 = [ "1", "1 2 3", " 1", "\t12345\t", "010 020 030"
            , "-1 -2 -3", "\t-12345\t", "0\t\n\t\n\t\t -4 -40"
          ]
tests1res :: [Maybe Integer]
tests1res = map Just [1, 6, 1, 12345, 60, -6,-12345, -44]


main :: IO ()
main = do
  mergeSortTests <- sequence (map (\x -> randomIntegerList x 1 10) [5..10])
  let mergeSortResults = map sort mergeSortTests
  putStrLn $ show mergeSortTests
  putStrLn $ show mergeSortResults

  hspec $ do
    describe "order3" $ do
      it "can sort Integeregers" $ do
        order3 (7 :: Integer, -1, 4) `shouldBe` (-1 :: Integer, 4, 7)
      it "can sort strings" $ do
        order3 ("cc", "aa", "bb") `shouldBe` ("aa", "bb", "cc")
    describe "highestBit" $ do
      it "degree" $ do
        highestBitAdv ((2 :: Integer) ^ (3 :: Integer)) `shouldBe` (Just (8, 3) :: Maybe (Integer, Integer))
    describe "smartReplicate" $ do
      it "givenTest" $ do
        smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]
    describe "contains" $ do
      it "givenTest" $ do
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]
    describe "removeAt" $ do
      it "givenTest1" $ do
        removeAtAdv (1 :: Integer) ([1,2,3] :: [Integer]) `shouldBe` ((Just 2 , [1,3]) :: (Maybe Integer, [Integer]))
      it "givenTest2" $ do
        removeAtAdv 10 [1,2,3] `shouldBe` ((Nothing, [1,2,3]) :: (Maybe Integer, [Integer]))
    describe "collectEvery" $ do
      it "givenTest1" $ do
        collectEvery 3 ([1..8] :: [Integer]) `shouldBe` (([1,2,4,5,7,8], [3,6]) :: ([Integer], [Integer]))
    describe "collectEvery" $ do
      it "givenTest1" $ do
        collectEvery 3 ([1..8] :: [Integer]) `shouldBe` (([1,2,4,5,7,8], [3,6]) :: ([Integer], [Integer]))
    describe "stringSum" $ do
      it "givenTest1" $ do
        map stringSum tests1 `shouldBe` tests1res
    describe "mergeSort" $ do
      it "randomTests" $ do
        map mergeSort mergeSortTests `shouldBe` mergeSortResults
