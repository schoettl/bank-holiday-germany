
module Main (main) where

import Data.Time.Calendar.BankHoliday.Germany
import Data.Time.Calendar.WeekDate
import Data.Time

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (forAll, hedgehog, (===))
import Test.DocTest

day :: Year -> Int -> Int -> Day
day = fromGregorian

year :: Day -> Year
year = (\(y, _, _) -> y) . toGregorian

main :: IO ()
main = do
  doctest ["src/"]
  hspec $ do
    describe "holidaysBetween" $ do
      it "works for Christmas" $
        holidaysBetween (day 2024 12 1) (day 2024 12 30)
          `shouldBe` [(day 2024 12 24, ChristmasEve), (day 2024 12 25, ChristmasDay), (day 2024 12 26, SecondChristmasDay)]
      it "is empty list when there are no holidays" $
        holidaysBetween (day 2024 12 1) (day 2024 12 3)
          `shouldBe` []
      it "counts 11 bank holidays and 9 public holidays per year (2010 to 2020)" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 2010 2020)
        let bankHolidays = holidaysBetween (day y 1 1) (day y 12 31)
        length bankHolidays === 11
        length (filter (isPublicHoliday . snd) bankHolidays) === 9
    describe "toDay" $ do
      it "always returns a day in the given year" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 0 5000)
        d <- forAll Gen.enumBounded
        year (toDay y d) === y
      it "Easter Monday is always a Monday" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 0 5000)
        let (_, _, d) = toWeekDate $ toDay y EasterMonday
        d === 1     -- 1 = Monday
    describe "calculateEasterSunday" $ do
      it "is always between Mar 22 and Apr 25" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 0 10000)
        (calculateEasterSunday y >= day y 3 22) === True
        (calculateEasterSunday y <= day y 4 25) === True
    describe "isPublicHoliday" $ do
      it "is False only for Chrismas Eve and New Year's Eve" $
        length (filter (not . isPublicHoliday) [minBound..maxBound])
          `shouldBe` 2
