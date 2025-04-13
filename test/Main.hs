
module Main (main) where

import Data.Time.Calendar.BankHoliday.Germany
import Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays (ExtraHoliday(..), FederalState(..))
import Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays qualified as EH
import Data.Time.Calendar.WeekDate
import Data.Time

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (forAll, hedgehog, (===))
import Test.DocTest

day :: Year -> Int -> Int -> Day
day = fromGregorian

jan1 :: Year -> Day
jan1 y = fromGregorian y 1 1

dec31 :: Year -> Day
dec31 y = fromGregorian y 12 31

year :: Day -> Year
year = (\(y, _, _) -> y) . toGregorian

main :: IO ()
main = do
  doctest ["src/"]
  hspec $ do
   describe "Data.Time.Calendar.BankHoliday.Germany" $ do
    describe "holidaysBetween" $ do
      it "works for Christmas" $
        holidaysBetween (day 2024 12 1) (day 2024 12 30)
          `shouldBe` [(day 2024 12 24, ChristmasEve), (day 2024 12 25, ChristmasDay), (day 2024 12 26, SecondChristmasDay)]
      it "is empty list when there are no holidays" $
        holidaysBetween (day 2024 12 1) (day 2024 12 3)
          `shouldBe` []
      it "counts 11 bank holidays and 9 public holidays per year (2010 to 2020)" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 2010 2020)
        let bankHolidays = holidaysBetween ((jan1 y)) ((dec31 y))
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
      it "is correct for some years" $ do
        calculateEasterSunday 2024 `shouldBe` day 2024 3 31
        calculateEasterSunday 2025 `shouldBe` day 2025 4 20
        calculateEasterSunday 2026 `shouldBe` day 2026 4 5
      it "is always a Sunday" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 0 5000)
        let (_, _, d) = toWeekDate $ calculateEasterSunday y
        d === 7  -- 7 = Sunday
    describe "isPublicHoliday" $ do
      it "is False only for Chrismas Eve and New Year's Eve" $
        length (filter (not . isPublicHoliday) [minBound..maxBound])
          `shouldBe` 2
    describe "yearFromDay" $ do
      it "works for any year" $ hedgehog $ do
        y <- forAll $ Gen.integral (Range.linear 0 5000)
        m <- forAll $ Gen.integral (Range.linear 1 12)
        d <- forAll $ Gen.integral (Range.linear 1 28)
        yearFromDay (day y m d) === y
    describe "germanHolidayName" $
      it "names are longer than 5 characters for all holidays (which mean there are no non-exhaustive patterns)" $
        all ((>5) . length . germanHolidayName) [minBound .. maxBound :: BankHoliday] `shouldBe` True

   describe "Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays" $ do
     describe "FederalState" $
       it "has right number of states" $
         length [minBound .. maxBound :: FederalState] `shouldBe` 16
     describe "toDay" $
       it "works for Fronleichnam (depending on Easter Sunday)" $ do
         -- https://www.arbeitstage.org/feiertage/fronleichnam/
         map (\y -> show $ EH.toDay y Fronleichnam) [2024..2027]
           `shouldBe` ["2024-05-30", "2025-06-19", "2026-06-04", "2027-05-27"]
     describe "holidaysBetween" $ do
       it "yields Bavaria's extra holidays" $
         map snd (EH.holidaysBetween Bayern (day 2024 11 1) (day 2024 12 31))
           `shouldBe` [Allerheiligen]
       it "there is only one extra holiday in Berlin" $ do
         filter (EH.isHolidayInState Berlin) [minBound..maxBound]
           `shouldBe` [InternationalerFrauentag]
       it "there are 3 extra holidays in Baden-Württemberg" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         length (EH.holidaysBetween BadenWuerttemberg ((jan1 y)) ((dec31 y))) === 3
       it "there are 2 extra holidays in Nordrhein-Westfalen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         length (EH.holidaysBetween NordrheinWestfalen ((jan1 y)) ((dec31 y))) === 2
       it "there is only 1 extra holiday in Niedersachsen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         length (EH.holidaysBetween Niedersachsen (jan1 y) (dec31 y)) === 1
       it "there is only Fronleichnam in Hessen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween Hessen (jan1 y) (dec31 y)) === [Fronleichnam]
       it "has holidays for Rheinland-Pfalz" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween RheinlandPfalz (jan1 y) (dec31 y)) === [Fronleichnam, Allerheiligen]
       it "has holidays for Sachsen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween Sachsen (jan1 y) (dec31 y)) === [Reformationstag, BussUndBettag]
       it "has holidays for Schleswig-Holstein" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween SchleswigHolstein (jan1 y) (dec31 y)) === [Reformationstag]
       it "has holidays for Brandenburg" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween Brandenburg (jan1 y) (dec31 y)) === [Reformationstag]
       it "has holidays for Sachsen-Anhalt" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween SachsenAnhalt (jan1 y) (dec31 y)) === [HeiligeDreiKoenige, Reformationstag]
       it "has holidays for Thüringen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween Thueringen (jan1 y) (dec31 y)) === [Fronleichnam, Weltkindertag, Reformationstag]
       it "has holidays for Hamburg" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2024 5000)
         (map snd $ EH.holidaysBetween Hamburg (jan1 y) (dec31 y)) === [Reformationstag]
       it "has holidays for Mecklenburg-Vorpommern" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2023 5000)
         (map snd $ EH.holidaysBetween MecklenburgVorpommern (jan1 y) (dec31 y)) === [InternationalerFrauentag, Reformationstag]
       it "has holidays for Saarland" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2023 5000)
         (map snd $ EH.holidaysBetween Saarland (jan1 y) (dec31 y)) === [Fronleichnam, MariaeHimmelfahrt, Allerheiligen]
       it "has holidays for Bremen" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 2023 5000)
         (map snd $ EH.holidaysBetween Bremen (jan1 y) (dec31 y)) === [Reformationstag]
       it "computes Buss- und Bettag correctly for some years" $ do
         EH.toDay 2024 BussUndBettag `shouldBe` day 2024 11 20
         EH.toDay 2025 BussUndBettag `shouldBe` day 2025 11 19
         EH.toDay 2026 BussUndBettag `shouldBe` day 2026 11 18
         EH.toDay 2033 BussUndBettag `shouldBe` day 2033 11 16
       it "Buss- und Bettag is always between 16. and 22. November (inclusive)" $ hedgehog $ do
         y <- forAll $ Gen.integral (Range.linear 0 5000)
         let (_, m, d) = toGregorian $ EH.toDay y BussUndBettag
         (d >= 16 && d <= 22) === True
         m === 11
     describe "germanHolidayName" $
       it "names are longer than 5 characters for all holidays (which mean there are no non-exhaustive patterns)" $
         all ((>5) . length . EH.germanHolidayName) [minBound .. maxBound :: ExtraHoliday] `shouldBe` True
