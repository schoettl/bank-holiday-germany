
{-|
Description: Calculation of bank holidays in Germany.

This module computes general bank holidays.
Most of these bank holidays are also public aka legal holidays
throughout Germany. You can use 'isPublicHoliday' to check if a
holiday is also a legal holiday.

Note: There are even more public holidays in each federal state which
are not covered by this module.

https://de.wikipedia.org/wiki/Bankfeiertag

-}

module Data.Time.Calendar.BankHoliday.Germany (
    BankHoliday(..),
    isBankHoliday,
    isPublicHoliday,
    calculateEasterSunday,
    holidaysBetween,
    fromDay,
    toDay,
    germanHolidayName
) where

import Data.Time.Calendar
import Data.Maybe

-- | Data type specifying German bank holidays.
--
-- Note: This type cannot be an instance of class 'Ord' because due to
-- Easter day calculation the order can change from year to year.
data BankHoliday
    = NewYearsDay        -- ^ Neujahrstag
    | GoodFriday         -- ^ Karfreitag
    | EasterMonday       -- ^ Ostermontag
    | LabourDay          -- ^ Tag der Arbeit
    | AscensionDay       -- ^ Christi Himmelfahrt
    | WhitMonday         -- ^ Pfingstmontag
    | GermanUnityDay     -- ^ Tag der Deutschen Einheit
    | ChristmasEve       -- ^ Heilig Abend
    | ChristmasDay       -- ^ 1. Weihnachtsfeiertag
    | SecondChristmasDay -- ^ 1. Weihnachtsfeiertag
    | NewYearsEve        -- ^ Silvestertag
    deriving (Enum, Eq, Bounded, Show, Read)


-- | Check if a given day is a 'BankHoliday'.
--
-- >>> isBankHoliday (fromGregorian 2024 1 1)
-- True
isBankHoliday :: Day -> Bool
isBankHoliday = isJust . fromDay

-- | Helper to extract the year from a date.
--
-- >>> yearFromDay $ fromGregorian 2020 1 1
-- 2020
yearFromDay :: Day -> Year
yearFromDay = (\(y, _, _) -> y) . toGregorian

-- | Calculate Easter Sunday using Spencer's algorithm.
calculateEasterSunday :: Year -> Day
calculateEasterSunday year =
    let
        a = year `rem` 19
        b = year `quot` 100
        c = year `rem` 100
        d = b `quot` 4
        e = b `rem` 4
        f = (b + 8) `quot` 25
        g = (b - f + 1) `quot` 3
        h = (19 * a + b - d - g + 15) `rem` 30
        i = c `quot` 4
        k = c `rem` 4
        l = (32 + 2 * e + 2 * i - h - k) `rem` 7
        m = (a + 11 * h + 22 * l) `quot` 451
        n = (h + l - 7 * m + 114) `quot` 31
        o = (h + l - 7 * m + 114) `rem` 31
    in
        fromGregorian year (fromIntegral n) (fromIntegral o + 1)

-- | Compute the date for a given year and bank holiday.
--
-- >>> toDay 2024 LabourDay
-- 2024-05-01
toDay :: Year -> BankHoliday -> Day
toDay year NewYearsDay        = fromGregorian year 1 1
toDay year GoodFriday         = addDays (-2) (calculateEasterSunday year)
toDay year EasterMonday       = addDays 1 (calculateEasterSunday year)
toDay year LabourDay          = fromGregorian year 5 1
toDay year AscensionDay       = addDays 39 (calculateEasterSunday year)
toDay year WhitMonday         = addDays 50 (calculateEasterSunday year)
toDay year GermanUnityDay     = fromGregorian year 10 3
toDay year ChristmasEve       = fromGregorian year 12 24
toDay year ChristmasDay       = fromGregorian year 12 25
toDay year SecondChristmasDay = fromGregorian year 12 26
toDay year NewYearsEve        = fromGregorian year 12 31

-- | Compute 'Maybe' the holiday for a given date.
--
-- >>> fromDay (fromGregorian 2024 1 1)
-- Just NewYearsDay
--
-- >>> fromDay (fromGregorian 2024 5 5)
-- Nothing
fromDay :: Day -> Maybe BankHoliday
fromDay day = listToMaybe $ filter (\d -> day == toDay (yearFromDay day) d) [minBound..maxBound]

-- | Compute pairs of date and holiday from start to end.
--
-- >>> map snd $ holidaysBetween (fromGregorian 2024 12 25) (fromGregorian 2024 12 26)
-- [ChristmasDay,SecondChristmasDay]
holidaysBetween :: Day -> Day -> [(Day, BankHoliday)]
holidaysBetween start end = catMaybes $ map (\d -> (d,) <$> fromDay d) [start..end]

-- | Translate the holiday name to German.
germanHolidayName :: BankHoliday -> String
germanHolidayName d = case d of
  NewYearsDay        -> "Neujahrstag"
  GoodFriday         -> "Karfreitag"
  EasterMonday       -> "Ostermontag"
  LabourDay          -> "Tag der Arbeit"
  AscensionDay       -> "Christi Himmelfahrt"
  WhitMonday         -> "Pfingstmontag"
  GermanUnityDay     -> "Tag der Deutschen Einheit"
  ChristmasEve       -> "Heilig Abend"
  ChristmasDay       -> "1. Weihnachtsfeiertag"
  SecondChristmasDay -> "2. Weihnachtsfeiertag"
  NewYearsEve        -> "Silvestertag"

-- | True only for German public holidays aka legal holidays.
-- Chrismas Eve and New Year's Eve are bank holidays but not public holidays.
isPublicHoliday :: BankHoliday -> Bool
isPublicHoliday ChristmasEve = False
isPublicHoliday NewYearsEve = False
isPublicHoliday _ = True
