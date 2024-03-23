
{-|

This module provides additional German public holidays that are not
covered by the [bank holidays]("Data.Time.Calendar.BankHoliday.Germany").

Public holidays – except for
'Data.Time.Calendar.BankHoliday.Germany.GermanUnityDay' – are under
federal obligations in Germany („Ländersache“).

Most bank holidays are also federal public holidays
(see 'Data.Time.Calendar.BankHoliday.Germany.isPublicHoliday').
But there are some additional extra holidays which may differ between
federal states.

For example, Heilige Drei Könige is not a bank holiday but it is a
public holiday in Bavaria.

Note: The extra holidays are currently only implemented for
Baden-Württemberg, Bayern, Berlin, Niedersachsen, Hessen, and
Nordrhein-Westfalen.

Example for computing all public holidays in Bavaria (Landkreis
Miesbach, Oberbayern) in the next couple years:

@
import Prelude
import Data.List
import Data.Time
import qualified Data.Time.Calendar.BankHoliday.Germany as BH
import qualified Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays as EH

start = fromGregorian 2025 1 1

end = fromGregorian 2025 12 31

holidays :: [[String]]
holidays = map (\(x,y) -> [show x, BH.germanHolidayName y]) (filter (BH.isPublicHoliday . snd) $ BH.holidaysBetween start end)
        ++ map (\(x,y) -> [show x, EH.germanHolidayName y]) (filter ((/= EH.Friedensfest) . snd) $ EH.holidaysBetween EH.Bayern start end)

main :: IO ()
main = putStrLn $ unlines $ sort $ map unwords holidays
@

@
2025-01-01 Neujahrstag
2025-01-06 Heilige Drei Könige
2025-04-18 Karfreitag
2025-04-21 Ostermontag
2025-05-01 Tag der Arbeit
2025-05-29 Christi Himmelfahrt
2025-06-09 Pfingstmontag
2025-06-19 Fronleichnam
2025-08-15 Mariä Himmelfahrt
2025-10-03 Tag der Deutschen Einheit
2025-11-01 Allerheiligen
2025-12-25 1. Weihnachtsfeiertag
2025-12-26 2. Weihnachtsfeiertag
@

Resources:

 - Übersicht: https://de.wikipedia.org/wiki/Gesetzliche_Feiertage_in_Deutschland
 - Weitere Übersicht: https://www.arbeitstage.org/
 - Bayern: https://www.stmi.bayern.de/suv/feiertage/
 - Baden-Württemberg: https://im.baden-wuerttemberg.de/de/service/feiertage
 - Niedersachsen: https://service.niedersachsen.de/portaldeeplink/?tsa_leistung_id=8664664&tsa_sprache=de_DE
 - Hessen: https://innen.hessen.de/buerger-staat/feiertage

-}

module Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays (
    ExtraHoliday(..),
    FederalState(..),
    holidaysBetween,
    fromDay,
    toDay,
    germanHolidayName,
    isHolidayInState
) where

import Prelude
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.BankHoliday.Germany (calculateEasterSunday, yearFromDay)

-- | Germany's federal states – Deutsche Bundesländer.
data FederalState
  = BadenWuerttemberg
  | Bayern
  | Berlin
  | Brandenburg
  | Bremen
  | Hamburg
  | Hessen
  | MecklenburgVorpommern
  | Niedersachsen
  | NordrheinWestfalen
  | RheinlandPfalz
  | Saarland
  | Sachsen
  | SachsenAnhalt
  | SchleswigHolstein
  | Thueringen
  deriving (Enum, Eq, Bounded, Show, Read)

-- TODO: Remove note below when all federal states are fully implemented.

-- | Extra federal holidays, no overlap with
-- 'Data.Time.Calendar.BankHoliday.Germany.BankHoliday'.
-- Spezielle Feiertage der Bundesländer.
--
-- Note: Currently, only some federal states' extra holidays are implemented.
-- See module description above for details.
--
-- \*regional holiday, only applies in parts of the federal state
data ExtraHoliday
  = HeiligeDreiKoenige     -- ^ Heilige Drei Könige (Bayern, Baden-Württemberg, …)
  | Fronleichnam           -- ^ Fronleichnam (Bayern, Baden-Württemberg, Nordrhein-Westfalen, Hessen, …)
  | Friedensfest           -- ^ Friedensfest (Bayern*, …)
  | MariaeHimmelfahrt      -- ^ Mariä Himmelfahrt (Bayern*, …)
  | Allerheiligen          -- ^ Allerheiligen (Bayern, Baden-Württemberg, Nordrhein-Westfalen, …)
  | Reformationstag        -- ^ Reformationstag (Niedersachsen, …)
  | InternationalerFrauentag -- ^ Internationaler Frauentag (Berlin, …)
  deriving (Enum, Eq, Bounded, Show, Read)

-- | Compute the date for a given year and extra holiday.
--
-- >>> toDay 2024 HeiligeDreiKoenige
-- 2024-01-06
toDay :: Year -> ExtraHoliday -> Day
toDay year HeiligeDreiKoenige      = fromGregorian year 1 6
toDay year Fronleichnam            = addDays 60 $ calculateEasterSunday year
toDay year Friedensfest            = fromGregorian year 8 8
toDay year MariaeHimmelfahrt       = fromGregorian year 8 15
toDay year Allerheiligen           = fromGregorian year 11 1
toDay year InternationalerFrauentag = fromGregorian year 3 8
toDay year Reformationstag          = fromGregorian year 10 31

-- | Compute 'Maybe' the holiday for a given date.
--
-- Note: In some years, two extra holidays may fall on the same
-- day. In such cases this function returns the holiday
-- that is defined first in the 'ExtraHoliday' 'Enum'.
--
-- >>> fromDay (fromGregorian 2024 11 1)
-- Just Allerheiligen
--
-- >>> fromDay (fromGregorian 2024 5 5)
-- Nothing
fromDay :: Day -> Maybe ExtraHoliday
fromDay day = listToMaybe $ filter (\d -> day == toDay (yearFromDay day) d) [minBound..maxBound]

-- | Compute pairs of date and holiday from start to end (inclusive) for the given federal state.
--
-- >>> map snd $ holidaysBetween Bayern (fromGregorian 2024 8 8) (fromGregorian 2024 8 15)
-- [Friedensfest,MariaeHimmelfahrt]
holidaysBetween :: FederalState -> Day -> Day -> [(Day, ExtraHoliday)]
holidaysBetween state start end = filter (isHolidayInState state . snd) $ catMaybes $ map (\d -> (d,) <$> fromDay d) [start..end]

-- | Translate the holiday name to German.
germanHolidayName :: ExtraHoliday -> String
germanHolidayName d = case d of
  HeiligeDreiKoenige     -> "Heilige Drei Könige"
  Fronleichnam           -> "Fronleichnam"
  Friedensfest           -> "Friedensfest"
  MariaeHimmelfahrt      -> "Mariä Himmelfahrt"
  Allerheiligen          -> "Allerheiligen"
  Reformationstag          -> "Reformationstag"
  InternationalerFrauentag -> "Internationaler Frauentag"

-- | Check if 'ExtraHoliday' is a holiday in the given federal state.
--
-- >>> isHolidayInState Bayern Allerheiligen
-- True
--
-- >>> isHolidayInState Berlin Allerheiligen
-- False
isHolidayInState :: FederalState -> ExtraHoliday -> Bool
isHolidayInState BadenWuerttemberg HeiligeDreiKoenige = True
isHolidayInState BadenWuerttemberg Fronleichnam = True
isHolidayInState BadenWuerttemberg Allerheiligen = True
isHolidayInState Bayern HeiligeDreiKoenige = True
isHolidayInState Bayern Fronleichnam = True
isHolidayInState Bayern Friedensfest = True
isHolidayInState Bayern MariaeHimmelfahrt = True
isHolidayInState Bayern Allerheiligen = True
isHolidayInState Berlin InternationalerFrauentag = True
isHolidayInState NordrheinWestfalen Fronleichnam = True
isHolidayInState NordrheinWestfalen Allerheiligen = True
isHolidayInState Niedersachsen Reformationstag = True
isHolidayInState Hessen Fronleichnam = True
isHolidayInState _ _ = False
