
{-|
Description: Calculation of bank holidays in Germany.

This module computes general bank holidays.
Most of these bank holidays are also public aka legal holidays
throughout Germany. You can use 'isPublicHoliday' to check if a
holiday is also a legal holiday.

Note: There are even more public holidays in each federal state which
are covered by this module.

You can test this package or just calculate a few bank holidays with GHCi:

@
$ stack ghci --package time --package bank-holiday-germany
ghci> import Data.Time
ghci> import Data.Time.Calendar.BankHoliday.Germany
ghci> isBankHoliday (fromGregorian 2024 5 1)  -- Tag der Arbeit
True
ghci> isPublicHoliday ChristmasEve
False
ghci> holidaysBetween (fromGregorian 2024 12 1) (fromGregorian 2024 12 26)
[(2024-12-24,ChristmasEve),(2024-12-25,ChristmasDay),(2024-12-26,SecondChristmasDay)]
@

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

The example prints all public holidays in Bavaria (Landkreis
Miesbach, Oberbayern) in 2025:


Resources:

Extra holidays are implemented for all 16 federal states. For some
states, we couldn't find official sources; That's why this list
only includes some states.

 - Bankfeiertage: https://de.wikipedia.org/wiki/Bankfeiertag
 - Übersicht: https://de.wikipedia.org/wiki/Gesetzliche_Feiertage_in_Deutschland
 - Weitere Übersicht: https://www.arbeitstage.org/
 - Bayern: https://www.stmi.bayern.de/suv/feiertage/
 - Baden-Württemberg: https://im.baden-wuerttemberg.de/de/service/feiertage
 - Niedersachsen: https://service.niedersachsen.de/portaldeeplink/?tsa_leistung_id=8664664&tsa_sprache=de_DE
 - Hessen: https://innen.hessen.de/buerger-staat/feiertage
 - Rheinland-Pfalz: https://mdi.rlp.de/themen/buerger-und-staat/verfassung-und-verwaltung/sonn-und-feiertagsrecht
 - Brandenburg: https://bravors.brandenburg.de/gesetze/ftg_2003/6
 - Sachsen-Anhalt: https://www.landesrecht.sachsen-anhalt.de/bsst/document/jlr-FeiertGSTrahmen/part/X
 - Thüringen: https://innen.thueringen.de/staats-und-verwaltungsrecht/oeffentliches-recht/feiertagsrecht
 - Hamburg: https://www.hamburg.de/ferien-und-feiertage/
 - Mecklenburg-Vorpommern u. Berlin (Frauentag): https://www.deutsche-rentenversicherung.de/DRV/DE/Ueber-uns-und-Presse/Presse/Meldungen/2024/240306_frauentag_feiertag_frei.html
 - Saarland: https://www.saarland.de/mibs/DE/themen-aufgaben/aufgaben/buerger_und_staat/sonn_u_feiertagsrecht/feiertagsrecht_node.html
 - Bremen: https://www.transparenz.bremen.de/metainformationen/gesetz-ueber-die-sonn-gedenk-und-feiertage-vom-12-november-1954-145882?asl=bremen203_tpgesetz.c.55340.de&template=20_gp_ifg_meta_detail_d
-}

module Data.Time.Calendar.Holiday.Germany (
    Holiday(..),
    FederalState(..),
    isBankHoliday,
    isGermanPublicHoliday,
    isFederalPublicHoliday,
    calculateEasterSunday,
    holidaysBetween,
    fromDay,
    toDay,
    germanHolidayName,
    yearFromDay
) where

import Prelude
import Data.Time.Calendar
import Data.Maybe

-- | Data type specifying German bank holidays including Christmas Eve and New Year's Eve.
--
-- Note: This type cannot be an instance of class 'Ord' because due to
-- Easter day calculation the order can change from year to year.
data Holiday
    = NewYearsDay        -- ^ Neujahrstag
    | GoodFriday         -- ^ Karfreitag
    | EasterMonday       -- ^ Ostermontag
    | LabourDay          -- ^ Tag der Arbeit
    | AscensionDay       -- ^ Christi Himmelfahrt
    | WhitMonday         -- ^ Pfingstmontag
    | GermanUnityDay     -- ^ Tag der Deutschen Einheit
    | ChristmasEve       -- ^ Heilig Abend
    | ChristmasDay       -- ^ 1​. Weihnachtsfeiertag
    | SecondChristmasDay -- ^ 2​. Weihnachtsfeiertag
    | NewYearsEve        -- ^ Silvestertag
    | HeiligeDreiKoenige     -- ^ Heilige Drei Könige (Bayern, Baden-Württemberg, Sachsen-Anhalt)
    | Fronleichnam           -- ^ Fronleichnam (Bayern, Baden-Württemberg, Nordrhein-Westfalen, Hessen, Rheinland-Pfalz, Thüringen*, Saarland)
    | Friedensfest           -- ^ Friedensfest (Bayern*)
    | MariaeHimmelfahrt      -- ^ Mariä Himmelfahrt (Bayern*, Saarland)
    | Allerheiligen          -- ^ Allerheiligen (Bayern, Baden-Württemberg, Nordrhein-Westfalen, Rheinland-Pfalz, Saarland)
    | Reformationstag        -- ^ Reformationstag (Niedersachsen, Sachsen, Schleswig-Holstein, Brandenburg, Sachsen-Anhalt, Thüringen, Hamburg, Mecklenburg-Vorpommern, Bremen)
    | InternationalerFrauentag -- ^ Internationaler Frauentag (Berlin, Mecklenburg-Vorpommern)
    | BussUndBettag          -- ^ Buß- und Bettag (Sachsen)
    | Weltkindertag          -- ^ Weltkindertag (Thüringen)
    deriving (Enum, Eq, Bounded, Show, Read)

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

-- | Check if a given holiday is a bank holiday.
--
-- >>> isBankHoliday ChristmasEve
-- True
isBankHoliday :: Holiday -> Bool
isBankHoliday ChristmasEve = True
isBankHoliday NewYearsEve = True
isBankHoliday holiday = isGermanPublicHoliday holiday

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
toDay :: Year -> Holiday -> Day
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
toDay year HeiligeDreiKoenige      = fromGregorian year 1 6
toDay year Fronleichnam            = addDays 60 $ calculateEasterSunday year
toDay year Friedensfest            = fromGregorian year 8 8
toDay year MariaeHimmelfahrt       = fromGregorian year 8 15
toDay year Allerheiligen           = fromGregorian year 11 1
toDay year InternationalerFrauentag = fromGregorian year 3 8
toDay year Reformationstag          = fromGregorian year 10 31
toDay year BussUndBettag           = calculateBussUndBettag year
toDay year Weltkindertag           = fromGregorian year 9 20

-- | Compute 'Maybe' the holiday for a given date.
--
-- Note: In some years, two bank holidays can fall on the same
-- day. E.g. 'LabourDay' and 'AscensionDay' in 2008 are both on
-- 2008-05-01. In such cases this function returns the bank holiday
-- that is defined first in the 'BankHoliday' 'Enum'.
--
-- >>> fromDay (fromGregorian 2024 1 1)
-- [NewYearsDay]
--
-- >>> fromDay (fromGregorian 2024 5 5)
-- []
fromDay :: Day -> [Holiday]
fromDay day = filter (\d -> day == toDay (yearFromDay day) d) [minBound..maxBound]

-- | Compute pairs of date and holiday from start to end (inclusive).
--
-- Note: In some years, two bank holidays can fall on the same
-- day. In such cases only one of them is in the resulting list.
-- See 'fromDay' for more information.
--
-- >>> map snd $ holidaysBetween (fromGregorian 2024 12 25) (fromGregorian 2024 12 26)
-- [ChristmasDay,SecondChristmasDay]
holidaysBetween :: Day -> Day -> [(Day, Holiday)]
holidaysBetween start end = concat $ map (\d -> map (d,) $ fromDay d) [start..end]

-- | Translate the holiday name to German.
germanHolidayName :: Holiday -> String
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
  HeiligeDreiKoenige     -> "Heilige Drei Könige"
  Fronleichnam           -> "Fronleichnam"
  Friedensfest           -> "Friedensfest"
  MariaeHimmelfahrt      -> "Mariä Himmelfahrt"
  Allerheiligen          -> "Allerheiligen"
  Reformationstag          -> "Reformationstag"
  InternationalerFrauentag -> "Internationaler Frauentag"
  BussUndBettag            -> "Buß- und Bettag"
  Weltkindertag            -> "Weltkindertag"

-- | True only for German public holidays aka legal holidays.
-- Christmas Eve and New Year's Eve are bank holidays but not public holidays.
isGermanPublicHoliday :: Holiday -> Bool
isGermanPublicHoliday NewYearsDay = True
isGermanPublicHoliday GoodFriday = True
isGermanPublicHoliday EasterMonday = True
isGermanPublicHoliday LabourDay = True
isGermanPublicHoliday AscensionDay = True
isGermanPublicHoliday WhitMonday = True
isGermanPublicHoliday GermanUnityDay = True
isGermanPublicHoliday ChristmasDay = True
isGermanPublicHoliday SecondChristmasDay = True
isGermanPublicHoliday _ = False

-- | Check if 'Holiday' is a holiday in the given federal state.
--
-- Note: Internationaler Frauentag is a holiday in Berlin (since 2019)
-- and Mecklenburg-Vorpommern (since 2023).
-- However this function doesn't take the year into account and hence
-- is incorrect for earlier years.
--
-- >>> isFederalPublicHoliday Bayern Allerheiligen
-- True
--
-- >>> isFederalPublicHoliday Berlin Allerheiligen
-- False
isFederalPublicHoliday :: FederalState -> Holiday -> Bool
isFederalPublicHoliday BadenWuerttemberg HeiligeDreiKoenige = True
isFederalPublicHoliday BadenWuerttemberg Fronleichnam = True
isFederalPublicHoliday BadenWuerttemberg Allerheiligen = True
isFederalPublicHoliday Bayern HeiligeDreiKoenige = True
isFederalPublicHoliday Bayern Fronleichnam = True
isFederalPublicHoliday Bayern Friedensfest = True
isFederalPublicHoliday Bayern MariaeHimmelfahrt = True
isFederalPublicHoliday Bayern Allerheiligen = True
isFederalPublicHoliday Berlin InternationalerFrauentag = True
isFederalPublicHoliday NordrheinWestfalen Fronleichnam = True
isFederalPublicHoliday NordrheinWestfalen Allerheiligen = True
isFederalPublicHoliday Niedersachsen Reformationstag = True
isFederalPublicHoliday Hessen Fronleichnam = True
isFederalPublicHoliday RheinlandPfalz Allerheiligen = True
isFederalPublicHoliday RheinlandPfalz Fronleichnam = True
isFederalPublicHoliday Sachsen Reformationstag = True
isFederalPublicHoliday Sachsen BussUndBettag = True
isFederalPublicHoliday SchleswigHolstein Reformationstag = True
isFederalPublicHoliday Brandenburg Reformationstag = True
isFederalPublicHoliday SachsenAnhalt HeiligeDreiKoenige = True
isFederalPublicHoliday SachsenAnhalt Reformationstag = True
isFederalPublicHoliday Thueringen Fronleichnam = True
isFederalPublicHoliday Thueringen Reformationstag = True
isFederalPublicHoliday Thueringen Weltkindertag = True
isFederalPublicHoliday Hamburg Reformationstag = True
isFederalPublicHoliday MecklenburgVorpommern Reformationstag = True
isFederalPublicHoliday MecklenburgVorpommern InternationalerFrauentag = True
isFederalPublicHoliday Saarland Fronleichnam = True
isFederalPublicHoliday Saarland Allerheiligen = True
isFederalPublicHoliday Saarland MariaeHimmelfahrt = True
isFederalPublicHoliday Bremen Reformationstag = True
isFederalPublicHoliday _ holiday = isGermanPublicHoliday holiday

-- | Calculate Buß- und Bettag.
--
-- https://de.wikipedia.org/wiki/Bu%C3%9F-_und_Bettag
calculateBussUndBettag :: Year -> Day
calculateBussUndBettag year =
   let november23 = fromGregorian year 11 23
       weekDay = dayOfWeek november23
    in if weekDay <= Wednesday
         then addDays (toInteger $ fromEnum Wednesday - fromEnum weekDay - 7) november23
         else addDays (toInteger $ fromEnum Wednesday - fromEnum weekDay) november23
