# bank-holiday-germany

*See below for a German version.*

This package provides calculation of bank holidays and public holidays
in Germany.

Most of these bank holidays are also public aka legal holidays
throughout Germany. You can use `isPublicHoliday` to check if a
holiday is also a legal holiday.

Legal holidays are generally off for all employees. Bank holidays that
are not legal holidays are generally only off for bank employees.

There are even more public holidays in each federal state which
are (partly) covered by the `ExtraHolidays` module of this package.

See the [module documentation](https://hackage.haskell.org/package/bank-holiday-germany)
on Hackage for more information.


-----

Dieses Modul behandelt deutsche Bankfeiertage und gesetzliche Feiertage.

Bis auf Heilig Abend und Silvester sind alle Bankfeiertage
gleichzeitig gesetzliche Feiertage in allen Bundesländern der
Bundesrepublik Deutschland. Die Funktion `isPublicHoliday` prüft ob
ein Bankfeiertag auch ein gesetzlicher Feiertag ist.

Gesetzliche Feiertage sind Ländersache – abgesehen vom
[Nationalfeiertag](https://www.bmi.bund.de/DE/themen/verfassung/staatliche-symbole/nationale-feiertage/nationale-feiertage-node.html)
*Tag der Deutschen Einheit*.

Bankfeiertage sind in der Regel für Bankangestellte frei.
Gesetzliche Feiertage sind in der Regel für alle Angestellten frei (im
Bundesland für das sie gelten).

Gesetzliche Feiertage der Bundesländer, die nicht gleichzeitig
Bankfeiertage sind, sind im Modul `ExtraHolidays` definiert.

Vorsicht: Manche gesetzliche Feiertage gelten nicht für ein ganzes
Bundesland sondern nur für bestimmte Landkreise, z.B. das Friedensfest
in Augsburg.

Ein Code-Beispiel im Modul `ExtraHolidays` zeigt, wie alle Feiertage
für ein bestimmtes Bundesland berechnet werden können.

## Sample code

Rank federal states by number of holidays:

`test.hs`:

```haskell
import Prelude
import Data.List
import Data.Time
import qualified Data.Time.Calendar.BankHoliday.Germany as BH
import qualified Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays as EH
import Data.Time.Calendar.BankHoliday.Germany (BankHoliday(..))
import Data.Time.Calendar.BankHoliday.Germany.ExtraHolidays (FederalState(..), ExtraHoliday(..))

holidays :: Year -> FederalState -> [Day]
holidays year state = map fst (filter (BH.isPublicHoliday . snd) $ BH.holidaysBetween start end)
                   ++ map fst (EH.holidaysBetween state start end)
  where
    start = fromGregorian year 1 1
    end = fromGregorian year 12 31


-- supported 2024-03-23
supportedFederalStates :: [FederalState]
supportedFederalStates =
  [ BadenWuerttemberg
  , Bayern
  , Berlin
  , Niedersachsen
  , Hessen
  , NordrheinWestfalen
  ]

year :: Year
year = 2024

showPadded :: Int -> String
showPadded n | n < 10 = " " ++ show n
             | otherwise = show n

main :: IO ()
main = putStrLn
         $ unlines
         $ map (\(x, n) -> showPadded n ++ "  " ++ show x)
         $ sortOn ((0-) . snd)
         $ map (\x -> (x, length $ holidays year x))
         $ supportedFederalStates
```

```
$ stack script --resolver=lts-22.0 --package time --package bank-holiday-germany test.hs
14  Bayern
 9  BadenWuerttemberg
 9  Berlin
 9  Niedersachsen
 9  Hessen
 9  NordrheinWestfalen
```

More examples:

- [Bank holidays](https://hackage.haskell.org/package/bank-holiday-germany/docs/Data-Time-Calendar-BankHoliday-Germany.html)
- [Extra holidays](https://hackage.haskell.org/package/bank-holiday-germany/docs/Data-Time-Calendar-BankHoliday-Germany-ExtraHolidays.html)
