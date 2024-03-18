# bank-holiday-germany

*See below for a German version.*

This package provides calculation of bank holidays and public holidays
in Germany.

Most of these bank holidays are also public aka legal holidays
throughout Germany. You can use `isPublicHoliday` to check if a
holiday is also a legal holiday.

Legal holidays are generally off for all employees. Bank holidays that
are not legal holidays as well are generally only off for bank employees.

There are even more public holidays in each federal state which
are (partly) covered by the `ExtraHolidays` module of this package.

See the module documentation for more information.

-----

Dieses Modul behandelt deutsche Bankfeiertage und gesetzliche Feiertage.

Bis auf Heilig Abend und Silvester sind alle Bankfeiertage
gleichzeitig gesetzliche Feiertage in allen Bundesländern der
Bundesrepublik Deutschland. Die Funktion `isPublicHoliday` prüft ob
ein Bankfeiertag auch ein gesetzlicher Feiertag ist.

Gesetzliche Feiertage sind Ländersache – abgesehen vom
[Nationalfeiertag](https://www.bmi.bund.de/DE/themen/verfassung/staatliche-symbole/nationale-feiertage/nationale-feiertage-node.html)
*Tag der deutschen Einheit*.

Bankfeiertage sind in der Regel für Bankangestellte frei.
Gesetzliche Feiertage sind in der Regel für alle Angestellten frei (im
Bundesland für das sie gelten).

Gesetzliche Feiertage der Bundesländer, die nicht gleichzeitig
Bankfeiertage sind, sind im Modul `ExtraHolidays` definiert.

Vorsicht: Manche gesetzliche Feiertage gelten nicht für das ganze
Bundesland sondern nur für bestimmte Landkreise, z.B. das Friedensfest
in Augsburg.

Ein Code-Beispiel im Modul `ExtraHolidays` zeigt, wie alle Feiertage
für ein bestimmtes Bundesland berechnet werden können.
