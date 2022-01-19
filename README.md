# cineschedule

A command-line Haskell program for checking showtimes and scheduling multiple film screenings during the same day at any [Cineworld](https://www.cineworld.co.uk/) cinema.
The XML file located at https://www.cineworld.co.uk/syndication/all-performances.xml, which is typically tens of thousands of lines long, is downloaded and cached locally, where is is then parsed using [Attoparsec](https://hackage.haskell.org/package/attoparsec).

## Usage

```
./cineschedule [-c cinema] [-d day] [-r]
```

The cinema name is case-insensitive, but must otherwise appear exactly as it is listed on the Cineworld website, including any spaces and/or hyphens.

There are several accepted formats for specifying the day:
* A natural number of days offset: `-d 0`, `-d 1`, `-d 2`, ... (0 is today, 1 is tomorrow, etc)
* `-d today` is an alias for `-d 0` and `-d tomorrow` is an alias for `-d 1`
* `-d Monday` ... `-d Sunday` (specifying the current weekday is equivalent to `-d 0`, not `-d 7`)

Setting `-r` will force-refresh the locally-cached copy of the XML file.

## Notes

* Highly-anticipated new releases and exclusive screenings for Cineworld Unlimited card-holders may be scheduled weeks in advance, but the majority of showings are scheduled only a few days in advance.
* The locally-cached copy of the XML file will be automatically refreshed if there are fewer than 10 films found to ensure it's up-to-date.
* For most screenings, the displayed showtime is actually the start of 20-30 minutes of adverts and trailers that are shown before the film begins. For this reason, the end time that is displayed is not precise, and will be earlier than the film's actual finish. The interval durations should be reasonably reliable, however.
