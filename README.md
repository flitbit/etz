etz  [![Build Status](https://travis-ci.org/flitbit/etz.png)](http://travis-ci.org/flitbit/etz)
======

Erlang time module to ease interoperability in a web-world.

## Features

* Full resolution times with timezone
* Round-trip ISO 8601 parsing and formatting, with fractional seconds and timezone info
* Convert to and from `calendar:datetime()`
* Convert a datetime from one timezone to another
* Manage ambient timezone

## Non-Features

* Timezone names - figuring out a timezone's name is not important to ISO 8601, it deals with math not politics.
* Non ISO 8601 datetime formats - look to [`qdate`](https://github.com/choptastic/qdate) or ['erlware_commons'](https://github.com/erlware/erlware_commons) for those.

## Change Log

* `0.1`, 2014-04-16 - Initial. Feedback, issues, and pull-requests welcome.

## iso_time

Erlang defines `calendar:datetime()` such that:

```erlang
{ {Year,Month,Day}, {Hour,Minute,Second} } = calendar:now_to_local_time(os:timestamp()).

```

`etz` defines `etz:iso_time()` such that:

```erlang
{{ {Year,Month,Day}, {Hour,Minute,Second} }, Microseconds, Timezone } = etz:now(),
{ Sign, TimezoneHours, TimezoneMinutes } = Timezone.
```

Note that what we care about in relation to a timezone is its offset in hours and minutes from Coordinated Universal Time (UTC). With this knowledge there is no ambiguity about what instant in time a particular `iso_time` instance references.

## ISO Easy

Format either an `iso_time` or a `datetime` for interop with other systems:

```erlang
EtzNow = etz:now(),
<<"2014-04-16T12:58:55.995018-06">> = etz:iso_format(EtzNow),

CalendarTime = calendar:local_time(),
<<"2014-04-16T12:58:55-06">> = etz:iso_format(CalendarTime).
```

Parse ISO 8601 formatted strings:

```erlang
{ok,{{{2014,4,16},{12,58,55}},995018,{'-',6,0}}} = etz:iso_parse(<<"2014-04-16T12:58:55.995018-06">>).
```

## Manage Ambient Timezone

Erlang associates the operating system's local timezone with each process and provides functions to convert between the local timezone and universal time. It doesn't help much when working with timezones other than local. `etz` adds the notion of an ambient timezone to your process which will be used in place of the local timezone when performing time and timezone math.

Like ISO 8601, `etz` is concerned with identifying points in time. It does so by recording a timezone offset (from Coordinated Universal Time) along with each date-time so that there is no ambiguity about the recorded instant.

### Ambient Timezone Functions

* `local_timezone()` - Gets the local machine's timezone if reported by the operating system otherwise UTC
* `use_timezone(Timezone)` - Uses the specified `Timezone` until reverted or replaced by a subsequent `use_timezone` call
* `current_timezone()` - Gets the current ambient timezone. This will be same as `local_timezone()` if unset or reverted
* `revert_timezone()` - Reverts the timezone to local

## DateTime Functions

* `universal_now()` - Gets the current universal time
* `now()` - Gets the current time in the current ambient timezone
* `timestamp_to_iso_time(Timestamp)` - Converts the specified `timestamp` to an `iso_time` in the current ambient timezone
* `timestamp_to_iso_time(Timestamp,Timezone)` - Converts the specified `timestamp` to an `iso_time` in the specified `Timezone`
* `datetime_to_iso_time(DateTime)` - Converts the specified `datetime` to an `iso_time` in the current ambient timezone
* `datetime_to_iso_time(DateTime,Timezone)` - Converts the specified `datetime` to an `iso_time` in the specified `Timezone`
* `to_timezone(IsoTime, Timezone)` - Converts the specified `iso_time` to the corresponding time in the specified `Timezone`
* `to_universal(IsoTime)` - Converts the specified `iso_time` to the corresponding universal time

## Format Functions

* `iso_format_universal(DateTime)` - Formats the specified `datetime` as a universal datetime
* `iso_format(DateTime)` - Formats the specified `datetime` in ISO 8601 format, according to the ambient timezone
* `iso_format(IsoTime)` - Formats the specified `iso_time` in ISO 8601 format

## Parse Function

* `iso_parse(Input)` - Parses an ISO 8601 string and constructs the corresponding `iso_time`. If the formatted date does not contain timestamp info it is assumed to be in the ambient timezone.

The parsing is implemented as a validating parser and will fail-fast if the input contains invalid characters or the resulting date would be invalid.

# More

There are a lot of insightful tests under in `test/etz_tests.erl`; please have a look.

Run the tests in bash:

```bash
make test
```

Since this is brand spanking new on `2014-04-16`, and since I'm relatively new to Erlang, I welcome any feedback, issues, and pull requests (especially those with more tests or integrations).

Peace.