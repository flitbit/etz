etz  [![Build Status](https://travis-ci.org/flitbit/etz.png)](http://travis-ci.org/flitbit/etz)
======

Erlang Time and Zone; an Erlang module for ISO 8601 datetime including timezones.

## Features

* Change the current process' timezone
* Generate full resolution datetimes
* Easy interop with calendar module's types and functions
* Parse ISO 8601 formatted datetimes to with full microsecond precision
* Convert erlang timestamps to full precision datetimes for ISO 8601 formatting

## Non-Features

* Timezone names - figuring out a timezone's name is not important to ISO 8601, it deals with math not politics.

## iso_time

Erlang defines a datetime (`calendar:datetime()`) such that:

```erlang
{ {Year,Month,Day}, {Hour,Minute,Second} } = calendar:now_to_local_time(os:timestamp()).

```

`etz` defines `etz:iso_time()` such that:

```erlang
{ { {Year,Month,Day}, {Hour,Minute,Second} }, Microseconds, Timezone } = etz:now(),
{ Sign, TimezoneHours, TimezoneMinutes } = Timezone.
```

## ISO Easy

With either a `calendar:datetime()` or an `etz:iso_time()` you can format for interop with other systems:

```erlang
EtzNow = etz:now(),
<<"2014-04-16T12:58:55.995018-06">> = etz:iso_format(EtzNow),

CalendarTime = calendar:local_time(),
<<"2014-04-16T13:00:59-06">> = etz:iso_format(CalendarTime).
```