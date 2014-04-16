-module(etz_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{ foreach,
		fun setup/0,
		fun cleanup/1,
		[ {"now: represents equivalent local os datetime",
				fun() ->
						% only compare the datetime part; they should both
						% execute in the same second unless the test machine
						% is extraordinarily slow.
						{DateTime,_,_} = etz:now(),
						Now = os:timestamp(),
						DateTime = calendar:now_to_local_time(Now)
				end},

			{"now: calculates time using local timezone",
				fun() ->
						LocalTz = etz:local_timezone(),
						{_,_,LocalTz} = etz:now()
				end},

			{"universal_now: is not the same as now (unless local timezone is UTC)",
				fun() ->
						{LDateTime,_,{_,TzH,TzM}} = etz:now(),
						{UDateTime,_,{'+',0,0}} = etz:universal_now(),
						LSeconds = calendar:datetime_to_gregorian_seconds(LDateTime),
						USeconds = calendar:datetime_to_gregorian_seconds(UDateTime),
						?_assert(LSeconds =/= USeconds orelse (TzH =:= 0 andalso TzM =:= 0))
				end},


			{"use_timezone: establishes new timezone for time calculations",
				fun() ->
						LocalTz = etz:local_timezone(),
						%% Times use the local timezone...
						{_,_,Tz} = etz:now(),
						?assertEqual(LocalTz, Tz),
						{Sgn,H,M} = LocalTz,
						case H of
							UB when UB >= 23 ->
								NewTz = {Sgn, 2, M};
							LB when LB < 2 ->
								NewTz = {Sgn, H + 2, M};
							_ ->
								NewTz = {Sgn, H + 1, M}
						end,
						%% establish a new timezone for the process
						ok = etz:use_timezone(NewTz),
						?assertNotEqual(LocalTz, etz:current_timezone()),
						%% Times now use the updated timezone
						{_,_,UpdatedTz} = etz:now(),
						?assertEqual(NewTz, UpdatedTz)
				end},


			{"revert_timezone: reverts custom timezone to local",
				fun() ->
						LocalTz = etz:local_timezone(),
						%% Times use the local timezone...
						{_,_,Tz} = etz:now(),
						?assertEqual(LocalTz, Tz),
						{Sgn,H,M} = LocalTz,
						case H of
							UB when UB >= 23 ->
								NewTz = {Sgn, 2, M};
							LB when LB < 2 ->
								NewTz = {Sgn, H + 2, M};
							_ ->
								NewTz = {Sgn, H + 1, M}
						end,
						%% establish a new timezone for the process
						ok = etz:use_timezone(NewTz),
						?assertNotEqual(LocalTz, etz:current_timezone()),
						{_,_,UpdatedTz} = etz:now(),
						%% Times now use the updated timezone
						?assertEqual(NewTz, UpdatedTz),
						%% revert to the local timezone
						ok = etz:revert_timezone(),
						{_,_,RevertedTz} = etz:now(),
						?assertEqual(LocalTz, RevertedTz)
				end}
			]
		}.

iso_parse_test_() ->
	{
		setup,
		fun setup/0,
		fun cleanup/1,
		{ inorder,
			[
				{ "parses year",
					?_test(begin ?assertMatch({ok, {{{0404,_,_},_}, _, _}},        etz:iso_parse("0404"))
						end)},
				{ "errors on invalid year",
					?_test(begin ?assertMatch({error, {invalid_year, _}},          etz:iso_parse("Y2K"))
						end)},
				{ "errors on 2 character year",
					?_test(begin ?assertMatch({error, {invalid_year, _}},          etz:iso_parse("88-12-04"))
						end)},
				{ "errors on 3 character year",
					?_test(begin ?assertMatch({error, {invalid_year, _}},          etz:iso_parse("404-11-03T10:12:59"))
						end)},

				{ "parses YYYYMM",
					?_test(begin ?assertMatch({ok, {{{1904,9,_},_}, _, _}},        etz:iso_parse("190409"))
						end)},
				{ "parses YYYY-MM",
					?_test(begin ?assertMatch({ok, {{{2093,5,_},_}, _, _}},        etz:iso_parse("2093-05"))
						end)},
				{ "errors on invalid month",
					?_test(begin ?assertMatch({error, {invalid_month_or_week, _}}, etz:iso_parse("2093-M"))
						end)},
				{ "errors on invalid month",
					?_test(begin ?assertMatch({error, {invalid_month_or_week, _}}, etz:iso_parse("2093-m07"))
						end)},
				{ "errors on 1 digit month",
					?_test(begin ?assertMatch({error, {invalid_month_or_week, _}}, etz:iso_parse("2093-5"))
						end)},
				{ "errors on month beyond lower bound",
					?_test(begin ?assertMatch({error, {invalid_month, _}},         etz:iso_parse("2093-00"))
						end)},
				{ "errors on month beyond upper bound",
					?_test(begin ?assertMatch({error, {invalid_month, _}},         etz:iso_parse("2000-13"))
						end)},

				{ "parses YYYYMMDD",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},_}, _, _}},       etz:iso_parse("19840922"))
						end)},
				{ "parses YYYY-MM-DD",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},_}, _, _}},       etz:iso_parse("1984-09-22"))
						end)},
				{ "errors on invalid day",
					?_test(begin ?assertMatch({error, {invalid_day_of_month, _}},  etz:iso_parse("1984-02-D4"))
						end)},
				{ "errors on one character day",
					?_test(begin ?assertMatch({error, {invalid_day_of_month, _}},  etz:iso_parse("1984-02-4"))
						end)},
				{ "errors on day beyond lower bound",
					?_test(begin ?assertMatch({error, {invalid_day_of_month, _}},  etz:iso_parse("20931100"))
						end)},
				{ "errors on day beyond last day of month (Feb 29, non-leap-year)",
					?_test(begin ?assertMatch({error, {invalid_day_of_month, _}},  etz:iso_parse("2001-02-29"))
						end)},
				{ "errors on day beyond last day of month (Jun 31)",
					?_test(begin ?assertMatch({error, {invalid_day_of_month, _}},  etz:iso_parse("2001-06-31"))
						end)},

				{ "parses YYYYMMDDTHH",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,_,_}}, _, _}}, etz:iso_parse("19840922T13"))
						end)},
				{ "parses YYYY-MM-DDTHH",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,_,_}}, _, _}}, etz:iso_parse("1984-09-22T13"))
						end)},
				{ "errors on invalid hour",
					?_test(begin ?assertMatch({error, {invalid_hour, _}},           etz:iso_parse("1984-02-04H1"))
						end)},
				{ "errors on invalid hour",
					?_test(begin ?assertMatch({error, {invalid_hour, _}},           etz:iso_parse("1984-02-04Th1"))
						end)},
				{ "errors on invalid hour",
					?_test(begin ?assertMatch({error, {invalid_hour, _}},           etz:iso_parse("1984-02-04T4PM"))
						end)},
				{ "errors on one character hour",
					?_test(begin ?assertMatch({error, {invalid_hour, _}},           etz:iso_parse("1984-02-04T9:32"))
						end)},
				{ "errors on hour beyond upper bound",
					?_test(begin ?assertMatch({error, {invalid_hour, _}},           etz:iso_parse("20931101T25"))
						end)},
				{ "parses YYYY-MM-DDT24 midnight",
					?_test(begin ?assertMatch({ok, {{{1999,12,31},{24,_,_}}, _, _}},  etz:iso_parse("1999-12-31T24"))
						end)},
				{ "parses YYYY-MM-DDT00 midnight",
					?_test(begin ?assertMatch({ok, {{{2000,1,1},{00,_,_}}, _, _}},  etz:iso_parse("2000-01-01T00"))
						end)},


				{ "parses YYYYMMDDTHHNN",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,_}}, _, _}}, etz:iso_parse("19840922T1334"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,_}}, _, _}}, etz:iso_parse("1984-09-22T13:34"))
						end)},
				{ "errors on invalid minute",
					?_test(begin ?assertMatch({error, {invalid_minute, _}},          etz:iso_parse("1984-02-04T04M4"))
						end)},
				{ "errors on invalid minute",
					?_test(begin ?assertMatch({error, {invalid_minute, _}},          etz:iso_parse("1984-02-04T124m"))
						end)},
				{ "errors on one character minute",
					?_test(begin ?assertMatch({error, {invalid_minute, _}},          etz:iso_parse("1984-02-04T09:3"))
						end)},
				{ "errors on minute beyond upper bound",
					?_test(begin ?assertMatch({error, {invalid_minute, _}},          etz:iso_parse("20931101T11:60"))
						end)},
				{ "parses YYYY-MM-DDT24:00 midnight",
					?_test(begin ?assertMatch({ok, {{{1999,12,31},{24,0,_}}, _, _}}, etz:iso_parse("1999-12-31T24:00"))
						end)},
				{ "parses YYYY-MM-DDT00:00 midnight",
					?_test(begin ?assertMatch({ok, {{{2000,1,1},{0,0,_}}, _, _}},    etz:iso_parse("2000-01-01T00:00"))
						end)},
				{ "error when minutes beyond midnight YYYY-MM-DDT24:01",
					?_test(begin ?assertMatch({error, {invalid_minute, _}},          etz:iso_parse("2000-01-01T24:01"))
						end)},


				{ "parses YYYYMMDDTHHNNSS",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, _, _}}, etz:iso_parse("19840922T133427"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SS",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, _, _}}, etz:iso_parse("1984-09-22T13:34:27"))
						end)},
				{ "errors on invalid second",
					?_test(begin ?assertMatch({error, {invalid_second, _}},          etz:iso_parse("1984-02-04T0404s4"))
						end)},
				{ "errors on invalid second",
					?_test(begin ?assertMatch({error, {invalid_second, _}},          etz:iso_parse("1984-02-04T12404S"))
						end)},
				{ "errors on one character second",
					?_test(begin ?assertMatch({error, {invalid_second, _}},          etz:iso_parse("1984-02-04T09:30:2"))
						end)},
				{ "errors on second beyond upper bound",
					?_test(begin ?assertMatch({error, {invalid_second, _}},          etz:iso_parse("20931101T11:10:65"))
						end)},
				{ "parses YYYY-MM-DDT24:00:00 midnight",
					?_test(begin ?assertMatch({ok, {{{1999,12,31},{24,0,0}}, _, _}}, etz:iso_parse("1999-12-31T24:00:00"))
						end)},
				{ "parses YYYY-MM-DDT00:00:00 midnight",
					?_test(begin ?assertMatch({ok, {{{2000,1,1},{0,0,_}}, _, _}},    etz:iso_parse("2000-01-01T00:00:00"))
						end)},
				{ "error when seconds beyond midnight YYYY-MM-DDT24:00:01",
					?_test(begin ?assertMatch({error, {invalid_second, _}},          etz:iso_parse("2000-01-01T24:00:01"))
						end)},

				{ "parses YYYYMMDDTHHNNSS.f",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 900000, _}}, etz:iso_parse("19840922T133427.9"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 120000, _}}, etz:iso_parse("19840922T133427.12"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.fff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 654000, _}}, etz:iso_parse("19840922T133427.654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 765400, _}}, etz:iso_parse("19840922T133427.7654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.fffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 876540, _}}, etz:iso_parse("19840922T133427.87654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ffffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 987654, _}}, etz:iso_parse("19840922T133427.987654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ffffffff",
					?_test(begin
							%% Rounded to microseconds
							?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 765432, _}}, etz:iso_parse("19840922T133427.7654321"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,f",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 900000, _}}, etz:iso_parse("19840922T133427,9"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,ff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 120000, _}}, etz:iso_parse("19840922T133427,12"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,fff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 654000, _}}, etz:iso_parse("19840922T133427,654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,ffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 765400, _}}, etz:iso_parse("19840922T133427,7654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,fffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 876540, _}}, etz:iso_parse("19840922T133427,87654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,ffffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 987654, _}}, etz:iso_parse("19840922T133427,987654"))
						end)},
				{ "parses YYYYMMDDTHHNNSS,ffffffff",
					?_test(begin
							%% Rounded to microseconds
							?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 765432, _}},            etz:iso_parse("19840922T133427.7654321"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SS.ffffff",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 123456, _}}, etz:iso_parse("1984-09-22T13:34:27.123456"))
						end)},
				{ "errors on invalid fractional second",
					?_test(begin ?assertMatch({error, {invalid_fractional_seconds, _}},    etz:iso_parse("1984-02-04T040404.f"))
						end)},
				{ "parses YYYY-MM-DDT24:00:00.0 midnight",
					?_test(begin ?assertMatch({ok, {{{1999,12,31},{24,0,0}}, 0, _}},       etz:iso_parse("1999-12-31T24:00:00.0"))
						end)},
				{ "parses YYYY-MM-DDT00:00:00.0 midnight",
					?_test(begin ?assertMatch({ok, {{{2000,1,1},{0,0,0}}, 0, _}},          etz:iso_parse("2000-01-01T00:00:00.0"))
						end)},
				{ "error when fractional seconds beyond midnight YYYY-MM-DDT24:00:00.000001",
					?_test(begin ?assertMatch({error, {invalid_fractional_seconds, _}},    etz:iso_parse("2000-01-01T24:00:00.000001"))
						end)},

				{ "defaults to local timezone when timezone not specified",
					?_test(
						begin
							Tz = etz:local_timezone(),
							?assertMatch({ok, {{{2014,4,16},{0,0,0}}, 0, Tz}},          etz:iso_parse("2014-04-16"))
						end)},

				{ "parses YYYY-MM-DDTHHZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,0,0}}, 0, {'+',0,0}}}, etz:iso_parse("1984-09-22T13Z"))
						end)},
				{ "parses YYYY-MM-DDTHH:NNZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,0}}, 0, {'+',0,0}}}, etz:iso_parse("1984-09-22T13:34Z"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SSZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 0, {'+',0,0}}}, etz:iso_parse("1984-09-22T13:34:27Z"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SS.ffffffZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 123456, {'+',0,0}}}, etz:iso_parse("1984-09-22T13:34:27.123456Z"))
						end)},

				{ "parses YYYYMMDDTHHZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,0,0}}, 0, {'+',0,0}}}, etz:iso_parse("19840922T13Z"))
						end)},
				{ "parses YYYYMMDDTHHNNZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,0}}, 0, {'+',0,0}}}, etz:iso_parse("19840922T1334Z"))
						end)},
				{ "parses YYYYMMDDTHHNNSSZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 0, {'+',0,0}}}, etz:iso_parse("19840922T133427Z"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ffffffZ",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 123456, {'+',0,0}}}, etz:iso_parse("19840922T133427.123456Z"))
						end)},

				{ "parses YYYY-MM-DDTHH-02:20",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,0,0}}, 0, {'-',2,20}}}, etz:iso_parse("1984-09-22T13-02:20"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN-02:20",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,0}}, 0, {'-',2,20}}}, etz:iso_parse("1984-09-22T13:34-02:20"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SS-02:20",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 0, {'-',2,20}}}, etz:iso_parse("1984-09-22T13:34:27-02:20"))
						end)},
				{ "parses YYYY-MM-DDTHH:NN:SS.ffffff-02:20",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 123456, {'-',2,20}}}, etz:iso_parse("1984-09-22T13:34:27.123456-02:20"))
						end)},

				{ "parses YYYYMMDDTHH+03:30",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,0,0}}, 0, {'+',3,30}}}, etz:iso_parse("19840922T13+03:30"))
						end)},
				{ "parses YYYYMMDDTHHNN+03:30",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,0}}, 0, {'+',3,30}}}, etz:iso_parse("19840922T1334+03:30"))
						end)},
				{ "parses YYYYMMDDTHHNNSS+03:30",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 0, {'+',3,30}}}, etz:iso_parse("19840922T133427+03:30"))
						end)},
				{ "parses YYYYMMDDTHHNNSS.ffffff+03:30",
					?_test(begin ?assertMatch({ok, {{{1984,9,22},{13,34,27}}, 123456, {'+',3,30}}}, etz:iso_parse("19840922T133427.123456+03:30"))
						end)},


				{ "parsed datetime is understood by calendar module",
					?_test(
						begin
							{ok, {DateTime,_,_}} = etz:iso_parse("19840922T133427.123456+03:30"),
							Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
							Value = calendar:gregorian_seconds_to_datetime(Seconds),
							?assertMatch(DateTime, Value)
						end)},

				{ "parsed midnight (end of day) is understood by calendar module as beginning of next day",
					?_test(
						begin
							%% Party like its 1999
							{ok, {DateTime,_,_}} = etz:iso_parse("1999-12-31T24Z"),
							Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
							Value = calendar:gregorian_seconds_to_datetime(Seconds),
							?assertMatch({{2000,1,1},{0,0,0}}, Value)
						end)},

				{ "parsed datetime can roundtrip back to equal string representation (in expanded format)",
					?_test(
						begin
							Input = <<"1984-09-22T13:34:27.123456+03:30">>,
							{ok, FullDateTime} = etz:iso_parse(Input),
							Output = etz:iso_format(FullDateTime),
							?assertEqual(Input, Output)
						end)},

				{ "value (now) equal after round-trip to iso 8601 string format",
					?_test(
						begin
							Now = etz:now(),
							Formatted = etz:iso_format(Now),
							{ok, Parsed} = etz:iso_parse(Formatted),
							?assertEqual(Now, Parsed)
						end)}

				]
			}}.

compatability_test_() ->
	{
		setup,
		fun setup/0,
		fun cleanup/1,
		[
				{ "calendar:local_time() can be represented as iso 8601 formatted string without fractional seconds",
					?_test(
						begin
							Now = calendar:local_time(),
							Formatted = etz:iso_format(Now),
							{ok, {Parsed,_Micro,_Tz}} = etz:iso_parse(Formatted),
							?assertEqual(Now, Parsed)
						end)},

				{ "erlang:now() can be represented as iso 8601 formatted string without loss of precision",
					?_test(
						begin
							Now = erlang:now(),
							%% Convert to iso_time which captures relevant info
							IsoTime = etz:timestamp_to_iso_time(Now),
							Formatted = etz:iso_format(IsoTime),
							{ok, Parsed} = etz:iso_parse(Formatted),
							?assertEqual(IsoTime, Parsed)
						end)},

				{ "os:timestamp() can be represented as iso 8601 formatted string without loss of precision",
					?_test(
						begin
							Now = os:timestamp(),
							%% Convert to iso_time which captures relevant info
							IsoTime = etz:timestamp_to_iso_time(Now),
							Formatted = etz:iso_format(IsoTime),
							{ok, Parsed} = etz:iso_parse(Formatted),
							?assertEqual(IsoTime, Parsed)
						end)},

				{ "calendar:universal_time() can be represented as iso 8601 formatted string without fractional seconds",
					?_test(
						begin
							Now = calendar:universal_time(),
							Formatted = etz:iso_format_universal(Now),
							{ok, {Parsed,_Micro,{'+',0,0}}} = etz:iso_parse(Formatted),
							?assertEqual(Now, Parsed)
						end)},


				{ "any valid erlang date time can be represented as iso 8601 formatted string; must specify timezone",
					[
						fun() -> ?assertEqual(<<"1939-09-01T04:45:00+02">>, etz:iso_format({{1939,09,01},{04,45,00}},{'+',2,0})) end,
						fun() -> ?assertEqual(<<"1941-12-07T07:48:00-10:30">>, etz:iso_format({{1941,12,07},{07,48,00}},{'-',10,30})) end,
						fun() -> ?assertEqual(<<"1945-05-05T00:00:00+02">>, etz:iso_format({{1945,05,05},{00,00,00}},{'+',2,0})) end,
						fun() -> ?assertEqual(<<"1952-04-28T00:00:00-07">>, etz:iso_format({{1952,04,28},{00,00,00}},{'-',7,0})) end
					]}


			]}.


setup() ->
	{ok, Pid} = etz:start_link(),
	Pid.

cleanup(Pid) ->
	MRef = erlang:monitor(process, Pid),
	gen_server:call(Pid, stop),
	receive {'DOWN', MRef, _, _, _} -> ok end.

