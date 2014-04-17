-module(etz).

-behaviour(gen_server).

-define(SRV, ?MODULE).

-export([
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	code_change/3,
	terminate/2
	]).

-export([
	use_timezone/1,
	use_timezone/2,
	current_timezone/0,
	current_timezone/1,
	revert_timezone/0,
	revert_timezone/1
	]).

-export([
	universal_now/0,
	now/0,
	now/1,
	local_timezone/0,
	timestamp_to_iso_time/1,
	timestamp_to_iso_time/2,
	datetime_to_iso_time/1,
	datetime_to_iso_time/2,
	to_timezone/2,
	to_universal/1,
	iso_format_universal/1,
	iso_format/1,
	iso_format/2,
	iso_format_timezone/1,
	iso_parse/1,
	iso_parse_input/3
	]).

-type year() :: non_neg_integer().
-type month() :: 1..12.
-type day() :: 1..31.

-type hour() :: 0..24.
-type minute() :: 0..59.
-type second() :: 0..59.

-type megaseconds() :: non_neg_integer().
-type seconds() :: non_neg_integer().
-type microseconds() :: 0..1000000.

-type timestamp() :: tuple(
		MegaSecs :: megaseconds(),
		Secs :: seconds(),
		MicroSecs :: microseconds()
		).

-type sign() :: '+' | '-'.

-type timezone() :: tuple(
		Sign :: sign(),
		Hours :: hour(),
		Minutes :: minute()
		).

-type date() :: tuple(
		Year :: year(),
		Month :: month(),
		Day :: day()
		).

-type time() :: tuple(
		Hour :: hour(),
		Minute :: minute(),
		Second :: second()
		).

-type datetime() :: tuple(
		Date :: date(),
		Time :: time()
		).

-type iso_time() :: tuple(
		Datetime :: datetime(),
		Microseconds :: microseconds(),
		Timezone :: timezone()
		).

-type parse_ok() :: tuple(ok, iso_time()).

-type parse_error() :: tuple(
	error,
	ErrorInfo :: tuple(
		ErrorKind :: atom(),
		Where :: list()
	)).

-type parse_result() :: parse_ok() | parse_error().

-define(UTC, {'+', 0, 0 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?SRV}, ?MODULE, [], []).

-spec use_timezone(Timezone :: timezone()) -> ok.
use_timezone(Timezone) ->
	use_timezone(self(),Timezone).

-spec use_timezone(
	Key :: any(),
	Timezone :: timezone()
	) -> ok.
use_timezone(Key,{Sgn,H,M}=Timezone) when
		Sgn =:= '+' orelse Sgn =:= '-',
		is_integer(H) andalso H >= 0 andalso H =< 23,
		is_integer(M) andalso M >= 0 andalso M =< 59 ->
	ok = gen_server:call(?SRV,{use_timezone,Key,Timezone}).

-spec current_timezone() -> timezone().
current_timezone() ->
	current_timezone(self()).

-spec current_timezone(Key :: any()) -> timezone().
current_timezone(Key) ->
	gen_server:call(?SRV,{current_timezone,Key}).

-spec revert_timezone() -> ok.
revert_timezone() ->
	revert_timezone(self()).

-spec revert_timezone(Key :: any()) -> ok.
revert_timezone(Key) ->
	ok = gen_server:call(?SRV, {revert_timezone, Key}).

-spec now() -> iso_time().
now() ->
	gen_server:call(?SRV, {now, self()}).

-spec now(Key:: any()) -> iso_time().
now(Tz) ->
	gen_server:call(?SRV, {now, self(), Tz}).

-spec universal_now() -> iso_time().
universal_now() ->
	gen_server:call(?SRV, universal_now).

-spec local_timezone() -> timezone().
local_timezone() ->
	gen_server:call(?SRV, local_timezone).

-spec timestamp_to_iso_time(
	Timestamp :: timestamp()
	) -> iso_time().
timestamp_to_iso_time(Timestamp) ->
	gen_server:call(?SRV, {timestamp_to_iso_time, self(), Timestamp}).

-spec timestamp_to_iso_time(
	Timestamp :: timestamp(),
	Timezone :: timezone()
	) -> iso_time().
timestamp_to_iso_time(Timestamp, Tz) ->
	gen_server:call(?SRV, {timestamp_to_iso_time, self(), Timestamp, Tz}).

-spec datetime_to_iso_time(
	DateTime :: calendar:datetime()
	) -> iso_time().
datetime_to_iso_time(DateTime) ->
	gen_server:call(?SRV, {datetime_to_iso_time, self(), DateTime}).

-spec datetime_to_iso_time(
	DateTime :: calendar:datetime(),
	Timezone :: timezone()
	) -> iso_time().
datetime_to_iso_time(DateTime, Tz) ->
	gen_server:call(?SRV, {datetime_to_iso_time, self(), DateTime, Tz}).

-spec to_timezone(
	IsoTime :: iso_time(),
	Timezone :: timezone()
	) -> iso_time().
to_timezone({DateTime,Micro,Tz}, TargetTz) ->
	U = to_utc(DateTime, Tz),
	T = from_utc_to_timezone(U, TargetTz),
	{T,Micro,TargetTz}.

-spec to_universal(iso_time()) -> iso_time().
to_universal(FullDateTime) ->
	to_timezone(FullDateTime, ?UTC).

-spec iso_parse(Input :: binary()) -> parse_result();
               (Input :: list(integer())) -> parse_result().
iso_parse(Input) ->
	gen_server:call(?SRV, {iso_parse, self(), Input}).

-spec iso_format_universal(
	DateTime :: calendar:datetime()
	) -> Iso8601Formatted :: binary().
iso_format_universal({{Y,M,D}, {H,N,S}}=DateTime) when
		is_integer(Y), is_integer(M), is_integer(D),
		is_integer(H), is_integer(N), is_integer(S) ->
	iso_format({DateTime, 0, ?UTC}).

-spec iso_format(
	DateTime :: calendar:datetime(),
	Timezone :: timezone()
	) -> Iso8601Formatted :: binary().
iso_format({{Y,M,D}, {H,N,S}}=DateTime, {Sgn,ZH,ZM}=Timezone) when
		is_integer(Y), is_integer(M), is_integer(D),
		is_integer(H), is_integer(N), is_integer(S),
		Sgn =:= '+' orelse Sgn =:= '-',
		is_integer(ZH), is_integer(ZM)
		->
	iso_format({DateTime, 0, Timezone}).

-spec iso_format(DateTime :: calendar:datetime()) -> Iso8601Formatted :: binary();
                (IsoTime :: iso_time()) -> Iso8601Formatted :: binary().
iso_format({{Y,M,D}, {H,N,S}}=DateTime) when
		is_integer(Y), is_integer(M), is_integer(D),
		is_integer(H), is_integer(N), is_integer(S) ->
	iso_format({DateTime, 0, current_timezone()});
iso_format({{{Y,M,D}, {H,N,S}}, Micro, Tz}) when Micro =:= 0 ->
	Z = iso_format_timezone(Tz),
	FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
	IsoStr = io_lib:format(FmtStr, [Y, M, D, H, N, S, Z]),
	list_to_binary(IsoStr);
iso_format({{{Y,M,D}, {H,N,S}}, Micro, Tz}) ->
	Z = iso_format_timezone(Tz),
	FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~s~s",
	% Couldn't figure out a format that let me specify padding left of the decimal
	% but not right of the decimal. This works but feels like a hack.
	[FormattedMicroseconds] = io_lib:format("~.f", [Micro / 1000000]),
	Micros = string:substr(FormattedMicroseconds, 3),
	IsoStr = io_lib:format(FmtStr, [Y, M, D, H, N, S, Micros, Z]),
	list_to_binary(IsoStr).

-spec iso_format_timezone(Timezone :: timezone()) -> list().
iso_format_timezone({Sgn, 0, 0}) when
		Sgn =:= '+' orelse Sgn =:= '-' ->
	'Z';
iso_format_timezone({Sgn, H, 0}) ->
	io_lib:format("~s~2.10.0B", [Sgn, H]);
iso_format_timezone({Sgn, H, M}) ->
	io_lib:format("~s~2.10.0B:~2.10.0B", [Sgn, H, M]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_timestamp_to_iso_time({_,_,Micro}=Timestamp, LocalTz, TargetTz) ->
	L = calendar:now_to_local_time(Timestamp),
	U = to_utc(L, LocalTz),
	T = from_utc_to_timezone(U, TargetTz),
	{T,Micro,TargetTz}.

convert_datetime_to_iso_time(DateTime, LocalTz, TargetTz) ->
	U = to_utc(DateTime, LocalTz),
	T = from_utc_to_timezone(U, TargetTz),
	{T,0,TargetTz}.

to_utc(DateTime, {Sgn,0,0}) when
		Sgn =:= '+' orelse Sgn =:= '-' ->
	DateTime;
to_utc(DateTime, {Sgn,H,M}) when
		Sgn =:= '+' orelse Sgn =:= '-',
		is_integer(H) andalso H >= 0 andalso H =< 23,
		is_integer(M) andalso M >= 0 andalso M =< 59 ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
	Offset = ((H * 60) + M) * 60,
	ResultSeconds = case Sgn of
		'-' -> Seconds + Offset;
		'+' -> Seconds - Offset
	end,
	calendar:gregorian_seconds_to_datetime(ResultSeconds).

from_utc_to_timezone(DateTime, {Sgn,0,0}) when
		Sgn =:= '+' orelse Sgn =:= '-' ->
	DateTime;
from_utc_to_timezone(DateTime, {Sgn,H,M}) when
		Sgn =:= '+' orelse Sgn =:= '-',
		is_integer(H) andalso H >= 0 andalso H =< 23,
		is_integer(M) andalso M >= 0 andalso M =< 59 ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
	Offset = ((H * 60) + M) * 60,
	ResultSeconds = case Sgn of
		'-' -> Seconds - Offset;
		'+' -> Seconds + Offset
	end,
	calendar:gregorian_seconds_to_datetime(ResultSeconds).

calc_local_timezone() ->
	T = calendar:universal_time(),
	U = calendar:universal_time_to_local_time(T),
	Ts = calendar:datetime_to_gregorian_seconds(T),
	Us = calendar:datetime_to_gregorian_seconds(U),
	S = (Us - Ts),
	case S of
		P when S > 0 ->
			{'+', hours_from_seconds(P), remaining_minutes_from_seconds(P)};
		N when S < 0 ->
			{'-', hours_from_seconds(N), remaining_minutes_from_seconds(N)};
		_ ->
			?UTC
	end.

hours_from_seconds(S) when S =/= 0 ->
	abs(S div 60 div 60);
hours_from_seconds(_) ->
	0.

remaining_minutes_from_seconds(S) when S =/= 0 ->
	abs(S div 60 rem 60);
remaining_minutes_from_seconds(_) ->
	0.

-record(date_parse, {
		kind = 'M' :: 'M' | 'W',
		expanded :: '-' | '+',
		year :: integer(),
		week :: integer(),
		week_day :: integer(),
		month = 1 :: integer(),
		day = 1 :: integer(),
		hour = 0 :: integer(),
		minute = 0 :: integer(),
		second = 0 :: integer(),
		microseconds = 0 :: integer(),
		sign = '+' :: '+' | '-',
		tzHours = 0 :: integer(),
		tzMinutes = 0 :: integer()
		}).

date_parse_to_iso_time(#date_parse{kind=K}=Parse)
		when K =:= 'M' ->
	#date_parse{
		year=YY,
		month=MM,
		day=DD,
		hour=HH,
		minute=NN,
		second=SS,
		microseconds=F,
		sign=ZS,
		tzHours=ZH,
		tzMinutes=ZM
		} = Parse,
	%% Translate special time 24:00
	{ok, {{{YY,MM,DD},{HH,NN,SS}}, F, {ZS,ZH,ZM}}}.

iso_parse_input(Input, Tz, Opt) when is_binary(Input) ->
	iso_parse_input(binary_to_list(Input), Tz, Opt);
iso_parse_input(Input, {Sgn,H,M}, {none,_}) when is_list(Input) ->
	iso_parse_year(Input, #date_parse{sign=Sgn,tzHours=H,tzMinutes=M});
iso_parse_input(Input, {Sgn,H,M}, {Digits,_}) when is_list(Input), is_integer(Digits) ->
	iso_parse_expanded_year(Input, Digits, #date_parse{sign=Sgn,tzHours=H,tzMinutes=M}).

iso_parse_expanded_year(['+'|Rest], Digits, Acc) ->
	iso_parse_expanded_year(Rest, Digits, Acc#date_parse{expanded='+'});
iso_parse_expanded_year(['-'|Rest], Digits, Acc) ->
	iso_parse_expanded_year(Rest, Digits, Acc#date_parse{expanded='-'});
iso_parse_expanded_year(_,_,_) ->
	incomplete_implementation.

iso_parse_year([Y1,Y2,Y3,Y4|Rest], Acc) when
		Y1 >= $0 andalso Y1 =< $9,
		Y2 >= $0 andalso Y2 =< $9,
		Y3 >= $0 andalso Y3 =< $9,
		Y4 >= $0 andalso Y4 =< $9 ->
	Year = list_to_integer([Y1,Y2,Y3,Y4]),
	iso_parse_month_or_week(Rest, Acc#date_parse{year=Year});
iso_parse_year(Inv,_) ->
	{error, {invalid_year, Inv}}.

iso_parse_month_or_week([$W,W1,W2|Rest], Acc) ->
	Week = list_to_integer([W1,W2]),
	iso_parse_week_day(Rest, Acc#date_parse{kind='W',week=Week});
iso_parse_month_or_week([$-,$W,W1,W2|Rest], Acc) ->
	Week = list_to_integer([W1,W2]),
	iso_parse_week_day(Rest, Acc#date_parse{kind='W',week=Week});
iso_parse_month_or_week([$-|Rest], Acc) ->
	iso_parse_month_or_week(Rest, Acc);
iso_parse_month_or_week([M1,M2|Rest], Acc) when
		M1 =:= $0 orelse M1 =:= $1,
		M2 >= $0 andalso M2 =< $9 ->
	Month = list_to_integer([M1,M2]),
	case Month of
		M when M > 0 andalso M =< 12 ->
			iso_parse_day(Rest, Acc#date_parse{kind='M',month=Month});
		_ ->
			{error, {invalid_month, [M1,M2|Rest]}}
	end;
iso_parse_month_or_week([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_month_or_week(Inv,_) ->
	{error, {invalid_month_or_week, Inv}}.

iso_parse_day([$-|Rest], Acc) ->
	iso_parse_day(Rest, Acc);
iso_parse_day([D0,D1|Rest], #date_parse{year=Y,month=M}=Acc) when
		D0 >= $0 andalso D0 =< $3,
		D1 >= $0 andalso D1 =< $9 ->
	Day = list_to_integer([D0,D1]),
	case Day of
		D when D > 0 andalso D =< 31 ->
			Ldom = calendar:last_day_of_the_month(Y, M),
			case Ldom of
				L when L >= D ->
					iso_parse_time(Rest, Acc#date_parse{day=D});
				_ ->
					{error, {invalid_day_of_month, [D0,D1|Rest]}}
			end;
		_ ->
			{error, {invalid_day_of_month, [D0,D1|Rest]}}
	end;
iso_parse_day([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_day(Inv,_) ->
	{error, {invalid_day_of_month, Inv}}.

iso_parse_time([$T,H0,H1|Rest], Acc) when
		H0 =:= $0 orelse H0 =:= $1 orelse H0 =:= $2,
		H1 >= $0 andalso H1 =< $9 ->
	Hour = list_to_integer([H0,H1]),
	case Hour of
		H when H >= 0 andalso H =< 24 ->
			iso_parse_minute_or_timezone(Rest, Acc#date_parse{hour=H});
		_ ->
			{error, {invalid_hour, [H0,H1|Rest]}}
	end;
iso_parse_time([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_time(Inv, _) ->
	{error, {invalid_hour, Inv}}.

iso_parse_minute_or_timezone([$Z], Acc) ->
	date_parse_to_iso_time(Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_minute_or_timezone([$-|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='-',tzHours=0,tzMinutes=0});
iso_parse_minute_or_timezone([$+|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_minute_or_timezone([$:,M0,M1|Rest], Acc) when
		M0 >= $0 andalso M0 =< $5,
		M1 >= $0 andalso M1 =< $9 ->
	validate_minute(M0,M1,Rest,Acc);
iso_parse_minute_or_timezone([M0,M1|Rest], Acc) when
		M0 >= $0 andalso M0 =< $5,
		M1 >= $0 andalso M1 =< $9 ->
	validate_minute(M0,M1,Rest,Acc);
iso_parse_minute_or_timezone([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_minute_or_timezone(Inv, _) ->
	{error, {invalid_minute, Inv}}.

validate_minute(M0, M1, Rest, #date_parse{hour=H}=Acc) ->
	Minute = list_to_integer([M0,M1]),
	case Minute of
		M when M > 0 andalso H =:= 24 ->
			{error, {invalid_minute, [M0, M1|Rest]}};
		_ ->
			iso_parse_second_or_timezone(Rest, Acc#date_parse{minute=Minute})
	end.

iso_parse_second_or_timezone([$Z], Acc) ->
	date_parse_to_iso_time(Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_second_or_timezone([$-|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='-',tzHours=0,tzMinutes=0});
iso_parse_second_or_timezone([$+|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_second_or_timezone([$:,S0,S1|Rest], Acc) when
		S0 >= $0 andalso S0 =< $5,
		S1 >= $0 andalso S1 =< $9 ->
	validate_second(S0, S1, Rest, Acc);
iso_parse_second_or_timezone([S0,S1|Rest], Acc) when
		S0 >= $0 andalso S0 =< $5,
		S1 >= $0 andalso S1 =< $9 ->
	validate_second(S0, S1, Rest, Acc);
iso_parse_second_or_timezone([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_second_or_timezone(Inv, _) ->
	{error, {invalid_second, Inv}}.

validate_second(S0, S1, Rest, #date_parse{hour=H}=Acc) ->
	Second = list_to_integer([S0,S1]),
	case Second of
		M when M > 0 andalso H =:= 24 ->
			{error, {invalid_second, [S0, S1|Rest]}};
		_ ->
			iso_parse_fraction_or_timezone(Rest, Acc#date_parse{second=Second})
	end.

iso_parse_fraction_or_timezone([$Z], Acc) ->
	date_parse_to_iso_time(Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_fraction_or_timezone([$-|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='-',tzHours=0,tzMinutes=0});
iso_parse_fraction_or_timezone([$+|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_fraction_or_timezone([$.|Rest], Acc) ->
	iso_parse_fraction(Rest, [$.|Rest], Acc);
iso_parse_fraction_or_timezone([$,|Rest], Acc) ->
	iso_parse_fraction(Rest, [$,|Rest], Acc);
iso_parse_fraction_or_timezone([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_fraction_or_timezone(Inv, _) ->
	{error, {invalid_fractional_seconds, Inv}}.

iso_parse_fraction([D|Rest], Frac, Parse) when D >= $0 andalso D =< $9 ->
	iso_parse_fraction_to_micro(Rest, Frac, Parse, [D, $., $0]);
iso_parse_fraction(Inv, _, _) ->
	{error, {invalid_fractional_seconds, Inv}}.

iso_parse_fraction_to_micro([D|Rest], Frac, Parse, Acc) when D >= $0 andalso D =< $9 ->
	iso_parse_fraction_to_micro(Rest, Frac, Parse, [D|Acc]);
iso_parse_fraction_to_micro(Rest, Frac, Parse, Acc) ->
	F = list_to_float(lists:reverse(Acc)),
	Micro = round(F * 1000000),
	validate_micro_second(Micro, Rest, Frac, Parse).

validate_micro_second(Micro, Rest, Frac, #date_parse{hour=H}=Acc) ->
	case Micro of
		M when M > 0 andalso H =:= 24 ->
			{error, {invalid_fractional_seconds, Frac}};
		_ ->
			iso_parse_timezone(Rest, Acc#date_parse{microseconds=Micro})
	end.

iso_parse_week_day([$-,Day|Rest], Acc)
		when is_integer(Day) andalso Day >= $1 andalso Day =< $7 ->
	calculate_week_date(Rest, Acc#date_parse{week_day=Day});
iso_parse_week_day([$-,Day|Rest], Acc)
		when is_integer(Day) andalso Day >= $1 andalso Day =< $7 ->
	calculate_week_date(Rest, Acc#date_parse{week_day=Day});
iso_parse_week_day([], Acc) ->
	calculate_week_date([], Acc#date_parse{week_day=0}).

iso_parse_timezone([$Z], Acc) ->
	date_parse_to_iso_time(Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_timezone([$-|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='-',tzHours=0,tzMinutes=0});
iso_parse_timezone([$+|Rest], Acc) ->
	iso_parse_timezone_hours(Rest, Acc#date_parse{sign='+',tzHours=0,tzMinutes=0});
iso_parse_timezone([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_timezone(Inv, _) ->
	{error, {invalid_timezone, Inv}}.

iso_parse_timezone_hours([H0,H1|Rest], Acc) when
		H0 =:= $0 orelse H0 =:= $1 orelse H0 =:= $2,
		H1 >= $0 andalso H1 =< $9 ->
	Hour = list_to_integer([H0,H1]),
	case Hour of
		H when H >= 0 andalso H =< 24 ->
			iso_parse_timezone_minutes(Rest, Acc#date_parse{tzHours=H});
		_ ->
			{error, {invalid_timezone_hours, [H0,H1|Rest]}}
	end;
iso_parse_timezone_hours(Inv, _) ->
	{error, {invalid_timezone_hours, Inv}}.

iso_parse_timezone_minutes([$:,M0,M1], Acc) ->
	iso_parse_timezone_minutes([M0,M1],Acc);
iso_parse_timezone_minutes([M0,M1], #date_parse{tzHours=H}=Acc) when
		M0 >= $0 andalso M0 =< $5,
		M1 >= $0 andalso M1 =< $9 ->
	Minutes = list_to_integer([M0,M1]),
	case Minutes of
		M when M > 0 andalso H =:= 24 ->
			{error, {invalid_timezone_minutes, [M0, M1]}};
		_ ->
			date_parse_to_iso_time(Acc#date_parse{tzMinutes=Minutes})
	end;
iso_parse_timezone_minutes([], Acc) ->
	date_parse_to_iso_time(Acc);
iso_parse_timezone_minutes(Inv, _) ->
	{error, {invalid_timezone_minutes, Inv}}.

calculate_week_date([], _) ->
	incomplete_implementation.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
		tz,
		options,
		local :: timezone(),
		defa
		}).

init(_) ->
	State = #state{
			tz=dict:new(),
			options=dict:new(),
			local=calc_local_timezone(),
			defa={none,full}
			},
	{ok, State}.

handle_cast(_,State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
	erlang:demonitor(MonitorRef),
	NewTZ = dict:erase(Pid, State#state.tz),
	NewState = State#state{tz=NewTZ},
	{noreply, NewState };
handle_info(_, State) ->
	{noreply, State}.

handle_call({use_timezone,Key,Tz}, _From, State) ->
	case Key of
		K when is_pid(K) ->
			erlang:monitor(process, Key);
		_ -> ok
	end,
	NewTZ = dict:store(Key, Tz, State#state.tz),
	NewState = State#state{tz=NewTZ},
	{reply, ok, NewState};

handle_call({revert_timezone,Key},_From, State) ->
	NewTZ = dict:erase(Key, State#state.tz),
	NewState = State#state{tz=NewTZ},
	{reply, ok, NewState};

handle_call({current_timezone,Key},_From, State) ->
	Reply = timezone_for(Key, State),
	{reply, Reply, State};

handle_call({now, Key}, _From, State) ->
	Tz = timezone_for(Key, State),
	Reply = convert_timestamp_to_iso_time(os:timestamp(), State#state.local, Tz),
	{reply, Reply, State};
handle_call({now, _Key, Tz}, _From, State) ->
	Reply = convert_timestamp_to_iso_time(os:timestamp(), State#state.local, Tz),
	{reply, Reply, State};

handle_call(universal_now, _From, State) ->
	Reply = convert_timestamp_to_iso_time(os:timestamp(), State#state.local, ?UTC),
	{reply, Reply, State};

handle_call(local_timezone, _From, #state{local=Local}=State) ->
	{reply, Local, State};

handle_call({timestamp_to_iso_time, Key, Timestamp}, _From, State) ->
	Tz = timezone_for(Key, State),
	Reply = convert_timestamp_to_iso_time(Timestamp, State#state.local, Tz),
	{reply, Reply, State};
handle_call({timestamp_to_iso_time, _Key, Timestamp, Tz}, _From, State) ->
	Reply = convert_timestamp_to_iso_time(Timestamp, State#state.local, Tz),
	{reply, Reply, State};

handle_call({datetime_to_iso_time, Key, DateTime}, _From, State) ->
	Tz = timezone_for(Key, State),
	Reply = convert_datetime_to_iso_time(DateTime, State#state.local, Tz),
	{reply, Reply, State};
handle_call({datetime_to_iso_time, _Key, DateTime, Tz}, _From, State) ->
	Reply = convert_datetime_to_iso_time(DateTime, State#state.local, Tz),
	{reply, Reply, State};

handle_call({iso_parse, Key, Input}, _From, State) ->
	Tz = timezone_for(Key, State),
	Options = parse_options_for(Key, State),
	Reply = iso_parse_input(Input, Tz, Options),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

timezone_for(Key, #state{tz=Dict,local=Local}) ->
	case dict:find(Key, Dict) of
		error -> Local;
		{ok,Tz} -> Tz
	end.

parse_options_for(Key, #state{options=Dict,defa=Defa}) ->
	case dict:find(Key, Dict) of
		error -> Defa;
		{ok,Opt} -> Opt
	end.

