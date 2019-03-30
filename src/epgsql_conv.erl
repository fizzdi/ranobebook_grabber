%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2017 8:20 PM
%%%-------------------------------------------------------------------
-module(epgsql_conv).
-author("vlad").

%% API
-export([]).
-compile(export_all).

-include_lib("epgsql/include/epgsql.hrl").

%% postgresql TYPES
-define(FIELD_TYPE_TINY, int2).
-define(FIELD_TYPE_SHORT, int4).
-define(FIELD_TYPE_LONG, int8).
-define(FIELD_TYPE_FLOAT, float4).
-define(FIELD_TYPE_DOUBLE, float8).
-define(FIELD_TYPE_TIMESTAMP, timestamp).
-define(FIELD_TYPE_INT24, int24).
-define(FIELD_TYPE_DATE, date).
-define(FIELD_TYPE_TIME, time).
-define(FIELD_TYPE_DATETIME, datetime).
-define(FIELD_TYPE_YEAR, year).
-define(FIELD_TYPE_VARCHAR, varchar).
-define(FIELD_TYPE_BIT, bool).
-define(FIELD_TYPE_GEOMETRY, geometry).
-define(FIELD_TYPE_TEXT, text).


-define(MAP_FUNCTION, [
  % TODO:
% ?FIELD_TYPE_NEWDATE
% ?FIELD_TYPE_ENUM
% ?FIELD_TYPE_SET
% ?FIELD_TYPE_GEOMETRY
%%  {?FIELD_TYPE_VARCHAR, fun identity/1},
  {?FIELD_TYPE_TINY, fun to_integer/1},
  {?FIELD_TYPE_SHORT, fun to_integer/1},
  {?FIELD_TYPE_LONG, fun to_integer/1},
  {?FIELD_TYPE_INT24, fun to_integer/1},
  {?FIELD_TYPE_TEXT, fun to_text/1}
%%{?FIELD_TYPE_YEAR, fun to_integer/1}
%%  {?FIELD_TYPE_FLOAT, fun to_float/1},
%%  {?FIELD_TYPE_DOUBLE, fun to_float/1},
%%  {?FIELD_TYPE_DATE, fun to_date/1},
%%  {?FIELD_TYPE_TIME, fun to_time/1},
%%  {?FIELD_TYPE_TIMESTAMP, fun to_timestamp/1},
%%  {?FIELD_TYPE_DATETIME, fun to_timestamp/1},
%%  {?FIELD_TYPE_BIT, fun to_bit/1}
]).

as_record({ok, _, Columns, Rows}, RecordName, Fields) ->
  as_record(Columns, Rows, RecordName, Fields);
as_record({ok, Columns, Rows}, RecordName, Fields) ->
  as_record(Columns, Rows, RecordName, Fields).
as_record(Columns, Rows, RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->
  RowsList = [tuple_to_list(CurRow) || CurRow <- Rows],

  Sequence = lists:seq(1, length(Columns)),
  P = lists:zip([binary_to_atom(C1#column.name, utf8) || C1 <- Columns], Sequence),
  T = [CurColumn#column.type || CurColumn <- Columns],
  F =
    fun(FieldName) ->
      case proplists:lookup(FieldName, P) of
        none ->
          fun(_) -> undefined end;
        {FieldName, Pos} ->
          fun(Row) ->
            case lists:nth(Pos, Row) of
              Elem ->
                CurType = lists:nth(Pos, T),
                FT = cast_fun_for(CurType),
                FT(Elem)
            end
          end
      end
    end,
  Fs = [F(FieldName) || FieldName <- Fields],

  F1 =
    fun(Row) ->
      RecordData = [Fx(Row) || Fx <- Fs],
      list_to_tuple([RecordName | RecordData])
    end,
  [F1(CurRow) || CurRow <- RowsList].

as_json(Result) ->
  {ok, Columns, Rows} = Result,
  Fields = [CurColumn#column.name || CurColumn <- Columns],
  [begin
     [{K, json_val(V)} || {K, V} <- lists:zip(Fields, tuple_to_list(Row))]
   end || Row <- Rows].

json_val(undefined) ->
  null;
json_val({date, {Year, Month, Day}}) ->
  iolist_to_binary(
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w",
      [Year, Month, Day]));
json_val({datetime, {{Year, Month, Day}, {Hour, Min, Sec}}}) ->
  iolist_to_binary(
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
      [Year, Month, Day, Hour, Min, Sec]));
json_val(Value) ->
  Value.

cast_fun_for(Type) ->
  case lists:keyfind(Type, 1, ?MAP_FUNCTION) of
    false ->
      fun identity/1;
    {Type, F} ->
      F
  end.

identity(Data) -> Data.
to_text(null) -> "";
to_text(Data) -> string:to_graphemes(Data).
to_integer(null) -> 0;
to_integer(Data) -> Data.
to_float(Data) ->
  {ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Data)) of
                              % note: does not need conversion
                              {error, _} ->
                                case io_lib:fread("~d", binary_to_list(Data)) of  % note: does not need conversion
                                  {ok, [_], []} = Res ->
                                    Res;
                                  {ok, [X], E} ->
                                    io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s", [X, ".0", E])))
                                end
                              ;
                              Res ->
                                Res
                            end,
  Num.
to_date(Data) ->
  case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of  % note: does not need conversion
    {ok, [Year, Month, Day], _} ->
      {date, {Year, Month, Day}};
    {error, _} ->
      binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
    _ ->
      exit({error, bad_date})
  end.
to_time(Data) ->
  case io_lib:fread("~d:~d:~d", binary_to_list(Data)) of  % note: does not need conversion
    {ok, [Hour, Minute, Second], _} ->
      {time, {Hour, Minute, Second}};
    {error, _} ->
      binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
    _ ->
      exit({error, bad_time})
  end.
to_timestamp(Data) ->
  case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Data)) of % note: does not need conversion
    {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
      {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
    {error, _} ->
      binary_to_list(Data);   % todo: test and possibly conversion to UTF-8
    _ ->
      exit({error, datetime})
  end.
to_bit(<<1>>) -> 1;
to_bit(<<0>>) -> 0.
