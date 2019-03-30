%% -*- coding: utf-8 -*-
-module(common).
-author("Natalya Plotnikova").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("ranobebook_grabber.hrl").


user_name() ->
  wf:session_default(user_name, common:unicode_to_two_byte("Ошибка получения имени пользователя")).

title() ->
  common:unicode_to_two_byte("RanobeBook grabber").

back() ->
  common:unicode_to_two_byte("Назад").

help() ->
  "window.location = '/help?page=".

access_denied() ->
  common:unicode_to_two_byte("403. Ошибка доступа.").

force_back_to_auth_system() ->
  %wf:logout(),
  back_to_main_system().

back_to_main_system() ->
  wf:redirect("/login").

flash_msg(Message) ->
  flash_msg(Message, 10000).

flash_msg(Message, Time) ->
  wf:wire(
    lists:flatten(lists:concat(["$('#RMES').jGrowl(\"", common:unicode_to_one_byte(Message), "\", { life: ", Time, ", themeState: 'flat', theme: 'right' });"]))).

hide_flash_msg() ->
  wf:wire(flash_msg, #hide{effect = blind, speed = 100}),
  wf:update(flash_msg, []).
hide_flash_msg(Id) ->
  wf:wire(Id, #hide{effect = blind, speed = 100}),
  wf:update(Id, []).

show(Element, Name) ->
  wf:wire(Name, lists:concat(["$('", Element, "[name=\"", Name, "\"]').show();"])).

hide(Element, Name) ->
  wf:wire(Name, lists:concat(["$('", Element, "[name=\"", Name, "\"]').hide();"])).

show(Name) ->
  wf:wire(lists:concat(["$('#", Name, "').show();"])).

hide(Name) ->
  wf:wire(lists:concat(["$('#", Name, "').hide();"])).

to_atom(List) when is_list(List) ->
  list_to_atom(List);
to_atom(Binary) when is_binary(Binary) ->
  binary_to_atom(Binary, unicode);
to_atom(Atom) when is_atom(Atom) ->
  Atom.

to_string([]) ->
  [];
to_string(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
to_string(Tuple) when is_tuple(Tuple) ->
  tuple_to_list(Tuple);
to_string(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
to_string(Binary) when is_binary(Binary) ->
  unicode_to_two_byte(binary_to_list(Binary));
to_string(Float) when is_float(Float) ->
  float_to_list(Float);
to_string(String) when is_list(String), is_integer(hd(String)) ->
  unicode_to_two_byte(String);
to_string(List) when is_list(List) ->
  [to_string(Item) || Item <- List];
to_string(Map) when is_map(Map) ->
  maps:fold(
    fun(Key, Value, Acc) ->
      maps:put(to_string(Key), to_string(Value), Acc)
    end, #{}, Map
  ).
to_string(Float, Precision) ->
  to_float_str(Float, Precision).

to_binary(List) when is_list(List) ->
  case io_lib:char_list(List) of
    true ->   list_to_binary(common:unicode_to_one_byte(List));
    _ -> lists:flatten([to_binary(X) || X <- List])
  end;
to_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, unicode);
to_binary(Integer) when is_integer(Integer) ->
  integer_to_binary(Integer);
to_binary(Binary) when is_binary(Binary) ->
  Binary;
to_binary(Float) when is_float(Float) ->
  float_to_binary(Float).
to_binary(Float, Precision) ->
  to_binary(to_float_str(Float, Precision)).


get_numeric(OriginStrValue) when is_float(OriginStrValue) -> OriginStrValue;
get_numeric(OriginStrValue) when is_integer(OriginStrValue) -> float(OriginStrValue);
get_numeric(OriginStrValue) when is_binary(OriginStrValue) ->
  get_numeric(binary_to_list(OriginStrValue));
get_numeric(OriginStrValue) ->
  StrWithoutSpaces = re:replace(OriginStrValue, "[ \xA0]", "", [{return, list}]),
  StrValue = re:replace(StrWithoutSpaces, "[.,]", ".", [{return, list}]),
  case is_float(catch list_to_float(StrValue)) of
    true -> list_to_float(StrValue);
    _ ->
      case is_integer(catch list_to_integer(StrValue)) of
        true -> float(list_to_integer(StrValue));
        _ -> 0.0
      end
  end.

is_numeric(OriginStrValue) when is_float(OriginStrValue) -> true;
is_numeric(OriginStrValue) when is_integer(OriginStrValue) -> true;
is_numeric(OriginStrValue) when is_binary(OriginStrValue) ->
  get_numeric(binary_to_list(OriginStrValue));
is_numeric(OriginStrValue) ->
  Str = case re:replace(OriginStrValue, "[.,]", ".") of
          [Part1, <<".">> | Part2] -> binary_to_list(Part1) ++ "." ++ binary_to_list(Part2);
          Value -> Value
        end,
  StrValue = lists:flatten(Str),
  case is_float(catch list_to_float(StrValue)) of
    true -> true;
    _ -> is_integer(catch list_to_integer(StrValue))
  end.

check_two_dates(Date1, Date2) ->
  try
    PDate1 = get_postgresql_date(Date1),
    PDate2 = get_postgresql_date(Date2),
    PDate1 =< PDate2
  catch
    _E21:E22 ->
      error_logger:info_msg("Exception: ~p~n", [E22]),
      false
  end.

get_postgresql_date(Date) ->
  [Day, Month, Year | _] = re:split(Date, "[.\s]"),
  binary_to_list(Year) ++ "-" ++ binary_to_list(Month) ++ "-" ++ binary_to_list(Day).

get_current_timestamp() ->
  {{Y, Mn, D}, {H, M, S}} = erlang:localtime(),
  io_lib:format("~B-~2..0B-~2..0B~2..0B~2..0B~2..0B", [Y, Mn, D, H, M, S]).

get_postgresql_date_time({{Y, Mn, D}, {H, M, _S}}) ->
  io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B", [Y, Mn, D, H, M]);
get_postgresql_date_time({datetime, {{Y, Mn, D}, {H, M, S}}}) ->
  get_postgresql_date_time({{Y, Mn, D}, {H, M, S}}).

get_sql_date(Date) ->
  [Day, Month, Year | _] = re:split(Date, "[.\s]"),
  binary_to_list(Year) ++ "-" ++ binary_to_list(Month) ++ "-" ++ binary_to_list(Day).

eval(Lst) ->
  Result = re:split(Lst, "[+\\s]"),
  lists:foldl(
    fun(CurR, CurA) ->
      case binary_to_list(CurR) of
        [] -> CurA;
        _ ->
          case is_float(catch binary_to_float(CurR)) of
            true -> binary_to_float(CurR) + CurA;
            _ -> binary_to_integer(CurR) + CurA
          end
      end
    end, 0.0, Result).


datetime_to_date_str({{Y, Mn, D}, {H, M, _S}}) ->
  io_lib:format("~2..0B.~2..0B.~B", [D, Mn, Y]);
datetime_to_date_str({datetime, {{Y, Mn, D}, {H, M, _S}}}) ->
  io_lib:format("~2..0B.~2..0B.~B", [D, Mn, Y]).


datetime_to_str({{Y, Mn, D}, {H, M, _S}}) ->
  io_lib:format("~2..0B.~2..0B.~B ~2..0B:~2..0B", [D, Mn, Y, H, M]);
datetime_to_str({datetime, {{Y, Mn, D}, {H, M, _S}}}) ->
  io_lib:format("~2..0B.~2..0B.~B ~2..0B:~2..0B", [D, Mn, Y, H, M]).
datetime_to_str_without_space({{Y, Mn, D}, {H, M, _S}}) ->
  io_lib:format("~2..0B.~2..0B.~B_~2..0B:~2..0B", [D, Mn, Y, H, M]).


date_to_str({{_, _, _} = Date, _}, {add, AddDays}) ->
  date_to_str(calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + AddDays));
date_to_str({_, _, _} = Date, {add, AddDays}) ->
  date_to_str(calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + AddDays)).
date_to_str({{_, _, _} = Date, {_,_,_}}) ->
  date_to_str(Date);
date_to_str({Y, M, D}) ->
  lists:flatten(io_lib:format("~2..0B.~2..0B.~B", [D, M, Y])).

get_next_date(Date) when is_list(Date) ->
  [BinD, BinM, BinY] = re:split(Date, "[.]"),
  Secs = calendar:datetime_to_gregorian_seconds(
    {{binary_to_integer(BinY), binary_to_integer(BinM), binary_to_integer(BinD)}, {7, 0, 0}}),
  NextDaySecs = Secs + 86400,
  {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(NextDaySecs),
  io_lib:format("~2..0B.~2..0B.~B", [D, M, Y]);
get_next_date({CurY, CurM, CurD}) ->
  Secs = calendar:datetime_to_gregorian_seconds({{CurY, CurM, CurD}, {7, 0, 0}}),
  NextDaySecs = Secs + 86400,
  {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(NextDaySecs),
  {Y, M, D}.

get_current_gang({{_Y, _Mn, _D}, {H, _M, _S}}) when (H > 19) or (H < 7) -> 2;
get_current_gang({{_Y, _Mn, _D}, {_H, _M, _S}}) -> 1.

get_gang_time(Date, Gang) when is_list(Gang) -> get_gang_time(Date, list_to_integer(Gang));
get_gang_time(Date, 2) when is_list(Date) ->
  {get_postgresql_date(Date) ++ " 19:00:00", get_postgresql_date(get_next_date(Date)) ++ " 09:00:00"};
get_gang_time(Date, 1) when is_list(Date) ->
  {get_postgresql_date(Date) ++ " 09:00:00", get_postgresql_date(Date) ++ " 19:00:00"};
get_gang_time({Y, Mn, D}, 1) ->
  {io_lib:format("~B-~2..0B-~2..0B 09:00:00", [Y, Mn, D]), io_lib:format("~B-~2..0B-~2..0B 19:00:00", [Y, Mn, D])};
get_gang_time({Y, Mn, D}, 2) ->
  {NextY, NextM, NextD} = get_next_date({Y, Mn, D}),
  {io_lib:format("~B-~2..0B-~2..0B 19:00:00", [Y, Mn, D]), io_lib:format("~B-~2..0B-~2..0B 09:00:00",
    [NextY, NextM, NextD])}.

barcode_validator(_Tag, Value) ->
  re:run(Value, "^\\d{13,13}$", [unicode]) /= nomatch.

numeric_validator(Tag, Value) when is_number(Value) -> numeric_validator(Tag, to_float_str(Value));
numeric_validator(_Tag, Value) ->
  (re:run(Value, "^\\d+$", [unicode]) /= nomatch) or (re:run(Value, "^\\d+[.,]\\d+$", [unicode]) /= nomatch).

required_validator(_Tag, Value) ->
  re:run(Value, ".+", [unicode]) /= nomatch.

save_cookie() ->
  case wf:cookie_default("ranobebook_grabber_cookie", false) of
    false -> wf:cookie("ranobebook_grabber_cookie", "McatCookie", "/", 200000);
    CurrentCookie -> wf:cookie("ranobebook_grabber_cookie", CurrentCookie, "/", 200000)
  end.

get_diam_in(Value) ->
  [_, BinDiamIn | _] = re:split(Value, "[Øм]", [unicode]),
  get_numeric(binary_to_list(BinDiamIn)).

build_history_row(CurOperation, CurDate, User, CurState, IsValid) ->
  #tablerow{cells = [
    #tablecell{class = "report_table_cell", body = #span{text = CurOperation}},
    #tablecell{class = "report_table_cell", body = #span{text = CurDate}},
    #tablecell{class = "report_table_cell", body = #span{text = User}},
    #tablecell{class = "report_table_cell", body = #span{text = CurState}},
    #tablecell{class = "report_table_cell", body = #span{text = IsValid}}
  ]}.

build_history_row(CurOperation, CurDate, CurBarcode, Description) ->
  #tablerow{cells = [
    #tablecell{class = "report_table_cell", body = #span{text = CurOperation}},
    #tablecell{class = "report_table_cell", body = #span{text = CurDate}},
    #tablecell{class = "report_table_cell", body = #span{text = CurBarcode}},
    #tablecell{class = "report_table_cell", body = #span{text = Description}}
  ]}.

is_two_byte_unicode(List) -> lists:filter(fun(X) -> X > 255 end, List) /= [].

unicode_to_one_byte(List) when is_list(List) ->
  case is_two_byte_unicode(List) of
    true -> binary_to_list(unicode:characters_to_binary(List));
    _ -> List
  end;
unicode_to_one_byte(List) when is_binary(List) -> binary_to_list(List);
unicode_to_one_byte(List) -> List.

unicode_to_two_byte(List) when is_list(List) ->
  case is_two_byte_unicode(List) of
    true -> List;
    _ -> unicode:characters_to_list(list_to_binary(List))
  end;
unicode_to_two_byte(List) when is_binary(List) -> unicode:characters_to_list(List);
unicode_to_two_byte(List) -> List.

transpose([[] | _]) -> [];
transpose(M) -> [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

get_json_element_value(KeyName, TupleList) ->
  case lists:keyfind(KeyName, 1, TupleList) of
    false -> undefined;
    Tuple -> element(2, Tuple)
  end.

to_float_str(Value, Precision) -> float_to_list(get_numeric(Value), [{decimals, Precision}]).
to_float_str(Value) -> to_float_str(Value, 0).

debug_message(each_string, []) -> ok;
debug_message(each_string, [H |T]) ->
  io:format("~n=============DEBUG MESSAGE=============~n\"~ts\"~n=======================================~n", [H]),
  debug_message(each_string, T);
debug_message(string, List) ->
  io:format("~n=============DEBUG MESSAGE=============~n\"~ts\"~n=======================================~n", [List]), List.
debug_message(List) ->
  io:format("~n=============DEBUG MESSAGE=============~n~p~n=======================================~n", [List]), List.

get_db_controller() ->
  {ok, CurrentDB} = application:get_env(ranobebook_grabber_app, current_db_controller),
  CurrentDB.

sub_dates({Y1, M1, D1}, {Y2, M2, D2}) ->
  CouDays1 = calendar:date_to_gregorian_days(Y1, M1, D1),
  CouDays2 = calendar:date_to_gregorian_days(Y2, M2, D2),
  CouDays1 - CouDays2.

not_standart_sort(List, FieldNumber) ->
  lists:sort(fun(Elem1, Elem2) -> cmp(element(FieldNumber, Elem1), element(FieldNumber, Elem2)) end, List).

cmp([], []) ->
  true;
cmp(_, []) ->
  false;
cmp([], _) ->
  true;
cmp([Char1 | RemStr1], [Char2 | RemStr2]) ->
  case Char1 == Char2 of
    true -> cmp(RemStr1, RemStr2);
    _ ->
      case (Char1 >= 1040) and (Char1 =< 1103) and (Char2 >= 65) and (Char2 =< 122) of
        true -> true;
        _ ->
          case (Char1 >= 65) and (Char1 =< 122) and (Char2 >= 1040) and (Char2 =< 1103) of
            true -> false;
            _ -> Char1 < Char2
          end
      end
  end.

event({redirect, URL}) ->
  wf:redirect(lists:concat(["/", URL])),
  ok;
event({block, BlockId}) ->
  wf:session(blockid, BlockId),
  wf:redirect(lists:concat(["/", menu:get_first_block_interface(BlockId)])),
  ok;
event(user_logout) ->
  wf:user(undefined),
  wf:session(role_id, undefined),
  wf:session(user_name, undefined),
  wf:redirect("/"),
  ok;
event(A) -> common:debug_message(["invalid event", A]), ok.

get_image_name(Element) ->
  PngImage = lists:concat(["images/", Element, ".png"]),
  JpgImage = lists:concat(["images/", Element, ".jpg"]),
  case file:open(lists:concat(["priv/static/", PngImage]), [raw]) of
    {error, _} -> JpgImage;
    _ -> PngImage
  end.

symbol_button(Type, Id) ->
  symbol_button(Type, Id, "").
symbol_button(Type, Id, AdditionalClass) ->
  Class = case Type of
            edit -> "fas fa-edit";
            user_edit -> "fas fa-user-edit";
            redo -> "fas fa-redo";
            trash -> "fas fa-trash-alt";
            plus -> "fas fa-plus";
            save -> "fas fa-save";
            user_add -> "fas fa-user-plus";
            file -> "far fa-file-alt";
            world -> "fas fa-globe";
            cogs -> "fas fa-cogs";
            passport -> "fas fa-passport";
            user_secret -> "fas fa-user-secret";
            file_excel -> "fas fa-file-excel";
            allow_in -> "fas fa-door-open";
            allow_out -> "fas fa-door-closed";
            weight -> "fas fa-weight";
            start -> "fas fa-play";
            stop -> "fas fa-stop";
            _ -> ""
          end,
  #span {id = Id, html_id = Id, class = ["button_icon", Class, AdditionalClass]}.
