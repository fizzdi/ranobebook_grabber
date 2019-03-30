%%%-------------------------------------------------------------------
%%% @author Vldislav Kevbrin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2016 4:36 PM
%%%-------------------------------------------------------------------
-module(log_name).
-author("Vladislav Kevbrin").
-include("ranobebook_grabber.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, init/1, stop/0, new_logfile/0]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> terminate([], []).

init([]) ->
  ssl:start(),
  Time = (?SECONDS_IN_DAY - calendar:time_to_seconds(time())) * ?SEC_TO_MSEC + ?DISPLACEMENT,
  Timer = erlang:send_after(Time, self(), check),
  {ok, Timer}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(check, OldTimer) ->
  erlang:cancel_timer(OldTimer),
  new_logfile(),
  Timer = erlang:send_after(?MILISECONDS_IN_DAY, self(), check),
  {noreply, Timer}.

new_logfile() ->
  {Year, Month, Day} = date(),
  YearStr = lists:flatten(io_lib:format("~4..0B", [Year])),
  MonthStr = lists:flatten(io_lib:format("~2..0B", [Month])),
  DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
  {ok, LogName} = application:get_env(ranobebook_grabber_app, log_name),
  FileName = lists:concat(["log/", LogName, "-", YearStr, "-", MonthStr, "-", DayStr, ".log"]),
  LogReaded = file:read_file(FileName),
  case LogReaded of
    {error, enoent} ->
      error_logger:logfile(close),
      case error_logger:logfile({open, FileName}) of
        {error, _} -> erlang:send_after(?DISPLACEMENT, self(), check);
        _ -> ok
      end;
    {error, Reason} ->
      error_logger:error_msg("This log file not readed because ~p", [Reason]);
    {_, Bin} ->
      error_logger:logfile(close),
      case error_logger:logfile({open, FileName}) of
        {error, _} -> erlang:send_after(?DISPLACEMENT, self(), check);
        _ -> error_logger:info_msg("REBOOT ~n ~ts ~n", [common:unicode_to_one_byte(Bin)])
      end
  end.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

handle_call(_Q, _From, State) -> {reply, ok, State}.