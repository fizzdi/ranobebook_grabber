%%%-------------------------------------------------------------------
%%% @author Natalya Plotnikova
%%% @copyright (C) 2015, <MaerTech>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ranobebook_grabber_app).
-author("Natalya Plotnikova").

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-define(APPS, [simple_cache, crypto, nprocreg, simple_bridge, epgsql, ranch, inets, ranobebook_grabber_app]).
start() ->
  [begin application:start(A) end || A <- ?APPS],
  {ok, Sync} = application:get_env(ranobebook_grabber_app, sync),
  case Sync of
    true -> application:start(sync);
    _ -> ok
  end.

start(_StartType, _StartArgs) ->
  log_name:new_logfile(),
  error_logger:info_msg("ranobebook_grabber application is started~n"),
  ranobebook_grabber_sup:start_link().

stop(_State) ->
  error_logger:logfile(close),
  ok.
