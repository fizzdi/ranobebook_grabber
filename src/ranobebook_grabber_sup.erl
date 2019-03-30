%%%-------------------------------------------------------------------
%%% @author Natalya Plotnikova
%%% @copyright (C) 2015, <MaerTech>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ranobebook_grabber_sup).
-author("Natalya Plotnikova").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  Children = [
    ?CHILD(pgsql, worker),
    ?CHILD(log_name, worker),
    ?CHILD(nitrogen_sup, supervisor)
  ],
  {ok, {{one_for_one, 1000000, 1}, Children}}.

