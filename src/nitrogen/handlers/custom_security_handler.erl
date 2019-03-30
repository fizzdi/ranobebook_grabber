%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Kuznetsov Dmitriy
%%% @copyright (C) 2017, <MaerTech LLC>
%%%-------------------------------------------------------------------
-module(custom_security_handler).
-author("Kuznetsov Dmitriy").
-behaviour(security_handler).

-export([init/2, finish/2]).

-include("ranobebook_grabber.hrl").

init(_Config, State) ->
%%  case authorize_attempt() of
%%    true -> ok;
%%    false ->
%%      wf:info("Authorization system: User has been not authenticated"),
%%      wf_context:page_module(main_page)
%%  end,
  case wf:page_module() of
    file_not_found_page ->
      wf_context:page_module(main_page);
    V -> V
  end,
  {ok, State}.

finish(_Config, State) ->
  {ok, State}.

%% PRIVATE FUNCTIONS %%
%% Try to authorize current user
authorize_attempt() ->
  Page = wf:page_module(),
  case Page of
    static_file -> true;
    login -> true;
    _ ->
      case wf:user() of
        undefined -> false;
        _ -> true
      end
  end.
