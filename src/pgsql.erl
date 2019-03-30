%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Kuznetsov Dmitriy
%%% @copyright (C) 2017, <MaerTech LLC>
%%% @doc
%%% postgresql interface for ranobebook_grabber app
%%% @end
%%%-------------------------------------------------------------------
-module(pgsql).
-author("Kuznetsov Dmitriy").
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([terminate/2, code_change/3, handle_info/2]).
-compile(export_all).
-include("ranobebook_grabber.hrl").
-include_lib("epgsql/include/epgsql.hrl").
%% Start process
%%-----------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% Stop process
%%-----------------------------------------------------------------------------
stop() -> terminate([], []).
%% Init state
%%-----------------------------------------------------------------------------
init(_) ->
  {ok, AppDbConf} = application:get_env(ranobebook_grabber_app, pgsql_conf),
  Host = proplists:get_value(host, AppDbConf),
  Port = proplists:get_value(port, AppDbConf),
  User = proplists:get_value(user, AppDbConf),
  Password = proplists:get_value(password, AppDbConf),
  DB = proplists:get_value(db, AppDbConf),
  {ok, State} = epgsql:connect(Host, User, Password, [
    {database, DB},
    {port, Port}
  ]),
  {ok, State}.
%% Get DB state
%%-----------------------------------------------------------------------------
get_db_state() -> gen_server:call(?MODULE, {get_db_state}).
%%% Query API


%%----------------------------------------------------------------------------------------------------------------------
%% BOOK CONTROL
%%----------------------------------------------------------------------------------------------------------------------

get_books({get_books, State}) ->
  Q = "SELECT * FROM books",
  P = [],
  R = epgsql:equery(State, Q, P),
%%  write_query_log("Get books:", Q, P, R),
  epgsql_conv:as_record(R, book, record_info(fields, book)).
get_books() -> gen_server:call(?MODULE, {get_books}).

get_chapters({get_chapters, BookId, State}) ->
  Q = "SELECT * FROM books_in_fb2 WHERE book_id = $1 ORDER BY chapter DESC",
  P = [BookId],
  R = epgsql:equery(State, Q, P),
%%  write_query_log("Get chapters:", Q, P, R),
  epgsql_conv:as_record(R, books_in_fb2, record_info(fields, books_in_fb2));
get_chapters(BookId) -> gen_server:call(?MODULE, {get_chapters, BookId}).

save_book_chapter({save_book_chapter, Chapter, State}) ->
  Q = "SELECT save_book($1, $2, $3, $4)",
  P = [Chapter#books_in_fb2.book_id, Chapter#books_in_fb2.chapter, Chapter#books_in_fb2.title, Chapter#books_in_fb2.file_name],
  R = epgsql:equery(State, Q, P),
%%  write_query_log("Save chapter:", Q, P, R),
 ok;
save_book_chapter(Chapter) -> gen_server:call(?MODULE, {save_book_chapter, Chapter}).

%%----------------------------------------------------------------------------------------------------------------------
%% Get DB state
handle_call({get_db_state}, _From, State) -> {reply, State, State};
%% Process synchronous query requests
%%----------------------------------------------------------------------------------------------------------------------
handle_call(Arg, _From, State) ->
  Function = element(1, Arg),
  NewArg = list_to_tuple(lists:append(tuple_to_list(Arg), [State])),
  Result = apply(?MODULE, Function, [NewArg]),
  {reply, Result, State}.
%%----------------------------------------------------------------------------------------------------------------------
%% Process all other asynchronous requests
handle_cast(_Msg, State) -> {noreply, State}.
%% Other gen_server handlers
%%-----------------------------------------------------------------------------
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) ->
  epgsql:close(_State),
  ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
%% Write log to file
write_query_log(Msg, Query, Params, QueryResult) ->
%%  ok.
  InfoMsg = io_lib:format("~ts~n~ts~nUSING:~n~p~n", [Msg, Query, Params]),
  case QueryResult of
    {error, ErrorPacket} ->
      error_logger:error_msg(InfoMsg ++ "PostgreSQL error: ~p~n", [ErrorPacket]), error;
    _ -> error_logger:info_msg(InfoMsg ++ "PostgreSQL result ok~n"), ok
  end.
