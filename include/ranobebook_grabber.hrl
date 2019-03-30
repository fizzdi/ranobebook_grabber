%%%-------------------------------------------------------------------
%%% @author Dmitriy Kuznetsov
%%% @copyright (C) 2019, MaerTech LLC
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2019
%%%-------------------------------------------------------------------
-author("Dmitriy Kuznetsov").

%% Record to process user role information
-record(book, {
  id                :: number(),
  full_name        :: binary() | string(),
  save_name        :: binary() | string(),
  url              :: binary() | string(),
  last_chapter     :: number(),
  author_sname     :: binary() | string(),
  author_fname     :: binary() | string(),
  mnemonic          :: binary() | string()
}).

-record(books_in_fb2, {
  id                :: number(),
  book_id                :: number(),
  chapter                :: number(),
  title        :: binary() | string(),
  file_name        :: binary() | string(),
  updated_at
}).

%% Time consts
-define(SECONDS_IN_DAY, 86400).
-define(SEC_TO_MSEC, 1000).
-define(DISPLACEMENT, 60000).
-define(MILISECONDS_IN_DAY, 86400000).