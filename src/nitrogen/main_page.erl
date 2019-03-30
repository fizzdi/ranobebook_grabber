%%%-------------------------------------------------------------------
%%% @author Dmitriy Kuznetsov
%%% @copyright (C) 2019, MaerTech LLC
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2019
%%%-------------------------------------------------------------------
-module(main_page).
-author("Dmitriy Kuznetsov").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("ranobebook_grabber.hrl").

-define(CHAPTER_STEP, 50).

main() -> #template{file = "./priv/templates/bare.html", bindings = [{'THIS_PAGE', ?MODULE}]}.

title() -> "Главная страница".
header_title() -> title().

help() -> atom_to_list(?MODULE).

get_chapter_row(Chapter) ->
  #tablerow{ cells=[
    #tablecell{text = Chapter#books_in_fb2.chapter },
    #tablecell{text = Chapter#books_in_fb2.title },
    #tablecell{body = #link {text = "Скачать", postback = {download, [Chapter#books_in_fb2.file_name]}} }
  ]}.

get_book_row(Book) ->
  Body = #tablerow{ cells=#tablecell{body=#table {
    rows = [
      #tablerow { cells = [
        #tableheader {body = #link {text = Book#book.full_name, url=Book#book.url}}
      ]},
      #tablerow {cells = #tablecell{body = #panel {id = Book#book.mnemonic}}}
    ]
  }}},
  update_book(Book),
  Body.


wizard_event({wizard, Book}) ->
  update_book(Book).

processing_chapters([]) ->
  [#table {
    rows = [
      #tablerow {cells = [
        #tableheader{text = "Глава"},
        #tableheader{text = "Название"},
        #tableheader{}
      ]
      }]
  }];
processing_chapters(Chapters) when length(Chapters) =< ?CHAPTER_STEP ->
  [#table {
    rows = [
      #tablerow {cells = [
        #tableheader{text = "Глава"},
        #tableheader{text = "Название"},
        #tableheader{body=#button{
          text="Скачать",
          postback = {download, [Chapter#books_in_fb2.file_name || Chapter <- Chapters]
          }}}
      ]},
      [get_chapter_row(Chapter) || Chapter <- Chapters]
    ]
  }];
processing_chapters(Chapters) ->
  {H, T} = lists:split(?CHAPTER_STEP, Chapters),
  [processing_chapters(H) | processing_chapters(T)].

update_book(Book) ->
  Chapters = pgsql:get_chapters(Book#book.id),
  Steps = processing_chapters(Chapters),
  wf:replace(defer, Book#book.mnemonic,
    #wizard{
      id=Book#book.mnemonic,
      steps=Steps,
      tag={wizard, Book}
    }
  ).

column_center() ->
  Books = pgsql:get_books(),
  Body = [
    #button{text = "Синхронизировать", postback = sync},
    #table {
      id = books,
      class = "sides",
      rows = [get_book_row(Book) || Book <- Books]
    }
  ],
  Body.

grabbing(Book) ->
  case grabber:grab_book(Book#book{last_chapter = Book#book.last_chapter + 1}) of
    {no_free, RBook} ->
      common:flash_msg(lists:concat([RBook#book.full_name, " Глава ", RBook#book.last_chapter, " <b>платная</b>"]));
    {ok, RBook} ->
      common:flash_msg(lists:concat([RBook#book.full_name, " Глава ", RBook#book.last_chapter, " сохранена"])),
      wf:flush(),
      grabbing(RBook);
    {limit, RBook} ->
      common:flash_msg(lists:concat([RBook#book.full_name, " Глава ", RBook#book.last_chapter, " предел"]))
  end.

event(sync) ->
  Books = pgsql:get_books(),
  lists:map(
    fun(Book) ->
      wf:comet(
        fun() ->
          grabbing(Book#book{last_chapter = Book#book.last_chapter}),
          wf:wire("location.reload()")
        end
      )
    end, Books
  );
event({download, FileName}) ->
  wf:session(files, FileName),
  wf:redirect("download");
event(Event) -> common:event(Event).

event_invalid(_) -> ok.