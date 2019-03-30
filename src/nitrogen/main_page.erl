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

-define(CHAPTER_STEP, 3).

main() -> #template{file = "./priv/templates/bare.html", bindings = [{'THIS_PAGE', ?MODULE}]}.

title() -> "Главная страница".
header_title() -> title().

help() -> atom_to_list(?MODULE).

get_chapter_row(Chapter) ->
  #tablerow{ cells=[
    #tablecell{text = Chapter#books_in_fb2.chapter },
    #tablecell{text = Chapter#books_in_fb2.title },
    #tablecell{body = #link {text = "Скачать", postback = {download, Chapter#books_in_fb2.file_name}} }
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

step(1) ->
  [
    #label{text="Enter your name (required)"},
    #textbox{id=name}
  ];
step(2) ->
  [
    #label{text="Enter your email address (optional)"},
    #textbox{id=email, type=email}
  ];
step(3) ->
  [
    #label{text="Enter your fantasy race (required)"},
    #dropdown{id=race, options=[
      {"","Choose Race"},
      {"Orc","Orc"},
      {"Human","Human"},
      {"Elf","Elf"},
      {"Robot","Robot"}
    ]},
    #label{text="Enter your class (Required)"},
    #dropdown{id=class, options=[
      {"","Choose Class"},
      {"Wizard","Wizard"},
      {"Fighter","Fighter"},
      {"Thief","Theif"},
      {"AI Tech","Artificial Intelligence Technician"}
    ]}
  ].

wizard_event(wizard) ->
  [Name, Email, Race, Class] = wf:mq([name, email, race, class]),
  wf:replace(create_character, #panel{body=[
    #h3{text="You've created a new character"},
    "Name: ",Name,#br{},
    "Email: ",Email,#br{},
    "Race: ",Race,#br{},
    "Class: ",Class
  ]}).

update_book(Book) ->
  Titles = ["Name", "Email", "Race"],
  Steps = [step(1), step(2), step(3)],
  %%      #wizard{
%%        id=create_character,
%%        titles=Titles,
%%        steps=Steps,
%%        tag=wizard
%%      }
 Chapters = pgsql:get_chapters(Book#book.id),
  wf:replace(defer, Book#book.mnemonic, #table {
    rows = [
      #tablerow {cells = [
        #tableheader{text = "Глава"},
        #tableheader{text = "Название"},
        #tableheader{}
      ]},
      [get_chapter_row(Chapter) || Chapter <- Chapters]
    ]
  }).

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
  common:debug_message(Book#book.last_chapter),
  case common:debug_message(grabber:grab_book(Book#book{last_chapter = Book#book.last_chapter + 1})) of
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
          grabbing(Book#book{last_chapter = Book#book.last_chapter + 1}),
          wf:wire("location.reload()")
        end
      )
    end, Books
  );
event({download, FileName}) ->
  wf:redirect(lists:concat(["download?filename=", FileName]));
event(Event) -> common:event(Event).

event_invalid(_) -> ok.