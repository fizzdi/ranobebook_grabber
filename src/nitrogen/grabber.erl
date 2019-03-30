%%%-------------------------------------------------------------------
%%% @author Dmitriy Kuznetsov
%%% @copyright (C) 2019, MaerTech LLC
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2019
%%%-------------------------------------------------------------------
-module(grabber).
-author("Dmitriy Kuznetsov").

-compile(export_all).

-include("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("ranobebook_grabber.hrl").

-define(FB2_HEADER, "<?xml version=\"1.0\"?><FictionBook xmlns=\"http://www.gribuser.ru/xml/fictionbook/2.0\">").
-define(FB2_FOOTER, "</FictionBook>").
-define(FB2_XMLNS, "http://www.gribuser.ru/xml/fictionbook/2.0").
-define(GENRE, "sf_fantasy").
-define(CHAPTER_URL, "https://ranobebook.com/book/biblioteka-nebesnogo-puti.9/chapter/948").
-define(ID, "LOTHP948").
-define(REPLACES, [
  {"<br>", "<empty-line/>"},
  {"<br/>", "<empty-line/>"},
  {"  ", " "},
  {"<b>", "<strong>"},
  {"</b>", "</strong>"},
  {"<i>", "<emphasis>"},
  {"</i>", "</emphasis>"}
 ]).

get_coverage(CoverageURL) ->
  case httpc:request(CoverageURL) of
    {ok, {_, _, Body}} ->
      base64:encode(Body);
    {error, Reason} ->
      error_logger:error_msg("Error in request book url: ~n~p~n", [Reason])
  end.

check_block_text([], Balance) ->
  Balance;
check_block_text([$<, $b, $r, $ ,$/, $> | TextPart] = TR, Balance) ->
  check_block_text(TextPart, Balance);
check_block_text([$<, $/ | TextPart] = TR, Balance) ->
  check_block_text(TextPart, Balance - 1);
check_block_text([$< | TextPart] = TR, Balance) ->
  check_block_text(TextPart, Balance + 1);
check_block_text([_ | TextPart] = TR, Balance) ->
  check_block_text(TextPart, Balance).

search_full_text(Tail, 0, Acc) when Acc /= [] ->
  {Tail, Acc};
search_full_text([], _, Acc) ->
  {[], Acc};
search_full_text([TextPart | Tail], Balance, Acc) ->
  case check_block_text(TextPart, Balance) of
    0 ->
      search_full_text(Tail, 0, Acc);
    NewBalance ->
      search_full_text(Tail, NewBalance, Acc ++ [TextPart])
  end.

test() ->
  {ok, TextB} = file:read_file("/home/admin/tmp.txt"),
  Text = binary_to_list(TextB),
  process_chapter([ string:trim(Str) || Str <- string:tokens(Text, "\r\n\r\n")], []).

process_chapter([[$<,$m,$e,$t,$a,$ ,$p,$r,$o,$p,$e,$r,$t,$y,$=,$",$o,$g,$:,$i,$m,$a,$g,$e,$",$ ,$c,$o,$n,$t,$e,$n,$t,$=,$" | CoverUrl] | Tail], Acc) ->
  process_chapter(Tail, Acc ++ [{cover_url, string:trim(CoverUrl, trailing, "\">")}]);
process_chapter([[$<, $t, $i, $t, $l, $e, $> | TailTitle] | Tail], Acc) ->
  FullTitle = string:trim(TailTitle, trailing, "</title>"),
  [ChapterTitle | _] = string:split(FullTitle, " | "),
  process_chapter(Tail, Acc ++ [{title, ChapterTitle}]);
process_chapter(["<div class=\"text-reader\">" | Tail], Acc) ->
  {RTail, Res} = search_full_text(Tail, 1, []),
  process_chapter(RTail, Acc ++ [{section, process_raw_chapter(Res)}]);
process_chapter(["<div class=\"text-reader\">", ChapterText, "</div>"| Tail], Acc) ->
  process_chapter(Tail, Acc ++ [{section, process_raw_chapter(ChapterText)}]);
process_chapter([H | T], Acc) ->
  process_chapter(T, Acc);
process_chapter([], Acc) -> Acc.

process_raw_chapter(ChapterRaw) ->
  lists:foldl(
    fun({OldStr, NewStr}, Acc) ->
      string:replace(Acc, OldStr, NewStr, all)
    end, ChapterRaw, ?REPLACES
  ).

save_fb2(Book, Elements) ->
  Description = {description, [
    {'title-info', [
      {genre, ["sf_fantasy"]},
      {author, [
        {'first-name', [common:unicode_to_one_byte(Book#book.author_fname)]},
        {'last-name', [common:unicode_to_one_byte(Book#book.author_sname)]}
      ]},
      {'book-title', [common:unicode_to_one_byte(Book#book.full_name)]},
      {lang, ["ru"]},
      {coverpage, [#xmlElement{
        name=image,
        attributes = [#xmlAttribute{name='l:href', value = "#cover.jpg"}]
      }]}
    ]},
    {'document-info', [
      {author, [
        {nickname, ["erlanger"]}
      ]},
      {date, [common:date_to_str(erlang:localtime())]},
      {'src-url', [?CHAPTER_URL]},
      {id, [?ID]},
      {version, ["1.0"]}
    ]}
  ]},

  ElementsXML = lists:map(
    fun
      ({cover_url, _}) -> "";
      ({ElementName, Content}) ->
        lists:concat(["<", ElementName, ">", Content, "</", ElementName, ">"])
    end, Elements
  ),

  [_, DescriptionXML] = xmerl:export_simple([Description], xmerl_xml),
  FileName = lists:flatten(lists:concat(["/home/admin/books/", Book#book.save_name, "_chapter_", Book#book.last_chapter, ".fb2"])),
  Content = lists:flatten([
    ?FB2_HEADER,
    DescriptionXML,
    "<body>", ElementsXML, "</body>",
    "<binary id=\"cover.jpg\" content-type=\"image/jpeg\">", get_coverage(proplists:get_value(cover_url, Elements)), "</binary>",
    ?FB2_FOOTER
  ]),
  R = file:write_file(FileName, Content),
  case R of
    ok -> {ok, #books_in_fb2{
      chapter = Book#book.last_chapter,
      book_id = Book#book.id,
      file_name = FileName,
      title = proplists:get_value(title, Elements)
    }};
    V -> V
  end.

grab_book(Book) ->
  case httpc:request(lists:concat([Book#book.url, "chapter/", Book#book.last_chapter])) of
    {ok, {_, _, Body}} ->
      Res = process_chapter([ string:trim(Str) || Str <- string:tokens(Body, "\r\n\r\n")], []),
      case proplists:is_defined(section, Res) of
        true ->
          case save_fb2(Book, Res) of
            {ok, Chapter} ->
              pgsql:save_book_chapter(Chapter),
              error_logger:info_msg("Save Book '~ts' chapter '~p': ~ts~n", [Book#book.full_name, Chapter#books_in_fb2.chapter, Chapter#books_in_fb2.file_name]),
              case Book#book.last_chapter >= 950 of
                true -> {limit, Book};
                _ -> {ok, Book}
              end;
            {error, Reason} ->
              error_logger:error_msg("Error in save Book '~ts' chapter '~p' reason: ~p~n", [Book#book.full_name, Book#book.last_chapter, Reason])
          end;
        _ ->
          {no_free, Book}
      end;
    {error, Reason} ->
      error_logger:error_msg("Error in request book url: ~n~p~n", [Reason])
  end.

sync() ->
  Books = pgsql:get_books(),
  lists:map(
    fun(Book) ->
      grab_book(Book#book{last_chapter = Book#book.last_chapter})
    end, Books
  ).