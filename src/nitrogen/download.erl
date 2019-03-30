%% -*- mode: nitrogen -*-
%% -*- coding: utf-8 -*-
-module(download).
-author("linsierra").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").


main() ->
  FilePath = wf:q(filename),
  [FileName | _] = lists:reverse(re:split(FilePath, "/")),
  wf:header("Content-Disposition", "attachment; filename=\"" ++ binary_to_list(FileName) ++ "\""),
  wf:content_type("application/x-download"),
  {ok, Data} = file:read_file(FilePath), Data.

title() -> "Загрузка файлов".
