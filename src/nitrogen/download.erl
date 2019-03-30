%% -*- mode: nitrogen -*-
%% -*- coding: utf-8 -*-
-module(download).
-author("linsierra").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").


main() ->
  FilePaths = wf:session(files),
  wf:content_type("application/x-download"),
  FileName = common:get_current_timestamp() ++ ".zip",
  wf:header("Content-Disposition", "attachment; filename=\"" ++ FileName ++ "\""),
  Files = lists:map(
    fun(FilePath) ->
      {ok, FileContent} = file:read_file(FilePath),
      {filename:basename(FilePath), FileContent}
    end, FilePaths
  ),
  ZipName = "tmp/" ++ FileName,
  zip:create(ZipName, Files),
  {ok, Data} = file:read_file(ZipName),
  file:delete(ZipName),
  Data.

title() -> "Загрузка файлов".
