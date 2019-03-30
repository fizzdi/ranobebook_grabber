%% -*- mode: nitrogen -*-
%% -*- coding: utf-8 -*-
-module(upload).
-author("linsierra").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").


main() ->
  CKEditorFuncNum = wf:q('CKEditorFuncNum'),
  CKEditor = wf:q('CKEditor'),
  LangCode = wf:q('langCode'),
  FilePath = "priv/static/images/",

  Req = wf_context:request_bridge(),
  UploadedFiles = Req:post_files(),
  case UploadedFiles of
    [] -> ok;
    [{_, FileName, TmpPath, _, _} | _] ->
      file:rename(TmpPath, FilePath ++ FileName),
      build_images_list()
  end.

title() -> "Загрузка файлов".

build_images_list() ->
  {_, Files} = file:list_dir("priv/static/images/"),
  {_, [_ | Result]} = lists:mapfoldl(
    fun(CurFileName, Acc) ->
      RelativePath = "/images/" ++ CurFileName,
      AbsFilePath = filename:absname(RelativePath),
      CurJson = ",{\"image\":\"" ++ RelativePath ++ "\"}",
      {CurJson, Acc ++ CurJson}
    end, "", Files),
  JSON = "[" ++ Result ++ "]",
  file:write_file("priv/static/ckeditor/plugins/imagebrowser/images.json", JSON).
