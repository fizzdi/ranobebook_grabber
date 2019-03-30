%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_legend).
-author("Maxim Khvatlin").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
  reflect/0,
  render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, form).

-spec render_element(#legend{}) -> body().
render_element(Record) ->
  Body =
    [
      wf:html_encode(Record#legend.text, Record#legend.html_encode),
      Record#legend.body
    ],
  wf_tags:emit_tag(legend, Body,
    [
      {id, Record#legend.html_id},
      {class, Record#legend.class},
      {style, Record#legend.style},
      {for, Record#legend.for},
      {data_fields, Record#legend.data_fields}
    ]).
