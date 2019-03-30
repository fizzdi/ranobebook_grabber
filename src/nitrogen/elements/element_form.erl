%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_form).
-author("Maxim Khvatlin").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
  reflect/0,
  render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, form).

-spec render_element(#form{}) -> body().
render_element(Record) ->
  Body =
    [
      wf:html_encode(Record#form.text, Record#form.html_encode),
      Record#form.body
    ],
  wf_tags:emit_tag(form, Body,
    [
      {id, Record#form.html_id},
      {class, Record#form.class},
      {style, Record#form.style},
      {data_fields, Record#form.data_fields}
    ]).
