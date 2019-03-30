%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_clabel).
-author("Maxim Khvatlin").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
  reflect/0,
  render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, clabel).

-spec render_element(#clabel{}) -> body().
render_element(Record) ->
  Text = wf:html_encode(Record#clabel.text, Record#clabel.html_encode),
  wf_tags:emit_tag(span, Text, [
    {id, Record#clabel.html_id},
    {class, Record#clabel.class},
    {style, "display: block; margin-bottom: 5px;"},
    {data_fields, Record#clabel.data_fields}
  ]).
