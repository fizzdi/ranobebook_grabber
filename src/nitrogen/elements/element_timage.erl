%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_timage).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
  reflect/0,
  render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, timage).

-spec render_element(#timage{}) -> body().
render_element(Record) ->
  Title = wf:html_encode(Record#timage.tooltip, Record#timage.html_encode),
  wf_tags:emit_tag(image, [], [
    {id, Record#timage.html_id},
    {class, Record#timage.class},
    {src, Record#timage.image},
    {name, Record#timage.html_name},
    {title, Title}
  ]).
