%% -*- mode: nitrogen -*-
%% -*- coding: utf-8 -*-
-module(authentication).
-author("Maxim Khvatlin").

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("ranobebook_grabber.hrl").

authenticate_attempt(Login) ->
  true.