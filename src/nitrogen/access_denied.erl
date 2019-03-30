%% -*- mode: nitrogen -*-
%% -*- coding: utf-8 -*-
-module(access_denied).
-author("Maxim Khvatlin").
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file = "./priv/templates/access_denied.html"}.
title() -> "Срок сессии истек".
context() -> common:force_back_to_auth_system().
