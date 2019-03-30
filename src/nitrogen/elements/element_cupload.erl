%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_cupload).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
  reflect/0,
  render_element/1,
  event/1
]).

%% #upload allows a user to upload a file.
%%
%% How it works: - This element creates an <input type=file ...> HTML element
%% on the page, wrapped in a <form>, with all of the required parameters
%% necessary to fake the system into believing it is a real postback call.
%%
%% - When the user clicks the upload button, first the 'upload_started' event
%% gets fired, calling start_upload_event(Tag) on the Module or Page.
%%
%% - Then, the browser begins uploading the file to the server. The multipart
%% file is parsed in SimpleBridge.
%%
%% - Finally, once the upload is complete, control is passed on to Nitrogen,
%% which reads the parameters sent over in the first step and calls the
%% 'upload_finished' event in this module.
%%
%% - The 'upload_finished' emits Javascript that causes *another* postback,
%% this time to the 'upload_event' event in this module, which then calls
%% Module:finish_upload_event(Tag, OriginalName, TempFile, Node).  The reason
%% we do this extra postback is because the upload itself happens in a form
%% separate from the main Nitrogen page (otherwise the main Nitrogen page would
%% need to refresh) so this is our way of getting the main page to see the
%% event.

-spec reflect() -> [atom()].
reflect() -> record_info(fields, cupload).

-spec render_element(#cupload{}) -> body().
render_element(Record) ->
  Anchor = Record#cupload.anchor,
  Multiple = Record#cupload.multiple,
  Droppable = Record#cupload.droppable,
  DroppableText = Record#cupload.droppable_text,
  FileInputText = Record#cupload.file_text,
  ShowButton = Record#cupload.show_button,
  ButtonText = Record#cupload.button_text,
  DataFields = Record#cupload.data_fields,
  Accept = Record#cupload.accept,
  StartedTag = {upload_started, Record},
  FinishedTag = {upload_finished, Record},
  FormID = wf:temp_id(),
  IFrameID = wf:temp_id(),
  ButtonID = wf:temp_id(),
  DropID = wf:temp_id(),
  DropListingID = wf:temp_id(),
  FileInputID = wf:temp_id(),
  FakeFileInputID = wf:temp_id(),

  Param = [
    {droppable, Droppable},
    {autoupload, not(ShowButton)}
  ],

  JSONParam = nitro_mochijson2:encode({struct, Param}),
  SubmitJS = wf:f("Nitrogen.$send_pending_files(jQuery('#~s').get(0),jQuery('#~s').get(0));", [FormID, FileInputID]),
  UploadJS = wf:f("Nitrogen.$attach_upload_handle_dragdrop(jQuery('#~s').get(0),jQuery('#~s').get(0),~s);",
    [FormID, FileInputID, JSONParam]),

  PostbackInfo = wf_event:serialize_event_context(FinishedTag, Record#cupload.id, undefined, false, ?MODULE),

  % Create a postback that is called when the user first starts the upload...
  wf:wire(Anchor, #event{show_if = (not ShowButton), type = change, delegate = ?MODULE, postback = StartedTag}),
  wf:wire(ButtonID, #event{show_if = ShowButton, type = click, delegate = ?MODULE, postback = StartedTag}),

  % If the button is invisible, then start uploading when the user selects a file.
  %wf:wire(Anchor, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
  wf:wire(ButtonID, #event{show_if = ShowButton, type = click, actions = SubmitJS}),

  wf:wire(UploadJS),

  % Render the controls and hidden iframe...
  FormContent = [
    %% IE9 does not support the droppable option, so let's just hide the drop field
    "<!--[if lte IE 9]>
        <style type='text/css'> .upload_drop {display: none} </style>
    <![endif]-->",

    #panel{
      show_if = Droppable,
      id = DropID,
      class = [upload_drop, 'dropzone-container'],
      body = [
        #panel{
          class = [dropzone, 'ui-corner-all'],
          text = DroppableText
        }
      ]
    },
    #panel{
      %show_if=Droppable,
      class = upload_progress,
      body = ""
    },
    #list{
      show_if = Droppable,
      id = DropListingID,
      class = upload_droplist
    },

    #panel{
      style = "position: relative;",
      body = [
        wf_tags:emit_tag(input, [
          {type, button},
          {class, "btn btn btn-info"},
          {style, "margin: 0 auto; "},
%%                    padding: 1px 6px; position: absolute; top: 0px; left: 0px; z-index: 1;"},
          {value, FileInputText},
          {id, FakeFileInputID}
        ]),

        wf_tags:emit_tag(input, [
          {name, file},
          {data_fields, DataFields},
          {multiple, Multiple},
          {class, [no_postback, FileInputID | Anchor]},
          {id, FileInputID},
          {type, file},
          {accept, Accept},
          {style, "width: 100%; height: 100%; margin-top: -24px; border: 2px outset rgb(221, 221, 221); padding: 1px 6px; opacity: 0; filter:alpha(opacity: 0); position: absolute; z-index: 2;"}
        ])
      ]
    },

    wf_tags:emit_tag(input, [
      {name, eventContext},
      {type, hidden},
      {class, no_postback},
      {value, PostbackInfo}
    ]),

    wf_tags:emit_tag(input, [
      {name, pageContext},
      {type, hidden},
      {class, no_postback},
      {value, ""}
    ]),

    wf_tags:emit_tag(input, [
      {type, hidden},
      {class, no_postback},
      {value, ""}
    ]),

    #button{id = ButtonID, class = btn, show_if = ShowButton, text = ButtonText}
  ],

  [
    wf_tags:emit_tag(form, FormContent, [
      {id, FormID},
      {name, upload},
      {method, 'POST'},
      {enctype, "multipart/form-data"},
      {class, no_postback},
      {target, IFrameID}
    ])
  ].


-spec event(any()) -> any().
% This event is fired when the user first clicks the upload button.
event({upload_started, Record}) ->
  Module = wf:coalesce([Record#cupload.delegate, wf:page_module()]),
  Module:start_upload_event(Record#cupload.tag);


% This event is called once the upload post happens behind the scenes.
% It happens somewhat outside of Nitrogen, so the next thing we do
% is trigger a postback that happens inside of Nitrogen.
event({upload_finished, Record}) ->
  wf_context:type(first_request),
  Req = wf_context:request_bridge(),

  % % Create the postback...
  {Filename, NewTag} = case Req:post_files() of
                         [] ->
                           {undefined, {upload_event, Record, undefined, undefined, undefined}};
                         [UploadedFile | _] ->
                           OriginalName = UploadedFile:original_name(),
                           TempFile = UploadedFile:temp_file(),
                           {OriginalName, {upload_event, Record, OriginalName, TempFile, node()}}
                       end,

  % Make the tag...
  Anchor = wf_context:anchor(),
  ValidationGroup = wf_context:event_validation_group(),
  HandleInvalid = wf_context:event_handle_invalid(),
  Postback = wf_event:generate_postback_script(NewTag, Anchor, ValidationGroup, HandleInvalid, undefined, ?MODULE,
    undefined),

  % Set the response...
  wf_context:data([
    "Nitrogen.$upload_finished(\"", wf:js_escape(Filename), "\");",
    Postback
  ]);

% This event is fired by the upload_finished event, it calls
% back to the page or control that contained the upload element.
event({upload_event, Record, OriginalName, TempFile, Node}) ->
  Module = wf:coalesce([Record#cupload.delegate, wf:page_module()]),
  Module:finish_upload_event(Record#cupload.tag, OriginalName, TempFile, Node).
