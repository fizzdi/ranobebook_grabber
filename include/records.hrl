%% Include the automatically generated plugins directory
-include_lib("nitrogen_core/include/wf.hrl").
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below
-record(form, {?ELEMENT_BASE(element_form),
		body=[]                 :: body(),
		text=""                 :: text(),
		html_encode=true        :: html_encode()
	}).

-record(legend, {?ELEMENT_BASE(element_legend),
		body=[]                 :: body(),
		text=""                 :: text(),
		html_encode=true        :: html_encode(),
		for=""                  :: id()
	}).

-record(cupload, {?ELEMENT_BASE(element_cupload),
		delegate                				:: module(),
		tag                     				:: term(),
		accept=""               				:: text(),
		show_button=true						:: boolean(),
		file_text="Выбрать файл"				:: text(),
		button_text="Загрузить"					:: text(),
		droppable=false							:: boolean(),
		droppable_text="Перетяните файлы сюда"	:: text(),
		multiple=false          				:: boolean()
	}).

-record(clabel, {?ELEMENT_BASE(element_clabel),
		text=""                 :: text(),
		html_encode=true        :: html_encode()
	}).

-record(udropdown, {?ELEMENT_BASE(element_udropdown),
	options=[]              :: undefined | options(),
	size=auto               :: auto | integer(),
	html_encode=true        :: html_encode(),
	postback                :: term(),
	handle_invalid=false    :: boolean(),
	on_invalid              :: undefined | actions(),
	delegate                :: module(),
	value                   :: atom() | text() | integer(),
	next                    :: id(),
	multiple=false          :: boolean(),
	disabled=false          :: boolean(),
	html_name               :: html_name()
}).

-record(timage, {?ELEMENT_BASE(element_timage),
	image=""                  :: text(),
	html_name = ""            :: text(),
	tooltip=""                :: text(),
	html_encode=true          :: html_encode()
}).


