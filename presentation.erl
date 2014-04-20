-module(presentation).

-behaviour(wx_object).

-export([new/2, show/1, destroy/1]).
-export([start/0, start/1, start_link/0, start_link/1]).
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, {parent, config, win, pen, brush, font }).



start() ->
	start([]).

start(Config) ->
	wx_object:start(?MODULE, Config, []).


start_link() ->
	start_link([]).

start_link(Config) ->
	wx_object:start_link(?MODULE, Config, []).

init(Config) ->
	wx:new(Config),
	wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent, []),

	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "wxGraphicsContext"}]),
	Win = wxPanel:new(Panel, []),
	Pen = ?wxBLACK_PEN,
	Brush = wxBrush:new({30, 175, 23, 127}),
	Font = ?wxITALIC_FONT,
	wxPanel:connect(Win, paint, [callback]),
	wxSizer:add(Sizer, Win, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxPanel:setSizer(Panel, MainSizer),
	{Panel, #state{parent=Panel, config=Config, win=Win, pen=Pen, brush=Brush, font=Font}}.


handle_sync_event(#wx{event = #wxPaint{}},_,#state{win=Win, pen=Pen, brush=Brush, font=Font}) ->
	DC = wxPaintDC:new(Win),
	draw(DC, Pen, Brush, Font),
	wxPaintDC:destroy(DC),
	ok.

handle_event(#wx{}, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	demo:format(State#state.config, "Got Info ~p\n", [Msg]),
	{noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
	wxPanel:destory(Panel),
	{stop, normal, ok, State};

handle_call(Msg, _From, State) ->
	demo:format(State#state.config, "Got Call ~p\n", [Msg]),
	{reply, {error, nyi}, State}.

handle_cast(Msg, State) ->
	io:format("Got cast ~p~n", [Msg]),
	{noreply, State}.	


code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, _State) ->
	ok.



draw(Win, Pen, Brush, Font) ->
	try
		Canvas = wxGraphicsContext:create(Win),
		wxGraphicsContext:setPen(Canvas, Pen),
		wxGraphicsContext:setBrush(Canvas, Brush),
		wxGraphicsContext:setFont(Canvas, Font, {0, 0, 50}),

		wxGraphicsContext:drawRoundedRectangle(Canvas, 35.0, 35.0, 100.0, 50.0, 10.0),
		wxGraphicsContext:drawText(Canvas, "Hello some text", 60.0, 55.0),
		ok
	catch _:{not_supported, _} ->
		Err = "wxGraphicsContext not available in this build",
		io:format(Err, [])
	end.

