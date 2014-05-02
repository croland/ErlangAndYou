-module(server).

-include_lib("wx/include/wx.hrl").

-export([new/1, init/1, do_init/1]).
-export([draw/3, handle_event/2, handle_sync_event/3, handle_info/2, handle_call/2, code_change/3, terminate/2]).

-behaviour(wx_object).
-record(state, {parent, config, win}).



new(Parent) ->
  wx:new(),
  wx_object:start_link(?MODULE, [Parent], []).

init([Parent]) ->
  State = wx:batch(fun() -> do_init(Parent) end),
  State.

do_init(Parent) ->
  %%Parent = proplists:get_value(parent, Config),
  Frame = wxFrame:new(wx:null(), -1, "Bam", []),
  Panel = wxPanel:new(Frame, []),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),


  Win = wxPanel:new(Panel, []),
  wxPanel:connect(Win, paint, [callback]),

  %%Button = wxButton:new(Frame, -1, [{label, "Press Me"}]),
  %%wxButton:connect(Button, command_button_clicked),

  wxSizer:add(Sizer, Win, [{flag, ?wxEXPAND}, {proportion,1}]),
  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion,1}]),
  wxPanel:setSizer(Panel, MainSizer),

  wxWindow:show(Frame),

  {Panel, #state{parent=Panel, win=Win, config={}}}.


draw(PaintDC, Pen, Brush) ->
  try
    Canvas = wxGraphicsContext:create(PaintDC),
    wxGraphicsContext:setPen(Canvas, wxPen:new(?wxBLACK)),
    wxGraphicsContext:setBrush(Canvas, Brush),
    %%wxGraphicsContext:setFont(Canvas, wxFont:new(?wxITALIC_FONT)),
    wxGraphicsContext:drawRectangle(Canvas, 0.0, 10.0, 200.0, 200.0),
    ok
    
  catch _:{not_supported, _} ->
    Err = "wxGraphicsContext",
    io:format(Err, [])
  end.



handle_sync_event(#wx{event=#wxPaint{}}, _, State=#state{parent=Panel, win=Win, config=Config}) ->
  PaintDC = wxPaintDC:new(Win),
  draw(PaintDC, ?wxBLACK, wxBrush:new({30, 175, 23, 127})),
  wxPaintDC:destroy(PaintDC),
  ok. 

handle_event(#wx{event=#wxClose{}}, State=#state{parent=Panel, win=Win, config=Config}) ->
      io:format("~p Closing ~n", [self()]),
      wxFrame:destroy(Win),
      ok;

handle_event(#wx{event=#wxCommand{type=command_button_clicked}}, State=#state{parent=Panel, win=Win, config=Config}) ->
  io:format("Clicked!~n"),
  {Panel, #state{parent=Panel, win=Win, config=Config}}.


handle_info(Msg, State) ->
  io:format("Got Info ~p ~n", [Msg]),
  {noreply, State}.

handle_call(Msg, State) ->
  io:format("Got Call ~p ~n", [Msg]),
  {reply, ok, State}.


code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  ok.
