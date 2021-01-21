%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Demo showing off epxw
%%% @end
%%% Created : 20 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(epxw_demo).

-export([start/0]).
-export([init/3,
	 configure/2,
	 key_press/2,
	 key_release/2,
	 button_press/2,
	 button_release/2,
	 enter/2,
	 leave/2,
	 focus_in/2,
	 focus_out/2,
	 close/1,
	 draw/3,
	 command/3
	 %% select/2  - fun 
	]).

-define(VIEW_WIDTH,  720).
-define(VIEW_HEIGHT, 720).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
    
start() ->
    application:ensure_all_started(epx),
    epxw:start(#{ module => ?MODULE,
		  %% demo select as fun
		  select => fun(Rect, State) -> 
				    io:format("SELECT: ~w\n", [Rect]),
				    OldRect = maps:get(selection, State),
				    epxw:invalidate(OldRect),
				    epxw:invalidate(Rect),
				    State#{ selection => Rect }
			    end
		},
	       [hello,world], 
	       [{scroll_horizontal, bottom},
		{scroll_vertical,   left},
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{left_bar, 20},
		{top_bar, 20},
		{right_bar, 20},
		{width, 256},
		{height, 256},
		{view_width,?VIEW_WIDTH},
		{view_height,?VIEW_HEIGHT}]).

init(_Window, _Screen, Opts) ->
    io:format("INIT: Opts=~w\n", [Opts]),
    {ok,Font} = epx_font:match([{size,24}]),
    Ascent = epx:font_info(Font, ascent),
    #{ font => Font, ascent => Ascent, text => "Hello World!",
       selection => {0,0,0,0} }.

configure(Event, State) ->
    io:format("CONFIGURE: ~w\n", [Event]),
    Status = io_lib:format("Hello World, Dimension: ~wx~w",
			   [epxw:width(),epxw:height()]),
    epxw:set_status_text(Status),
    State.

key_press(Event, State) ->
    io:format("KEY_PRESS: ~w\n", [Event]),
    State.

key_release(Event, State) ->
    io:format("KEY_RELEASE: ~w\n", [Event]),
    State.

button_press(Event, State) ->
    io:format("BUTTON_PRESS: ~w\n", [Event]),
    State.

button_release(Event, State) ->
    io:format("BUTTON_RELEASE: ~w\n", [Event]),
    State.

enter(Event, State) ->
    io:format("ENTER: ~w\n", [Event]),
    State.

leave(Event, State) ->
    io:format("LEAVE: ~w\n", [Event]),
    State.

focus_in(Event, State) ->
    io:format("FOCUS_IN: ~w\n", [Event]),
    State.

focus_out(Event, State) ->
    io:format("FOCUS_OUT: ~w\n", [Event]),
    State.

close(State) ->
    io:format("CLOSE:\n", []),
    State.

draw(Pixels, _Dirty, 
     State= #{ font := Font, text := Text, ascent := Ascent,
	       selection := Selection }) ->
    io:format("DRAW: Rect = ~p\n", [_Dirty]),
    {X0,Y0} = epxw:content_pos(),
    epx_gc:set_font(Font),
    {W,H}  = epx_font:dimension(Font,Text),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    X = (?VIEW_WIDTH - W) div 2,
    Y = ((?VIEW_HEIGHT - H) div 2) + Ascent,
    epx:draw_string(Pixels, X-X0, Y-Y0, Text),
    case Selection of 
	undefined -> empty;
	{_,_,0,_} -> empty;
	{_,_,_,0} -> empty;
	_ ->
	    epx_gc:set_fill_style(blend),
	    epx_gc:set_fill_color({127,127,127,127}),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(2),
	    epx:draw_rectangle(Pixels, Selection)
    end,
    State.

%%select(Rect, State) ->
%%    io:format("SELECT: ~w\n", [Rect]),
%%    State# { selection => Rect }.

command(Key, Mod, State) ->
    io:format("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.
