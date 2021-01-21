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
	]).

-define(VIEW_WIDTH,  1024).
-define(VIEW_HEIGHT, 768).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
    
start() ->
    application:ensure_all_started(epx),
    epxw:start(?MODULE, [], 
	       [{view_width,?VIEW_WIDTH},{view_height,?VIEW_HEIGHT}]).

init(_Window, _Screen, _Opts) ->
    {ok,Font} = epx_font:match([{size,24}]),
    Ascent = epx:font_info(Font, ascent),
    epxw:set_status_text("Hello World"),
    #{ font => Font, ascent => Ascent, text => "Hello World!" }.

configure(Event, State) ->
    io:format("CONFIGURE: ~w\n", [Event]),
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

draw(Pixels, _Rect, 
     State= #{ font := Font, text := Text, ascent := Ascent }) ->
    %% io:format("Rect = ~p\n", [Rect]),
    {X0,Y0} = epxw:content_pos(),
    epx_gc:set_font(Font),
    {W,H}  = epx_font:dimension(Font,Text),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    X = (?VIEW_WIDTH - W) div 2,
    Y = ((?VIEW_HEIGHT - H) div 2) + Ascent,
    epx:draw_string(Pixels, X-X0, Y-Y0, Text),
    State.

command(Key, Mod, State) ->
    io:format("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.
