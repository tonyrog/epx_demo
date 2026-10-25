%%% File    : epx_event_test.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : epx event tester
%%% Created : 29 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_event_test).

-compile(export_all).

-include_lib("epx/include/epx.hrl").

start() ->
    start([key_press, key_release]).

start(EventMask) ->
    epx:start(),
    Win = epx:window_create(50, 50, 100, 100, EventMask),
    epx:window_attach(Win),
    loop(Win).

loop(Win) ->
    receive
	{epx_event, Win, Event} ->
	    event(Win,Event);
	Other ->
	    io:format("GOT: win=~p,  ~p\n", [Win,Other]),
	    loop(Win)
    end.

event(Win, Event) ->
    case Event of
	close ->
	    io:format("CLOSE\n", []),
	    epx:window_detach(Win),
	    ok;	    
	{key_press, Sym, Mod, Code} ->
	    io:format("KEY PRESS: ~w  mod=~w, code=~w\n", [Sym,Mod,Code]),
	    if Sym == $q ->
		    epx:window_detach(Win),
		    ok;
	       true ->
		    loop(Win)
	    end;
	{key_release, Sym, Mod, Code} ->
	    io:format("KEY RELEASE: ~w  mod=~w, code=~w\n", [Sym,Mod,Code]),
	    loop(Win);
	{button_press, Button, Where} ->
	    io:format("BUTTON PRESS: ~w where=~w\n", [Button, Where]),
	    loop(Win);
	{button_release, Button, Where} ->
	    io:format("BUTTON RELEASE: ~w where=~w\n", [Button, Where]),
	    loop(Win);
	{motion, Button, Where} ->
	    io:format("MOTION: ~w where=~w\n", [Button, Where]),
	    loop(Win);
	Other ->
	    io:format("OTHER: ~w\n", [Other]),
	    loop(Win)
    end.

text_demo() ->
    text_demo("\"Hello World.\"").

text_demo(Text) ->
    epx:start(),
    Win = epx:window_create(50, 50, 320, 240, []),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(320,240),
    epx:pixmap_fill(Pix, {80,80,80}),
    epx:pixmap_attach(Pix),
    {ok,Font} = epx_font:match([{name,"Times"},{size,18}]),
    FInfo = epx_font:info(Font),
    io:format("Font Info = ~p\n", [FInfo]),
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color({0,255,0,0}),
    
    epx:draw_string(Pix, 10, FInfo#epx_font_info.ascent, Text),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, 320, 240),
    receive
	{epx_event, Win, close} ->
	    epx:pixmap_detach(Pix),
	    epx:window_detach(Win),
	    ok
    end.
    
-define(LDEMO_WIDTH,  1000).
-define(LDEMO_HEIGHT, 500).

line_demo() ->
    epx:start(),
    Win = epx:window_create(50, 50, ?LDEMO_WIDTH, ?LDEMO_HEIGHT, []),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(?LDEMO_WIDTH, ?LDEMO_HEIGHT),
    epx:pixmap_attach(Pix),
    Gc = epx:gc_create(),
    draw_rot_loop(Pix,Win,Gc,0,5000),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, ?LDEMO_WIDTH, ?LDEMO_HEIGHT),
    receive
	{epx_event, Win, close} ->
	    io:format("CLOSE\n", []),
	    epx:pixmap_detach(Pix),
	    epx:window_detach(Win),
	    ok
    end.

draw_rot_loop(_Pix, _Win, _Gc, _A, 0) ->
    ok;
draw_rot_loop(Pix, Win, Gc, A, I) ->
    Fg = (A rem 200) + 40,
    Bg = (A rem 200),
    epx:pixmap_fill(Pix, {255,192,Fg,Fg}),
    epx_gc:set_foreground_color(Gc, {255,Bg,Bg,Bg}),
    draw_rot(Pix,Gc,A rem 360),
    epx:pixmap_draw(Pix, Win, 0, 0, 0, 0, ?LDEMO_WIDTH, ?LDEMO_HEIGHT),
    draw_rot_loop(Pix,Win,Gc,A+1,I-1).


draw_rot(Pix,Gc) ->
    draw_rot(Pix,Gc,0).

draw_rot(Pix,Gc,A) ->
    draw_rot(Pix,Gc,A,360+A,10).

draw_rot(_Pix,_Gc,A,Max,_Step) when A >= Max ->
    ok;
draw_rot(Pix,Gc,A, Max,Step) ->
    Ar = A*math:pi()/180,
    X = math:cos(Ar),
    Y = math:sin(Ar),
    epx_gc:set_line_style(Gc, solid),
    X0 = ?LDEMO_WIDTH / 4,
    Y0 = ?LDEMO_HEIGHT / 2,
    epx:pixmap_draw_line(Pix,Gc, X0, Y0, 
			 trunc(X0*X+X0), trunc(Y0*Y+Y0)),
    epx_gc:set_line_style(Gc, aalias),
    epx:pixmap_draw_line(Pix, Gc, 3*X0, Y0, 
			 trunc(X0*X+3*X0), trunc(Y0*Y+Y0)),
    draw_rot(Pix,Gc,A+Step,Max,Step).
