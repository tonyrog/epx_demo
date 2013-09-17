%%% File    : epx_event_test.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : epx event tester
%%% Created : 29 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epx_event_test).

-compile(export_all).

-include_lib("epx/include/epx.hrl").

run() ->
    run([key_press, key_release]).

run(EventMask) ->
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
	    ewindow:destroy(Win),
	    ok;	    
	{key_press, Sym, Mod, Code} ->
	    io:format("KEY PRESS: ~w  mod=~w, code=~w\n", [Sym,Mod,Code]),
	    if Sym == $q ->
		    ewindow:destroy(Win),
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
	    loop(Win)
    end.

text_demo() ->
    text_demo("timBI18.efnt").

text_demo(Filename) ->
    text_demo(Filename, "\"Hello World.\"").

text_demo(Filename, Text) ->
    {ok,Win} = epx:window_create(50, 50, 320, 240, []),
    epx:window_attach(Win),
    {ok,Pic} = epx:pixmap_create(320,240),
    epx:pixmap_fill(Pic, {80,80,80}),
    epx:pixmap_attach(Pic),
    {ok,Font} = efnt:open(Filename),
    ok = efnt:map(Font),
    {ok,FInfo} = efnt:info(Font),
    io:format("Font Info = ~p\n", [FInfo]),
    egc:set_font(Font),
    egc:set_foreground_color({0,255,0,0}),
    
    efnt:draw_string(Pic, 10, FInfo#efont_info.ascent, Text),
    epixmap:draw(Pic, Win, 0, 0, 0, 0, 320, 240),
    receive
	{eevent, Win, close} ->
	    ewindow:destroy(Pic),
	    ewindow:destroy(Win),
	    ok
    end.
    

line_demo() ->
    Win = epx:window_create(50, 50, 200, 100, []),
    epx:window_attach(Win),
    Pic = epx:pixmap_create(200,100),
    epx:pixmap_attach(Pic),
    Gc = epx:gc_create(),
    draw_rot_loop(Pic,Win,Gc,0,5000),
    epx:pixmap_draw(Pic, Win, 0, 0, 0, 0, 200, 100),
    receive
	{epx_event, Win, close} ->
	    io:format("CLOSE\n", []),
	    epx:pixmap_detach(Pic),
	    epx:window_detach(Win),
	    ok
    end.

draw_rot_loop(_Pic, _Win, _Gc, _A, 0) ->
    ok;
draw_rot_loop(Pic, Win, Gc, A, I) ->
    Fg = (A rem 200) + 40,
    Bg = (A rem 200),
    epx:pixmap_fill(Pic, {255,192,Fg,Fg}),
    epx:gc_set_foreground_color(Gc, {255,Bg,Bg,Bg}),
    draw_rot(Pic,Gc,A rem 360),
    epx:pixmap_draw(Pic, Win, 0, 0, 0, 0, 200, 100),
    draw_rot_loop(Pic,Win,Gc,A+1,I-1).


draw_rot(Pic,Gc) ->
    draw_rot(Pic,Gc,0).

draw_rot(Pic,Gc,A) ->
    draw_rot(Pic,Gc,A,360+A,10).

draw_rot(_Pic,_Gc,A,Max,_Step) when A >= Max ->
    ok;
draw_rot(Pic,Gc,A, Max,Step) ->
    Ar = A*math:pi()/180,
    X = math:cos(Ar),
    Y = math:sin(Ar),
    epx:gc_set_line_style(Gc, solid),
    epx:pixmap_draw_line(Pic,Gc, 50, 50, 
			 trunc(50*X+50), trunc(50*Y+50)),
    epx:set_line_style(Gc, aalias),
    epx:pixmap_draw_line(Pic, Gc, 150, 50, 
			 trunc(50*X+150), trunc(50*Y+50)),
    draw_rot(Pic,Gc,A+Step,Max,Step).
