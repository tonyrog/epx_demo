%%% File    : eevent.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPX event tester
%%% Created : 29 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(eevent).

-compile(export_all).

start() ->
    epx:start(),
    spawn(
      fun() ->
	      Win = epx:window_create(50, 50, 100, 100,
				      [key_press, key_release,
				       button_press, button_release,
				       motion, configure, resize, focus,
				       crossing,
				       button, wheel]),
	      epx:window_attach(Win),
	      loop(Win)
      end).

start1() ->
    epx:start(),
    spawn(
      fun() ->
	      Win = epx:window_create(50, 50, 100, 100,
				      [key_press, key_release,
				       motion, configure, resize, focus,
				       crossing]),
	      epx:window_attach(Win),
	      loop(Win)
      end).

loop(Win) ->
    receive
	{epx_event, Win, Event} ->
	    event(Win,Event)
    end.

event(Win, Event) ->
    case Event of
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
	{resize,{Width,Height,_Depth}} ->
	    io:format("RESIZE: width=~w, height=~w\n", [Width,Height]),
	    loop(Win);
	{enter, Where} ->
	    io:format("ENTER: where=~w\n", [Where]),
	    loop(Win);	    
	{leave, Where} ->
	    io:format("LEAVE: where=~w\n", [Where]),
	    loop(Win);	    
	focus_in ->
	    io:format("FOCUS_IN\n", []),
	    loop(Win);	    	    
	focus_out ->
	    io:format("FOCUS_OUT\n", []),
	    loop(Win);
	close ->
	    io:format("CLOSE\n", []),
	    epx:window_detach(Win),
	    ok;
	Other  ->
	    io:format("OTHER: ~p\n", [Other]),
	    loop(Win)
    end.


	
	

    



