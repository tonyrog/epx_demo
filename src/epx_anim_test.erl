%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Minor animation test functions
%%% @end
%%% Created : 27 Oct 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_anim_test).

-export([show/1]).

-record(state,
	{
	  backend,
	  window,
	  foreground_pixels,
	  background_pixels,
	  width,
	  height,
	  background = {255,0,0}
	}).
%%
%% Show dds file
%%
show([File]) when is_atom(File) ->
    show([atom_to_list(File)]);
show([File]) when is_list(File) ->
    epx:start(),
    Backend = epx_backend:default(),
    Anim = epx:animation_open(File),
    Width0 = epx:animation_info(Anim, width),
    Height0 = epx:animation_info(Anim, height),
    Format = epx:animation_info(Anim, pixel_format),
    io:format("width=~w, height=~w, pixel format = ~w\n",
	      [Width0,Height0,Format]),
    Width = Width0 + 20,
    Height = Height0 + 20,
    Window = epx:window_create(40, 40, Width, Height),
    ForegroundPx = epx:pixmap_create(Width, Height, Format),
    BackgroundPx = epx:pixmap_create(Width, Height, Format),
    epx:window_attach(Window, Backend),
    epx:pixmap_attach(BackgroundPx, Backend),
    S0 = #state { backend = Backend,
		  window = Window,
		  foreground_pixels = ForegroundPx,
		  background_pixels = BackgroundPx,
		  width = Width,
		  height = Height },
    Count = epx:animation_info(Anim, count),
    io:format("Count=~w\n", [Count]),
    loop(S0, Anim, 0, Count, 0).

loop(S, Anim, Index, Count, Timeout) ->
    receive
	{epx_event, Win, close} when Win =:= S#state.window ->
	    io:format("Got window1 close\n", []),
	    epx:pixmap_detach(S#state.background_pixels),
	    epx:window_detach(S#state.window),
	    ok;
	{epx_event, Win, Event} when Win =:= S#state.window ->
	    io:format("Got window event ~p\n", [Event]),
	    loop(S, Anim, Index, Count, Timeout);
	{epx_event, _Win, Event} ->
	    io:format("Got other window ~w event ~p\n", [_Win,Event]),
	    loop(S, Anim, Index, Count, Timeout)
    after Timeout ->
	    epx:pixmap_fill(S#state.background_pixels, S#state.background),
	    epx:animation_draw(Anim,Index,
			       S#state.background_pixels,epx_gc:current(),
			       10, 10),
	    epx:pixmap_draw(S#state.background_pixels, S#state.window,
			    0, 0, 0, 0, 
			    S#state.width, S#state.height),
	    Index1 = (Index + 1) rem Count,
	    loop(S, Anim, Index1, Count, erlang:max(10,1000 div Count))
    end.

