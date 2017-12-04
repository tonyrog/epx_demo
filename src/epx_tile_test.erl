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

-module(epx_tile_test).

-export([show/1]).

-record(state,
	{
	  backend,
	  window,
	  background_pixels,
	  pixmap,
	  width,
	  height,
	  background = {255,0,0}
	}).

-include_lib("epx/include/epx_image.hrl").

-define(WIDTH,  640).
-define(HEIGHT, 480).

%%
%% Show image file scale into 640x480 - size
%%
show([File]) when is_atom(File) ->
    show([atom_to_list(File)]);
show([File]) when is_list(File) ->
    epx:start(),
    Backend = epx_backend:default(),
    case epx_image:load(File) of
	{error,Reason} ->
	    io:format("unable to load file ~s : ~p\n", [File, Reason]),
	    erlang:halt(1);
	{ok,#epx_image {pixmaps=[Pixmap]}} ->
	    Width = ?WIDTH,
	    Height = ?HEIGHT,
	    Window = epx:window_create(40, 40, Width, Height),
	    BackgroundPx = epx:pixmap_create(Width, Height, argb),
	    epx:window_attach(Window, Backend),
	    epx:pixmap_attach(BackgroundPx, Backend),
	    S0 = #state { backend = Backend,
			  window = Window,
			  background_pixels = BackgroundPx,
			  pixmap = Pixmap,
			  width  = Width,
			  height = Height },
	    draw(S0),
	    loop(S0)
    end.


draw(S) ->
    epx:pixmap_fill(S#state.background_pixels, S#state.background),
    pixmap_tile_area(S#state.pixmap, S#state.background_pixels,
		     0, 0, ?WIDTH, ?HEIGHT, [blend]),
    epx:pixmap_draw(S#state.background_pixels, S#state.window,
		    0, 0, 0, 0, 
		    S#state.width, S#state.height).
    
loop(S) ->
    receive
	{epx_event, Win, close} when Win =:= S#state.window ->
	    io:format("Got window1 close\n", []),
	    epx:pixmap_detach(S#state.background_pixels),
	    epx:window_detach(S#state.window),
	    ok;
	{epx_event, Win, Event} when Win =:= S#state.window ->
	    io:format("Got window event ~p\n", [Event]),
	    loop(S);
	{epx_event, _Win, Event} ->
	    io:format("Got other window ~w event ~p\n", [_Win,Event]),
	    loop(S)
    end.

pixmap_tile_area(Src, Dst, DstX, DstY, DstW, DstH, Flags) ->
    SrcW  = epx:pixmap_info(Src, width),
    SrcH = epx:pixmap_info(Src, height),
    SrcX = 0,
    SrcY = 0,
    pixmap_tile_vertical(Src, Dst, 
			 SrcX, SrcY, SrcW, SrcH,
			 DstX, DstY, DstW, DstY+DstH,
			 Flags).
			
%% Tile one line
pixmap_tile_vertical(_Src, _Dst, 
		     _SrcX, _SrcY, _SrcW, _SrcH,
		     _DstX, DstY, _DstW, EndY,
		     _Flags) when DstY > EndY ->
    ok;
pixmap_tile_vertical(Src, Dst,
		     SrcX, SrcY, SrcW, SrcH,
		     DstX, DstY, DstW, EndY, Flags) ->
    pixmap_tile_horizontal(Src, Dst,
			   SrcX, SrcY, SrcW, SrcH,
			   DstX, DstY, DstX+DstW, Flags),
    pixmap_tile_vertical(Src, Dst, 
			 SrcX, SrcY, SrcW, SrcH,
			 DstX, DstY+SrcH, DstW, EndY, Flags).


%% Tile one line
pixmap_tile_horizontal(_Src, _Dst,
		       _SrcX, _SrcY, _SrcW, _SrcH,
		       DstX, _DstY, EndX, _Flags) when DstX > EndX ->
    ok;
pixmap_tile_horizontal(Src, Dst,
		       SrcX, SrcY, SrcW, SrcH,
		       DstX, DstY, EndX, Flags) ->
    epx:pixmap_copy_area(Src, Dst, SrcX, SrcY,
			 DstX, DstY, SrcW, SrcH, Flags),
    pixmap_tile_horizontal(Src, Dst,
			   SrcX, SrcY, SrcW, SrcH,
			   DstX+SrcW, DstY, EndX, Flags).

