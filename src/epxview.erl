%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%     Present Three views src, dst and result
%%% @end
%%% Created : 27 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(epxview).

-compile(export_all).

-include_lib("epx/include/epx_image.hrl").
%%
%% Create a window 100x380 with three picture frames
%% one src, one dst and one result
%%  
%%

start() ->
    epx:start(),
    spawn(fun() -> init() end).

load_src(File) ->
    epxview ! {load_src, File}.

load_dst(File) ->
    epxview ! {load_dst, File}.

operation(Op) ->
    epxview ! {operation, Op}.


init() ->
    register(?MODULE, self()),
    Win = epx:window_create(30, 30, 380, 120),
    epx:window_attach(Win),
    Src = epx:pixmap_create(100, 100, argb),
    Dst = epx:pixmap_create(100, 100, argb),
    Res = epx:pixmap_create(100, 100, argb),
    epx:pixmap_attach(Src),
    epx:pixmap_attach(Dst),
    epx:pixmap_attach(Res),
    loop(Win,Src,Dst,Res).

loop(Win, Src, Dst, Res) ->
    receive
	{load_src, File} ->
	    case epx_image:load(File) of
		{ok,Image} ->
		    [Pixmap|_] = Image#epx_image.pixmaps,
		    epx:pixmap_scale(Pixmap, Src, 100, 100),
		    epx:pixmap_draw(Src, Win, 0, 0, 20, 10, 100, 100),
		    loop(Win, Src, Dst, Res);
		{error,Reason} ->
		    io:format("load_src: error ~p\n", [Reason]),
		    loop(Win, Src, Dst, Res)
	    end;
	{load_dst, File} ->
	    case epx_image:load(File) of
		{ok,Image} ->
		    [Pixmap|_] = Image#epx_image.pixmaps,
		    epx:pixmap_scale(Pixmap, Dst, 100, 100),
		    epx:pixmap_draw(Dst, Win, 0, 0, 140, 10, 100, 100),
		    loop(Win, Src, Dst, Res);
		{error,Reason} ->
		    io:format("load_src: error ~p\n", [Reason]),
		    loop(Win, Src, Dst, Res)
	    end;
	{operation, Op} ->
	    epx:pixmap_copy_to(Dst, Res),
	    epx:pixmap_operation_area(Src, Res, Op, 0, 0, 0, 0, 100, 100),
	    epx:pixmap_draw(Res, Win, 0, 0, 260, 10, 100, 100),
	    loop(Win, Src, Dst, Res)
    end.


		    
		    



