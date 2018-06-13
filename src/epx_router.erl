%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Example to show dynamic grid routing
%%% @end
%%% Created : 18 Jun 2013 by Tony Rogvall <tony@rogvall.se>

-module(epx_router).


-export([start/0, start/1, start/2]).

-define(BGCOLOR, white).

-define(XOFFS,  5).
-define(YOFFS,  5).
-define(TASK_W, 8).
-define(TASK_H, 8).
-define(XPAD,   2).
-define(YPAD,   2).

start() ->
    start(50,30).

start([Arg]) when is_atom(Arg) ->
    N = list_to_integer(atom_to_list(Arg)),
    start(N,N);
start([Arg1,Arg2]) when is_atom(Arg1), is_atom(Arg2) ->
    N = list_to_integer(atom_to_list(Arg1)),
    M = list_to_integer(atom_to_list(Arg1)),
    start(N,M).

start(N,M) ->
    epx:start(),
    spawn_link(fun() -> init(N,M) end).

init(N, M) ->
    Width  = ?XOFFS + N*(?TASK_W+2*?XPAD) + ?XOFFS,
    Height = ?YOFFS + M*(?TASK_H+2*?YPAD) + ?YOFFS,
    Window = epx:window_create(50, 50, Width, Height),
    Bg   = epx:pixmap_create(Width, Height, argb),
    Fg   = epx:pixmap_create(Width, Height, argb),
    epx:window_attach(Window),
    epx:pixmap_fill(Bg, ?BGCOLOR),    
    epx:pixmap_attach(Fg),
    ets:new(xy_reg,      [public, named_table, ordered_set]),
    ets:new(color_reg,   [public, named_table, ordered_set]),
    ets:insert(color_reg, {0, black}),
    ets:insert(color_reg, {1, red}),
    ets:insert(color_reg, {2, green}),
    ets:insert(color_reg, {3, blue}),
    ets:insert(color_reg, {4, cyan}),
    ets:insert(color_reg, {5, orange}),
    ets:insert(color_reg, {6, yellow}),
    %% Create NxM routing processes
    Pxy = [spawn_link(fun() ->
			      ets:insert(xy_reg, {{I,J},self()}),
			      task_init(I,J,N,M) 
		      end) || 
	      I <- lists:seq(1,N),
	      J <- lists:seq(1,M) ],
    redraw(Pxy,Bg,Fg,Window),
    %% start the for corners with red,green,blue,cyan
    send({1,1}, {route,{0,1},1}),
    send({N,1}, {route,{N+1,1},2}),
    send({1,M}, {route,{0,M},3}),
    send({N,M}, {route,{N+1,M},4}),
    
    redraw(Pxy,Bg,Fg,Window),
    wait(Pxy,Bg,Fg,Window).

wait(Pxy,Bg,Fg,Window) ->
    receive
	{epx_event,Win, close} ->
	    epx:window_detach(Win)
    after 100 ->
	    redraw(Pxy,Bg,Fg,Window),
	    wait(Pxy,Bg,Fg,Window)
    end.

redraw(Pxy,Bg,Fg,Window) ->
    %% io:format("send redraw\n"),
    lists:foreach(fun(Pid) -> Pid ! {redraw,self(),Bg} end, Pxy),
    lists:foreach(fun(Pid) -> receive {Pid,done} -> ok end end, Pxy),
    %% io:format("swap\n"),
    epx:pixmap_copy_to(Bg, Fg),
    epx:pixmap_draw(Fg, Window, 0, 0, 0, 0,
		    epx:window_info(Window, width),
		    epx:window_info(Window, height)),
    epx:pixmap_fill(Bg, ?BGCOLOR).
%%
%%       A
%%       ^
%%    +==II==+
%%    |      |
%%    |      |
%% D <-      -> B
%%   <-      ->
%%    |      |
%%    |      |
%%    +==II==+
%%      v
%%      C
%%
task_init(I,J,N,M) ->
    %% neighbour map
    Nv = {{I+1,J},{I,J-1},{I-1,J},{I,J+1}},
    %% valid neighbour indices
    Nw = [Y || Y <- lists:seq(1,4),
	       element(1,element(Y,Nv)) >= 1,
	       element(1,element(Y,Nv)) =< N,
	       element(2,element(Y,Nv)) >= 1,
	       element(2,element(Y,Nv)) =< M],
    %% io:format("init: ~w nv=~w, nw=~w\n", [{I,J},Nv,Nw]),
    task_loop(I,J,{0,0,0,0},Nv,Nw,undefined).

task_loop(I,J,RULD,Nv,Nw,TRef) ->
    %% first poll redraw requests
    receive
	{redraw,From,Pixmap} ->
	    task_redraw(Pixmap,I,J,RULD),
	    From ! {self(),done}
    after 0 ->
	    ok
    end,
    %% now wait for both route/redraw
    receive
	{redraw,From1,Pixmap1} ->
	    task_redraw(Pixmap1,I,J,RULD),
	    From1 ! {self(),done},
	    task_loop(I,J,RULD,Nv,Nw,TRef);
		
	{route,{Ni,Nj},K} when TRef =:= undefined ->
	    case index({Ni,Nj},Nv) of
		0 ->
		    %% ignore requests from non-neighbours
		    task_loop(I,J,RULD,Nv,Nw,TRef);
		Ix ->
		    RULD1 = setelement(Ix,RULD,K),
		    Nk = count(K,RULD1),
		    Rs = if Nk >= 2 ->
				 Nw -- [Ix];
			    true ->
				 random_list(Nw -- [Ix], 2)
			 end,
		    TRef1 = erlang:start_timer(100, self(), 
					       {route_propagate,Rs,K}),
		    task_loop(I,J,RULD1,Nv,Nw,TRef1)
	    end;
	{route,_,_} -> %% propagation time
	    task_loop(I,J,RULD,Nv,Nw,TRef);

	{timeout,TRef,{route_propagate,Rs,K}} ->
	    %% io:format("propagate: ~w k=~w\n", [Rs,K]),
	    lists:foreach(
	      fun(Rw) ->
		      W = element(Rw,Nv),
		      send(W,{route,{I,J},K})
	      end, Rs),
	    task_loop(I,J,RULD,Nv,Nw,undefined);
	_Other ->
	    io:format("OTHER: ~w\n", [_Other]),
	    task_loop(I,J,RULD,Nv,Nw,TRef)
    end.

task_redraw(Pixmap,I,J,_RULD={R,U,L,D}) ->
    X = ?XOFFS + (I-1)*(?TASK_W+2*?XPAD) + ?XPAD,
    Y = ?YOFFS + (J-1)*(?TASK_H+2*?YPAD) + ?YPAD,
    epx_gc:set_fill_style(none),
    epx_gc:set_foreground_color(black),
    epx:draw_rectangle(Pixmap, X, Y, ?TASK_W, ?TASK_H),
    epx_gc:set_fill_style(solid),
    fill_rectangle(Pixmap,X+3,Y+3,2,2,0),
    fill_rectangle(Pixmap,X+5,Y+3,5,2,R),  %% right
    fill_rectangle(Pixmap,X+3,Y-2,2,5,U),  %% up
    fill_rectangle(Pixmap,X-2,Y+3,5,2,L),  %% left
    fill_rectangle(Pixmap,X+3,Y+5,2,5,D),  %% down
    ok.


%% randomly select N elements from the list Xs
random_list(Xs, N) ->
    Ys = [X || {_,X} <- lists:sort([{rand:uniform(),X} || X <- Xs])],
    lists:sublist(Ys, N).


%% find first element E in tuple T and return index, return 0 if not found
index(E, T) ->
    index_(tuple_size(T),T,E).

index_(0,_T,_E) -> 
    0;  %% not present
index_(I,T,E) when element(I,T) =:= E ->
    I;
index_(I,T,E) when I>0 ->
    index_(I-1,T,E).
%%
%% Count number of occurences of element E in tuple T
%%
count(E, T) ->
    count_(tuple_size(T),T,E,0).

count_(0,_T,_E,Count) -> Count;
count_(I,T,E,Count) when element(I,T) =:= E ->
    count_(I-1,T,E,Count+1);
count_(I,T,E,Count) when I>0 ->
    count_(I-1,T,E,Count).


send(IJ, Mesg) ->
    case ets:lookup(xy_reg, IJ) of
	[{_, Pid}] -> Pid ! Mesg;
	_ -> ok
    end.

fill_rectangle(Pixmap,X,Y,W,H,C) ->
    [{_,Color}] = ets:lookup(color_reg, C),
    epx_gc:set_fill_color(Color),
    epx:draw_rectangle(Pixmap,X,Y,W,H).
