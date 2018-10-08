%%% File    : eproc.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Trace drawing
%%% Created : 15 Nov 2009 by Tony Rogvall <tony@rogvall.se>

-module(eproc).

-compile(export_all).

-define(WIN_WIDTH,  640).
-define(WIN_HEIGHT, 480).
-define(WIN_BG_COLOR, {255,0,0,0}).

-include_lib("epx/include/epx.hrl").

-record(p,
	{
	  id,        %% Pid|Port
	  name,      %% Pid|Port|registered_name
	  x,         %% screen x location
	  y,         %% screen y location
	  t_start,   %% time when registered (ns)
	  t_in,      %% last swap in time (ns)
	  t_last=0,  %% last cpu time (ns)
	  t_sum=0,   %% cpu time last second (ns)
	  h_size=0,  %% last known heap size
	  h_max=0    %% max heap size since last refresh
	 }).

-define(C_MIN_DISTANCE, 15).  %% min 15 pixels distance
-define(C_STEP, 2).
-define(C_DRAW_SET, 5).  %% high light message passing for 5 ticks
-define(L_DRAW_SET, 10). %% high light link creation for 10 ticks
-record(c,
	{
	  id,             %% [ID1 | ID2] (sorted pair)
	  mcount=0,       %% number of messages (ID1>ID2) or (ID2>ID1)
	  draw=0,         %% 0=not drawn, >0 draw and decrement
	  lcount=0,       %% number link high light ticks
	  link = false,   %% is linked to
	  monitor=false   %% is monitoring
	 }).
	  
-record(w,
	{
	  win,
	  pix,
	  font,
	  ascent,
	  descent,
	  width,
	  height,
	  t_redraw,   %% redraw time (timestamp)
	  t_sum = 0,  %% sum of all cpu accounted
	  ptab,       %% ets [#eproc]
	  ctab        %% message counter {P1,P2} link status
	 }).

start() ->
    start(?WIN_WIDTH, ?WIN_HEIGHT).

start(W, H) ->
    spawn(fun() -> init(W, H) end).

init(W, H) ->
    epx:start(),
    E = [key_press,key_release, button_press, button_release,
	 resize,configure, motion, left],
    Win = epx:window_create(50, 50, W, H, E),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W, H),
    epx:pixmap_attach(Pix),
    {ok,Font} = epx_font:match([{name,"Arial"},{size,10}]),
    FInfo = epx_font:info(Font),

    EWin = #w { win    = Win, 
		pix    = Pix, 
		font   = Font, 
		t_redraw = erlang:system_time(micro_seconds),
		ascent   =FInfo#epx_font_info.ascent,
		descent  =FInfo#epx_font_info.descent,
		width=W, 
		height=H,
		ptab = ets:new(eproc, [public, set, {keypos,#p.id}]),
		ctab = ets:new(eproc, [public, set, {keypos,#c.id}])
	       },
    erlang:trace(existing, true, [monotonic_timestamp, 
				  running, procs, ports, exiting, 
				  garbage_collection, send, set_on_spawn]),

    %% insert all processes
    lists:foreach(
      fun(Pid) ->
	      P = new_p(EWin, Pid),
	      insert_p(EWin, P)
      end, processes()),

    %% insert all ports
    lists:foreach(
      fun(Port) ->
	      P = new_p(EWin, Port),
	      insert_p(EWin, P)
      end, erlang:ports()),

    %% insert all process links (should include most port links...)
    lists:foreach(
      fun(Pid) ->
	      {links,Links} = process_info(Pid, links),
	      lists:foreach(fun(ID) -> do_link(EWin, Pid, ID) end, Links)
      end, processes()),

    %% {_, Rt} = statistics(runtime),
    erlang:start_timer(100, self(), redraw),
    loop(EWin).

update(W) ->
    epx:pixmap_draw(W#w.pix, W#w.win, 0, 0, 0, 0,
		    W#w.width, W#w.height).
%%
%% set color by cpu time scaled by the the refresh time
%% ranging from green => yellow => red
%%
inter_color({R0,G0,B0},{R1,G1,B1},T) ->
    {trunc(R0*(1-T) + R1*T),
     trunc(G0*(1-T) + G1*T),
     trunc(B0*(1-T) + B1*T)}.

%%
%% We must scale a bit. The idea is to show 
%% process usage cpu time as part of real time (100000 us)
%% The total cpu time is a lot less 

calc_color(Ts, TTs) ->
    %% Calculate the runtime procentage    
    S = if TTs =< 0 -> 0;
	   true -> Ts / TTs
	end,
    if S > 0.5 ->
	    inter_color({255,255,0},{255,0,0}, 2*(S-0.5));
       true ->
	    inter_color({0,255,0},{255,255,0}, 2*S)
    end.

ckey(ID1,ID2) when ID1 < ID2 ->
    [ID1|ID2];
ckey(ID1,ID2) -> [ID2|ID1].


%%
%% Radius of process log 
%% 233 => radius 10
%% 1k  => 13
%% 64k => 22
%% 1M  => 27
%% 50M => 35
%%
calc_p_radius(P) ->
    H = P#p.h_max,
    if H == 0 -> 3;
       H > 0 -> trunc(math:log(H)*2)
    end.

draw_p(W, P, _Td) ->
    if is_pid(P#p.id) ->
	    epx_gc:set_foreground_color({0,255,255,255}),
	    X = trunc(P#p.x),
	    Y = trunc(P#p.y),
	    epx:draw_string(W#w.pix, X, Y-W#w.descent, P#p.name),
	    S = 2*calc_p_radius(P),
	    C = calc_color(P#p.t_sum, W#w.t_sum),
	    epx_gc:set_fill_color(C),
	    epx_gc:set_fill_style([solid,blend]),
	    epx:draw_ellipse(W#w.pix, X, Y, S, S),
	    ets:update_element(W#w.ptab, P#p.id, {#p.h_max, P#p.h_size});
       is_port(P#p.id) ->
	    Sx = P#p.h_size,
	    Sy = P#p.h_size,
	    X = trunc(P#p.x),
	    Y = trunc(P#p.y),
	    epx_gc:set_foreground_color({0,255,255,255}),
	    epx:draw_string(W#w.pix, X, Y-W#w.descent, P#p.name),
	    epx_gc:set_fill_color({0,0,255}),
	    epx_gc:set_fill_style([solid,blend]),
	    epx:draw_rectangle(W#w.pix, X, Y, Sx, Sy)
    end.

draw_c(W, C) ->
    Key = [ID1|ID2] = C#c.id,
    case {ets:lookup(W#w.ptab, ID1),ets:lookup(W#w.ptab, ID2)} of
	{[],_} ->    ok;
	{_,[]} ->    ok;
	{[P1],[P2]} ->
	    R1 = calc_p_radius(P1),
	    R2 = calc_p_radius(P2),
	    X1 = P1#p.x+R1,
	    Y1 = P1#p.y+R1,
	    X2 = P2#p.x+R2,
	    Y2 = P2#p.y+R2,
	    Dx = X2-X1,
	    Dy = Y2-Y1,
	    Dist = math:sqrt(Dx*Dx + Dy*Dy),
	    if Dist > R1 + R2 + ?C_MIN_DISTANCE ->
		    Dx1 = (?C_STEP/Dist)*Dx,
		    Dy1 = (?C_STEP/Dist)*Dy,
		    ets:update_element(W#w.ptab, P1#p.id,
				       [{#p.x,P1#p.x+Dx1},
					{#p.y,P1#p.y+Dy1}]),
		    ets:update_element(W#w.ptab, P2#p.id,
				       [{#p.x,P2#p.x-Dx1},
					{#p.y,P2#p.y-Dy1}]);
	       true ->
		    ok
	    end,
	    %% High light counter from C_DRAW_SET -> 0
	    BaseColor = {255,0,0},
	    Color = if C#c.draw > 0 ->
			    inter_color(BaseColor, {255,255,255},
					C#c.draw/?C_DRAW_SET);
		       true -> BaseColor
		    end,
	    epx_gc:set_foreground_color(Color),
	    epx:draw_line(W#w.pix, 
			  trunc(X1), trunc(Y1), 
			  trunc(X2), trunc(Y2)),
	    ets:update_counter(W#w.ctab,Key,{#c.draw,-1,0,0})
    end.

%% draw link
draw_cl(W, C) ->
    Key = [ID1|ID2] = C#c.id,
    case {ets:lookup(W#w.ptab, ID1),ets:lookup(W#w.ptab, ID2)} of
	{[],_} ->    ok;
	{_,[]} ->    ok;
	{[P1],[P2]} ->
	    R1 = calc_p_radius(P1),
	    R2 = calc_p_radius(P2),
	    X1 = P1#p.x+R1,
	    Y1 = P1#p.y+R1,
	    X2 = P2#p.x+R2,
	    Y2 = P2#p.y+R2,
	    %% High light counter from L_DRAW_SET -> 0
	    BaseColor = {0,0,255},
	    Color = if C#c.lcount > 0 ->
			    inter_color(BaseColor, {255,255,255},
					C#c.lcount/?L_DRAW_SET);
		       true -> BaseColor
		    end,
	    epx_gc:set_foreground_color(Color),
	    epx:draw_line(W#w.pix, 
			  trunc(X1), trunc(Y1), 
			  trunc(X2), trunc(Y2)),
	    if C#c.lcount > 0 ->
		    ets:update_counter(W#w.ctab,Key,{#c.lcount,-1,0,0});
	       true ->
		    ok
	    end
    end.

%%
%% Links:    draw in blue, form a tree (if possible)
%% Monitors: draw in light blue directed graph
%% Messages: draw in red pulls processes together 
%%

%% each message between ID1,ID2 is bringing them closer (but with spring)
%% if the message passing is stopped then they are moved apart again???

redraw(W) ->
    Td = erlang:system_time(micro_seconds) - W#w.t_redraw,
    epx_gc:set_font(W#w.font),
    ets:foldl(
      fun(P,_Acc) -> draw_p(W, P, Td) end, 0, W#w.ptab),
    ets:foldl(
      fun(C,_ACC) when C#c.draw > 0 ->
	      draw_c(W, C);
	 (C,_Acc) when C#c.link ->
	      draw_cl(W, C);
	 (_C,Acc) ->
	     Acc
      end, 0, W#w.ctab).
      

loop(W) ->
    receive
	{trace_ts,ID,in,_,T} ->
	    W1 = do_in(W, ID, T),
	    loop(W1);
	{trace_ts,ID,out,_,T} ->
	    W1 = do_out(W, ID, T),
	    loop(W1);
	{trace_ts,ID,gc_end,Info,_T} ->
	    do_size(W, ID, Info), 
	    loop(W);
	{trace_ts,ID,send,_Msg,ID2,_T} ->
	    do_send(W, ID, ID2),
	    loop(W);
	{trace_ts,ID,out_exited,_,_T} ->
	    do_exit(W,ID), 
	    loop(W);
	{trace_ts,_ID,spawn,ID2,_Mfa,_T} ->
	    P = new_p(W, ID2),
	    insert_p(W, P),
	    loop(W);
	{trace_ts,ID,register,Name,_T} ->
	    do_register(W, ID, Name),
	    loop(W);
	{trace_ts,ID,unregister,_Name,_T} ->
	    do_unregister(W, ID),
	    loop(W);
	{trace_ts,ID,link,ID2,_T} ->
	    do_link(W, ID, ID2),
	    loop(W);
	{trace_ts,ID,unlink,ID2,_T} ->
	    do_unlink(W, ID, ID2),
	    loop(W);	    
	{trace_ts,_ID,open,ID2,_Name,_T} ->
	    P = new_p(W, ID2),
	    insert_p(W, P),
	    loop(W);
	{trace_ts,ID,closed,_Reason,_T} ->
	    do_exit(W,ID),
	    loop(W);
	{timeout, _TRef, redraw} ->
	    epx:pixmap_fill(W#w.pix, ?WIN_BG_COLOR),
	    redraw(W),
	    update(W),
	    erlang:start_timer(100, self(), redraw),
	    loop(W#w { t_redraw = erlang:system_time(micro_seconds)} );

	{epx_event,Win,destroy} when Win == W#w.win ->
	    ok;
	{epx_event,Win,close} when Win == W#w.win ->
	    epx:window_detach(Win);
	{epx_event,Win,{key_press,Sym,_Mod,_Code}} when Sym==$Q; Sym==$q->
	    epx:window_detach(Win);
	_ ->
	    loop(W)
    end.


do_exit(W, ID) ->
    %% mark ID as killed ? clean up later...
    %% Delete connections ....
    ets:delete(W#w.ptab, ID).

do_size(W, Pid, Info) ->
    H_size = case proplists:lookup(heap_block_size, Info) of
		 {_,Sz} -> Sz;
		 _ -> 0
	     end,
    case lookup_p(W, Pid) of
	undefined ->
	    ok;
	P ->
	    H_Max = erlang:max(H_size, P#p.h_max),
	    insert_p(W, P#p { h_size=H_size, h_max=H_Max})
    end.


do_send(W, ID1, ID2) ->
    P1 = lookup_p(W, ID1),
    P2 = lookup_p(W, ID2),
    if P1 == undefined; P2 == undefined ->
	    ok;
       true ->
	    Key = ckey(ID1,ID2),
	    case ets:member(W#w.ctab, Key) of
		true ->
		    ets:update_counter(W#w.ctab, Key, 
				       [{#c.mcount, 1},
					{#c.draw,?C_DRAW_SET,
					 ?C_DRAW_SET,?C_DRAW_SET}]);
		false ->
		    insert_c(W, #c { id=Key, mcount=1, draw = ?C_DRAW_SET})
	    end
    end.

do_link(W, ID1, ID2) ->
    case {ets:lookup(W#w.ptab, ID1),ets:lookup(W#w.ptab, ID2)} of
	{[],_} ->    ok;
	{_,[]} ->    ok;
	{[_P1],[_P2]} ->
	    Key = ckey(ID1,ID2),
	    case ets:lookup(W#w.ctab, Key) of
		[] ->
		    insert_c(W, #c { id=Key, link=true, 
				     lcount=?L_DRAW_SET});
		[C] when C#c.link ->
		    ok;
		[C] ->
		    insert_c(W, C#c { link=true, 
				      lcount=?L_DRAW_SET})
	    end
    end.

do_unlink(W, ID1, ID2) ->
    case {ets:lookup(W#w.ptab, ID1),ets:lookup(W#w.ptab, ID2)} of
	{[],_} ->    ok;
	{_,[]} ->    ok;
	{[_P1],[_P2]} ->
	    Key = ckey(ID1,ID2),
	    case ets:lookup(W#w.ctab, Key) of
		[] ->
		    ok;
		[C] ->
		    insert_c(W, C#c { link=false})
	    end
    end.

    

		    
do_in(W, Pid, T) ->
    case lookup_p(W, Pid) of
	undefined ->
	    W;
	P ->
	    insert_p(W, P#p { t_in = T }),
	    W
    end.

do_out(W, Pid, T) ->
    case lookup_p(W, Pid) of
	undefined ->
	    W;
	P ->
	    T_last = T - P#p.t_in,
	    T_sum = T_last + P#p.t_sum,
	    insert_p(W, P#p { t_in=0, t_last=T_last, t_sum=T_sum}),
	    WT_sum = T_last + W#w.t_sum,
	    W#w { t_sum = WT_sum }
    end.

lookup_p(_W, undefined) ->
    undefined;
lookup_p(W, Id) when is_atom(Id) ->
    lookup_p(W, whereis(Id));
lookup_p(W, Id) ->
    case ets:lookup(W#w.ptab, Id) of
	[] ->
	    P = new_p(W, Id),
	    insert_p(W, P),
	    P;
	[P] ->
	    P
    end.

insert_p(_W, undefined) ->   ok;
insert_p(W, P) -> ets:insert(W#w.ptab, P).

insert_c(_W, undefined) ->   ok;
insert_c(W, C) -> ets:insert(W#w.ctab, C).
    

%% 
do_register(W, ID, undefined) ->
    do_unregister(W, ID);
do_register(W, ID, Name) when is_atom(Name) ->
    case ets:lookup(W#w.ptab, ID) of
	[P] ->
	    insert_p(W, P#p { name=atom_to_list(Name)});
	[] ->
	    ok
    end.

%% unregister the name
do_unregister(W, ID) ->
    case ets:lookup(W#w.ptab, ID) of
	[P] ->
	    if is_pid(ID) ->
		    insert_p(W, P#p { name= pid_to_list(ID)});
	       is_port(ID) ->
		    insert_p(W, P#p { name= erlang:port_to_list(ID)})
	    end;
	[] ->
	    ok
    end.
    
    
new_p(W, Pid) when is_pid(Pid) ->
    case process_info(Pid, heap_size) of
	undefined -> 
	    undefined;
	{heap_size,Sz} ->
	    {X,Y} = random_point(W),
	    Now = erlang:monotonic_time(nanosecond),
	    Name = case process_info(Pid, registered_name) of
		       undefined -> pid_to_list(Pid); %% process died
		       [] -> pid_to_list(Pid);
		       {registered_name,Nm} -> atom_to_list(Nm)
		   end,
	    io:format("New_p: Name=~p\n", [Name]),
	    #p { id=Pid, name=Name, x=X, y=Y, h_size=Sz, h_max=Sz, 
		 t_in=Now, t_start=Now }
    end;
new_p(W, Port) when is_port(Port) ->
    {X,Y} = random_point(W),
    Now = erlang:monotonic_time(nanosecond),
    Name = case erlang:port_info(Port, registered_name) of
	       [] -> erlang:port_to_list(Port);
	       {registered_name, Nm} -> atom_to_list(Nm)
	   end,
    #p { id=Port, name=Name, x=X, y=Y, h_size=10, h_max=20,
	 t_in=Now, t_start=Now };
new_p(_W, undefined) ->
    undefined;
new_p(W, Name) when is_atom(Name) ->
    new_p(W, whereis(Name)).
    

random_point(W) ->
    X = 10+rand:uniform(W#w.width-20)-1,
    Y = 10+rand:uniform(W#w.height-20)-1,
    {X,Y}.
