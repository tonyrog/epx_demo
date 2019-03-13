%%%-------------------------------------------------------------------
%%% File    : epx_mon.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : 
%%%
%%% Draw timing curves on erlang processes, 
%%% use swap-in and swap-out information on 
%%% processes and ports to display information
%%%
%%%
%%% Created :  1 Jun 2009 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(epx_mon).

-compile(export_all).

-define(DEFAULT_BUFFER,    1024).  %% 1K default sample buffer
-define(DEFAULT_TIME,      1000).  %% 1s default sample time
-define(DEFAULT_TIMEOUT,   5000).  %% 5s trigger timeout
-define(DEFAULT_DELAY,     0).     %% 0s delay before trigger check
-define(DEFAULT_PREBUFFER, 50).    %% default prebuffer size
-define(DEFAULT_INIT,      []).
-define(DEFAULT_TRIGGER,   fun(_Evt,_St) -> true end).
-define(DEFAULT_FLAGS, [set_on_spawn, set_on_link,
			send, 'receive', exiting,
			garbage_collection]).

%% Window constants
-define(MIN_WIDTH,  100).
-define(MIN_HEIGHT, 64).

-define(WIN_WIDTH,  400).
-define(WIN_HEIGHT, 240).
-define(WIN_BG_COLOR, {0,0,0}).
%% Buffer view constants
-define(VIEW_LEFT_OFFS,   64).
-define(VIEW_RIGHT_OFFS,  20).
-define(VIEW_TOP_OFFS,    20).
-define(VIEW_BOTTOM_OFFS, 32).
-define(VIEW_CELL_HEIGHT, 20).

%% Two cell colors alternating
-define(VIEW_BG_COLOR1, {16#C0,16#C0,16#C0}).
-define(VIEW_BG_COLOR2, {16#B0,16#B0,16#B0}).

-define(VIEW_FG_COLOR, {16#AA,16#AA,16#AA}).

-define(BUFFER_BG_COLOR, {16#C0,16#C0,16#C0}).
-define(BUFFER_FG_COLOR, {16#FF,16#FF,16#FF}).

-define(TIME_FG_COLOR, {16#3f,16#3f,16#ff}).
-define(GARB_FG_COLOR, {16#F0,16#F0,16#F0}).
-define(FLANK_LENGTH, 3).     %% just for looks
-define(TIME_HEIGHT_OFFS, 4). %% offset
-define(TIME_HEIGHT,     10).

-define(TIME_STEP, 10000).    %% step is 10us

-include_lib("epx/include/epx.hrl").

-record(emon,
	{
	  win,          %% window handle
	  pix,          %% pixmap handle
	  font,         %% font handle
	  ascent,       %% font ascent value
	  width,        %% window width
	  height,       %% window height
	  view,         %% view rectangle
	  scale=0,      %% scale factor 2^(scale)
	  pi=0,         %% process index of "top" process
	  t0,           %% first time stamp
	  t1,           %% last time stamp
	  td0,          %% first display timestamp
	  td1,          %% last displayed timestamp
	  ptab,         %% process/port table
	  ntab,         %% process/port name table
	  etab,         %% event table
	  esize         %% max event table size
	 }).
%%
%%
%% Options supported
%%  {time, T}      Number of milliseconds to buffer
%%  {buffer, N}    Max samples to buffer
%%  {delay, T}     Delay before start
%%  {prebuffer,N}  Number of samples to save before trigger
%%  {timeout, T}   Trigger timeout
%%  {trigger,Fun}  Trigger Function, return true to start trigger
%%  {init,Arg}     Initial argument to trigger function
%%  {add,[Flag]}   Add Trace flags to default flags
%%  {delete,[Flag]}   Delete Trace flags from default flags
%%  {flags,[Flag]} Set Trace flags:
%%      Always added for internal use
%%          timestamp 
%%             Adds a timestamp T to the trace message and change the
%%             trace tuple tag from 'trace' to 'trace_ts'
%%          running
%%             Message
%%                {trace_ts,ID,in,_MFa,T}
%%                {trace_ts,ID,out,_MFa,T}
%%
%%      Supported
%%            set_on_spawn
%%                Propgate trace flags (including set_on_spawn) to spawned
%%                processes.
%%
%%            set_on_first_spawn
%%                Propgate trace flags (excluding set_on_spawn) to spawned
%%                processes.
%%
%%            set_on_link
%%                Propgate trace flags (including set_on_link) to linked
%%                processes.
%%
%%            set_on_first_link
%%                Propgate trace flags (excluding set_on_link) to linked
%%                processes.
%%
%%            send 
%%                Message
%%                   {trace_ts,SRC,send,Message,DST,T}
%%                   {trace_ts,SRC,send_to_non_existing,Message,DST,T}
%%
%%            receive
%%                   {trace_ts,ID,'receive',Message,T}
%%
%%            garbage_collection
%%                   {trace_ts,ID,gc_start,Info,T}
%%                   {trace_ts,ID,gc_end,Info,T}
%%                
%%            exiting
%%                   {trace_ts,ID,in_exiting,Reason,T}
%%                   {trace_ts,ID,out_exiting,Reason,T}
%%                   {trace_ts,ID,out_exited,Reason,T}
%%
%%            procs
%%                   {trace_ts,ID,spawn,ID2,{M,F,A},T}
%%                   {trace_ts,ID,exit,Reason,T}
%%                   {trace_ts,ID,register,Name,T}
%%                   {trace_ts,ID,unregister,Name,T}
%%                   {trace_ts,ID,link,ID2,T}
%%                   {trace_ts,ID,unlink,ID2,T}
%%                   {trace_ts,ID,getting_linked,ID2,T}
%%                   {trace_ts,ID,getting_unlinked,ID2,T}
%%            ports
%%                   {trace_ts,ID,open,ID2,Name,T}
%%                   {trace_ts,ID,closed,Reason,T}
%%                   {trace_ts,ID,register,Name,T}
%%                   {trace_ts,ID,unregister,Name,T}
%%
start() ->
    start(all).

start(Spec) ->
    start_it(Spec,[]).

start(Spec, Opts) ->
    start_it(Spec, Opts).

start(M,F,A) when is_atom(M),is_atom(F),is_list(A) ->
    start_it({M,F,A},[]).

start(M,F,A,Opts) when is_atom(M),is_atom(F),is_list(A),is_list(Opts) ->
    start_it({M,F,A},Opts).


start_it(Pid, Opts) when is_pid(Pid), is_list(Opts) ->
    start_it_now(Pid,Opts);
start_it(Fun, Opts) when is_function(Fun), is_list(Opts) ->
    start_it_now(Fun,Opts);
start_it(S={M,F,A},Opts) when is_atom(M),is_atom(F),is_list(A),is_list(Opts) ->
    start_it_now(S,Opts);
start_it(existing, Opts) ->
    start_it_now(existing, Opts);
start_it(new, Opts) ->
    start_it_now(new, Opts);
start_it(all, Opts) ->
    start_it_now(all, Opts).

start_it_now(Spec, Opts) ->
    spawn_link(fun() -> init(Spec,Opts) end).

options() ->
    [buffer,time,timeout,delay,prebuffer,trigger,init,flags,width,height].

default(buffer)      -> ?DEFAULT_BUFFER;
default(time)        -> ?DEFAULT_TIME;
default(timeout)     -> ?DEFAULT_TIMEOUT;
default(delay)       -> ?DEFAULT_DELAY;
default(prebuffer)   -> ?DEFAULT_PREBUFFER;
default(trigger)     -> ?DEFAULT_TRIGGER;
default(init)        -> ?DEFAULT_INIT;
default(flags)       -> ?DEFAULT_FLAGS;
default(add)         -> [];
default(delete)      -> [];
default(width)       -> ?WIN_WIDTH;
default(height)      -> ?WIN_HEIGHT;
default(_)           -> undefined.
    
help() ->
    io:format("emon:start(Spec[,Options])\n"),
    io:format("  Spec: {M,F,A} | fun(Evt) -> boolean | existing | new | all\n"),
    io:format("  Options:\n"),
    io:format("    {buffer,N}    - Max number of samples stored (~w)\n",
	      [default(buffer)]),
    io:format("    {prebuffer,N} - Max number of samples before trigger (~w)\n",
	      [default(prebuffer)]),
    io:format("    {time,T}      - Max time to buffer in ms (~w)\n",
	      [default(time)]),
    io:format("    {delay,T}     - Delay in ms before trigger check (~w)\n",
	      [default(delay)]),
    io:format("    {timeout,T}    - Max time to wait for trigger in ms (~w)\n",
	      [default(timeout)]),
    io:format("    {flags,Fs}     - Trace flags to use (~w)\n",
	      [default(flags)]),
    io:format("    {add,Fs}       - Add trace flags to default flags\n", []),
    io:format("    {delete,Fs}    - Delete trace flags from default flags\n", []),
    io:format("    {trigger,Fun}  - Trigger function fun(Evt,St) -> true|St'\n",
	      []),
    io:format("    {init,Term}    - Trigger initial state (~w)\n",
	      [default(init)]).


init(Spec, Opts) ->
    %% ptab: ID => Index x State
    %%       Index => ID
    %% etab: Time-stamp => Pid x Event
    %% ntab  Name => ID
    ESize = getopt(buffer, Opts),
    Em = #emon { ptab = ets:new(emon, [public,set]),
		 ntab = ets:new(emon, [public,set]),
		 etab = ets:new(emon, [public,ordered_set]),
		 esize = ESize
	       },
    %% Trap exit?
    Collector = spawn_link(
		  fun() -> 
			  collect(Spec, Em, Opts)
		  end),
    %% Remove Collector / self from ptab 
    del_id(Em, Collector),
    del_id(Em, self()),
    
    W = getopt(width,Opts),
    H = getopt(height,Opts),

    Ref = erlang:monitor(process,Collector),
    %% Fixme: draw initial after a short time?
    receive
	{'DOWN',Ref,_, _, _} ->
	    draw(Em,W,H)
    end.

draw(Em,W,H) ->
    update_names(Em),
    epx:start(),
    T0 = ets:first(Em#emon.etab),
    T1 = ets:last(Em#emon.etab),
    Td0 = find_trigger_time(Em, T0),
    Em1 = window(Em,W,H),
    Em2  = Em1#emon { scale=0, td0=Td0, t0=T0, t1=T1, pi=0 },
    Em3 = redraw(Em2),
    window_loop(Em3).

find_trigger_time(Em,T0) ->
    case ets:match(Em#emon.etab, {'$1','$2',trigger}) of
	[[Td0,_]|_] ->
	    Td0 + 10*?TIME_STEP;
	[] ->
	    T0
    end.
    

redraw(Em) ->
    epx:pixmap_fill(Em#emon.pix, {0,0,0}),     %% fill background black
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color({255,255,255}),       %% rectangle color
    epx_gc:set_border_width(0),
    epx_gc:set_line_style([solid]), %% blend,aalias]),
    epx_gc:set_line_width(1),
    epx_gc:set_font(Em#emon.font),

    %% Draw time lines 10us appart
    draw_time(Em, 10),

    %% epx_gc:set_line_style([blend,aalias]), %% solid),

    %% Draw active lines with blue color
    epx_gc:set_foreground_color({0,0,255}),

    %% Update PTAB with process state at Td0 - a bit heavy....
    update_ptab(Em),

    %% write pids and names
    draw_names(Em),

    %% scan trough ETab checking status on PTab

    {X0,_,Width,_} = Em#emon.view,
    T0 = Em#emon.td0,
    T1 = ets:next(Em#emon.etab, Em#emon.td0),
    X1 = X0+Width-1,
    Em1 = draw_ts(Em,T0,T1,math:pow(2,Em#emon.scale),X0,X1),
    %% draw buffer scale at the bottom
    draw_buffer_scale(Em1),

    %% update window
    epx:pixmap_draw(Em1#emon.pix, Em1#emon.win, 0, 0, 0, 0,
		    Em1#emon.width, Em1#emon.height),
    Em1.
%%
%% Draw events in region X0 - X1 => T0 - T1
%%
draw_ts(Em,T,'$end_of_table',_Scale,_X,_X1) ->
    Em#emon{ td1=T };
draw_ts(Em,T,Ti,Scale,X,X1) when X < X1 ->
    case ets:lookup(Em#emon.etab,Ti) of
	[{_Ti,Pid,Evt}] ->
	    Xn = X + trunc(((Ti-T)*Scale)/1000),
	    if Xn < X1 ->
		    draw_ps(Em,Em#emon.pi,Pid,Evt,X,Xn),
		    draw_ts(Em,Ti,ets:next(Em#emon.etab,Ti),Scale,Xn,X1);
	       true ->
		    draw_ps(Em,Em#emon.pi,undefined,undefined,X,X1),
		    Em#emon{ td1=T+(X1-X) }
	    end;
	[] ->
	    draw_ts(Em,T,ets:next(Em#emon.etab,Ti),Scale,X,X1)
    end;
draw_ts(Em,T,_Ti,_Scale,_X,_X1) ->
    Em#emon{ td1=T }.


%% toal height is CELL_HEIGHT
yoffs(Y,in) -> Y+?TIME_HEIGHT_OFFS;
yoffs(Y,out) -> Y+(?VIEW_CELL_HEIGHT-?TIME_HEIGHT_OFFS);
yoffs(Y,exited) -> Y+(?VIEW_CELL_HEIGHT-?TIME_HEIGHT_OFFS).

%%
%% Draw state diagram for process/port ID
%%
draw_ps(Em,I,ID,E1,X0,X1) ->
    Y = I*?VIEW_CELL_HEIGHT + ?VIEW_TOP_OFFS,
    case ets:lookup(Em#emon.ptab, I) of
	[{I,ID}] ->
	    [{ID,I,E0}] = ets:lookup(Em#emon.ptab, ID),
	    E01 = draw_ps_1(Em,E0,E1,X0,X1,Y),
	    ets:insert(Em#emon.ptab, {ID,I,E01}),
	    draw_ps(Em,I+1,ID,E1,X0,X1);
	[{I,ID1}] ->
	    [{ID1,I,E0}] = ets:lookup(Em#emon.ptab, ID1),
	    draw_ps_2(Em,E0,X0,X1,Y),
	    draw_ps(Em,I+1,ID,E1,X0,X1);
	[] ->
	    ok
    end.

%%
%% Reduce state = {in,Garb}|{out,Garb}|exited
%% GC = boolean  if in garbage_collection
%%
draw_ps_1(Em,{J0,Garb},I1,X0,X1,Y) ->
    Pix = Em#emon.pix,
    J1 = case info_state(I1) of
	     false -> J0;
	     State -> State
	 end,
    case {J0,J1} of
	{in,in} ->
	    draw_in_in(Pix,Garb,X0,X1,Y),
	    draw_action(Em,Garb,J1,I1,Pix,X1,Y);
	{in,out} ->
	    draw_in_out(Pix,Garb,X0,X1,Y),
	    draw_action(Em,Garb,J1,I1,Pix,X1,Y);
	{out,in} ->
	    draw_out_in(Pix,Garb,X0,X1,Y),
	    draw_action(Em,Garb,J1,I1,Pix,X1,Y);
	{out,out} ->
	    draw_out_out(Pix,Garb,X0,X1,Y),
	    draw_action(Em,Garb,J1,I1,Pix,X1,Y);
	{exited,_Exited} ->
	    {exited,false}
    end.

draw_in_in(Pix,Garb,X0,X1,Y) ->
    Y0 = yoffs(Y,in),
    %% Y1 = yoffs(Y,out),
    epx_gc:set_fill_color(?TIME_FG_COLOR),
    %% epx:draw_rectangle(Pix,X0,Y0,(X1-X0)+1,(Y1-Y0)+1),
    epx:draw_line(Pix,X0,Y0,X1,Y0),
    draw_garb(Pix,Garb,X0,X1,Y0).

draw_in_out(Pix,Garb,X0,X1,Y) ->
    Y0 = yoffs(Y,in),
    Y1 = yoffs(Y,out),
    Flank = flank_length(X0,X1),
    X  = X1-Flank,
    epx_gc:set_fill_color(?TIME_FG_COLOR),
    %% epx:draw_rectangle(Pix,X0,Y0,(X-X0)+1,(Y1-Y0)+1)
    epx:draw_line(Pix,X0,Y0,X,Y0),
    epx:draw_line(Pix,X,Y0,X1,Y1),
    draw_garb(Pix,Garb,X0,X1,Y0).

draw_out_in(Pix,Garb,X0,X1,Y) ->
    Y0 = yoffs(Y,in),
    Y1 = yoffs(Y,out),
    Flank = flank_length(X0,X1),
    X  = X1-Flank,
    epx:draw_line(Pix, X0, Y1, X, Y1),
    epx:draw_line(Pix,X,Y1,X1,Y0),
    draw_garb(Pix,Garb,X0,X1,Y1).

draw_out_out(Pix,Garb,X0,X1,Y) ->
    %% Y0 = yoffs(Y,in),
    Y1 = yoffs(Y,out),
    epx:draw_line(Pix, X0, Y1, X1, Y1),
    draw_garb(Pix,Garb,X0,X1,Y1).

flank_length(X0,X1) ->
    N = X1-X0,
    if N >= ?FLANK_LENGTH -> ?FLANK_LENGTH;
       true -> 2
    end.
	    
%% Draw GARGB_FG_COLOR during garbage collection 
draw_garb(_Pix,false,_X0,_X1,_Y0) ->
    ok;
draw_garb(Pix,true,X0,X1,Y0) ->
    epx_gc:set_fill_color(?GARB_FG_COLOR),
    epx:draw_rectangle(Pix,X0,Y0+1,(X1-X0)+1,2).


draw_action(Em, Gc, J1, Info, Pix, X0, Y) ->
    Y0 = yoffs(Y,J1),
    case Info of
	{send,ID2} ->
	    case ets:lookup(Em#emon.ptab, ID2) of
		[] -> 
		    %% just mark send
		    draw_send(Pix, X0, Y0, Y0+4);
		[{ID2,I2,{J2,_GC}}] ->
		    Y1 = yoffs(I2*?VIEW_CELL_HEIGHT+?VIEW_TOP_OFFS, J2),
		    draw_send(Pix, X0, Y0, Y1)
	    end,
	    {J1,Gc};
	{'receive',Type} ->
	    draw_receive(Pix, Type, X0, Y0),
	    {J1,Gc};
	trigger ->
	    draw_trigger(Pix,X0,Y0),
	    {J1,Gc};
	gc_end ->
	    {J1,false};
	gc_start ->
	    {J1,true};
	out_exited ->
	    {exited,false};
	_ ->
	    {J1,Gc}
    end.

%% Draw state
draw_ps_2(Em,E0={J0,Garb},X0,X1,Y) ->
    Pix = Em#emon.pix,
    case J0 of
	in ->
	    draw_in_in(Pix,Garb,X0,X1,Y),
	    E0;
	out ->
	    draw_out_out(Pix,Garb,X0,X1,Y),
	    E0;
	exited ->
	    E0
    end.

-define(T, 0,0,0,0).
-define(R, 255,255,0,0).
-define(G, 255,0,255,0).
-define(B, 255,0,0,255).
-define(C, 255,255,255,0).
%%
%%  ..x..
%%  .xxx.
%%  xxxxx
%%
red_arrow_up() ->
    <<
     ?T,?T,?R,?T,?T,
     ?T,?R,?R,?R,?T,
     ?R,?R,?R,?R,?R
     >>.

%%
%%  xxxxx
%%  .xxx.
%%  ..x..
%%
red_arrow_down() ->
    <<
     ?R,?R,?R,?R,?R,
     ?T,?R,?R,?R,?T,
     ?T,?T,?R,?T,?T
     >>.
     

%% draw send arrow vertical
draw_send(Pix, X, Y1, Y2) ->
    %% draw meesage line
    epx_gc:push(),
    epx_gc:set_foreground_color({255,0,0}),
    if Y1 < Y2 ->  %% down
	    epx:draw_line(Pix, X, Y1, X, Y2-4),
	    epx:pixmap_put_pixels(Pix,X-2,Y2-3,5,3,argb,red_arrow_down(),blend);
       true -> %% up
	    epx:draw_line(Pix, X, Y1, X, Y2+4),
	    epx:pixmap_put_pixels(Pix,X-2,Y2+1,5,3,argb,red_arrow_up(),blend)
    end,
    %%epx_gc:set_foreground_color({0,0,255}),
    epx_gc:pop(),
    ok.

blue_arrow_down() ->
    <<
     ?B,?B,?B,?B,?B,
     ?T,?B,?B,?B,?T,
     ?T,?T,?B,?T,?T
     >>.

blue_timeout() ->
    <<
     ?B,?B,?B,?B,?B,
     ?T,?T,?B,?T,?T,
     ?T,?T,?B,?T,?T,
     ?T,?T,?B,?T,?T
     >>.

%%
%%  ..xxx..
%%  .xxxxx.
%%  xxxxxxx
%%  xxxxxxx
%%  xxxxxxx
%%  .xxxxx.
%%  ..xxx..
%%

blue_ring() ->
    <<
     ?T,?T,?B,?B,?B,?T,?T,
     ?T,?B,?B,?B,?B,?B,?T,
     ?B,?B,?B,?B,?B,?B,?B,
     ?B,?B,?B,?B,?B,?B,?B,
     ?B,?B,?B,?B,?B,?B,?B,
     ?T,?B,?B,?B,?B,?B,?T,
     ?T,?T,?B,?B,?B,?T,?T
     >>.

draw_receive(Pix, message, X, Y) ->
    epx:pixmap_put_pixels(Pix,X-2,Y-3,5,3,argb,blue_arrow_down(),blend);
draw_receive(Pix, timeout, X, Y) ->
    epx:pixmap_put_pixels(Pix,X-2,Y-4,5,4,argb,blue_timeout(),blend).

trigger() ->
    <<
     ?T,?C,?T,?C,?T,
     ?T,?C,?T,?C,?T,
     ?T,?C,?T,?C,?T,
     ?T,?C,?T,?C,?T
     >>.

draw_trigger(Pix, X, Y) ->
    epx:pixmap_put_pixels(Pix,X-2,Y-4,5,4,argb,trigger(),blend).

%%
%% Draw time grid lines N Us appart
%%

draw_time(EM, Us) ->
    {X0,Y0,Width,Height} = EM#emon.view,
    epx_gc:draw(
      fun() ->
	      fill_view(EM#emon.pix,0,Y0,Height,?VIEW_CELL_HEIGHT,X0,Width),
	      %% slightly draker lines
	      epx_gc:set_foreground_color(?VIEW_FG_COLOR),
	      Y1 = Y0 + Height -1,
	      X1 = X0 + Width - 1,
	      draw_time_lines(EM#emon.pix, Us, X0, X1, Y0, Y1)
      end).

fill_view(Pix,I,Y,Height,H,X0,Width) ->
    if Height < H ->
	    ok;
       true ->
	    if 
		I band 1 == 0 ->
		    epx_gc:set_fill_color(?VIEW_BG_COLOR1);
		true ->
		    epx_gc:set_fill_color(?VIEW_BG_COLOR2)
	    end,
	    epx:draw_rectangle(Pix,X0,Y,Width,H),
	    fill_view(Pix,I+1,Y+H,Height-H,H,X0,Width)
    end.

draw_time_lines(Pix, Step, X0, X1, Y0, Y1) when X0 < X1 ->
    epx:draw_line(Pix, X0, Y0, X0, Y1),
    draw_time_lines(Pix,Step,X0+Step,X1,Y0,Y1);
draw_time_lines(_Pix, _Step, _X0, _X1, _Y0, _Y1) ->
    ok.

%%
%% Draw pids and names on the left side
%%
draw_names(Em) ->
    epx_gc:draw(
      fun() ->
	      epx_gc:set_foreground_color({0,255,255,255}),
	      draw_names(Em#emon.pi, Em)
      end).

draw_names(I, Em) ->
    Y = I*?VIEW_CELL_HEIGHT + ?VIEW_TOP_OFFS + Em#emon.ascent,
    case ets:lookup(Em#emon.ptab, I) of
	[{I,ID}] ->
	    Name = case ets:lookup(Em#emon.ntab,ID) of
		       [{ID,Nm}] -> atom_to_list(Nm);
		       [] when is_pid(ID) -> pid_to_list(ID);
		       [] when is_port(ID) -> erlang:port_to_list(ID)
		   end,
	    epx:draw_string(Em#emon.pix, 2, Y, Name),
	    draw_names(I+1, Em);
	[] ->
	    ok
    end.


buffer_view(Em) ->
    {Vx,Vy,Vw,Vh} = Em#emon.view,
    {Vx,Vy+(Vh-1)+3,Vw, 10}.

%%
%% Draw buffer 
%%
draw_buffer_scale(Em) ->
    T_TotalWidth = Em#emon.t1 - Em#emon.t0,
    T_ViewWidth = Em#emon.td1 - Em#emon.td0,
    {Bx,By,Bw,Bh} = buffer_view(Em),
    Ratio = T_ViewWidth/T_TotalWidth,
    Bl = erlang:min(round(Bw*Ratio),Bw),   %% buffer part

    %% left part of view samples
    T0 = Em#emon.td0 - Em#emon.t0,   %% relative left position in samples
    X0 = Bx + round((T0/T_TotalWidth)*Bw),
    epx_gc:draw(
      fun() ->
	      %% Draw buffer background (as gray)
	      epx_gc:set_fill_color(?BUFFER_BG_COLOR),
	      epx:draw_rectangle(Em#emon.pix,Bx,By,Bw,Bh),
	      %% Draw active buffer (as white)
	      epx_gc:set_fill_color(?BUFFER_FG_COLOR),
	      epx:draw_rectangle(Em#emon.pix,X0,By+1,Bl,Bh-2)
      end).

%% Find new Td0 from X & Y
buffer_time_xy(Em, X, Y) ->
    {Bx,By,Bw,Bh} = buffer_view(Em),
    Bx1 = Bx + Bw - 1,
    By1 = By + Bh - 1,
    if Y >= By, Y =< By1, X >= Bx, X =< Bx1 ->
	    Xr = (X - Bx) / Bw,   %% ratio in buffer area
	    Em#emon.t0 + trunc(Xr*(Em#emon.t1 - Em#emon.t0));
       true ->
	    false
    end.
    
find_ps_pid(Pid, Ps0,Ps1) ->
    case lists:keysearch(Pid, 1, Ps0) of
	V={value,_PE} ->
	    V;
	false ->
	    lists:keysearch(Pid, 1, Ps1)
    end.

window(Em,W,H) ->
    E = [key_press,key_release, button_press, button_release,
	 resize, motion, left],
    Win = epx:window_create(50, 50, W, H, E),
    epx:window_attach(Win),
    B = epx_backend:default(),
    %% epx:backend_adjust(B, [{use_off_screen,0},{use_exposure,1}]),
    [{width,BW},{height,BH}] = epx:backend_info(B,[width,height]),
    epx:window_adjust(Win, [{name, "Process Tracer"},
			    {min_width,W div 2},{max_width,BW},
			    {min_height,H div 2},{max_height,BH}]),
    {ok,Font} = epx_font:match([{name,"Arial"},{size,10}]),
    FInfo = epx_font:info(Font),
    Em0 = Em#emon { win=Win, font=Font, ascent=FInfo#epx_font_info.ascent},
    configure(Em0, W, H).

configure(Em, W0, H0) ->
    W = max(?MIN_WIDTH, W0),
    H = max(?MIN_HEIGHT, H0),
    Pix1 = resize_pixmap(Em#emon.pix, W, H),
    V_X = ?VIEW_LEFT_OFFS,
    V_Y = ?VIEW_TOP_OFFS,
    V_W = W - (V_X + ?VIEW_RIGHT_OFFS),
    V_H = H - (V_Y + ?VIEW_BOTTOM_OFFS),
    Em#emon { view={V_X,V_Y,V_W,V_H}, 
	      pix = Pix1, width = W, height = H }.

resize_pixmap(undefined, W, H) ->
    Pixmap = next_pixmap(W,H),
    epx:pixmap_attach(Pixmap),
    Pixmap;
resize_pixmap(Pixmap, W, H) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    epx:pixmap_detach(Pixmap),
	    Pixmap1 = next_pixmap(W,H),
	    epx:pixmap_attach(Pixmap1),
	    Pixmap1;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H) ->
    NPW = W + (W div 2), %% 1 bsl ceil(math:log2(W)),
    NPH = H + (H div 2), %% 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH).
    
window_loop(Em) ->
    Win = Em#emon.win,
    receive
	{epx_event,Win, destroy} ->
	    ok;
	{epx_event,Win, close} ->
	    epx:window_detach(Win),
	    ok;
	{epx_event,Win,{key_press,up,_,_}} ->
	    Em2 = redraw(Em#emon { scale = min(4, Em#emon.scale + 1)}),
	    window_loop(Em2);
	{epx_event,Win,{key_press,down,_,_}} ->
	    Em2 = redraw(Em#emon { scale = max(-4, Em#emon.scale - 1)}),
	    window_loop(Em2);
	{epx_event,Win,{key_press,left,_,_}} ->
	    %% auto-repeat for mac os x ?
	    Scale = math:pow(2,Em#emon.scale),
	    Td0 = erlang:max(Em#emon.t0, Em#emon.td0 - trunc(?TIME_STEP*Scale)),
	    Em1 = Em#emon { td0 = Td0},
	    Em2 = redraw(Em1),
	    window_loop(Em2);
	{epx_event,Win,{key_press,right,_,_}} ->
	    %% auto-repeat for mac os x ?
	    Scale = math:pow(2,Em#emon.scale),
	    T = Em#emon.td1 + trunc(?TIME_STEP*Scale),
	    Td0 = if T >= Em#emon.t1 ->
			  Em#emon.td0 + (Em#emon.t1 - Em#emon.td1);
		     true ->
			  Em#emon.td0 + trunc(?TIME_STEP*Scale)
		  end,
	    Em1 = Em#emon { td0 = Td0},
	    Em2 = redraw(Em1),
	    window_loop(Em2);
	{epx_event,Win,{key_release,_,_,_}} ->
	    window_loop(Em);
	{epx_event,Win,{motion,[left],{X,Y,_}}} ->
	    case buffer_time_xy(Em, X, Y) of
		false ->
		    window_loop(Em);
		T ->
		    Em1 = Em#emon { td0 = T},
		    Em2 = redraw(Em1),
		    window_loop(Em2)
	    end;	    
	{epx_event,Win,{button_press,[left],{X,Y,_}}} ->
	    case buffer_time_xy(Em, X, Y) of
		false ->
		    window_loop(Em);
		T ->
		    Em1 = Em#emon { td0 = T},
		    Em2 = redraw(Em1),
		    window_loop(Em2)
	    end;
	{epx_event,Win,{button_release,[left],_}} ->
	    window_loop(Em);

	{epx_event,Win,{resize, {W,H,_D}}} ->
	    if W =:= Em#emon.width, H =:= Em#emon.height ->
		    window_loop(Em);
	       true ->
		    Em1 = configure(Em, W, H),
		    epx:sync(Win),
		    Em2 = redraw(Em1),
		    window_loop(Em2)
	    end;
	_Other ->
	    io:format("EVENT: ~p\n", [_Other]),
	    window_loop(Em)
    end.

%%
%% Wait for trigger the collect until time is up.
%% FIXME (or buffer is full)
%%
collect(Spec, Em, Opts) ->
    Trigger = getopt(trigger, Opts),
    TriggerArg = getopt(trigger_arg, Opts),
    Time = getopt(time, Opts),
    Delay = getopt(delay, Opts),
    Timeout = getopt(timeout,Opts),
    Flags = filter_flags(getopt(flags, Opts) ++ getopt(add,Opts)) --
	getopt(delete, Opts),
    What = run(Spec),
    erlang:trace(What, true, [monotonic_timestamp, running | Flags]),
    PreBuffer = getopt(prebuffer, Opts),
    Buf0 = evt_buffer:new(PreBuffer),
    %% Wait for Delay ms before checking for trigger
    DelayRef = erlang:start_timer(Delay, self(), delay),
    Buf1 = delay_loop(DelayRef,Buf0),
    %% Wait for Trigger or Timeout ms
    TimeoutRef = erlang:start_timer(Timeout,self(),trigger),
    trigger_loop(Trigger,TriggerArg,Em,TimeoutRef,Buf1),
    DoneRef = erlang:start_timer(Time, self(), collect),
    collect_loop(DoneRef,Em).


filter_flags(Fs) ->
    filter_flags(Fs,[]).

filter_flags([Flag|Fs],Ac) ->
    case Flag of
	running -> filter_flags(Fs,Ac);   %% always added
	timestamp -> filter_flags(Fs,Ac);   %% always added
	monotonic_timestamp -> filter_flags(Fs,Ac);   %% always added
	strict_monotonic_timestamp -> filter_flags(Fs,Ac);   %% always added
	cpu_timestamp -> filter_flags(Fs,Ac);   %% always added
	set_on_spawn -> filter_flags(Fs,[Flag|Ac]);
	set_on_link  -> filter_flags(Fs,[Flag|Ac]);
	set_on_first_spawn -> filter_flags(Fs,[Flag|Ac]);
	set_on_first_link  -> filter_flags(Fs,[Flag|Ac]);
	send -> filter_flags(Fs,[Flag|Ac]);
	'receive' -> filter_flags(Fs,[Flag|Ac]);
	garbage_collection -> filter_flags(Fs,[Flag|Ac]);
	procs -> filter_flags(Fs,[Flag|Ac]);
	ports -> filter_flags(Fs,[Flag|Ac]);
	_ -> filter_flags(Fs,Ac)
    end;
filter_flags([], Ac) ->
    lists:usort(Ac).
	    

run({M,F,A}) ->
    spawn(M,F,A);  %% spawn link? monitor?
run(Fun) when is_function(Fun) ->
    spawn(Fun);    %% spawn link? monitor?
run(What) -> What.

%%
%% Delay before trigger test
%%
delay_loop(Timer,Buf) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) =:= trace_ts ->
	    case internal_ts(Ts) of
		undefined ->
		    delay_loop(Timer,Buf);
		Evt ->
		    delay_loop(Timer,evt_buffer:add_element(Evt,Buf))
	    end;
	{timeout,Timer,_} ->
	    Buf
    end.
%%
%% Wait for trigger condition before buffering
%%
trigger_loop(Fun,Arg,Em,Timer,Buf) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) == trace_ts ->
	    case internal_ts(Ts) of
		undefined ->
		    trigger_loop(Fun,Arg,Em,Timer,Buf);
		Evt ->
		    case Fun(Evt,Arg) of
			true ->
			    cancel(Timer),
			    add_buffer_events(Em,Buf),
			    add_event(Em,Evt),
			    add_event(Em,{element(1,Evt),self(),trigger});
			Arg1 ->
			    trigger_loop(Fun,Arg1,Em,Timer,
					 evt_buffer:add_element(Evt,Buf))
		    end
	    end;
	{timeout,Timer,_} ->
	    add_event(Em,{erlang:monotonic_time(nanosecond),self(),trigger}),
	    add_buffer_events(Em,Buf)
    end.

add_buffer_events(Em,Buf) ->
    EvtList = evt_buffer:to_list(Buf),
    io:format("Adding ~w pre buffered events\n", [length(EvtList)]),
    lists:foreach(fun(Evt) ->
			  add_event(Em,Evt) 
		  end, EvtList).

%% Cancel and flush timer
cancel(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timer,Ref,_} -> ok
	    after 0 -> ok
	    end;
	_ -> ok
    end.

%%
%% Collect data until done message is received
%%
collect_loop(Timer,Em) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) =:= trace_ts ->
	    Evt = internal_ts(Ts),
	    add_event(Em,Evt),
	    collect_loop(Timer,Em);
	{timeout,Timer,_} ->
	    ok;
	What ->
	    io:format("emon:collect_loop got message ~p\n", [What]),
	    collect_loop(Timer,Em)
    end.

%%
%% rewrite trace message to internal form
%% {TimeStamp,Pid,Type}
%%

%% running
internal_ts({trace_ts,ID,in,_,T}) -> {T,ID,in};
internal_ts({trace_ts,ID,out,_,T}) -> {T,ID,out};
%% garbage_collection
internal_ts({trace_ts,ID,gc_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_end,_Info,T}) -> {T,ID,gc_end};
internal_ts({trace_ts,ID,gc_minor_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_minor_end,_Info,T}) -> {T,ID,gc_end};
internal_ts({trace_ts,ID,gc_major_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_major_end,_Info,T}) -> {T,ID,gc_end};
%% send
internal_ts({trace_ts,ID,send,_Msg,DST,T}) -> {T,ID,{send,DST}};
internal_ts({trace_ts,ID,send_to_non_existing,_Msg,DST,T}) -> {T,ID,{send,DST}};
%% receive
internal_ts({trace_ts,ID,'receive',timeout,T})-> {T,ID,{'receive',timeout}};
internal_ts({trace_ts,ID,'receive',_,T})-> {T,ID,{'receive',message}};
%% exiting
internal_ts({trace_ts,ID,in_exiting,_,T}) ->{T,ID,in_exiting};
internal_ts({trace_ts,ID,out_exiting,_,T}) ->{T,ID,out_exiting};
internal_ts({trace_ts,ID,out_exited,_,T}) -> {T,ID,out_exited};
%% proc
internal_ts({trace_ts,ID,spawn,ID2,_MFa,T}) -> {T,ID,{spawn,ID2}};
internal_ts({trace_ts,ID,exit,Reason,T}) -> {T,ID,{exit,Reason}};
internal_ts({trace_ts,ID,register,Name,T}) -> {T,ID,{register,Name}};    
internal_ts({trace_ts,ID,unregister,Name,T}) -> {T,ID,{unregister,Name}};
internal_ts({trace_ts,ID,link,ID2,T}) -> {T,ID,{link,ID2}};
internal_ts({trace_ts,ID,unlink,ID2,T}) -> {T,ID,{unlink,ID2}};
internal_ts({trace_ts,ID,getting_linked,ID2,T}) -> {T,ID,{linked,ID2}};
internal_ts({trace_ts,ID,getting_unlinked,ID2,T}) -> {T,ID,{unlinked,ID2}};
%% Other
internal_ts(Ts) ->
    io:format("internal_ts: not handled ~p\n", [Ts]),
    undefined.

%% Load name table ntab - not always 100% consistent
update_names(Em) ->
    update_names(Em,0).

update_names(Em,I) ->
    case ets:lookup(Em#emon.ptab, I) of
	[] ->
	    ok;
	[{I,ID}] ->
	    add_name(Em,ID,registered_name(ID)),
	    update_names(Em,I+1)
    end.

registered_name(ID) when is_pid(ID) ->
    case erlang:process_info(ID, registered_name) of
	{_,Name} -> Name;
	_ -> undefined
    end;
registered_name(ID) when is_port(ID) ->
    case erlang:port_info(ID, registered_name) of
	{_,Name} -> Name;
	_ -> undefined
    end;
registered_name(_) ->
    undefined.
    

%% update status in the Pid -> IxStatus mapping given the
%% by checking backwards from Td0.
%% If no info is found in ETab before Td0 then we must check
%% among events after Td0 and "invert" the status.

update_ptab(Em) ->
    T = Em#emon.td0,
    update_ptab(Em,T,0).

update_ptab(Em,T,I) ->
    case ets:lookup(Em#emon.ptab, I) of
	[] ->
	    ok;
	[{I,ID}] ->
	    S = state(Em,T,ID),
	    ets:insert(Em#emon.ptab, {ID,I,{S,false}}),
	    update_ptab(Em,T,I+1)
    end.
%%
%% Find State (in|out|exited) for 
%% Port/Pid with index I at time T
%%
state(Em, T, I) when is_integer(I) ->
    case ets:lookup(Em#emon.ptab, I) of
	[]        -> exited;
	[{I,Pid}] -> state(Em,T,Pid)
    end;
state(Em, T, ID) when is_pid(ID); is_port(ID) ->
    case find_prev_state(Em#emon.etab, T+1, ID) of
	in     -> in;
	out    -> out;
	exited -> exited;
	false ->
	    case find_next_state(Em#emon.etab,T,ID) of
		false    -> out;
		in       -> out;
		out      -> in;
		exited   -> out   %% or in?
	    end
    end.

%%
%% check if ID is running at time T 
%%   return in|out|exited|false
%%
find_prev_state(ETab, T, ID) ->
    case ets:prev(ETab, T) of
	'$end_of_table' ->
	    false;
	T1 ->
	    case ets:lookup(ETab, T1) of
		[{_T1,ID,Info}] ->
		    case info_state(Info) of
			false -> find_prev_state(ETab,T1,ID);
			State -> State
		    end;
		[_] ->
		    find_prev_state(ETab, T1, ID)
	    end
    end.

%%
%% Find state (change) in the future return in|out|exited|false
%%
find_next_state(ETab, T, ID) ->
    case ets:next(ETab, T) of
	'$end_of_table' ->
	    false;
	T1 ->
	    case ets:lookup(ETab, T1) of
		[{_T1,ID,Info}] ->
		    case info_state(Info) of
			false -> find_next_state(ETab,T1,ID);
			State -> State
		    end;
		[_] ->
		    find_next_state(ETab,T1,ID)
	    end
    end.
%%
%% Map info to state  in|out|exited|false
%%
info_state(Info) ->
    case Info of
	%% running
	in             -> in;
	out            -> out;
	%% exiting
	in_exiting     -> in;
	out_exiting    -> out;
	out_exited     -> exited;
	%% send
	{send,_}       -> in;
	%% receive
	{'receive',_}  -> false;
	%% garbage_collection
	gc_start       -> false;
	gc_end         -> false;
	%% procs
	{spawn,_}      -> in;
	{register,_}   -> in;
	{unregister,_} -> in;
	{link,_}       -> in;
	{unlink,_}     -> in;
	{exit,_}       -> in;
	{linked,_}     -> in;
	{unlinked,_}   -> in;
	%% synthetic
	trigger        -> in
    end.
		    

add_event(Em,undefined) ->
    Em;
add_event(Em,{Ts,ID,Info}) ->
    add_id(Em, ID),
    case Info of
	%% Only ports! processes are handled trough set_on_link,
	%% set_on_spawn ...
	{send,ID2} when is_port(ID2) -> add_id(Em,ID2);
	{link,ID2} when is_port(ID2) -> add_id(Em,ID2);
	{unlink,ID2} when is_port(ID2) -> add_id(Em,ID2);
	%% Maybe check names while drawing them for the current view?
	{register,Name} -> add_name(Em,ID,Name);
	{unregister,_Name} -> %% mark when name disappered? 
	    ok;
	_ -> 
	    ok
    end,
    ets:insert(Em#emon.etab, {Ts,ID,Info}),
    Size = ets:info(Em#emon.etab, size),
    if Size > trunc(Em#emon.esize*1.25) ->
	    del_evt(Em#emon.etab, ets:first(Em#emon.etab),
		      Size-trunc(Em#emon.esize*0.75)),
	    Em;
       true ->
	    Em
    end.

add_id(Em,ID) ->
    case ets:member(Em#emon.ptab, ID) of
	false ->
	    I = ets:info(Em#emon.ptab, size) div 2,
	    io:format("Trace: add id=~p index=~w\n", [ID,I]),
	    ets:insert(Em#emon.ptab, {ID,I,{out,false}}),
	    ets:insert(Em#emon.ptab, {I,ID});
	true ->
	    ok
    end.

del_id(Em, ID) ->
    case ets:lookup(Em#emon.ptab, ID) of
	[] ->
	    ok;
	[{ID,I,_}] ->
	    ets:delete(Em#emon.ptab, ID),
	    ets:delete(Em#emon.ptab, I)
    end.

add_name(_Em,_ID,undefined) ->
    ok;
add_name(Em,ID,Name) ->
    ets:insert(Em#emon.ntab, {ID,Name}),
    ets:insert(Em#emon.ntab, {Name,ID}).

%% remove events
del_evt(_ETab, _Ti, 0) ->
    ok;
del_evt(_ETab, '$end_of_table', 0) ->
    ok;
del_evt(ETab, Ti, I) ->
    Tj = ets:next(ETab, Ti),
    ets:delete(ETab, Ti),
    del_evt(ETab, Tj, I-1).

getopt(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
	false ->
	    case lists:member(Key, Opts) of
		true -> true;  %% boolean, present = true
		false -> default(Key)
	    end;
	{value,{_,Value}} ->
	    Value
    end.
