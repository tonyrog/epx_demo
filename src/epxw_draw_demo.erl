%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Demo showing partial update functio
%%% @end
%%% Created : 20 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(epxw_draw_demo).
-behavious(epxw).

-include_lib("epx/include/epx_menu.hrl").

-export([start/0]).
-export([init/1,
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
	 command/3,
	 select/2,
	 motion/2
	 %% menu/2
	]).
-export([handle_info/2]).


%% -define(verbose(F,A), ok).
-define(verbose(F,A), io:format((F),(A))).

-define(WINDOW_WIDTH,  512).
-define(WINDOW_HEIGHT, 512).
-define(VIEW_WIDTH,    4096).
-define(VIEW_HEIGHT,   4096).
-define(BORDER, 2).
-define(NUM_RECTS, 1003).
-define(TICK_INTERVAL, 100).

-record(elem,
	{
	 index,   %% index 1..N
	 count,   %% counter 0..max-1
	 max,     %% max+1 counter value
	 color,   %% rect color
	 rect     %% the rect
	}).

start() ->
    application:ensure_all_started(epx),
    epxw:start(#{ module => ?MODULE,
		  %% demo select as fun
		  select => fun(Event={_Phase,Rect={X,Y,W,H}}, State) -> 
				    ?verbose("SELECT FUN: ~w\n", [Event]),
				    OldRect = maps:get(selection, State),
				    epxw:invalidate(OldRect),
				    epxw:invalidate({X-?BORDER-2,Y-?BORDER-2,
						     W+2*?BORDER+4,
						     H+2*?BORDER+4}),
				    State#{ selection => Rect }
			    end
		},
	       [], 
	       [{title, "DrawDemo"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   left},    %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar,0},
		{top_bar,0},
		{right_bar,0},
		{width,?WINDOW_WIDTH},
		{height, ?WINDOW_HEIGHT},
		{view_width,?VIEW_WIDTH},
		{view_height,?VIEW_HEIGHT}]).

init(_Opts) ->
    ?verbose("INIT: Opts=~w\n", [_Opts]),
    Es =
	lists:map(
	  fun(I) ->
		  W = uniform(20, 40),
		  H = uniform(W div 3, W div 2),
		  X = uniform(0, ?VIEW_WIDTH-W-1),
		  Y = uniform(0, ?VIEW_HEIGHT-H-1),
		  N = uniform(10, 100),
		  Color = {rand:uniform(256),
			   rand:uniform(256),
			   rand:uniform(256)},
		  #elem{index=I, count=0, max=N, color=Color, rect={X,Y,W,H}}
	  end,  lists:seq(1, ?NUM_RECTS)),
    tick_start(?TICK_INTERVAL),
    State = 
	#{ 
	   elems => Es,
	   selection => {0,0,0,0}
	 },
    {ok, State}.

configure(_Rect, State) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    {Sx,_Sy} = epxw:scale(),
    Scale = float(Sx*100),
    Status = io_lib:format("Hello World, Dimension: ~wx~w, Scale: ~.2f%",
			   [epxw:width(),epxw:height(),Scale]),
    epxw:set_status_text(Status),
    epxw:invalidate(),
    State.

key_press(_Event, State) ->
    ?verbose("KEY_PRESS: ~w\n", [_Event]),
    State.

key_release(_Event, State) ->
    ?verbose("KEY_RELEASE: ~w\n", [_Event]),
    State.

button_press(_Event, State) ->
    ?verbose("BUTTON_PRESS: ~w\n", [_Event]),
    State.

button_release(_Event, State) ->
    ?verbose("BUTTON_RELEASE: ~w\n", [_Event]),
    State.

enter(_Event, State) ->
    ?verbose("ENTER: ~w\n", [_Event]),
    State.

leave(_Event, State) ->
    ?verbose("LEAVE: ~w\n", [_Event]),
    State.

focus_in(_Event, State) ->
    ?verbose("FOCUS_IN: ~w\n", [_Event]),
    State.

focus_out(_Event, State) ->
    ?verbose("FOCUS_OUT: ~w\n", [_Event]),
    State.

close(State) ->
    ?verbose("CLOSE:\n", []),
    State.

%% draw/3 update content region (same as draw/4(content,...)
draw(Pixels, DirtyRect, 
     State = #{ elems := Es, selection := Selection }) ->
    ?verbose("DRAW: Rect = ~p\n", [DirtyRect]),

    epx_gc:set_fill_style(solid),

    N = lists:foldl(
	  fun(E,I) ->
		  case epx_rect:intersect(E#elem.rect, DirtyRect) of
		      {0,0,0,0} -> %% empty
			  I;
		      _ ->
			  R = E#elem.count / E#elem.max,
			  draw_rect(Pixels,E#elem.rect,R,E#elem.color, white),
			  I+1
		  end
	  end, 0, Es),

    case Selection of
	undefined -> empty;
	{_,_,Sw,Sh} when Sw < 2, Sh < 2 -> empty;
	_ ->
	    epx_gc:set_fill_style(blend),
	    epx_gc:set_fill_color({127,127,127,127}),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(?BORDER),
	    epx:draw_rectangle(Pixels, Selection)
    end,
    io:format("DEMO ~w rects\n", [N]),
    State.

%% NOTE! this is overridden by the select fun above
select({_Phase,Rect}, State) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    State# { selection => Rect }.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

command(Key, Mod, State) ->
    ?verbose("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({timeout,_Ref,tick}, State = #{ elems := Es }) ->
    tick_start(?TICK_INTERVAL),
%%    epxw:invalidate(),
%%    Es1 = update_all_elems(Es),
    VisibleRect = epxw:visible_rect(),
    Es1 = epxw:draw(fun(Pixels, _Area) ->
			    draw_all_elems(Pixels, VisibleRect, Es) 
		    end),
    {noreply, State#{ elems=>Es1 }}.

update_all_elems(Es) ->
    lists:map(
      fun(E=#elem{count=I,max=N}) ->
	      E#elem { count = (I+1) rem N }
      end, Es).

draw_all_elems(Pixels, VisibleRect, Es) ->
    draw_elems(Es, Pixels, VisibleRect, 0, []).

draw_elems([E=#elem{count=I0,max=N,rect=Rect,color=Color}|Es], 
	   Pixels, VisibleRect, Cnt, Acc) ->
    I = (I0 + 1) rem N,
    R = I / N,
    case epx_rect:intersect(Rect, VisibleRect) of	      
	{0,0,0,0} ->
	    draw_elems(Es, Pixels, VisibleRect, Cnt, 
		       [E#elem { count = I }|Acc]);
	_ ->
	    draw_rect(Pixels,Rect,R,Color,white),
	    epxw:invalidate(Rect),
	    draw_elems(Es, Pixels, VisibleRect, Cnt+1, 
		       [E#elem { count = I }|Acc])
    end;
draw_elems([], _Pixels, _VisibleRect, Cnt, Acc) ->
    io:format("DEMO_ELEM ~w rects\n", [Cnt]),
    Acc.


uniform(Min, Max) ->
    Range = (Max - Min) + 1,
    Min + (rand:uniform(Range)-1).

draw_rect(Pixels, {X,Y,W,H}, Ratio, Color1, Color2) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_border_width(0),
    epx_gc:set_fill_color(Color1),
    W1 = trunc(W*Ratio),
    W2 = W-W1,
    epx:draw_rectangle(Pixels, {X,Y,W1,H}),
    epx_gc:set_fill_color(Color2),
    epx:draw_rectangle(Pixels, {X+W1,Y,W2,H}),
    epx_gc:set_fill_style(none),
    epx_gc:set_border_color(black),
    epx_gc:set_border_width(1),
    epx:draw_rectangle(Pixels, {X, Y, W, H}).

tick_start(TimeMs) ->
    erlang:start_timer(TimeMs, self(), tick).
