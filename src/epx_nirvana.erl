%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Nirvana demo
%%% @end
%%% Created : 24 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(epx_nirvana).
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

-define(verbose(F,A), ok).
%% -define(verbose(F,A), io:format((F),(A))).

-define(WIDTH,  1024).
-define(HEIGHT, 1024).
-define(RADIUS, 200).
-define(STEP,   40).
-define(DIVIDER, (1/(40*?STEP))).
-define(CX, (?WIDTH div 2)).
-define(CY, (?HEIGHT div 2)).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
-define(BORDER, 2).

start() ->
    application:ensure_all_started(epx),
    epxw:start(?MODULE, [], 
	       [{title, "Nirvana"},
		{screen_color, grey10},
		{scroll_horizontal, none},  %% none|top|bottom
		{scroll_vertical,   none},  %% none|left|right
		{scroll_bar_size,   0},
		{scroll_hndl_size,  0},
		{left_bar, 0},
		{top_bar, 0},
		{right_bar, 0},
		{bottom_bar, 0},
		{width,  ?WIDTH},
		{height, ?HEIGHT},
		{view_width,?WIDTH},
		{view_height,?HEIGHT}]).

init(_Opts) ->
    ?verbose("INIT: Opts=~w\n", [_Opts]),
    {ok,Font} = epx_font:match([{size,24}]),
    Ascent = epx:font_info(Font, ascent),
    State = 
	#{ font => Font, 
	   text => "Nirvana",
	   ascent => Ascent,
	   toffs => 0.0,
	   count => 0
	 },
    {ok, State}.

configure(_Rect, State) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    {Sx,_Sy} = epxw:scale(),
    Scale = float(Sx*100),
    Status = io_lib:format("Hello World, Dimension: ~wx~w, Scale: ~.2f%",
			   [epxw:width(),epxw:height(),Scale]),
    epxw:set_status_text(Status),
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
%%
%%
draw(Pixels, _Dirty, State = #{ toffs := T0, count := Count }) ->
    ?verbose("DRAW: T=~f Rect = ~p\n", [T0, _Dirty]),
    epx_gc:set_fill_style(blend),
    lists:foreach(
	fun(A0) ->
		A = (A0 ) rem 360,
		%% A = A0,
		X1 = ?WIDTH*math:cos(math:pi()*(A/180)),
		Y1 = ?HEIGHT*math:sin(math:pi()*(A/180)),
		%% A = (A0 + Count) rem 360,
		%% A = A0,
		%% X1 = ?WIDTH*math:cos(math:pi()*(A/180)),
		%% Y1 = ?HEIGHT*math:sin(math:pi()*(A/180)),
		lists:foreach(
		  fun(I) ->
			  T = I / ?STEP,
			  Xt = ?CX + X1*(T+T0),
			  Yt = ?CY + Y1*(T+T0),
			  J = (I band 3) + 1,
			  Color = 
			      case Count band 3 of
				  0 ->
				      element(J, {blue, red, green, yellow});
				  1 ->
				      element(J, {yellow,blue, red, green});
				  2 ->
				      element(J, {green, yellow,blue, red});
				  3 ->
				      element(J, {red, green, yellow,blue})
			      end,
			  draw_circle(Pixels, Xt, Yt, (T+T0)*?RADIUS, Color)
		  end, lists:seq(0, ?STEP-1))
	end, lists:seq(0, 360, 15)),
    T1 = T0 + ?DIVIDER,
    timer:send_after(50, draw),
    if T1 >= (1/?STEP) ->
	    State#{ toffs => ?DIVIDER, count => Count + 1 };
       true ->
	    State#{ toffs => T1 }
    end.



select({_Phase,Rect}, State) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    State# { selection => Rect }.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

command(Key, Mod, State) ->
    ?verbose("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.

handle_info(draw, State) ->
    ?verbose("Handle info\n", []),
    epxw:invalidate(),
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


draw_circle(Pixels, X, Y, R, Color) ->
    %% epx_gc:set_border_width(0);
    %% epx_gc:set_border_color(black),
    %% epx_gc:set_border_width(Bw)
    epx_gc:set_fill_color(Color),
    epx:draw_ellipse(Pixels, {X-R,Y-R,R,R}).
