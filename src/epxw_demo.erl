%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Demo showing off epxw
%%% @end
%%% Created : 20 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(epxw_demo).
-behavious(epxw).

-include_lib("epx/include/epx_menu.hrl").

-export([start/0]).
-export([init/3,
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
	 menu/2
	]).

%% profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 %% menu_info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = grey5,   %% light
	 menu_background_color         = grey10,  %% dark
	 menu_border_color             = green
	}).

-define(verbose(F,A), ok).
%%-define(verbose(F,A), io:format((F),(A))).

-define(VIEW_WIDTH,  720).
-define(VIEW_HEIGHT, 720).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
-define(BORDER, 2).

menu(green) ->
    [
     {"Green", ""}
    ];
menu(blue) ->
    [
     {"Blue", ""}
    ];
menu(red) ->
    [
     {"Red", ""}
    ];
menu(yellow) ->
    [
     {"Yellow", ""}
    ];
menu(pink) ->
    [
     {"Green",  "G"},
     {"Blue",   "B"},
     {"Red",    "R"},
     {"Yellow", "Y"}
    ].

start() ->
    application:ensure_all_started(epx),
    epxw:start(#{ module => ?MODULE,
		  %% demo select as fun
		  select => fun({select,Rect={X,Y,W,H}}, State) -> 
				    ?verbose("SELECT FUN: ~w\n", [Rect]),
				    OldRect = maps:get(selection, State),
				    epxw:invalidate(OldRect),
				    epxw:invalidate({X-?BORDER-2,Y-?BORDER-2,
						     W+2*?BORDER+4,
						     H+2*?BORDER+4}),
				    State#{ selection => Rect }
			    end
		},
	       [hello,world], 
	       [{title, "Demo"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   left},    %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar, 32},
		{top_bar, 20},
		{right_bar, 8},
		{width, 256},
		{height, 256},
		{view_width,?VIEW_WIDTH},
		{view_height,?VIEW_HEIGHT}]).

init(_Window, _Screen, _Opts) ->
    ?verbose("INIT: Opts=~w\n", [_Opts]),
    {ok,Font} = epx_font:match([{size,24}]),
    Ascent = epx:font_info(Font, ascent),

    Profile = #profile{},

    MProfile = create_menu_profile(Profile),

    RedMenu = epx_menu:create(MProfile#menu_profile{background_color=red},
			      menu(red)),
    GreenMenu = epx_menu:create(MProfile#menu_profile{background_color=green},
				menu(green)),
    BlueMenu = epx_menu:create(MProfile#menu_profile{background_color=blue},
			       menu(blue)),
    YellowMenu = epx_menu:create(MProfile#menu_profile{background_color=yellow},
				 menu(yellow)),
    PinkMenu = epx_menu:create(MProfile#menu_profile{background_color=pink},
			       menu(pink)),

    Rw = 100, Rh = 100, Dx = 3, Dy = 3,

    #{ font => Font, 
       ascent => Ascent,
       text => "Hello World!",
       red_menu => RedMenu,
       green_menu => GreenMenu,
       blue_menu => BlueMenu,
       yellow_menu => YellowMenu,
       pink_menu => PinkMenu,
       menu_map =>
	   #{ {(?VIEW_WIDTH div 2)-50,(?VIEW_HEIGHT div 2)-50,Rw,Rh} =>
		  {white,PinkMenu},
	      {Dx,Dy,Rw,Rh} => 
		  {red,RedMenu},
	      {Dx,?VIEW_HEIGHT-Rh-Dy,Rw,Rh} =>
		  {green,GreenMenu},
	      {?VIEW_WIDTH-Rw-Dx,Dy,Rw,Rh} => 
		  {blue,BlueMenu},
	      {?VIEW_WIDTH-Rw-Dx,?VIEW_HEIGHT-Rh-Dy,Rw,Rh} => 
		  {yellow,YellowMenu}
	    },
       selection => {0,0,0,0}
     }.

configure(_Event, State) ->
    ?verbose("CONFIGURE: ~w\n", [_Event]),
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

draw(Pixels, _Dirty, 
     State= #{ font := Font, text := Text, ascent := Ascent,
	       selection := Selection }) ->
    ?verbose("DRAW: Rect = ~p\n", [_Dirty]),

    epx_gc:set_fill_style(solid),

    maps:fold(
      fun(Rect,{Color,_Menu}, _Acc) ->
	      draw_rect(Pixels, Rect, ?BORDER, Color, Selection)
      end, [], maps:get(menu_map, State)),
    
    epx_gc:set_font(Font),
    {TxW,TxH}  = epx_font:dimension(Font,Text),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    X = (?VIEW_WIDTH - TxW) div 2,
    Y = ((?VIEW_HEIGHT - TxH) div 2) + Ascent,
    epx:draw_string(Pixels, X, Y, Text),    
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
    State.

menu({menu,Pos}, State) ->
    {reply, select_menu(Pos, maps:get(menu_map, State)), State}.
    
%% NOTE! this is overridden by the select fun above
select({select,Rect}, State) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    State# { selection => Rect }.

command(Key, Mod, State) ->
    ?verbose("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

select_menu(Pos, MenuMap) ->
    Iter = maps:iterator(MenuMap),
    select_menu_(Pos, Iter).

select_menu_(_Pos, none) ->
    epxw:edit_menu();
    %% undefined;
select_menu_(Pos, Iter) ->
    {Rect,{_Color,Menu},Iter1} = maps:next(Iter),
    case epx_rect:contains(Rect, Pos) of
	true ->
	    Menu;
	false ->
	    select_menu_(Pos, Iter1)
    end.

draw_rect(Pixels, Rect, Bw, Color, Selection) ->
    case epx_rect:intersect(Rect, Selection) of
	{_,_,0,0} ->
	    epx_gc:set_border_width(0);
	_ ->
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(Bw)
    end,
    epx_gc:set_fill_color(Color),
    epx:draw_rectangle(Pixels, Rect).
