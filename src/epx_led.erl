%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Attempt to draw a LED in various colors off and levels
%%% @end
%%% Created : 13 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(epx_led).

-export([start/0]).
-export([show/2]).

start() ->
    show(red, 0.5).

show(Color, Brightness) ->
    application:ensure_all_started(epx),
    epx_view:show(
      fun(Pixels) ->
	      epx:pixmap_fill(Pixels, black),
	      W = epx:pixmap_info(Pixels, width),
	      H = epx:pixmap_info(Pixels, height),
	      R = W div 20,
	      Xc = W div 2,
	      Yc = H div 2,
	      Gray = {90,90,90},
	      draw_circle(Pixels, Xc, Yc, R, Gray, solid),
	      DkGray = {75,75,75},
	      draw_circle(Pixels, Xc, Yc, R/7, DkGray, solid),

	      D1 = R/4,
	      LtGray1 = {200,200,200},
	      draw_circle(Pixels, Xc+D1, Yc-D1, R/6, LtGray1, solid),
	      DkGray = {75,75,75},

	      LtGray2 = {200,200,200},
	      draw_circle(Pixels, Xc-2*D1, Yc, R/8, LtGray2, solid),
	      DkGray = {75,75,75},

	      LtGray3 = {220,220,220},
	      draw_circle(Pixels, Xc, Yc+2*D1, R/8, LtGray3, solid),
	      DkGray = {75,75,75},

	      L = case get(brightness) of
		      undefined -> Brightness;
		      L0 -> 
			  L1 = L0 + 0.1,
			  if L1 >= 1.0 -> 0.1;
			     true -> L1
			  end
		  end,
	      put(brightness, L),

	      RGB = get_color(Color),
	      ARGB = set_brightness(L, RGB),
	      %% io:format("blend color = ~p\n", [ARGB]),
	      draw_circle(Pixels, Xc, Yc, R, ARGB, blend),

	      ARGB2 = scale_color(L, RGB),
	      draw_circle(Pixels, Xc, Yc, 2*R, ARGB2, blend),

	      ok
      end, {30,30,640,480}, [key_press, button_press, configure]).

set_brightness(Brightness, Color) when is_number(Brightness),
				       Brightness >= 0,
				       Brightness =< 1 ->
    {R,G,B} = get_color(Color),
    A = trunc(min(255, Brightness*255)),
    {A,R,G,B}.

scale_color(Brightness, Color) ->
    {H,S,_L} = epx_color:rgb_to_hsl(Color),
    A = trunc(min(255, Brightness*255)),
    {R,G,B} = epx_color:hsl_to_rgb({H,S,Brightness}),
    {min(90,A),R,G,B}.

get_color(Name) when is_list(Name); is_atom(Name) ->
    epx_color:from_name(Name);
get_color(Color={_R,_G,_B}) ->
    Color.

draw_circle(Pixels, Xc, Yc, R, Color, Style) ->
    epx_gc:set_fill_style(Style),
    epx_gc:set_fill_color(Color),
    epx:draw_ellipse(Pixels, {Xc-R,Yc-R,2*R,2*R}).
