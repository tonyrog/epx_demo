%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%     Present Three views src, dst and result
%%% @end
%%% Created : 27 Nov 2010 by Tony Rogvall <tony@rogvall.se>

-module(epx_fade).

-compile(export_all).

%%
%% Create a window 100x380 with three picture frames
%% one src, one dst and one result
%%  
%%
start() ->
    epx:start(),
    spawn_link(fun() -> init() end).

set_color(RGB,Time) ->
    epx_fade ! {set_color,RGB,Time}.

init() ->
    register(?MODULE, self()),
    Win = epx:window_create(30, 30, 640, 480),
    epx:window_attach(Win),
    Pixels = epx:pixmap_create(640,480,argb),
    epx:pixmap_attach(Pixels),
    RGB = {255,0,0},
    RGBW = {255,0,0,255},
    C0 = from_rgb(RGB),
    Tm = erlang:system_time(milli_seconds),
    draw_color(Win,Pixels,{RGB,RGBW,RGB,RGB}),
    loop(Win,Pixels,C0,C0,Tm,0,0,infinity).

loop(Win,Pixels,C0,C1,Tm0,Ti,Time,Tmo) ->
    receive
	{set_color,RGB3,Time1} when is_integer(Time1), Time1 >= 0 ->
	    C3 = from_rgb(RGB3),
	    Tm1 = erlang:system_time(milli_seconds),
	    %% io:format("hsx2=~w\n", [C2]),
	    C2 =
		if Ti < Time ->
			Td = Tm1 - Tm0,
			Ti1 = min(Ti+Td,Time),
			T = Ti1 / Time,  %% (0 - 1)
			current_color(C0,C1,T);
		   true ->
			C1
		end,
	    io:format("from=~w to=~w\n", [C2,C3]),
	    loop(Win,Pixels,C2,C3,Tm1,0,Time1,50)
    after Tmo ->
	    if Ti < Time ->
		    Tm1 = erlang:system_time(milli_seconds),
		    Td = Tm1 - Tm0,
		    Ti1 = min(Ti+Td,Time),
		    T = Ti1 / Time,  %% (0 - 1)
		    C2 = current_color(C0,C1,T),
		    RGB2 = to_rgb(C2),
		    draw_color(Win,Pixels,RGB2),
		    loop(Win,Pixels,C0,C1,Tm1,Ti1,Time,Tmo);
	       true ->
		    loop(Win,Pixels,C0,C1,Tm0,0,0,infinity)
	    end
    end.

draw_color(Win,Pixels,{RGB1,RGBW,RGB2,RGB3}) ->
    Width = 640,
    Height = 480,
    Width2 = Width div 2,
    H4 = 480 div 4,
    epx_gc:set_fill_style(solid),
    epx_gc:set_border_width(0),

    %% hsl part
    X0 = 0,
    Y0 = 0,
    epx_gc:set_fill_color(RGB1),
    epx:draw_rectangle(Pixels, X0, Y0, Width, H4),
    epx:draw_line(Pixels, X0, Y0+H4-1, Width-1, Y0+H4-1),
    %% hsl part / rgbw
    {R,G,B,W} = RGBW,
    X1 = X0,
    Y1 = Y0+H4,
    epx_gc:set_fill_color({R,G,B}),
    epx:draw_rectangle(Pixels, X0, Y1, Width2, H4),

    epx_gc:set_fill_color({W,W,W}),
    epx:draw_rectangle(Pixels, Width2, Y1, Width2, H4),

    epx:draw_line(Pixels, X0, Y1+H4-1, Width-1, Y1+H4-1),

    %% hsv part
    X2 = X1,
    Y2 = Y1+H4,
    epx_gc:set_fill_color(RGB2),
    epx:draw_rectangle(Pixels, X2, Y2, Width, H4),
    epx:draw_line(Pixels, X2, Y2+H4-1, Width-1, Y2+H4-1),

    %% rgb part
    X3 = X2,
    Y3 = Y2+H4,
    epx_gc:set_fill_color(RGB3),
    epx:draw_rectangle(Pixels, X3, Y3, Width, H4),

    epx:pixmap_draw(Pixels, Win, 0, 0, 0, 0, Width, Height).

    
to_rgb({C1,C2,C3}) ->
    {to_rgb(hsl,C1),epx_color:hsl_to_rgbw(C1),
     to_rgb(hsv,C2),to_rgb(rgb,C3)}.

to_rgb(hsl,C) ->
    epx_color:hsl_to_rgb(C);
to_rgb(hsv,C) ->
    epx_color:hsv_to_rgb(C);
to_rgb(rgb,{R,G,B}) ->
    {trunc(R),trunc(G),trunc(B)}.

from_rgb(RGB) ->
    {from_rgb(hsl,RGB),from_rgb(hsv,RGB),from_rgb(rgb,RGB)}.

from_rgb(hsl,RGB) ->
    epx_color:rgb_to_hsl(RGB);
from_rgb(hsv,RGB) ->
    epx_color:rgb_to_hsv(RGB);
from_rgb(rgb,RGB) ->
    RGB.
    
current_color({A1,A2,A3},{B1,B2,B3},T) when T =< 1.0 ->
    {current_color(hsl,A1,B1,T),
     current_color(hsv,A2,B2,T),
     current_color(rgb,A3,B3,T)}.

%% Calculate the current HSx value
current_color(rgb,{R0,G0,B0},{R1,G1,B1},T) when T =< 1.0 ->
    {R0+(R1-R0)*T,G0+(G1-G0)*T,B0+(B1-B0)*T}; 
current_color(_ColorSpace,{H0,S0,X0},{H1,S1,X1},T) when T =< 1.0 ->
    Ha = abs(H1-H0),
    Hd = if H0 < H1 ->
		 if Ha > 180 -> -(360 - Ha); true -> Ha end;
	    true ->
		 if Ha > 180 -> (360 - Ha); true -> -Ha end
	 end,
    H = fmod(H0 + Hd*T + 360, 360),
    %% io:format("H = ~w\n", [H]),
    {H, S0 + (S1-S0)*T, X0 + (X1-X0)*T}.

fmod(A, B) when is_number(A), is_number(B), B =/= 0 ->
    AB = abs(A / B),
    C = (AB - trunc(AB))*abs(B),
    if A < 0 -> -C;
       true -> C
    end.
