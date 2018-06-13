%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     epx sprite demo drawing kit
%%% @end
%%% Created : 27 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

-module(epx_sprite_planet).

-export([main/0]).
-export([start/0]).

-compile(export_all).
-include_lib("epx/include/epx_image.hrl").

start() ->
    start(640, 480, 5).

start(Width,Height,N) ->
    epx:start(),
    spawn(fun() -> main(Width,Height,N) end).

main() ->
    main(640, 480, 5).

main(Width,Height,N) when is_integer(N) ->
    main(Width,Height,[randomi(10,30) || _ <- lists:seq(1, N)]);
main(Width,Height,Rs) when is_list(Rs) ->
    {ok,Game} = epx_sprite:start_link([{width,Width},{height,Height},
				       {color,{255,0,0,255}},
				       {format, argb},
				       {interval, 50}]),
    Sx = (Width-65) div 2,
    Sy = (Height-65) div 2,
    {ok, Sun} = epx_sprite:create(Game, 65, 65,
				  [{x,Sx},{y,Sy},{wrap,false}]),
    Sm = volume(64)*math:pow(2, 33),
    epx_sprite:set_mass(Game, Sun, Sm),
    circle(Game, Sun, 65, 65, yellow),
    [begin
	 Dist = 70+randomf(0,min(Width,Height)/4-70), %% distance from sun
	 Angle = randomf(0, 2*math:pi()),
	 Ca    = math:cos(Angle),
	 Sa    = math:sin(Angle),
	 Xc = Dist*Ca,
	 Yc = Dist*Sa,
	 X  = Sx + (Xc - R),
	 Y  = Sy + (Yc - R),
	 Side = R*2 + 1,
	 {ok, S} = epx_sprite:create(Game, Side, Side,
				     [{x,X},{y,Y},{wrap,false}]),
	 {_,Rc,Gc,Bc} = random_rgb(),
	 Color = {127,Rc,Gc,Bc},
	 circle(Game, S, Side, Side, Color),
	 V  = randomf(40,80),
	 Vx = -Sa*V,
	 Vy = Ca*V,
	 epx_sprite:set_velocity(Game, S, Vx, Vy),
	 M = volume(R)*math:pow(2, R),
	 io:format("R=~w, Mass = ~w\n", [R,M]),
	 epx_sprite:set_mass(Game, S, M)
	 %% Va = randomi(-15, 15),
	 %% epx_sprite:set_rotation_velocity(Game, S, Va)
     end || R <- Rs].


volume(R) ->
    (R*R*R)*math:pi()*4/3.


rectangle(Game, S, Width, Height, Color) ->
    epx_sprite:update_pixels(
      Game, S, fun(Pixmap) ->
		       epx:pixmap_fill(Pixmap,{0,0,0,0}),  %% transparent
		       epx_gc:set_fill_style(solid),
		       epx_gc:set_fill_color(Color),
		       epx:draw_rectangle(Pixmap, 0, 0, Width, Height)
	       end).
    

circle(Game, S, Width, Height, Color) ->
    epx_sprite:update_pixels(
      Game, S, fun(Pixmap) ->
		       epx:pixmap_fill(Pixmap,{0,0,0,0}),  %% transparent
		       epx_gc:set_fill_style(solid),
		       epx_gc:set_fill_color(Color),
		       epx:draw_ellipse(Pixmap, 0, 0, Width, Height)
		       %% epx_gc:set_foreground_color(black),
		       %% Y = Height div 2,
		       %% epx:draw_line(Pixmap, 0, Y, Width, Y)
	       end).

random_rgb() ->
    { 255, randomi(0,255), randomi(0,255), randomi(0,255) }.

random_argb() ->
    { randomi(0,255), randomi(0,255), randomi(0,255), randomi(0,255) }.

randomi(A, B) when is_integer(A), is_integer(B), A =< B ->   
    rand:uniform((B - A) + 1) - 1 + A.

randomf(A, B) when is_number(A), is_number(B), A =< B ->
    rand:uniform()*(B-A) + A.
