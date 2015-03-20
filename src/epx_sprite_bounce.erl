%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     epx sprite demo drawing kit
%%% @end
%%% Created : 27 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------

-module(epx_sprite_bounce).

-export([main/0]).
-export([start/0]).

-compile(export_all).
-include_lib("epx/include/epx_image.hrl").

-define(NUM_SPRITES, 100).

start() ->
    start(640, 480, 100).

start(Width,Height,N) ->
    epx:start(),
    spawn(fun() -> main(Width,Height,N) end).

main() ->
    main(640, 480, 100).

main(Width,Height,N) when is_integer(N) ->
    main(Width,Height,[randomi(20, 32) || _ <- lists:seq(1, N)]);
main(Width,Height,Ws) when is_list(Ws) ->
    {ok,Game} = epx_sprite:start_link([{width,Width},{height,Height},
				       {color,{255,0,0,255}},
				       {format, argb},
				       {interval, 50}]),
    [begin
	 Side = (W-10)*2 + 1,
	 X = randomi(0, Width-W-1),
	 Y = randomi(0, Height-W-1),
	 {ok, S} = epx_sprite:create(Game, Side, Side,
				     [{x,X},
				      {y,Y},
				      {wrap,false}
				     ]),
	 {_,R,G,B} = random_rgb(),
	 Color = {127,R,G,B},
	 circle(Game, S, Side, Side, Color),
	 Vx = randomf(-20, 20),
	 Vy = randomf(-20, 20),
	 epx_sprite:set_velocity(Game, S, Vx, Vy),
	 Radius = W/2,
	 Volume = volume(Radius),
	 M = Volume*math:pow(2, W),
	 io:format("W=~w, Mass = ~w\n", [W,M]),
	 epx_sprite:set_mass(Game, S, M)
	 %% Va = randomi(-15, 15),
	 %% epx_sprite:set_rotation_velocity(Game, S, Va)
     end || W <- Ws].

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
    random:uniform((B - A) + 1) - 1 + A.

randomf(A, B) when is_number(A), is_number(B), A =< B ->
    random:uniform()*(B-A) + A.
