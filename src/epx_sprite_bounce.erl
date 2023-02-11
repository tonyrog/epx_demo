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
%% -include_lib("epx/include/epx_image.hrl").
-include("sprite.hrl").

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
				       {interval, 50},
				       {damping,0.99}
				      ]),
    Dir = code:priv_dir(epx_demo),
    {ok,Img} = epx_image:load(filename:join(Dir,"astroid.png")),
    [AstroPixels] = epx_image:pixmaps(Img),
    N = length(Ws),
    [begin
	 U = rand:uniform(N),
	 Anti = U < N div 4,
	 %% Anti = false,
	 W1 = if Anti -> 24;
		 true -> W
	      end,
	 Side = (W1-10)*2 + 1,
	 X = randomi(0, Width-W1-1),
	 Y = randomi(0, Height-W1-1),

	 Color = 
	     if Anti -> {255,255,0,0};
		true -> 
		     {_,R,G,B} = random_rgb(),
		     {127,R,G,B}
	     end,
	 {Va,Af} = if Anti -> {0.0, fun to_accel/1};
		      true -> {float(7-rand:uniform(15)), undefined}
		  end,
	 Vx = randomf(-10, 10),
	 Vy = randomf(-10, 10),
	 Radius = W1/2,
	 Scale1 = math:pow(2, W1/3),
	 Scale2 = math:pow(2, W1/2),
	 Volume = volume(Radius),
	 M = if Anti -> -Volume*Scale2;
		true -> Volume*Scale1
	     end,
	 {ok, ID} = epx_sprite:create(Game, Side, Side,
				      [{x,X},
				       {y,Y},
				       {vx,Vx},
				       {vy,Vy},
				       {va,Va},
				       {m, M},
				       {anglef,Af},
				       {wrap,false}
				      ]),
	 if Anti ->
		 rectangle(Game, ID, Side, Side, Color);
	    true ->
		 epx_sprite:set_image(Game, ID, AstroPixels)
	 end
     end || W <- Ws].

to_accel(S) ->
    math:atan2(S#sprite.vy, S#sprite.vx)*(180/math:pi()).
%%    math:atan2(S#sprite.ax, S#sprite.ay)*(180/math:pi()).

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
