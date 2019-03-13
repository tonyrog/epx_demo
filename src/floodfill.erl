%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Flood fill algorithm
%%% @end
%%% Created : 27 Dec 2018 by Tony Rogvall <tony@rogvall.se>

-module(floodfill).
-compile(export_all).

circle_fill_1() ->
    circle_fill_(640,480,fun fill1/5).

circle_fill_2() ->
    circle_fill_(640,480,fun fill2/5).

circle_fill_3() ->
    circle_fill_(640,480,fun fill3/5).

circle_fill_(Width,Height,Fun) ->
    start(1,
	  fun(Pixmap) ->
		  BgColor = {255,0,0,0},  %% black
		  Color   = {255,255,0,0}, %% red
		  epx:pixmap_fill(Pixmap, BgColor),
		  epx_gc:set_fill_style(none),
		  epx_gc:set_border_color(Color),
		  epx_gc:set_foreground_color(Color),

		  {Xc,Yc} = draw_pattern(Pixmap),

		  T0 = erlang:system_time(milli_seconds),
		  Fun(Pixmap, Xc, Yc, BgColor, Color),
		  T1 = erlang:system_time(milli_seconds),
		  io:format("draw time ~p  = ~w ms\n",
			    [erlang:fun_info(Fun,name), T1-T0])
	  end, Width, Height).

draw_pattern(Pixmap) ->
    Width = epx:pixmap_info(Pixmap, width),
    Height = epx:pixmap_info(Pixmap, height),
    Side = min(Width,Height) - 10,

    Radius = Side div 2,
    Xc = Width div 2,
    Yc = Height div 2,

    EyeRadius = Radius div 5,
    EyeOffset = EyeRadius*2,
    NoseRadius = Radius div 6,

    draw_circle(Pixmap, Xc-EyeOffset, Yc-EyeOffset, EyeRadius),
    draw_circle(Pixmap, Xc+EyeOffset, Yc-EyeOffset, EyeRadius),
    draw_circle(Pixmap, Xc, Yc, NoseRadius),
    draw_circle(Pixmap, Xc, Yc, Radius),
    {Xc,Yc+NoseRadius+1}.

draw_circle(Pixmap,X,Y,R) ->
    Side = R+R,
    epx:draw_ellipse(Pixmap, X-R, Y-R, Side, Side).

fill1(Pixmap, X, Y, Target, Color) ->
    W = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),
    fill1(Pixmap, X, Y, W, H, Target, Color).

fill1(_Pixmap, _X, _Y, _W, _H, Target, Target) -> ok;
fill1(Pixmap, X, Y, W, H, Target, Color) ->
    case empty_pixel(Pixmap, X, Y, W, H, Target) of
	true ->
	    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
	    fill1(Pixmap, X, Y-1, W, H, Target, Color),
	    fill1(Pixmap, X, Y+1, W, H, Target, Color),
	    fill1(Pixmap, X+1, Y, W, H, Target, Color),
	    fill1(Pixmap, X-1, Y, W, H, Target, Color);
	false ->
	    ok
    end.

fill2(Pixmap, X, Y, Target, Color) ->
    W = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),
    fill2(Pixmap, X, Y, W, H, Target, Color).

fill2(_Pixmap, _X, _Y, _W, _H, Target, Target) -> ok;
fill2(Pixmap, X, Y, W, H, Target, Color) ->
    case empty_pixel(Pixmap,X,Y,W,H,Target) of
	true ->
	    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
	    Q = queue:from_list([{X,Y}]),
	    fill2_(Pixmap,Q,W,H,Target,Color);
	false ->
	    ok
    end.

fill2_(Pixmap,Q,W,H,Target,Color) ->
    case queue:out(Q) of
	{empty,_} -> ok;
	{{value,{X,Y}}, Q1} ->
	    Q2 = add_pixel(Pixmap,X-1,Y,W,H,Target,Color,Q1),
	    Q3 = add_pixel(Pixmap,X+1,Y,W,H,Target,Color,Q2),
	    Q4 = add_pixel(Pixmap,X,Y-1,W,H,Target,Color,Q3),
	    Q5 = add_pixel(Pixmap,X,Y+1,W,H,Target,Color,Q4),
	    fill2_(Pixmap,Q5,W,H,Target,Color)
    end.


fill3(Pixmap, X, Y, Target, Color) ->
    W = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),
    fill3(Pixmap, X, Y, W, H, Target, Color).

fill3(_Pixmap, _X, _Y, _W, _H, Target, Target) -> ok;
fill3(Pixmap, X, Y, W, H, Target, Color) ->
    case empty_pixel(Pixmap,X,Y,W,H,Target) of
	true ->
	    Q = queue:from_list([{X-1,Y,-1},{X,Y,1}]),
	    fill3_(Pixmap,Q,W,H,Target,Color);
	false ->
	    ok
    end.

fill3_(Pixmap,Q,W,H,Target,Color) ->
    case queue:out(Q) of
	{empty,_} -> ok;
	{{value,{X,Y,Dx}}, Q0} ->
	    io:format("fill3 x=~w,y=~w,dx=~w\n", [X,Y,Dx]),
	    case empty_pixel(Pixmap,X,Y,W,H,Target) of
		true ->
		    Above = empty_pixel(Pixmap,X,Y-1,W,H,Target),
		    Below = empty_pixel(Pixmap,X,Y+1,W,H,Target),
		    Q1 = scan_line(Pixmap,X,X,Y,Dx,W,H,Target,Q0,Above,Below),
		    fill3_(Pixmap,Q1,W,H,Target,Color);
		false ->
		    fill3_(Pixmap,Q,W,H,Target,Color)
	    end
    end.


scan_line(Pixmap,X0,X,Y,Dx,W,H,Target,Q,Above,Below) ->
    X1 = X+Dx,
    Empty  = empty_pixel(Pixmap,X1,Y,W,H,Target),
    Above1 = empty_pixel(Pixmap,X1,Y-1,W,H,Target),
    Below1 = empty_pixel(Pixmap,X1,Y+1,W,H,Target),
    Q1 = if not Above1 and Above ->
		 queue:in({X,Y-1,1}, Q);
	    true ->
		 Q
	 end,
    Q2 = if not Below1 and Below ->
		 queue:in({X,Y+1,1}, Q1);
	    true ->
		 Q1
	 end,
    if not Empty ->
	    epx:draw_line(Pixmap,X0,Y,X,Y),
	    Q2;
       true ->
	    scan_line(Pixmap,X0,X1,Y,Dx,W,H,Target,Q2,Above1,Below1)
    end.


empty_pixel(Pixmap,X,Y,W,H,Target) ->
    if X < 0 -> false;
       X >= W -> false;
       Y < 0 -> false;
       Y >= H -> false;
       true ->
	    epx:pixmap_get_pixel(Pixmap, X, Y) =:= Target
    end.

add_pixel(Pixmap,X,Y,W,H,Target,Color,Q) ->
    case empty_pixel(Pixmap,X,Y,W,H,Target) of
	true ->
	    epx:pixmap_put_pixel(Pixmap,X,Y,Color),
	    queue:in({X,Y}, Q);
	false -> Q
    end.

start(N, Func, W, H) ->
    epx:start(),
    Win = epx:window_create(50,50,W,H,[button_press,key_press]),
    epx:window_attach(Win),
    Pix = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Pix, white),
    epx:pixmap_attach(Pix),
    loop(N, Func, Win, Pix, W, H),
    recv_loop(N+1, Func, Win, Pix, W, H).

recv_loop(I, Func, Win, Pix, W, H) ->
    receive
	{epx_event,Win,{button_press,[left],{_X,_Y,_Z}}} ->
	    exec(Func, Win, Pix, W, H),
	    recv_loop(I+1, Func, Win, Pix, W, H);
	{epx_event,Win, close} ->
	    epx:window_detach(Win),
	    epx:pixmap_detach(Pix),
	    ok
    end.

loop(0, _Func, _Win, _Pix, _W, _H) ->
    ok;
loop(I, Func, Win, Pix, W, H) ->
    exec(Func, Win, Pix, W, H),
    loop(I-1, Func, Win, Pix, W, H).

exec(Func, Win, Pix, W, H) ->
    epx:pixmap_fill(Pix, {255,255,255,255}),
    Func(Pix),
    epx:pixmap_draw(Pix,Win,0,0,0,0,W, H),
    epx:sync(Pix,Win).
